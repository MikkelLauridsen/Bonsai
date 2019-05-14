module Inference
    (
      infer
    ) where

import Ast
import Data.Map.Strict as Map
import Data.Set as Set
import Data.List as List
import Control.Monad.Except
import Control.Monad.State
import Prettifier
import Actions

data TypeClass = TClass String (Type -> Bool)

instance Eq TypeClass where
    TClass name1 _ == TClass name2 _ = name1 == name2

instance Ord TypeClass where
    TClass name1 _ `compare` TClass name2 _ = name1 `compare` name2

instance Show TypeClass where
    show (TClass name _) = name

data Type = PrimT Prim
          | FuncT Type Type
          | TuplT [Type]
          | ListT Type
          | AlgeT TypeId [Type]
          | UniqT Type Bool
          | PolyT TypeVar
          deriving (Eq, Ord)

instance Show Type where
    show (PrimT prim)                = show prim 
    show (FuncT typ1 typ2)           = "(" ++ show typ1 ++ " -> " ++ show typ2 ++ ")"
    show (TuplT typs)                = "(" ++ ([show typ | typ <- init typs] >>= (++ ", ")) ++ show (last typs) ++ ")"
    show (ListT typ)                 = "[" ++ show typ ++ "]"
    show (AlgeT typeId [])           = typeName typeId
    show (AlgeT typeId ps)           = typeName typeId ++ "<" ++ ([show typ' | typ' <- init ps] >>= (++ ", ")) ++ show (last ps) ++ ">"
    show (UniqT typ _)               = show typ ++ "*"
    show (PolyT tvar@(TVar _ _))     = show tvar
    
-- a termconstructor has a name an associated type and optionally a signature
type TermConstructor = (TypeId, Type, Type)

type Sig = Set TermConstructor

type Ups = Map TypeId Type

type Constraint = (Type, Type, UtilData)

type Substitution = Map TypeVar Type

data TypeVar = TVar String [TypeClass]

instance Eq TypeVar where
    TVar name1 _ == TVar name2 _ = name1 == name2

instance Ord TypeVar where
    TVar name1 _ `compare` TVar name2 _ = name1 `compare` name2

instance Show TypeVar where
    show (TVar name []) = name
    show (TVar name classes) = name ++ "<<" ++ ([show class' | class' <- init classes] >>= (++ ", ")) ++ show (last classes) ++ ">>"

data Scheme = ForAll [TypeVar] Type
            | LazyT ExprAST
            | LazyS ExprAST CompTypeAST

instance Show Scheme where
    show (ForAll _ typ) = show typ
    show (LazyT _)      = "e"
    show (LazyS _ _)      = "e::s"

-- type-environment type
newtype TypeEnv = TypeEnv (Map VarId Scheme) deriving Show

-- type-environment binding format
type Binding = (VarId, Scheme)

class Substitutable a where
    ftv :: a -> Set TypeVar
    substitute :: Substitution ->  a -> a

instance Substitutable Type where
    ftv (PrimT _)         = Set.empty
    ftv (FuncT typ1 typ2) = (ftv typ1) `Set.union` (ftv typ2)
    ftv (TuplT typs)      = (List.foldr (Set.union . ftv) Set.empty) typs
    ftv (ListT typ)       = ftv typ
    ftv (AlgeT _ typs)    = (List.foldr (Set.union . ftv) Set.empty) typs
    ftv (UniqT typ _)     = ftv typ
    ftv (PolyT var)       = Set.singleton var

    substitute _ typ@(PrimT _)       = typ
    substitute sub (FuncT typ1 typ2) = FuncT (substitute sub typ1) (substitute sub typ2)
    substitute sub (TuplT typs)      = TuplT [substitute sub typ | typ <- typs]
    substitute sub (ListT typ)       = ListT (substitute sub typ)
    substitute sub (AlgeT name typs) = AlgeT name [substitute sub typ | typ <- typs]
    substitute sub (UniqT typ valid) = UniqT (substitute sub typ) valid
    substitute sub typ@(PolyT var)   = Map.findWithDefault typ var sub

instance Substitutable Scheme where
    ftv (ForAll vars typ) = (ftv typ) `Set.difference` Set.fromList vars
    ftv (LazyT _)         = Set.empty
    ftv (LazyS _ _)       = Set.empty

    substitute sub (ForAll vars typ)  = ForAll vars (substitute (List.foldr Map.delete sub vars) typ)
    substitute sub scheme@(LazyT _)   = scheme
    substitute sub scheme@(LazyS _ _) = scheme

instance Substitutable a => Substitutable [a] where
    ftv = List.foldr (Set.union . ftv) Set.empty

    substitute = fmap . substitute -- http://dev.stephendiehl.com/fun/006_hindley_milner.html

instance Substitutable TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)

    substitute sub (TypeEnv env) = TypeEnv (Map.map (substitute sub) env)

data TypeError = LinearTypeError Type UtilData
               | VariableScopeError VarId UtilData
               | VariableRedefinitionError VarId UtilData
               | TypeRedefinitionError TypeId UtilData
               | TermConstructorRedefinitionError TypeId UtilData
               | UndefinedTermConstructorError TypeId UtilData
               | UndefinedTypeError TypeId UtilData
               | TermConstructorTypeMisuseError TypeId VarId UtilData
               | NotAlgebraicTypeError Type UtilData
               | TermConstructorPatternMisuseError TypeId UtilData
               | TypeClassMismatchError Type Type UtilData
               | TypeVariableClassMismatchError VarId UtilData
               | TypeMismatchError Type Type UtilData
               | UndefinedTypeClassError TypeId UtilData
               | MatchPatternMismatchError Type PatternAST UtilData
               | LengthMismatchError UtilData
               | DebugError String UtilData

data InferState = InferState { 
                               next        :: Integer
                             , globalEnv   :: TypeEnv
                             , constraints :: [Constraint]
                             , sigma       :: Sig
                             , upsilon     :: Ups
                             , lsigma      :: LazySig
                             , debug       :: String
                             }

type InferT a = ExceptT TypeError (State InferState) a

initState = InferState { next = 0, constraints = [], sigma = Set.empty, upsilon = Map.empty, lsigma = Set.empty, debug = "", globalEnv = TypeEnv Map.empty }

runInferT :: InferT Substitution -> Maybe String
runInferT m = case evalState (runExceptT m) initState of
    (Left err) -> Just $ evalError err
    (Right _)  -> Nothing

evalError :: TypeError -> String
evalError (LinearTypeError typ utilData)                      = formatErr ("instance of unique type '" ++ show typ ++ "' cannot be used more than once") utilData
evalError (VariableScopeError varId utilData)                 = formatErr ("variable '" ++ varName varId ++ "' is out of scope") utilData
evalError (VariableRedefinitionError varId utilData)          = formatErr ("global variable '" ++ varName varId ++ "' cannot be redefined globally") utilData
evalError (TypeRedefinitionError typeId utilData)             = formatErr ("algebraic type '" ++ typeName typeId ++  "' cannot be redefined") utilData
evalError (TermConstructorRedefinitionError typeId utilData)  = formatErr ("termconstructor '" ++ typeName typeId ++ "' cannot be redefined") utilData
evalError (UndefinedTermConstructorError typeId utilData)     = formatErr ("unknown termconstructor '" ++  typeName typeId ++ "'") utilData
evalError (UndefinedTypeError typeId utilData)                = formatErr ("unknown type '" ++ typeName typeId ++ "'") utilData
evalError (TermConstructorTypeMisuseError id varId utilData)  = formatErr ("algebraic type '" ++ typeName id ++ "' does not have typevariable '" ++ varName varId ++ "'") utilData
evalError (NotAlgebraicTypeError typ utilData)                = formatErr ("type '" ++ show typ ++ "' cannot be used polymorphically") utilData
evalError (TermConstructorPatternMisuseError typeId utilData) = formatErr ("termconstructor '" ++ typeName typeId ++ "' cannot be used as a constant") utilData
evalError (TypeClassMismatchError typ1 typ2 utilData)         = formatErr ("type mismatch, expected '" ++ show typ1 ++ "' but actual type '" ++ show typ2 ++ "' does not conform to the typeclasses") utilData
evalError (TypeVariableClassMismatchError varId utilData)     = formatErr ("typevariable '" ++ varName varId ++ "' cannot be used with different typeclasses") utilData
evalError (UndefinedTypeClassError typeId utilData)           = formatErr ("unknown typeclass '" ++ typeName typeId ++ "'") utilData
evalError (TypeMismatchError typ1 typ2 utilData)              = formatErr ("type mismatch, could not match expected type '" ++ show typ1 ++ "' with actual type '" ++ show typ2 ++ "'") utilData
evalError (MatchPatternMismatchError typ pat utilData)        = formatErr ("type-pattern mismatch, could not match type '" ++ show typ ++ "' with pattern '" ++ prettyShow pat 0 ++ "'") utilData
evalError (LengthMismatchError utilData)                      = formatErr ("cannot match types of different numbers of immediates") utilData
evalError (DebugError msg utilData)                           = formatErr msg utilData

genTVar :: [TypeClass] -> InferT Type
genTVar classes = do
    state <- get
    put state{ next = next state + 1 }
    return $ PolyT (TVar ("a" ++ show (next state)) classes)

freshTVars :: [TypeVar] -> InferT [Type]
freshTVars [] = return []
freshTVars ((TVar _ classes):ts) = do
    tvar  <- genTVar classes
    tvars <- freshTVars ts
    return (tvar:tvars)

-- error message creation

-- returns a formated error message
-- based on input message and utility data
formatErr :: String -> UtilData -> String
formatErr err UtilData{position=pos, sourceLine=line} = 
    let (l, c, o) = pos
        in (show l ++ ":" ++ show c ++ ": error: " ++ 
            err ++ " in:\n" ++ (Prelude.take (o - 1) (repeat ' ')) ++ 
            "   " ++ line ++ "\n" ++ 
            "   " ++ (getIndicator (o - 1) (length line)))

getIndicator :: Int -> Int -> String
getIndicator offset len = Prelude.take offset (repeat ' ') ++ Prelude.take len (repeat '^')

-- utility functions

except :: TypeEnv -> Binding -> TypeEnv
except (TypeEnv env) (var, scheme) = TypeEnv $ Map.insert var scheme env

getVar :: TypeEnv -> VarId -> Maybe Scheme
getVar (TypeEnv env) varId = Map.lookup varId env

-- checks whether the input termconstructor (by name)
-- is defined in input set of Algebraic types
has :: Sig -> TypeId -> Bool
has sigma t =
    case find (\(t', _, _) -> t' == t) (Set.toList sigma) of
        Nothing            -> False
        (Just (_, _, _)) -> True

getSignature :: Sig -> TypeId -> Maybe Type
getSignature sigma t =
    case find (\(t', _, _) -> t' == t) (Set.toList sigma) of
        Nothing            -> Nothing
        (Just (_, _, typ)) -> Just typ
        
getTermConstructor :: Sig -> TypeId -> Maybe TermConstructor
getTermConstructor sigma t = find (\(t', _, _) -> t' == t) (Set.toList sigma)
        
-- unification

unifyAll :: Substitution -> [Constraint] -> InferT Substitution
unifyAll sub [] = return sub
unifyAll sub cs@((typ1, typ2, utilData):c') = do
    state <- get
    put state{ debug = debug state ++ "\n\n" ++ (List.foldr ((++) . (\(t1,t2,_) -> (show t1 ++ " :: " ++ show t2 ++ "\n"))) "" cs) }
    (sub', c'') <- unify typ1 typ2 utilData c'
    unifyAll (sub' `compose` sub) c''

unify :: Type -> Type -> UtilData -> [Constraint] -> InferT (Substitution, [Constraint])
unify typ1@(PrimT prim1) typ2@(PrimT prim2) utilData c' =
    if prim1 == prim2
        then return (Map.empty, c')
        else throwError $ TypeMismatchError typ1 typ2 utilData

unify (PolyT var1@(TVar _ classes1)) (PolyT var2@(TVar _ classes2)) _ c' = do
    tvar  <- genTVar classes'
    let sub = Map.fromList [(var1, tvar), (var2, tvar)]
    let substituteConstraint = \(t1, t2, utilData) -> (substitute sub t1, substitute sub t2, utilData)
    return (sub, List.map substituteConstraint c')
    where
        classes' = classes1 `List.union` classes2

unify typ1@(PolyT var@(TVar _ classes)) typ2 utilData c' =
    if List.foldr ((&&) . (checkClass typ2)) True classes
        then return (sub, List.map substituteConstraint c')
        else throwError $ TypeClassMismatchError typ1 typ2 utilData
    where
        sub = Map.singleton var typ2
        substituteConstraint = \(t1, t2, ud) -> (substitute sub t1, substitute sub t2, ud)

unify typ1 typ2@(PolyT var@(TVar _ classes)) utilData c' =
    if List.foldr ((&&) . (checkClass typ1)) True classes
        then return (sub, List.map substituteConstraint c')
        else throwError $ TypeClassMismatchError typ2 typ1 utilData
    where
        sub = Map.singleton var typ1
        substituteConstraint = \(t1, t2, ud) -> (substitute sub t1, substitute sub t2, ud)

unify (FuncT s1 s2) (FuncT t1 t2) utilData c' = return (Map.empty, c' ++ [(s1, t1, utilData), (s2, t2, utilData)])

unify typ1@(TuplT typs1) typ2@(TuplT typs2) utilData c' =
    if length typs1 == length typs2
        then return (Map.empty, c' ++ (List.map (\(t1, t2) -> (t1, t2, utilData)) (zip typs1 typs2)))
        else throwError $ TypeMismatchError typ1 typ2 utilData

unify (ListT typ1) (ListT typ2) utilData c' = return (Map.empty, c' ++ [(typ1, typ2, utilData)])

unify typ1@(AlgeT name1 typs1) typ2@(AlgeT name2 typs2) utilData c' = do
    state <- get
    if name1 == name2 && length typs1 == length typs2
        then case Map.lookup name1 (upsilon state) of 
            Just _  -> return (Map.empty, c' ++ (List.map (\(t1, t2) -> (t1, t2, utilData)) (zip typs1 typs2)))
            Nothing -> throwError $ UndefinedTypeError name1 utilData
        else throwError $ TypeMismatchError typ1 typ2 utilData

unify typ1@(UniqT typ1' valid1) typ2@(UniqT typ2' valid2) utilData c' =
    case (valid1, valid2) of
        (False, True)  -> throwError $ LinearTypeError typ1 utilData
        (True, False)  -> throwError $ LinearTypeError typ2 utilData
        _              -> return (Map.empty, c' ++ [(typ1', typ2', utilData)])

-- catch all
unify typ1 typ2 utilData _ = throwError $ TypeMismatchError typ1 typ2 utilData

compose :: Substitution -> Substitution -> Substitution
compose sub1 sub2 = Map.map (substitute sub1) sub2 `Map.union` sub1

checkClass :: Type -> TypeClass -> Bool
checkClass typ (TClass _ fun) = fun typ

genClasses :: [TypeId] -> UtilData -> InferT [TypeClass]
genClasses [] _ = return []
genClasses (t:ts) utilData = do
    c  <- genClass t utilData
    cs <- genClasses ts utilData
    return (c:cs)

genClass :: TypeId -> UtilData -> InferT TypeClass
genClass t utilData =
    case typeName t of
        "Num"  -> return numClass
        "Eq"   -> return eqClass
        "Ord"  -> return ordClass
        "Show" -> return showClass
        "Bi"   -> return biClass
        _      -> throwError $ UndefinedTypeClassError t utilData

numClass  = TClass "Num" numFun 
eqClass   = TClass "Eq" eqFun
ordClass  = TClass "Ord" ordFun
showClass = TClass "Show" showFun
biClass   = TClass "Bi" biFun

numFun :: Type -> Bool
numFun (PrimT IntPrim)          = True
numFun (PrimT FloatPrim)        = True
numFun (PrimT CharPrim)         = True
numFun (UniqT typ _)            = numFun typ
numFun (PolyT (TVar _ classes)) = elem numClass classes
numFun _                        = False

eqFun :: Type -> Bool
eqFun (PrimT _)                 = True
eqFun (FuncT _ _)               = False
eqFun (TuplT typs)              = List.foldr ((&&) . eqFun) True typs
eqFun (ListT typ)               = eqFun typ
eqFun (AlgeT _ typs)            = List.foldr ((&&) . eqFun) True typs
eqFun (PolyT (TVar _ classes))  = elem eqClass classes
eqFun (UniqT typ _)             = eqFun typ

ordFun :: Type -> Bool
ordFun (PrimT IntPrim)          = True
ordFun (PrimT FloatPrim)        = True
ordFun (PrimT CharPrim)         = True
ordFun (ListT typ)              = ordFun typ
ordFun (UniqT typ _)            = ordFun typ
ordFun (PolyT (TVar _ classes)) = elem ordClass classes
ordFun _                        = False

showFun :: Type -> Bool
showFun (PrimT _)                = True
showFun (FuncT _ _)              = False
showFun (TuplT typs)             = List.foldr ((&&) . showFun) True typs
showFun (ListT typ)              = eqFun typ
showFun (AlgeT _ typs)           = List.foldr ((&&) . showFun) True typs
showFun (PolyT (TVar _ classes)) = elem showClass classes
showFun (UniqT typ _)            = showFun typ

biFun :: Type -> Bool
biFun (PrimT IntPrim)            = True
biFun (PrimT CharPrim)           = True
biFun (UniqT typ _)              = biFun typ
biFun (PolyT (TVar _ classes))   = elem biClass classes
biFun _                          = False

-- constraint rules begin

addConstraint :: Type -> Type -> UtilData -> InferT ()
addConstraint typ1 typ2 utilData = do
    state <- get
    put state{ constraints = constraints state ++ [(typ1, typ2, utilData)] }

addConstraints :: [Constraint] -> InferT ()
addConstraints constraints' = do
    state <- get
    put state{ constraints = constraints state ++ constraints' }

proj :: Scheme -> InferT Type
proj (ForAll vars typ) = do
    vars' <- mapM fresh vars
    let s = Map.fromList (zip vars vars')
    return $ substitute s typ
    where
        fresh = \(TVar _ classes) -> genTVar classes

gen :: TypeEnv -> Type -> Scheme -- http://dev.stephendiehl.com/fun/006_hindley_milner.html
gen env typ = ForAll vars typ
    where
        vars = Set.toList (ftv typ `Set.difference` ftv env)

stdin'  = (VarId "stdin" Untyped, ForAll [] (UniqT (PrimT FilePrim) True))
stdout' = (VarId "stdout" Untyped, ForAll [] (UniqT (PrimT FilePrim) True))
initEnv = TypeEnv $ Map.fromList [stdin', stdout']

infer :: FilePath -> ProgAST -> Maybe String
infer path ast =
    case runInferT (inferProg ast) of
        Nothing  -> Nothing
        Just msg -> Just (path ++ ":" ++ msg)

inferProg :: ProgAST -> InferT Substitution -- TODO: typeDcl and main!
inferProg (ProgAST dt dv _) = do
    inferTypeDclLazily dt
    inferTypeDcl dt
    inferVarDclLazily dv
    inferVarDcl dv
    state <- get
    unifyAll Map.empty (constraints state)

inferTypeDclLazily :: TypeDclAST -> InferT ()
inferTypeDclLazily EpsTypeDclAST = return ()
inferTypeDclLazily (TypeDclAST name _ dt utilData) = do
    state <- get
    case find (\(name', _) -> name' == name) (Set.toList (lsigma state)) of
        Just _ -> throwError $ TypeRedefinitionError name utilData
        Nothing -> do
            put state{ lsigma = Set.insert (name, []) (lsigma state), upsilon = Map.insert name (AlgeT name []) (upsilon state) } 
            inferTypeDclLazily dt

inferTypeDclLazily (TypePolyDclAST name polys _ dt utilData) = do
    state <- get
    case find (\(name', _) -> name' == name) (Set.toList (lsigma state)) of
        Just _ -> throwError $ TypeRedefinitionError name utilData
        Nothing -> do
            tvars <- mapVars vars
            put state{ lsigma = Set.insert (name, vars) (lsigma state), upsilon = Map.insert name (AlgeT name tvars) (upsilon state) }  
            inferTypeDclLazily dt
    where
        vars = List.map varName polys

inferTypeDcl :: TypeDclAST -> InferT ()
inferTypeDcl EpsTypeDclAST = return ()
inferTypeDcl (TypeDclAST name cons dt utilData) = do
    evalCons cons name Map.empty utilData
    inferTypeDcl dt

inferTypeDcl (TypePolyDclAST name polys cons dt utilData) = do
    typ <- getAlgebraicType name utilData
    case typ of
        AlgeT _ tvars -> do 
            let binds = List.foldr (\(k, v) -> Map.insert k v) Map.empty $ zip polys tvars
            _ <- evalCons cons name binds utilData
            inferTypeDcl dt
        _ -> error "type is not algebraic" -- should not happen

getAlgebraicType :: TypeId -> UtilData -> InferT Type
getAlgebraicType name utilData = do
    state <- get
    case Map.lookup name (upsilon state) of
        Just typ -> return typ
        Nothing  -> throwError $ UndefinedTypeError name utilData

mapVars :: [String] -> InferT [Type]
mapVars [] = return []
mapVars (v:vs) = do
    tvar <- genTVar []
    tvars <- mapVars vs
    return (tvar:tvars)

evalCons :: [ConsAST] -> TypeId -> Map VarId Type -> UtilData -> InferT (Map VarId Type)
evalCons [] _ binds _ = return binds
evalCons (tc:tcs') memberName binds utilData = do
    state <- get
    memberTyp <- getAlgebraicType memberName utilData
    case tc of
        (SingleConsAST name utilData) ->
            if (sigma state) `has` name
                then throwError $ TermConstructorRedefinitionError name utilData
                else do
                    put state{ sigma = Set.insert (name, memberTyp, memberTyp) (sigma state) }
                    evalCons tcs' memberName binds utilData
        (DoubleConsAST name s utilData) ->
            if (sigma state) `has` name
                then throwError $ TermConstructorRedefinitionError name utilData
                else do
                    sig <- buildSignature s memberName binds
                    put state{ sigma = Set.insert (name, memberTyp, FuncT sig memberTyp)  (sigma state) }
                    evalCons tcs' memberName binds utilData

types :: CompTypeAST -> Map VarId Type -> InferT (Type, Map VarId Type)
types (CompSimpleAST typeId utilData) binds = do
    typ <- lazyIdToTypes typeId utilData
    return (typ, binds)

types (CompSimplePolyAST varId _) binds =
    case Map.lookup varId binds of
        Just tvar -> return (tvar, binds)
        Nothing   -> do
            tvar <- genTVar []
            return (tvar, Map.insert varId tvar binds)

types (CompClssAST varId classNames utilData) binds =
    case Map.lookup varId binds of
        Just typ@(PolyT (TVar _ classes)) -> do 
            classes' <- genClasses classNames utilData
            if classes == classes'
                then return (typ, binds)
                else throwError $ TypeVariableClassMismatchError varId utilData 
        _ -> do
            classes <- genClasses classNames utilData
            tvar <- genTVar classes
            return (tvar, Map.insert varId tvar binds)

types (CompPolyAST typeId comps' utilData) binds = do
    typ <- lazyIdToTypes typeId utilData
    case typ of
        AlgeT _ typs ->
            if length typs == length comps'
                then do
                    (typs', binds') <- typesList comps' binds
                    return (AlgeT typeId typs', binds')
                else throwError $ LengthMismatchError utilData
        _ -> throwError $ NotAlgebraicTypeError typ utilData

types (CompListAST comp' _) binds = do
    (typ, binds') <- types comp' binds
    return (ListT typ, binds')

types (CompTupleAST comps' _) binds = do
    (typs, binds') <- typesList comps' binds
    return (TuplT typs, binds')

types (CompFuncAST comp1' comp2' _) binds = do
    (typ1, binds')  <- types comp1' binds
    (typ2, binds'') <- types comp2' binds'
    return (FuncT typ1 typ2, binds'')

typesList :: [CompTypeAST] -> Map VarId Type -> InferT ([Type], Map VarId Type)
typesList [] binds = return ([], binds)
typesList (comp:comps) binds = do
    (typ, binds')   <- types comp binds
    (typs, binds'') <- typesList comps binds'
    return (typ:typs, binds'')

buildSignature :: CompTypeAST -> TypeId -> Map VarId Type -> InferT Type
buildSignature (CompSimpleAST typeId utilData) _ _ = do 
    typ <- lazyIdToTypes typeId utilData
    return typ

buildSignature (CompSimplePolyAST varId utilData) memberName binds =
    case Map.lookup varId binds of
        Just tvar -> return tvar
        Nothing   -> throwError $ TermConstructorTypeMisuseError memberName varId utilData

buildSignature (CompPolyAST typeId comps' utilData) memberName binds = do
    typ <- lazyIdToTypes typeId utilData
    case typ of
        AlgeT _ typs ->
            if length typs == length comps'
                then do
                    typs' <- buildSignatureList comps' memberName binds
                    return $ AlgeT typeId typs'
                else throwError $ LengthMismatchError utilData
        _ -> throwError $ NotAlgebraicTypeError typ utilData

buildSignature (CompListAST comp' _) memberName binds = do
    typ <- buildSignature comp' memberName binds
    return $ ListT typ

buildSignature (CompTupleAST comps' _) memberName binds = do
    typs <- buildSignatureList comps' memberName binds
    return $ TuplT typs

buildSignature (CompFuncAST comp1' comp2' _) memberName binds = do
    typ1 <- buildSignature comp1' memberName binds
    typ2 <- buildSignature comp2' memberName binds
    return $ FuncT typ1 typ2

buildSignatureList :: [CompTypeAST] -> TypeId -> Map VarId Type -> InferT [Type]
buildSignatureList [] _ binds = return []
buildSignatureList (comp:comps') memberName binds = do
    typ  <- buildSignature comp memberName binds
    typs <- buildSignatureList comps' memberName binds
    return (typ:typs)

lazyIdToTypes :: TypeId -> UtilData -> InferT Type
lazyIdToTypes id utilData = do
    state <- get
    if last (typeName id) == '*'
        then case stringToNonUniquePrim (init (typeName id)) of
            (Just typ) -> return $ UniqT typ True
            Nothing    ->
                case find (\(id', _) -> typeName id' == init (typeName id)) (Set.toList (lsigma state)) of
                    (Just (name, polys)) -> do 
                        typ <- getAlgebraicType name utilData
                        return $ UniqT typ True
                    Nothing -> throwError $ UndefinedTypeError id utilData
        else case stringToNonUniquePrim (typeName id) of
            (Just typ) -> return typ
            Nothing    ->
                case find (\(id', _) -> id' == id) (Set.toList (lsigma state)) of
                    (Just (name, polys)) -> getAlgebraicType name utilData
                    Nothing -> throwError $ UndefinedTypeError id utilData

stringToNonUniquePrim :: String -> Maybe Type
stringToNonUniquePrim "Int"    = Just $ PrimT IntPrim
stringToNonUniquePrim "Float"  = Just $ PrimT FloatPrim
stringToNonUniquePrim "Bool"   = Just $ PrimT BoolPrim
stringToNonUniquePrim "Char"   = Just $ PrimT CharPrim
stringToNonUniquePrim "File"   = Just $ PrimT FilePrim
stringToNonUniquePrim "System" = Just $ PrimT SystemPrim
stringToNonUniquePrim "String" = Just $ ListT (PrimT CharPrim)
stringToNonUniquePrim _        = Nothing

freshType :: Type -> InferT Type
freshType typ = do
    let tvars = Set.toList $ ftv typ
    tvars' <- freshTVars tvars
    let sub = Map.fromList $ zip tvars tvars'
    return $ substitute sub typ

inferVarDclLazily :: VarDclAST -> InferT ()
inferVarDclLazily EpsVarDclAST = return ()
inferVarDclLazily (VarDclAST (UntypedVarAST varId _) e dv _) = do 
    state <- get
    put state{ globalEnv = (globalEnv state) `except` (varId, LazyT e) }
    inferVarDclLazily dv

inferVarDclLazily (VarDclAST (TypedVarAST varId s _) e dv _) = do 
    state <- get
    put state{ globalEnv = (globalEnv state) `except` (varId, LazyS e s) }
    inferVarDclLazily dv

inferVarDcl :: VarDclAST -> InferT ()
inferVarDcl EpsVarDclAST = return ()
inferVarDcl (VarDclAST (UntypedVarAST varId _) _ dv _) = do
    state <- get
    case getVar (globalEnv state) varId of
        Just (LazyT e) -> do
            tvar     <- genTVar []
            let prevLength = length (constraints state)
            (typ, _) <- inferExpr e (initEnv `except` (varId, (ForAll [] tvar)))
            state'   <- get
            sub      <- unifyAll Map.empty (List.drop prevLength (constraints state'))
            let scheme = gen (substitute sub (globalEnv state')) (substitute sub typ)
            put state'{ globalEnv = (substitute sub (globalEnv state')) `except` (varId, scheme) }
            inferVarDcl dv
        Just (ForAll _ _) -> inferVarDcl dv 
        _ -> error "unknown variable" -- should not happen

inferVarDcl (VarDclAST (TypedVarAST varId _ _) _ dv utilData) = do
    state <- get
    case getVar (globalEnv state) varId of
        Just (LazyS e s) -> do
            (typ, _)  <- types s Map.empty
            (typ', _) <- inferExpr e (initEnv `except` (varId, (ForAll [] typ)))
            state' <- get
            let scheme = gen (globalEnv state') typ
            put state'{ globalEnv = (globalEnv state') `except` (varId, scheme) }
            addConstraint typ typ' utilData
            inferVarDcl dv
        Just (ForAll _ _) -> inferVarDcl dv
        _ -> error "unknown variable" -- should not happen

getGlobal :: VarId -> UtilData -> InferT Type
getGlobal varId utilData = do
    state <- get
    case getVar (globalEnv state) varId of
        Just scheme@(ForAll vars (UniqT typ True)) -> do
            put state{ globalEnv = (globalEnv state) `except` (varId, ForAll vars (UniqT typ False)) }
            ins <- proj scheme
            return ins
        Just (ForAll _ (UniqT typ False)) -> throwError $ LinearTypeError (UniqT typ False) utilData
        Just scheme@(ForAll _ _) -> do
            ins <- proj scheme
            return ins
        Just (LazyT e) -> do
            tvar     <- genTVar []
            let prevLength = length (constraints state)
            (typ, _) <- inferExpr e (initEnv `except` (varId, (ForAll [] tvar)))
            state'   <- get
            sub      <- unifyAll Map.empty (List.drop prevLength (constraints state'))
            let scheme = gen (substitute sub (globalEnv state')) (substitute sub typ)
            put state'{ globalEnv = (substitute sub (globalEnv state')) `except` (varId, scheme) }
            ins <- proj scheme
            return ins
        Just (LazyS e s) -> do
            (typ, _)  <- types s Map.empty
            (typ', _) <- inferExpr e (initEnv `except` (varId, (ForAll [] typ)))
            let scheme = gen (globalEnv state) typ
            state'    <- get
            put state'{ globalEnv = (globalEnv state') `except` (varId, scheme) }
            addConstraint typ typ' utilData
            ins <- proj scheme
            return ins
        _ -> throwError $ VariableScopeError varId utilData

inferExpr :: ExprAST -> TypeEnv -> InferT (Type, [Binding])
inferExpr (VarExprAST varId utilData) env =
    case getVar env varId of
        Just scheme@(ForAll vars typ) -> do
            ins <- proj scheme
            let binds = case typ of
                    UniqT utyp _ -> [(varId, ForAll vars (UniqT utyp False))]
                    _            -> []
            return (ins, binds)
        _ -> do
            ins <- getGlobal varId utilData
            return (ins, [])

inferExpr (ConstExprAST c _) _ = do 
    typ <- inferConst c
    return (typ, [])

inferExpr (TypeExprAST typeId utilData) _ = do
    state <- get
    case getSignature (sigma state) typeId of
        Nothing  -> throwError $ UndefinedTermConstructorError typeId utilData
        Just typ -> do 
            typ' <- freshType typ
            return (typ', [])

inferExpr (ParenExprAST expr _) env = inferExpr expr env

inferExpr (LambdaExprAST (UntypedVarAST varId _) expr _) env = do
    tvar <- genTVar []
    let env' = env `except` (varId, ForAll [] tvar)
    (typ, bindings) <- inferExpr expr env'
    return (FuncT tvar typ, bindings)

inferExpr (LambdaExprAST (TypedVarAST varId s utilData) expr _) env = do
    (typ', _) <- types s Map.empty
    let env' = env `except` (varId, ForAll [] typ')
    (typ, bindings) <- inferExpr expr env'
    return (FuncT typ' typ, bindings)

inferExpr (FunAppExprAST expr1 expr2 utilData) env = do
    (typ1, binds)  <- inferExpr expr1 env
    let env' = applyBindings env binds
    (typ2, binds') <- inferExpr expr2 env'
    tvar <- genTVar []
    addConstraint typ1 (FuncT typ2 tvar) utilData
    return (tvar, binds ++ binds')

inferExpr (TupleExprAST exprs _) env = do
    (typs, binds) <- inferExprs exprs env
    return (TuplT typs, binds)

inferExpr (ListExprAST exprs utilData) env = do
    (typs, binds) <- inferExprs exprs env
    case typs of
        [] -> do
            tvar <- genTVar []
            return (ListT tvar, binds)
        (typ:typs') -> do
            addConstraints [(typ, typ', utilData) | typ' <- typs']
            return (ListT typ, binds) 

inferExpr (LetInExprAST (UntypedVarAST varId _) expr1 expr2 utilData) env = do
    tvar <- genTVar []
    let env' = env `except` (varId, ForAll [] tvar)
    state <- get
    let prevLength = length (constraints state)
    (typ, binds) <- inferExpr expr1 env'
    state' <- get
    sub    <- unifyAll Map.empty (List.drop prevLength (constraints state'))
    let env'' = substitute sub (applyBindings env' binds)
    let scheme = gen env'' (substitute sub typ)
    (typ2, binds') <- inferExpr expr2 (env'' `except` (varId, scheme))
    return (typ2, binds ++ binds')

inferExpr (LetInExprAST (TypedVarAST varId s _) expr1 expr2 utilData) env = do
    state <- get
    (typ1, _) <- types s Map.empty
    let env' = env `except` (varId, ForAll [] typ1)
    (typ1', binds) <- inferExpr expr1 env'
    addConstraint typ1 typ1' utilData
    let env'' = applyBindings env' binds
    let scheme = gen env'' typ1
    (typ2, binds') <- inferExpr expr2 (env'' `except` (varId, scheme))
    return (typ2, binds ++ binds')

inferExpr (CaseExprAST branches utilData) env = do
    typs <- inferCaseBranches branches env
    case typs of
        [] -> error "a case expression must have at least one branch" -- should not happen
        (typ:typs') -> do
            addConstraints [(typ, typ', utilData) | typ' <- typs']
            return (typ, [])

inferExpr (MatchExprAST expr branches utilData) env = do
    (typ1, binds) <- inferExpr expr env
    let env' = applyBindings env binds
    typ2s <- inferMatchBranches typ1 branches env'
    case typ2s of
        [] -> error "a match expression must have at least one branch" -- should not happen
        (typ2:typ2s') -> do 
            addConstraints [(typ2, typ2', utilData) | typ2' <- typ2s']
            return (typ2, binds)

inferExprs :: [ExprAST] -> TypeEnv -> InferT ([Type], [Binding])
inferExprs [] _ = return ([], [])
inferExprs (e:es) env = do
    (typ, binds)   <- inferExpr e env
    let env' = applyBindings env binds
    (typs, binds') <- inferExprs es env'
    return (typ:typs, binds ++ binds')

applyBindings :: TypeEnv -> [Binding] -> TypeEnv
applyBindings env binds = List.foldr (flip except) env binds

inferCaseBranches :: [(PredAST, ExprAST)] -> TypeEnv -> InferT [Type]
inferCaseBranches [] _ = return []
inferCaseBranches ((PredWildAST _, expr):branches) env = do
    (typ, _) <- inferExpr expr env
    typs <- inferCaseBranches branches env
    return (typ:typs)

inferCaseBranches ((PredExprAST expr1 utilData, expr2):branches) env = do
    (typ1, binds) <- inferExpr expr1 env
    let env' = applyBindings env binds
    (typ2, _) <- inferExpr expr2 env'
    addConstraint typ1 (PrimT BoolPrim) utilData
    typ2s <- inferCaseBranches branches env
    return (typ2:typ2s)

inferMatchBranches :: Type -> [(PatternAST, ExprAST)] -> TypeEnv -> InferT [Type]
inferMatchBranches _ [] _ = return []
inferMatchBranches typ1 ((pat, expr):branches) env = do
    (typ1', binds) <- match typ1 pat
    let env' = applyBindings env binds
    (typ2, _) <- inferExpr expr env'
    typs2 <- inferMatchBranches typ1' branches env
    return (typ2:typs2)

binaryFun :: [TypeClass] -> InferT Type
binaryFun classes = do
    tvar <- genTVar classes
    return $ FuncT tvar (FuncT tvar tvar)

inferConst :: ConstAST -> InferT Type
inferConst (IntConstAST _ _)   = return $ PrimT IntPrim
inferConst (BoolConstAST _ _)  = return $ PrimT BoolPrim 
inferConst (FloatConstAST _ _) = return $ PrimT FloatPrim
inferConst (CharConstAST _ _)  = return $ PrimT CharPrim

inferConst (UnaryMinusConstAST _) = do
    tvar <- genTVar [numClass]
    return $ FuncT tvar tvar

inferConst (PlusConstAST _)   = binaryFun [numClass]
inferConst (MinusConstAST _)  = binaryFun [numClass]
inferConst (TimesConstAST _)  = binaryFun [numClass]
inferConst (DivideConstAST _) = binaryFun [numClass]
inferConst (ModuloConstAST _) = binaryFun [numClass]

inferConst (EqualsConstAST _) = do
    tvar <- genTVar [eqClass]
    return $ FuncT tvar (FuncT tvar (PrimT BoolPrim))

inferConst (NotConstAST _) = return $ FuncT (PrimT BoolPrim) (PrimT BoolPrim)

inferConst (GreaterConstAST _) = do
    tvar <- genTVar [ordClass]
    return $ FuncT tvar (FuncT tvar (PrimT BoolPrim))

inferConst (LessConstAST _) = do
    tvar <- genTVar [ordClass]
    return $ FuncT tvar (FuncT tvar (PrimT BoolPrim)) 

inferConst (GreaterOrEqualConstAST _) = do
    tvar <- genTVar [ordClass]
    return $ FuncT tvar (FuncT tvar (PrimT BoolPrim))

inferConst (LessOrEqualConstAST _) = do
    tvar <- genTVar [ordClass]
    return $ FuncT tvar (FuncT tvar (PrimT BoolPrim)) 

inferConst (AppenConstAST _) = do
    tvar <- genTVar []
    return $ FuncT (tvar) (FuncT (ListT tvar) (ListT tvar))

inferConst (ConcatenateConstAST _) = do
    tvar <- genTVar []
    return $ FuncT (ListT tvar) (FuncT (ListT tvar) (ListT tvar))

inferConst (AndConstAST _) = return $ FuncT (PrimT BoolPrim) (FuncT (PrimT BoolPrim) (PrimT BoolPrim)) 
inferConst (OrConstAST _)  = return $ FuncT (PrimT BoolPrim) (FuncT (PrimT BoolPrim) (PrimT BoolPrim))

inferConst (BiLShiftConstAST _) = do
    tvar <- genTVar [biClass]
    return $ FuncT tvar (FuncT (PrimT IntPrim) tvar)

inferConst (BiRShiftConstAST _) = do
    tvar <- genTVar [biClass]
    return $ FuncT tvar (FuncT (PrimT IntPrim) tvar)

inferConst (BiNotConstAST _) = binaryFun [biClass] 
inferConst (BiAndConstAST _) = binaryFun [biClass] 
inferConst (BiXorConstAST _) = binaryFun [biClass] 
inferConst (BiOrConstAST _)  = binaryFun [biClass] 

inferConst (OpenReadConstAST _)  = return $ FuncT (UniqT (PrimT SystemPrim) True) (FuncT (ListT (PrimT CharPrim)) (TuplT [PrimT BoolPrim, UniqT (PrimT SystemPrim) True, UniqT (PrimT FilePrim) True]))
inferConst (OpenWriteConstAST _) = return $ FuncT (UniqT (PrimT SystemPrim) True) (FuncT (ListT (PrimT CharPrim)) (TuplT [PrimT BoolPrim, UniqT (PrimT SystemPrim) True, UniqT (PrimT FilePrim) True]))
inferConst (CloseConstAST _)     = return $ FuncT (UniqT (PrimT SystemPrim) True) (FuncT (UniqT (PrimT FilePrim) True) (TuplT [PrimT BoolPrim, UniqT (PrimT SystemPrim) True]))
inferConst (ReadConstAST _)      = return $ FuncT (UniqT (PrimT FilePrim) True) (TuplT [PrimT BoolPrim, PrimT CharPrim, UniqT (PrimT FilePrim) True])
inferConst (WriteConstAST _)     = return $ FuncT (PrimT CharPrim) (FuncT (UniqT (PrimT FilePrim) True) (TuplT [PrimT BoolPrim, UniqT (PrimT FilePrim) True]))
inferConst (DeleteConstAST _)    = return $ FuncT (UniqT (PrimT SystemPrim) True) (FuncT (UniqT (PrimT FilePrim) True) (TuplT [PrimT BoolPrim, UniqT (PrimT SystemPrim) True]))
inferConst (ToIntConstAST _)     = return $ FuncT (ListT (PrimT CharPrim)) (TuplT [PrimT BoolPrim, PrimT IntPrim])
inferConst (ToFloatConstAST _)   = return $ FuncT (ListT (PrimT CharPrim)) (TuplT [PrimT BoolPrim, PrimT FloatPrim])
inferConst (IntToCharAST _)      = return $ FuncT (PrimT IntPrim) (PrimT CharPrim)
inferConst (CharToIntAST _)      = return $ FuncT (PrimT CharPrim) (PrimT IntPrim)

inferConst (ShowConstAST _) = do
    tvar <- genTVar [showClass]
    return $ FuncT tvar (ListT (PrimT CharPrim))

inferPat :: PatternAST -> InferT (Type, [Binding])
inferPat (ConstPatternAST c _) = do
    typ <- inferConst c
    return (typ, [])

inferPat (VarPatternAST varId _) = do
    tvar <- genTVar []
    return (tvar, [(varId, ForAll [] tvar)])

inferPat (WildPatternAST _) = do
    tvar <- genTVar []
    return (tvar, [])

inferPat (TypePatternAST typeId utilData) = do
    state <- get
    case getTermConstructor (sigma state) typeId of
        Nothing -> throwError $ UndefinedTermConstructorError typeId utilData
        (Just (_, _, typ@(AlgeT _ _))) -> do 
            typ' <- freshType typ
            return (typ', [])
        (Just _) -> throwError $ TermConstructorPatternMisuseError typeId utilData

inferPat (TypeConsPatternAST typeId pat' utilData) = do
    state <- get
    case getTermConstructor (sigma state) typeId of
        Nothing -> throwError $ UndefinedTermConstructorError typeId utilData
        (Just (_, _, FuncT sig typ@(AlgeT name typs))) -> do
            typ' <- freshType typ
            case typ' of
                (AlgeT _ typs') -> do
                    sub <- analyzeVars typs typs' Map.empty
                    let sig' = substitute sub sig
                    (_, binds) <- match sig' pat'
                    return (AlgeT name typs', binds)
                _ -> error "type is not algebraic" -- should not happen
        (Just _) -> throwError $ TermConstructorPatternMisuseError typeId utilData

inferPat (ListPatternAST [] _) = do
    tvar <- genTVar []
    return (ListT tvar, [])

inferPat (ListPatternAST (pat:pats) utilData) = do
     (typ, binds) <- inferPat pat
     let typs = List.take (length pats) (repeat typ)
     (_, binds') <- matchMultiple typs pats utilData
     return (ListT typ, binds ++ binds')

inferPat (TuplePatternAST pats _) = do
    (typs, binds) <- inferPats pats
    return (TuplT typs, binds)

inferPat (DecompPatternAST pat varId _) = do
    (typ, binds) <- inferPat pat
    return (ListT typ, ((varId, ForAll [] typ):binds))

inferPats :: [PatternAST] -> InferT ([Type], [Binding])
inferPats [] = return ([], [])
inferPats (pat:pats) = do
    (typ, binds)   <- inferPat pat
    (typs, binds') <- inferPats pats
    return (typ:typs, binds ++ binds')

match :: Type -> PatternAST -> InferT (Type, [Binding])
match typ (VarPatternAST varId _) = return (typ, [(varId, ForAll [] typ)])

match typ (WildPatternAST _) = return (typ, [])

match typ@(PolyT (TVar _ classes)) pat = do
    (typ', binds) <- inferPat pat
    if List.foldr ((&&) . (checkClass typ')) True classes
        then do 
            addConstraint typ typ' (getUtilDataPat pat)
            return (typ', binds)
        else throwError $ MatchPatternMismatchError typ pat (getUtilDataPat pat)

match typ@(PrimT (IntPrim)) (ConstPatternAST (IntConstAST _ _) _)     = return (typ, [])
match typ@(PrimT (FloatPrim)) (ConstPatternAST (FloatConstAST _ _) _) = return (typ, [])
match typ@(PrimT (BoolPrim)) (ConstPatternAST (BoolConstAST _ _) _)   = return (typ, [])
match typ@(PrimT (CharPrim)) (ConstPatternAST (CharConstAST _ _) _)   = return (typ, [])

match typ@(AlgeT name _) pat@(TypePatternAST typeId utilData) = do
    state <- get
    case getTermConstructor (sigma state) typeId of
        Nothing -> throwError $ UndefinedTermConstructorError typeId utilData
        (Just (_, _, AlgeT name' _)) ->
            if name' == name
                then return (typ, [])
                else throwError $ MatchPatternMismatchError typ pat utilData
        (Just _) -> throwError $ MatchPatternMismatchError typ pat utilData

match (ListT typ') (DecompPatternAST pat' varId _) = do
    (typ'', binds) <- match typ' pat'
    return (ListT typ'', ((varId, ForAll [] (ListT typ'')):binds))

match typ@(AlgeT name typs) pat@(TypeConsPatternAST typeId pat' utilData) = do
    state <- get
    case getTermConstructor (sigma state) typeId of
        Nothing -> throwError $ UndefinedTermConstructorError typeId utilData
        Just (_, _, FuncT sig (AlgeT name' typs')) ->
            if name' == name
                then do
                    sub <- analyzeVars typs' typs Map.empty
                    let sig' = substitute sub sig
                    (_, binds) <- match sig' pat'
                    return (typ, binds)
                else throwError $ MatchPatternMismatchError typ pat utilData
        Just _ -> throwError $ MatchPatternMismatchError typ pat utilData

match typ@(TuplT typs) (TuplePatternAST ps utilData) = do 
    (typs, binds) <- matchMultiple typs ps utilData
    return (TuplT typs, binds)

match typ@(ListT typ') (ListPatternAST ps utilData) = do 
    (typs'', binds) <- matchMultiple typs' ps utilData
    case typs'' of
        []        -> return (typ, binds)
        (typ'':_) -> return (ListT typ'', binds)
    where
        typs' = List.take (length ps) (repeat typ')

match typ pat = throwError $ MatchPatternMismatchError typ pat (getUtilDataPat pat)

matchMultiple :: [Type] -> [PatternAST] -> UtilData -> InferT ([Type], [Binding])
matchMultiple [] [] _       = return ([], [])
matchMultiple [] _ utilData = throwError $ LengthMismatchError utilData
matchMultiple _ [] utilData = throwError $ LengthMismatchError utilData
matchMultiple (t:ts) (p:ps) utilData = do
    (typ, binds)   <- match t p
    (typs, binds') <- matchMultiple ts ps utilData
    return $ (typ:typs, binds ++ binds')

analyzeVars :: [Type] -> [Type] -> Substitution -> InferT Substitution
analyzeVars [] [] sub = return sub
analyzeVars ((PolyT tvar):t1s) (t2:t2s) sub = analyzeVars t1s t2s (Map.insert tvar t2 sub)
analyzeVars _ _ _ = error "mismatched typevariables" -- should not happen