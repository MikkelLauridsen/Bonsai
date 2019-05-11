module Inference
    (
      --typeBonsai
    ) where

import Ast
import Data.Map.Strict as Map
import Data.Set as Set
import Data.List as List
import Control.Monad.Except
import Control.Monad.State
import Prettifier

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
    show (PolyT (TVar name []))      = name
    show (PolyT (TVar name classes)) = name ++ "<<" ++ ([show class' | class' <- init classes] >>= (++ ", ")) ++ show (last classes) ++ ">>"

type Sig = Set TermConstructor

type Constraint = (Type, Type, UtilData)

type Substitution = Map TypeVar Type

data TypeVar = TVar String [TypeClass] deriving (Eq, Ord)

data Scheme = ForAll [TypeVar] Type  

-- type-environment type
newtype TypeEnv = TypeEnv (Map String Scheme)

-- type-environment binding format
type Binding = (String, Scheme)

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

    substitute sub (ForAll vars typ) = ForAll vars (substitute (List.foldr Map.delete sub vars) typ)

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
               | TypeDeclarationConflictError TypeId UtilData
               | TypeClassMismatchError Type Type UtilData
               | TypeMismatchError Type Type UtilData
               | MatchPatternMismatchError Type PatternAST UtilData

data InferState = InferState { 
                               next        :: Integer
                             , constraints :: [Constraint]
                             , sigma       :: Sig
                             }

type InferT a = ExceptT TypeError (State InferState) a

initState = InferState { next = 0, constraints = [], sigma = Set.empty }

runInferT :: InferT (Substitution, Type) -> Maybe String
runInferT m = case evalState (runExceptT m) initState of
    (Left err) -> Just $ evalError err
    (Right _)  -> Nothing

evalError :: TypeError -> String
evalError (LinearTypeError typ utilData)                     = formatErr ("instance of unique type '" ++ show typ ++ "' cannot be used more than once") utilData
evalError (VariableScopeError varId utilData)                = formatErr ("variable '" ++ varName varId ++ "' is out of scope") utilData
evalError (VariableRedefinitionError varId utilData)         = formatErr ("global variable '" ++ varName varId ++ "' cannot be redefined globally") utilData
evalError (TypeRedefinitionError typeId utilData)            = formatErr ("algebraic type '" ++ typeName typeId ++  "' cannot be redefined") utilData
evalError (TermConstructorRedefinitionError typeId utilData) = formatErr ("termconstructor '" ++ typeName typeId ++ "' cannot be redefined") utilData
evalError (TypeClassMismatchError typ1 typ2 utilData)        = formatErr ("type mismatch, expected '" ++ show typ1 ++ "' but actual type '" ++ show typ2 ++ "' does not conform to the typeclasses") utilData
evalError (TypeMismatchError typ1 typ2 utilData)             = formatErr ("type mismatch, could not match expected type '" ++ show typ1 ++ "' with actual type '" ++ show typ2 ++ "'") utilData
evalError (MatchPatternMismatchError typ pat utilData)       = formatErr ("type-pattern mismatch, could not match type '" ++ show typ ++ "' with pattern '" ++ prettyShow pat 0 ++ "'") utilData

genTVar :: [TypeClass] -> InferT Type
genTVar classes = do
    state <- get
    put state{ next = next state + 1 }
    return $ PolyT (TVar (".a" ++ show (next state)) classes)

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

-- checks whether the input termconstructor (by name)
-- is defined in input set of Algebraic types
has :: Sig -> TypeId -> Bool
has sigma t =
    case find (\(t', _, _) -> t' == t) (Set.toList sigma) of
        Nothing            -> False
        (Just (_, _, _)) -> True

-- unification

unifyAll :: InferT Substitution
unifyAll = do
    state <- get
    case constraints state of
        [] -> return Map.empty
        ((typ1, typ2, utilData):c') -> do
            put state{ constraints = c' }
            sub  <- unify typ1 typ2 utilData
            sub' <- unifyAll
            return $ sub `Map.union` sub'

unify :: Type -> Type -> UtilData -> InferT Substitution
unify typ1@(PrimT prim1) typ2@(PrimT prim2) utilData =
    if prim1 == prim2
        then return Map.empty
        else throwError $ TypeMismatchError typ1 typ2 utilData

unify typ1@(PolyT var@(TVar _ classes)) typ2 utilData =
    if List.foldr ((&&) . (checkClass typ2)) True classes
        then do 
            state <- get
            put state{ constraints = List.map (substituteConstraint) (constraints state) }
            return sub
        else throwError $ TypeClassMismatchError typ1 typ2 utilData
    where
        sub = Map.singleton var typ2
        substituteConstraint = \(t1, t2, utilData) -> (substitute sub t1, substitute sub t2, utilData)

unify typ1 typ2@(PolyT var@(TVar _ classes)) utilData =
    if List.foldr ((&&) . (checkClass typ1)) True classes
        then do 
            state <- get
            put state{ constraints = List.map (substituteConstraint) (constraints state) }
            return sub
        else throwError $ TypeClassMismatchError typ2 typ1 utilData
    where
        sub = Map.singleton var typ1
        substituteConstraint = \(t1, t2, utilData) -> (substitute sub t1, substitute sub t2, utilData)

unify (FuncT s1 s2) (FuncT t1 t2) utilData = do
    state <- get
    put state{ constraints = (constraints state) ++ [(s1, t1, utilData), (s2, t2, utilData)] }
    return Map.empty

unify typ1@(TuplT typs1) typ2@(TuplT typs2) utilData =
    if length typs1 /= length typs2
        then throwError $ TypeMismatchError typ1 typ2 utilData
        else do
            state <- get
            put state{ constraints = (constraints state) ++ [(typ1', typ2', utilData) | typ1' <- typs1, typ2' <- typs2] }
            return Map.empty

unify (ListT typ1) (ListT typ2) utilData = do
    state <- get
    put state{ constraints = (constraints state) ++ [(typ1, typ2, utilData)] }
    return Map.empty

unify typ1@(AlgeT name1 typs1) typ2@(AlgeT name2 typs2) utilData = do
    state <- get
    if name1 == name2  && (sigma state) `has` name1 && length typs1 == length typs2
        then do
            put state{ constraints = (constraints state) ++ [(typ1', typ2', utilData) | typ1' <- typs1, typ2' <- typs2] }
            return Map.empty
        else throwError $ TypeMismatchError typ1 typ2 utilData

unify typ1@(UniqT typ1' valid1) typ2@(UniqT typ2' valid2) utilData =
    case (valid1, valid2) of
        (False, True)  -> throwError $ LinearTypeError typ1 utilData
        (True, False)  -> throwError $ LinearTypeError typ2 utilData
        _              -> do
            state <- get
            put state{ constraints = constraints state ++ [(typ1', typ2', utilData)] }
            return Map.empty

-- catch all
unify typ1 typ2 utilData = throwError $ TypeMismatchError typ1 typ2 utilData

checkClass :: Type -> TypeClass -> Bool
checkClass typ (TClass _ fun) = fun typ

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