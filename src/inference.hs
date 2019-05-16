-- constraint based inference type system for Bonsai
-- interface:
--   1. function 'infer' takes a filepath and AST,
--      and returns an error message if the AST cannot be well-typed.
--      Otherwise, returns nothing
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

-- A typeclass has a name and a function,
-- used to establish whether a given type is
-- accepted by the class
data TypeClass = TClass String (Type -> Bool)

instance Eq TypeClass where
    TClass name1 _ == TClass name2 _ = name1 == name2

instance Ord TypeClass where
    TClass name1 _ `compare` TClass name2 _ = name1 `compare` name2

instance Show TypeClass where
    show (TClass name _) = name

-- direct implementation of type category 'Typ'
data Type = PrimT Prim
          | FuncT Type Type
          | TuplT [Type]
          | ListT Type
          | AlgeT TypeId [Type]
          | UniqT Type Bool -- rather than splitting environments, we tag linear types with a 'valid' bool 
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

data Prim = IntPrim
          | FloatPrim
          | BoolPrim
          | CharPrim
          | FilePrim
          | SystemPrim
          deriving (Eq, Ord) 

instance Show Prim where
    show IntPrim    = "Int"
    show FloatPrim  = "Float"
    show BoolPrim   = "Bool"
    show CharPrim   = "Char"
    show FilePrim   = "File"
    show SystemPrim = "System"

type LazySig = Set LazyAlgebraicType 

type LazyAlgebraicType = (TypeId, [String])

-- a termconstructor has a name, an associated type and a signature
-- const termconstructors have their membertype as signature.
-- function termconstructors have signature: types(s in Sdt) -> memberType
type TermConstructor = (TypeId, Type, Type)

-- Sig is a set of termconstructors
type Sig = Set TermConstructor

-- Ups is a map of algebraic type names to actual algebraic types
type Ups = Map TypeId Type

-- an inference constraint contains two types and utility information for error messages
type Constraint = (Type, Type, UtilData)

-- a substitution is a map of typevariables to types,
-- and is used to replace typevariables which have been bound
type Substitution = Map TypeVar Type

-- a typevariable has a name and a list of typeclasses it is restricted by
-- it may only be bound to types that are 'in' all its typeclasses
data TypeVar = TVar String [TypeClass]

instance Eq TypeVar where
    TVar name1 _ == TVar name2 _ = name1 == name2

instance Ord TypeVar where
    TVar name1 _ `compare` TVar name2 _ = name1 `compare` name2

instance Show TypeVar where
    show (TVar name [])      = name
    show (TVar name classes) = name ++ "<<" ++ ([show class' | class' <- init classes] >>= (++ ", ")) ++ show (last classes) ++ ">>"

-- besides the scheme introduced in the report,
-- we introduce to lazy schemes for global declarations:
--  1. LazyT denotes an unevaluated unannotated global variable
--  2. LazyS denotes an unevaluated annotated global variable
-- these will be replaced by actual schemes upon evaluation
data Scheme = ForAll [TypeVar] Type
            | LazyT ExprAST
            | LazyS ExprAST CompTypeAST

instance Show Scheme where
    show (ForAll _ typ) = show typ
    show (LazyT _)      = "e"
    show (LazyS _ _)    = "e::s"

-- a type environment is a map from variable ids to schemes
-- we use both a local and global environment
newtype TypeEnv = TypeEnv (Map VarId Scheme) deriving Show

-- type environment binding format
type Binding = (VarId, Scheme)

-- we define a class, Subsitutable,
-- which provides a function, ftv, used to find all typevariables present in deriving types
-- Function substitute applies an input substitution on a deriving type
class Substitutable a where
    ftv :: a -> Set TypeVar
    substitute :: Substitution ->  a -> a

-- Type is the first deriving type.
-- we recursively traverse the possible dataconstructors,
-- to find all typevariables and:
--  1. return a set of them when using ftv
--  2. replace them if bound in the substitution when using substitute
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

-- We also need to be able to find free typevariables in Schemes,
-- as well as apply substitutions for the let-in and var-decl ct-rules
-- ftv and substitute have no effect on lazy schemes
-- the actual Scheme shows what 'free typevariable' means:
-- A typevariable is free if it appears in either the type or the list of typevariables,
-- but not in both. In other words, we can instantiate it with no negative effects on the inference
instance Substitutable Scheme where
    ftv (ForAll vars typ) = (ftv typ) `Set.difference` Set.fromList vars
    ftv (LazyT _)         = Set.empty
    ftv (LazyS _ _)       = Set.empty

    substitute sub (ForAll vars typ)  = ForAll vars (substitute (List.foldr Map.delete sub vars) typ)
    substitute sub scheme@(LazyT _)   = scheme
    substitute sub scheme@(LazyS _ _) = scheme

-- For lists of substitutable types, 
-- we union the set of free typevariables provides by applying ftv to all elements
-- as for substitute, we apply the substitution to each element and return the mapped list
instance Substitutable a => Substitutable [a] where
    ftv l = List.foldr (Set.union . ftv) Set.empty l

    substitute sub l = (fmap . substitute) sub l -- http://dev.stephendiehl.com/fun/006_hindley_milner.html

-- For type environments, we use ftv on the list of schemes, 
-- which we supported above
-- as for substitute, we apply the substitution to each scheme in the map
instance Substitutable TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)

    substitute sub (TypeEnv env) = TypeEnv (Map.map (substitute sub) env)

-- We now introduce the inference monad.
-- First, we define TypeError, which represents exceptions
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
               | InfinityTypeError TypeVar Type UtilData
               | DebugError String UtilData

-- then we introduce InferState,
-- which holds the 'state' of the inference run
-- It holds:
--  1. next, which is used to generate unique typevariable names
--  2. globalEnv, which is the type environment for global variables
--  3. constraints, which holds all gathered constraints
--  4. sigma, which represents all valid termconstructors
--  5. upsilon, which is a map from type ids to valid algebraic types
--  6. lsigma, which holds pairs of algebraic type names and their corresponding typevariable names (used to define termconstructors)
--  7. debug, which is a String of all 'states' of constraints when unifying
data InferState = InferState { 
                               next        :: Integer
                             , globalEnv   :: TypeEnv
                             , constraints :: [Constraint]
                             , sigma       :: Sig
                             , upsilon     :: Ups
                             , lsigma      :: LazySig
                             , debug       :: String
                             }

-- now we define the actual Monad
-- it is a transformer monad of Except T and State:
--  1. ExceptT allows us to throw exceptions of TypeError
--  2. State allows us to 'get' and 'put' a state of InferState
type InferT a = ExceptT TypeError (State InferState) a

-- the initial state stored in the InferState instance of the monad
initState = InferState { next = 0, constraints = [], sigma = Set.empty, upsilon = Map.empty, lsigma = Set.empty, debug = "", globalEnv = TypeEnv Map.empty }

-- runInferT evaluates an inference monad,
-- and either returns an error message or Nothing
runInferT :: InferT Substitution -> Maybe String
runInferT m = case evalState (runExceptT m) initState of
    (Left err) -> Just $ evalError err
    (Right _)  -> Nothing

-- evalError converts a TypeError instance to string
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
evalError (InfinityTypeError tvar typ utilData)               = formatErr ("typevariable '" ++ show tvar ++ "' must not occur in substituted type '" ++ show typ ++ "'") utilData
evalError (DebugError msg utilData)                           = formatErr msg utilData

-- genTVar generates the 'next' typevariable with input typeclasses,
-- and updates the infer state, to point at the 'next' typevariable
genTVar :: [TypeClass] -> InferT Type
genTVar classes = do
    state <- get
    put state{ next = next state + 1 }
    return $ PolyT (TVar ("a" ++ show (next state)) classes)

-- 'refreshes' a list of typevariables.
-- That is, generates new typevariables with the same typeclasses
freshTVars :: [TypeVar] -> InferT [Type]
freshTVars [] = return []
freshTVars ((TVar _ classes):ts) = do
    tvar  <- genTVar classes
    tvars <- freshTVars ts
    return (tvar:tvars)

{--#-- ERROR MESSAGE FORMATTING --#--}

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

{--#-- SET/MAP UTILITY --#--}

-- add/replace a binding in the input type environment
except :: TypeEnv -> Binding -> TypeEnv
except (TypeEnv env) (var, scheme) = TypeEnv $ Map.insert var scheme env

-- lookup in the input type environment
getVar :: TypeEnv -> VarId -> Maybe Scheme
getVar (TypeEnv env) varId = Map.lookup varId env

-- checks whether the input termconstructor (by name)
-- is defined in input set of termconstructors
has :: Sig -> TypeId -> Bool
has sigma t =
    case find (\(t', _, _) -> t' == t) (Set.toList sigma) of
        Nothing            -> False
        (Just (_, _, _)) -> True

-- tries to find the termconstructor with input type id
-- returns the signature of the constructor if search was successful
getSignature :: Sig -> TypeId -> Maybe Type
getSignature sigma t =
    case find (\(t', _, _) -> t' == t) (Set.toList sigma) of
        Nothing            -> Nothing
        (Just (_, _, typ)) -> Just typ
        
-- tries to find the termconstructor with input type id
-- returns it upon success
getTermConstructor :: Sig -> TypeId -> Maybe TermConstructor
getTermConstructor sigma t = find (\(t', _, _) -> t' == t) (Set.toList sigma)
        
{--#-- UNIFICATION --#--}

-- takes a subsitution (initially empty) and a list of constraints
-- keeps unifying until the list is empty or an exception is thrown
-- composes the new substitution with the old
unifyAll :: Substitution -> [Constraint] -> InferT Substitution
unifyAll sub [] = return sub
unifyAll sub cs@((typ1, typ2, utilData):c') = do
    state <- get
    put state{ debug = debug state ++ "\n\n" ++ (List.foldr ((++) . (\(t1,t2,_) -> (show t1 ++ " :: " ++ show t2 ++ "\n"))) "" cs) } -- update debug field
    (sub', c'') <- unify typ1 typ2 utilData c'
    unifyAll (sub' `compose` sub) c''

-- unifies two types and returns a pair,
-- consisting of a new substitution and an updated list of constraints
-- the input list of constraints may be appended or substituted upon
unify :: Type -> Type -> UtilData -> [Constraint] -> InferT (Substitution, [Constraint])

-- check whether two primitives are equal,
-- if not: throw an exception
unify typ1@(PrimT prim1) typ2@(PrimT prim2) utilData c' =
    if prim1 == prim2
        then return (Map.empty, c')
        else throwError $ TypeMismatchError typ1 typ2 utilData

-- unify two type variables,
-- substitute both for a new one with typeclasses of both original typevariables
-- this substitution is applied to the list of constraints
unify (PolyT var1@(TVar _ classes1)) (PolyT var2@(TVar _ classes2)) _ c' = do
    tvar  <- genTVar classes'
    let sub = Map.fromList [(var1, tvar), (var2, tvar)]
    let substituteConstraint = \(t1, t2, utilData) -> (substitute sub t1, substitute sub t2, utilData)
    return (sub, List.map substituteConstraint c')
    where
        classes' = classes1 `List.union` classes2

-- substitute typevariable (left) with a non-typevariable type,
-- if it conforms to the typeclasses of the typevariable
-- this results in a substitution, which is applied to the list of constraints
unify typ1@(PolyT var@(TVar _ classes)) typ2 utilData c' 
    | occursCheck var typ2 = throwError $ InfinityTypeError var typ2 utilData
    | otherwise = if List.foldr ((&&) . (checkClass typ2)) True classes
                    then return (sub, List.map substituteConstraint c')
                    else throwError $ TypeClassMismatchError typ1 typ2 utilData
    where
        sub = Map.singleton var typ2
        substituteConstraint = \(t1, t2, ud) -> (substitute sub t1, substitute sub t2, ud)

-- substitute typevariable (right) with a non-typevariable type,
-- if it conforms to the typeclasses of the typevariable
-- this results in a substitution, which is applied to the list of constraints
unify typ1 typ2@(PolyT var@(TVar _ classes)) utilData c'
    | occursCheck var typ1 = throwError $ InfinityTypeError var  typ1 utilData
    | otherwise = if List.foldr ((&&) . (checkClass typ1)) True classes
                    then return (sub, List.map substituteConstraint c')
                    else throwError $ TypeClassMismatchError typ2 typ1 utilData
    where
        sub = Map.singleton var typ1
        substituteConstraint = \(t1, t2, ud) -> (substitute sub t1, substitute sub t2, ud)

-- unify two function types, append s1 'uni' t1 and s2 'uni t2 to the list of constraints        
unify (FuncT s1 s2) (FuncT t1 t2) utilData c' = return (Map.empty, c' ++ [(s1, t1, utilData), (s2, t2, utilData)])

-- unify two tuple types, append s1 'uni' t1 ... sn 'uni' tn to the list of constraints
unify typ1@(TuplT typs1) typ2@(TuplT typs2) utilData c' =
    if length typs1 == length typs2
        then return (Map.empty, c' ++ (List.map (\(t1, t2) -> (t1, t2, utilData)) (zip typs1 typs2)))
        else throwError $ TypeMismatchError typ1 typ2 utilData

-- unify two list types, append s1 'uni' t1 to the list of constraints
unify (ListT typ1) (ListT typ2) utilData c' = return (Map.empty, c' ++ [(typ1, typ2, utilData)])

-- unify two algebraic types, check whether names and lists of polymorphic types are equal in length
-- if so, check whether the types are legal
-- if so, append s1 'uni' t1 ... sn 'uni' tn to the list of constraints
unify typ1@(AlgeT name1 typs1) typ2@(AlgeT name2 typs2) utilData c' = do
    state <- get
    if name1 == name2 && length typs1 == length typs2
        then case Map.lookup name1 (upsilon state) of 
            Just _  -> return (Map.empty, c' ++ (List.map (\(t1, t2) -> (t1, t2, utilData)) (zip typs1 typs2)))
            Nothing -> throwError $ UndefinedTypeError name1 utilData
        else throwError $ TypeMismatchError typ1 typ2 utilData

-- unify two linear types, check whether they are both valid
-- if so, append s1 'uni' t1 to the list of constraints
unify typ1@(UniqT typ1' valid1) typ2@(UniqT typ2' valid2) utilData c' =
    case (valid1, valid2) of
        (False, True)  -> throwError $ LinearTypeError typ1 utilData
        (True, False)  -> throwError $ LinearTypeError typ2 utilData
        _              -> return (Map.empty, c' ++ [(typ1', typ2', utilData)])

-- catch all remaining cases and throw an exception
unify typ1 typ2 utilData _ = throwError $ TypeMismatchError typ1 typ2 utilData

occursCheck :: TypeVar -> Type -> Bool
occursCheck tvar typ = Set.member tvar $ ftv typ

-- compose two substitutions: https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter7/poly_constraints/src/Infer.hs
compose :: Substitution -> Substitution -> Substitution
compose sub1 sub2 = Map.map (substitute sub1) sub2 `Map.union` sub1

{--#-- TYPECLASS UTILITY --#--}

-- checks whether input type is accepted by input typeclass
checkClass :: Type -> TypeClass -> Bool
checkClass typ (TClass _ fun) = fun typ

-- converts input list of type ids to a list of typeclasses
-- throws an exception, if an unknown id is used
genClasses :: [TypeId] -> UtilData -> InferT [TypeClass]
genClasses [] _ = return []
genClasses (t:ts) utilData = do
    c  <- genClass t utilData
    cs <- genClasses ts utilData
    return (c:cs)

-- convert input type id to a typeclass
-- throws an exception if the type id is unknown
genClass :: TypeId -> UtilData -> InferT TypeClass
genClass t utilData =
    case typeName t of
        "Num"  -> return numClass
        "Eq"   -> return eqClass
        "Ord"  -> return ordClass
        "Show" -> return showClass
        "Bi"   -> return biClass
        _      -> throwError $ UndefinedTypeClassError t utilData

-- legal typeclasses
numClass  = TClass "Num" numFun 
eqClass   = TClass "Eq" eqFun
ordClass  = TClass "Ord" ordFun
showClass = TClass "Show" showFun
biClass   = TClass "Bi" biFun

-- recursive typeclass definitions
-- for more information, refer to the report
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

{--#-- CONSTRAINT RULES --#--}

-- appends input types and utildata to list of constraints in the infer state
addConstraint :: Type -> Type -> UtilData -> InferT ()
addConstraint typ1 typ2 utilData = do
    state <- get
    put state{ constraints = constraints state ++ [(typ1, typ2, utilData)] }

-- appends list of constraints to the list of constraints in the infer state
addConstraints :: [Constraint] -> InferT ()
addConstraints constraints' = do
    state <- get
    put state{ constraints = constraints state ++ constraints' }

-- implementation of the (proj) rule
-- instantiates input scheme to type,
-- by substituting all typevariables in 'vars'
-- Note that typeclasses are kept!
proj :: Scheme -> InferT Type
proj (ForAll vars typ) = do
    vars' <- mapM fresh vars
    let s = Map.fromList (zip vars vars')
    return $ substitute s typ
    where
        fresh = \(TVar _ classes) -> genTVar classes

-- implementation of (gen) rule,
-- returns a Scheme with input type,
-- where the list of vars is equal to the free variables between input type and type environment
gen :: TypeEnv -> Type -> Scheme -- http://dev.stephendiehl.com/fun/006_hindley_milner.html
gen env typ = ForAll vars typ
    where
        vars = Set.toList (ftv typ `Set.difference` ftv env)

-- initial type environment holding I/O related variables
stdin'  = (VarId "stdin", ForAll [] (UniqT (PrimT FilePrim) True))
stdout' = (VarId "stdout", ForAll [] (UniqT (PrimT FilePrim) True))
initEnv = TypeEnv $ Map.fromList [stdin', stdout']

-- infers input AST and returns an error message,
-- if no substitution resulting in a well-typed AST exists
infer :: FilePath -> ProgAST -> Maybe String
infer path ast =
    case runInferT (inferProg ast) of
        Nothing  -> Nothing
        Just msg -> Just (path ++ ":" ++ msg)

-- implementation of (prog) rule,
-- infers type- and variable declarations
-- then checks whether main is defined
-- if so, adds a constraint to the type of main,
-- such that it conforms to the System* parameter
-- then tries to unify all gathered constraints        
inferProg :: ProgAST -> InferT Substitution
inferProg (ProgAST dt dv utilData) = do
    inferTypeDclLazily dt
    inferTypeDcl dt
    inferVarDclLazily dv
    inferVarDcl dv
    state <- get
    case getVar (globalEnv state) (VarId "main") of
        Just (ForAll _ typ) -> do
            tvar <- genTVar []
            addConstraint typ (FuncT (UniqT (PrimT SystemPrim) True) tvar) utilData
            state' <- get
            unifyAll Map.empty (constraints state')
        _ -> throwError $ VariableScopeError (VarId "main") utilData

-- first parse on type declaration AST,
-- gathers all algebraic type names and associated generic variables (uninstantiated typevariables)
-- throws an exception if a type is defined more than once
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

-- second parse on type declaration AST,
-- defines legal termconstructors and stores them in the sigma infer state field
-- throws an exception if the same termconstructor is defined more than once,
-- or if an illegal generic variable or unknown algebraic type is referenced
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

-- looks up input type id in upsilon and returns the associated algebraic typ upon success
-- if it cannot be linked to a valid type, an exception is thrown
getAlgebraicType :: TypeId -> UtilData -> InferT Type
getAlgebraicType name utilData = do
    state <- get
    case Map.lookup name (upsilon state) of
        Just typ -> return typ
        Nothing  -> throwError $ UndefinedTypeError name utilData

-- converts a list of generic variables (strings) to a list of typevariables (PolyT)
mapVars :: [String] -> InferT [Type]
mapVars [] = return []
mapVars (v:vs) = do
    tvar <- genTVar []
    tvars <- mapVars vs
    return (tvar:tvars)

-- evaluates input list of termconstructors,
-- and adds the resulting termconstructors to sigma in the infer state
-- throws an exception if a termconstructor is defined more than once,
-- or if an illegal typevariable or unknown type is referenced
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

-- implementation of the types function (refer to the report)
-- converts an AST from syntactic category Sdt to an instantiated Type
-- types is used in association with type annotations
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

-- helper function for types
typesList :: [CompTypeAST] -> Map VarId Type -> InferT ([Type], Map VarId Type)
typesList [] binds = return ([], binds)
typesList (comp:comps) binds = do
    (typ, binds')   <- types comp binds
    (typs, binds'') <- typesList comps binds'
    return (typ:typs, binds'')

-- implementation of function build (refer to the report)
-- converts an AST from syntactic category Sdt to an instantiated Type
-- buildSignature is used in association with signatures of termconstructors
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

-- helper function for buildSignature
buildSignatureList :: [CompTypeAST] -> TypeId -> Map VarId Type -> InferT [Type]
buildSignatureList [] _ binds = return []
buildSignatureList (comp:comps') memberName binds = do
    typ  <- buildSignature comp memberName binds
    typs <- buildSignatureList comps' memberName binds
    return (typ:typs)

-- converts input typeId to a type
-- this can be a linear type (if the type id name ends with '*'),
-- possible 'immediates' are algebraic types and primitives
-- if the type id cannot be associated with a defined type, an exception is thrown
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

-- helper function for lazyIdToTypes,
-- tries to convert input string to a primitive type
-- Note that string is considered a primitive
stringToNonUniquePrim :: String -> Maybe Type
stringToNonUniquePrim "Int"    = Just $ PrimT IntPrim
stringToNonUniquePrim "Float"  = Just $ PrimT FloatPrim
stringToNonUniquePrim "Bool"   = Just $ PrimT BoolPrim
stringToNonUniquePrim "Char"   = Just $ PrimT CharPrim
stringToNonUniquePrim "File"   = Just $ PrimT FilePrim
stringToNonUniquePrim "System" = Just $ PrimT SystemPrim
stringToNonUniquePrim "String" = Just $ ListT (PrimT CharPrim)
stringToNonUniquePrim _        = Nothing

-- completetely re-instantiates input type,
-- by substituting all typevariables found in the type with fresh equivalents
freshType :: Type -> InferT Type
freshType typ = do
    let tvars = Set.toList $ ftv typ
    tvars' <- freshTVars tvars
    let sub = Map.fromList $ zip tvars tvars'
    return $ substitute sub typ

-- first parse on the variable declaration AST
-- saves all unannotated global variables as LazyT schemes
-- annotated global variables are saved as LazyS schemes (refer to the definition of Scheme above)
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

-- second parse on the variable declaration AST
-- evaluates all global variables that are saved as Lazy schemes
-- we only want to evaluate them once, after which they are saved as actual schemes
-- they may be referenced while evaluating other globals, as such not all globals will be affected explicitly by this parse
-- However, when this parse has concluded, it is guaranteed that all globals have been inferred, such that we can unify
inferVarDcl :: VarDclAST -> InferT ()
inferVarDcl EpsVarDclAST = return ()

-- global declarations are similar to let-in expressions,
-- which means we must apply a partial unification for unannotated globals after evaluating them
-- we do this by finding the 'difference' in constraints before and after evaluating the expression
-- we then apply this unification, to make sure the saved type for the variable is strict enough
-- we then generalize it with (gen) and store it
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

-- for annotated globals, we generalize the annotated type and store it instead,
-- this way we do not need to perform a partial unification
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

-- getGlobal is a helper function for the (var) infer rule,
-- which handles referencing of global variables
-- this may include evaluation of Lazy schemes,
-- which is also why the second parse on the variable declaration AST may not evaluate all globals,
-- as they may be evaluated while referencing globals!
-- as such, we see the same behavior here for unannotated globals, in which we perform a partial unification
-- besides this, we also handle the case in which the global has already been referenced,
-- here we make sure to update linear types, such that they are not valid the next time they are referenced
-- Note that we always instantiate the referenced scheme with (proj), to ensure we do not tamper with polymorphism of functions
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

-- implementation of infer rules of expressions
-- these rules always carry a type environment and an expression AST
-- a pair of an inferred type as well as a list of linear bindings is always returned
-- the linear bindings represent the splitting of type environments,
-- as we use returned bindings in the same manner!
inferExpr :: ExprAST -> TypeEnv -> InferT (Type, [Binding])

-- implementation of the (var) infer rule
-- we differentiate between global and local environments in the implementation
-- we first check the local environment, if the variable is bound there, returns the instantiated type (proj)
-- if not, we call function getGlobal and return the result
-- Note that no returned bounds are necessary for global referencing, as we save the global environment in the infer state
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

-- implementation of (const) infer rule
-- returns an instantiated type corresponding to input const AST
inferExpr (ConstExprAST c _) _ = do 
    typ <- inferConst c
    return (typ, [])

-- implementation of (ter) infer rule
-- returns a 'fresh copy' of the signature associated with input type id
-- throws an exception is the type id cannot be associated with a termconstructor
inferExpr (TypeExprAST typeId utilData) _ = do
    state <- get
    case getSignature (sigma state) typeId of
        Nothing  -> throwError $ UndefinedTermConstructorError typeId utilData
        Just typ -> do 
            typ' <- freshType typ
            return (typ', [])

-- implementation of the (paren) infer rule
-- returns the result of inferring the immediate expression
inferExpr (ParenExprAST expr _) env = inferExpr expr env

-- implementation of the (lambda-1) infer rule
-- generates a new typevariable to represent the variable id,
-- when inferring the expression
-- returns a function type from the (possibly constrained) typevariable,
-- to the inferred type of the expression, along with the linear binds from the call 
inferExpr (LambdaExprAST (UntypedVarAST varId _) expr _) env = do
    tvar <- genTVar []
    let env' = env `except` (varId, ForAll [] tvar)
    (typ, bindings) <- inferExpr expr env'
    return (FuncT tvar typ, bindings)

-- implementation of the (lambda-2) infer rule
-- this rule differs from the above in that we expect the type of the annotation
-- as such, we do not need to generate a typevariable
inferExpr (LambdaExprAST (TypedVarAST varId s utilData) expr _) env = do
    (typ', _) <- types s Map.empty
    let env' = env `except` (varId, ForAll [] typ')
    (typ, bindings) <- inferExpr expr env'
    return (FuncT typ' typ, bindings)

-- implementation of the (app) infer rule
-- infers both expressions and generates a typevariable
-- adds a constraint between the type inferred by expr1
-- and a function type from the type inferred by expr2 and the typevariable
-- returns the typevariable to denote the (possibly constrained) resulting type
-- also returns the linear binds of both calls
inferExpr (FunAppExprAST expr1 expr2 utilData) env = do
    (typ1, binds)  <- inferExpr expr1 env
    let env' = applyBindings env binds
    (typ2, binds') <- inferExpr expr2 env'
    tvar <- genTVar []
    addConstraint typ1 (FuncT typ2 tvar) utilData
    return (tvar, binds ++ binds')

-- implementation of the (tuple) infer rule
-- infers all immediate expressions and returns a tuple type with the resulting types
-- also returns the linear binds from all the calls
inferExpr (TupleExprAST exprs _) env = do
    (typs, binds) <- inferExprs exprs env
    return (TuplT typs, binds)

-- implementation of the (list) infer rule
-- this rule is similar to (tuple), but here we must account for two more things:
--   1. the empty list is handled by generating an unbound typevariable
--   2. all inferred types must be bound to each other, so we add constraints between the first element's type and all the others
inferExpr (ListExprAST exprs utilData) env = do
    (typs, binds) <- inferExprs exprs env
    case typs of
        [] -> do
            tvar <- genTVar []
            return (ListT tvar, binds)
        (typ:typs') -> do
            addConstraints [(typ, typ', utilData) | typ' <- typs']
            return (ListT typ, binds) 

-- implementation of the (let-1) infer rule
-- for the unannotated let-in expression, we must generate a new typevariable
-- we use this in the environment in which we evaluate expr1
-- then we perform a partial unification by unifying all constraints gathered while inferring expr1
-- we then generalize the resulting substituted type and put it in the environment in which we infer expr2
-- we then return the resulting type as well as the linear binds from the two calls
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

-- implementation of the (let-2) infer rule
-- as we have an annotated type here, we do not need to perform a partial unification
-- instead, we deduce the annotated type and generalize it, as we would have done after unifying in (let-1)
-- we then infer expr2 and return the resulting type, as well as the linear binds from both calls
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

-- implementation of the (case) infer rule
-- we infer predicates and branches in pairs
-- predicates must be wildcards or boolean values, as such we add constraints to boolean values
-- we also need to make sure that all branches are of the same type, as such we add constraints between these
inferExpr (CaseExprAST branches utilData) env = do
    typs <- inferCaseBranches branches env
    case typs of
        [] -> error "a case expression must have at least one branch" -- should not happen
        (typ:typs') -> do
            addConstraints [(typ, typ', utilData) | typ' <- typs']
            return (typ, [])

-- implementation of the (match) infer rule
-- we first infer expr1 and use the resulting type when evaluating patterns
-- as with the case rule, we need the resulting types to be equal, as such we add constraints between these
inferExpr (MatchExprAST expr branches utilData) env = do
    (typ1, binds) <- inferExpr expr env
    let env' = applyBindings env binds
    typ2s <- inferMatchBranches typ1 branches env'
    case typ2s of
        [] -> error "a match expression must have at least one branch" -- should not happen
        (typ2:typ2s') -> do 
            addConstraints [(typ2, typ2', utilData) | typ2' <- typ2s']
            return (typ2, binds)

-- helper function for inferExpr, which infers a list of expressions
inferExprs :: [ExprAST] -> TypeEnv -> InferT ([Type], [Binding])
inferExprs [] _ = return ([], [])
inferExprs (e:es) env = do
    (typ, binds)   <- inferExpr e env
    let env' = applyBindings env binds
    (typs, binds') <- inferExprs es env'
    return (typ:typs, binds ++ binds')

-- helper functionfor inferExpr, which applies linear binds to input type environment
applyBindings :: TypeEnv -> [Binding] -> TypeEnv
applyBindings env binds = List.foldr (flip except) env binds

-- helper function for the (case) infer rule,
-- which infers predicate branch pairs one at a time
-- if no exception is thrown, a list of resulting branch types is returned
inferCaseBranches :: [(PredAST, ExprAST)] -> TypeEnv -> InferT [Type]
inferCaseBranches [] _ = return []

-- if the predicate is a wildcard,
-- we just infer the branch expression
-- and perform a recursive call
inferCaseBranches ((PredWildAST _, expr):branches) env = do
    (typ, _) <- inferExpr expr env
    typs <- inferCaseBranches branches env
    return (typ:typs)

-- if the predicate is an expression,
-- we infer it and then we infer the branch expression
-- before a recursive call, we add a constraint to the predicate type, to a boolean primitive
inferCaseBranches ((PredExprAST expr1 utilData, expr2):branches) env = do
    (typ1, binds) <- inferExpr expr1 env
    let env' = applyBindings env binds
    (typ2, _) <- inferExpr expr2 env'
    addConstraint typ1 (PrimT BoolPrim) utilData
    typ2s <- inferCaseBranches branches env
    return (typ2:typ2s)

-- helper function for the (match) infer rule,
-- which evaluates pattern branch pairs and returns a list of resulting branch types upon success
-- first, the pattern is evaluated with the match function (refer to the report),
-- which results in a type other patterns must follow, as well as variable binds
-- these are used when inferring the branch expression
inferMatchBranches :: Type -> [(PatternAST, ExprAST)] -> TypeEnv -> InferT [Type]
inferMatchBranches _ [] _ = return []
inferMatchBranches typ1 ((pat, expr):branches) env = do
    (typ1', binds) <- match typ1 pat
    let env' = applyBindings env binds
    (typ2, _) <- inferExpr expr env'
    typs2 <- inferMatchBranches typ1' branches env
    return (typ2:typs2)

-- helper function for inferConst
binaryFun :: [TypeClass] -> InferT Type
binaryFun classes = do
    tvar <- genTVar classes
    return $ FuncT tvar (FuncT tvar tvar)

-- implementation of the apply function from the typesystem
-- returns a fresh instance of the type associated with input constant
-- refer to the report for more information
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

-- this function is used by the match function,
-- when the type to match upon is a typevariable
-- we then infer a type from the pattern, which is returned by match
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

-- helper function for inferPat, which infers the types of a list of patterns,
-- and returns the resulting list of types and variable bindings
inferPats :: [PatternAST] -> InferT ([Type], [Binding])
inferPats [] = return ([], [])
inferPats (pat:pats) = do
    (typ, binds)   <- inferPat pat
    (typs, binds') <- inferPats pats
    return (typ:typs, binds ++ binds')

-- implementation of the match function
-- checks whether the input type can be 'matched' with input pattern
-- upon success, returns a pair consisting of a type the remaining patterns must conform to
-- and a list of variable bindings
-- the resulting type is the input type unless it is a typevariable,
-- in this case we decide the type by inferring the type of the pattern
match :: Type -> PatternAST -> InferT (Type, [Binding])

-- always accepts a variable pattern, and returns the corresponding binding
match typ (VarPatternAST varId _) = return (typ, [(varId, ForAll [] typ)])

-- always accepts a wildcard, and does not return any binds
match typ (WildPatternAST _) = return (typ, [])

-- upon matching a typevariable with a pattern,
-- infers the type of the pattern, and checks whether it conforms to the typeclasses of the typevariable
-- if so, adds a constraint between the input type and the pattern type and returns the pattern type,
-- as it is more strict
match typ@(PolyT (TVar _ classes)) pat = do
    (typ', binds) <- inferPat pat
    if List.foldr ((&&) . (checkClass typ')) True classes
        then do 
            addConstraint typ typ' (getUtilDataPat pat)
            return (typ', binds)
        else throwError $ MatchPatternMismatchError typ pat (getUtilDataPat pat)

-- accepts numerals and boolean types if the corresponding pattern is of the same type
match typ@(PrimT (IntPrim)) (ConstPatternAST (IntConstAST _ _) _)     = return (typ, [])
match typ@(PrimT (FloatPrim)) (ConstPatternAST (FloatConstAST _ _) _) = return (typ, [])
match typ@(PrimT (BoolPrim)) (ConstPatternAST (BoolConstAST _ _) _)   = return (typ, [])
match typ@(PrimT (CharPrim)) (ConstPatternAST (CharConstAST _ _) _)   = return (typ, [])

-- handles the case in which the type is of an algebraic type
-- and the pattern is a constant termconstructor
-- we check whether the pattern's termconstructor exists
-- if so, we check whether the signature of the termconstructor
-- is the input algebraic type (then it is constant and of the correct type)
match typ@(AlgeT name _) pat@(TypePatternAST typeId utilData) = do
    state <- get
    case getTermConstructor (sigma state) typeId of
        Nothing -> throwError $ UndefinedTermConstructorError typeId utilData
        (Just (_, _, AlgeT name' _)) ->
            if name' == name
                then return (typ, [])
                else throwError $ MatchPatternMismatchError typ pat utilData
        (Just _) -> throwError $ MatchPatternMismatchError typ pat utilData

-- upon matching a list type with a cons pattern,
-- we perform a recursive call and return a list of the resulting type,
-- as well as the bindings of the recursive call along with the immediate pair
match (ListT typ') (DecompPatternAST pat' varId _) = do
    (typ'', binds) <- match typ' pat'
    return (ListT typ'', ((varId, ForAll [] (ListT typ'')):binds))

-- handles the case in which the type is of an algebraic type
-- and the pattern is a function termconstructor
-- we check whether the pattern's termconstructor exists
-- if so, we check whether the signature of the termconstructor
-- is a function from one type to the input algebraic type
-- if so, we create a fresh instance of the parameter type,
-- and use it in a recursive call with the immediate pattern
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

-- upon matching a tuple type with a a tuple pattern,
-- we perform recursive calls on pairs of immediates,
-- and return a tuple type of the resulting types
-- as well as the gathered variable binds
match typ@(TuplT typs) (TuplePatternAST ps utilData) = do 
    (typs, binds) <- matchMultiple typs ps utilData
    return (TuplT typs, binds)

-- upon match a list type with a list pattern,
-- we match all immediate patterns with the list type,
-- and return the resulting type as well as gathered variable binds
match typ@(ListT typ') (ListPatternAST ps utilData) = do 
    (typs'', binds) <- matchMultiple typs' ps utilData
    case typs'' of
        []        -> return (typ, binds)
        (typ'':_) -> return (ListT typ'', binds)
    where
        typs' = List.take (length ps) (repeat typ')

-- catch all other cases and throw an exception
match typ pat = throwError $ MatchPatternMismatchError typ pat (getUtilDataPat pat)

-- helper function for the match function, 
-- which matches a list of types with a list of patterns
-- and returns the resulting types and binds
matchMultiple :: [Type] -> [PatternAST] -> UtilData -> InferT ([Type], [Binding])
matchMultiple [] [] _       = return ([], [])
matchMultiple [] _ utilData = throwError $ LengthMismatchError utilData
matchMultiple _ [] utilData = throwError $ LengthMismatchError utilData
matchMultiple (t:ts) (p:ps) utilData = do
    (typ, binds)   <- match t p
    (typs, binds') <- matchMultiple ts ps utilData
    return $ (typ:typs, binds ++ binds')

-- helper function for the match function,
-- which returns a substitution based on a list of PolyT types and substituting types
analyzeVars :: [Type] -> [Type] -> Substitution -> InferT Substitution
analyzeVars [] [] sub = return sub
analyzeVars ((PolyT tvar):t1s) (t2:t2s) sub = analyzeVars t1s t2s (Map.insert tvar t2 sub)
analyzeVars _ _ _ = error "mismatched typevariables" -- should not happen