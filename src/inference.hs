module Inference
    (
      --typeBonsai
    ) where

import Ast
import Data.Map.Strict as Map
import Data.Set as Set
import Data.List as List

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
newtype TypeEnv = TypeEnv (Map VarId Scheme)

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

    substitute sub (ForAll vars typ) = ForAll vars (substitute (List.foldr Map.delete sub vars) typ)

instance Substitutable a => Substitutable [a] where
    ftv = List.foldr (Set.union . ftv) Set.empty

    substitute = fmap . substitute -- http://dev.stephendiehl.com/fun/006_hindley_milner.html

instance Substitutable TypeEnv where
    ftv (TypeEnv env) = ftv (Map.elems env)

    substitute sub (TypeEnv env) = TypeEnv (Map.map (substitute sub) env)

genTVarName :: Integer -> String
genTVarName count = ".a" ++ show count

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

unifyAll :: [Constraint] -> Sig -> Either String Substitution
unifyAll [] _ = Right Map.empty
unifyAll ((typ1, typ2, utilData):c') sigma =
    case unify typ1 typ2 c' utilData sigma of
        (Left msg)          -> Left msg
        (Right (sub, c''))  ->
            case unifyAll c'' sigma of
                err@(Left _) -> err
                (Right sub') -> Right (sub `Map.union` sub') 

unify :: Type -> Type -> [Constraint] -> UtilData -> Sig -> Either String (Substitution, [Constraint])
unify typ1@(PrimT prim1) typ2@(PrimT prim2) constraints utilData _ =
    if prim1 == prim2
        then Right (Map.empty, constraints)
        else Left (formatErr ("type mismatch, expected '" ++ show typ1 ++ "' but actual type is '" ++ show typ2 ++ "'") utilData)

unify typ1@(PolyT var@(TVar _ classes)) typ2 constraints utilData _ =
    if List.foldr ((&&) . (checkClass typ2)) True classes
        then Right (sub, constraints')
        else Left (formatErr ("type mismatch, expected '" ++ show typ1 ++ "' but actual type '" ++ show typ2 ++ "' does not conform to the typeclasses") utilData)
    where
        sub = Map.singleton var typ2
        substituteConstraint = \(t1, t2, utilData) -> (substitute sub t1, substitute sub t2, utilData)
        constraints' = List.map (substituteConstraint) constraints

unify typ1 typ2@(PolyT var@(TVar _ classes)) constraints utilData _ =
    if List.foldr ((&&) . (checkClass typ1)) True classes
        then Right (sub, constraints')
        else Left (formatErr ("type mismatch, expected '" ++ show typ1 ++ "' but actual type '" ++ show typ2 ++ "' does not conform to the typeclasses") utilData)
    where
        sub = Map.singleton var typ1
        substituteConstraint = \(t1, t2, utilData) -> (substitute sub t1, substitute sub t2, utilData)
        constraints' = List.map (substituteConstraint) constraints

unify (FuncT s1 s2) (FuncT t1 t2) constraints utilData _ = Right (Map.empty, constraints')
    where
        constraints' = constraints ++ [(s1, t1, utilData), (s2, t2, utilData)]

unify typ1@(TuplT typs1) typ2@(TuplT typs2) constraints utilData _ =
    if length typs1 == length typs2
        then Right (Map.empty, constraints')
        else Left (formatErr ("type mismatch, could not match expected type '" ++ show typ1 ++ "' with actual type '" ++ show typ2 ++ "'") utilData)
    where
        constraints' = constraints ++ [(typ1', typ2', utilData) | typ1' <- typs1, typ2' <- typs2]

unify (ListT typ1) (ListT typ2) constraints utilData _ = Right (Map.empty, constraints')
    where
        constraints' = constraints ++ [(typ1, typ2, utilData)]

unify typ1@(AlgeT name1 typs1) typ2@(AlgeT name2 typs2) constraints utilData sigma =
    if name1 == name2 && sigma `has` name1 && length typs1 == length typs2
        then Right (Map.empty, constraints')
        else Left (formatErr ("type mismatch, could not match expected type '" ++ show typ1 ++ "' with actual type '" ++ show typ2 ++ "'") utilData)
    where
        constraints' = constraints ++ [(typ1', typ2', utilData) | typ1' <- typs1, typ2' <- typs2]

unify typ1@(UniqT typ1' valid1) typ2@(UniqT typ2' valid2) constraints utilData _ =
    case (valid1, valid2) of
        (False, False) -> Left (formatErr ("unique types '" ++ show typ1 ++ "' and '" ++ show typ2 ++ "'cannot be used twice") utilData)
        (False, True)  -> Left (formatErr ("unique type '" ++ show typ1 ++ "' cannot be used twice") utilData)
        (True, False)  -> Left (formatErr ("unique type '" ++ show typ2 ++ "' cannot be used twice") utilData)
        _              -> Right (Map.empty, constraints')
    where
        constraints' = constraints ++ [(typ1', typ2', utilData)]

-- catch all
unify typ1 typ2 _ utilData _ = Left (formatErr ("type mismatch, could not match expected type '" ++ show typ1 ++ "' with actual type '" ++ show typ2 ++ "'") utilData)

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

