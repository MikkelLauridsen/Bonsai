module Inference
    (
      --typeBonsai
    ) where

import Ast
import Data.Map.Strict as Map
import Data.Set as Set

type Sig = Set TermConstructor

type Constraint = (Types, Types, UtilData)

type Substitution = Map String Types

-- type-environment type
type Env = Map ExprAST Types

-- type-environment binding format
type Binding = (ExprAST, Types)

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



-- unification

unify :: [Constraint] -> Sig -> Either String Substitution
unify [] _ = Right Map.empty

unify ((typ1@(PrimType prim1), typ2@(PrimType prim2), utilData):c') sigma =
    if prim1 == prim2
        then unify c' sigma
        else Left (formatErr ("type mismatch, expected '" ++ show typ1 ++ "' but actual type is '" ++ show typ2 ++ "'") utilData)

unify ((typ1, typ2@(PolyClss var classes), utilData):c') sigma =
    if checkClasses classes typ1
        then case unify c'' sigma of
            err@(Left _) -> err
            (Right sub)  -> Right (sub `Map.union` (Map.singleton var typ1))
        else Left (formatErr ("type mismatch, expected '" ++ show typ2 ++ "' but actual type '" ++ show typ1 ++ "' does not conform to the typeclasses") utilData)
    where
        c'' = replaceTypeVar c' var typ1

unify ((typ1@(PolyClss var classes), typ2, utilData):c') sigma =
    if checkClasses classes typ2
        then case unify c'' sigma of
            err@(Left _) -> err
            (Right sub)  -> Right (sub `Map.union` (Map.singleton var typ2))
        else Left (formatErr ("type mismatch, expected '" ++ show typ1 ++ "' but actual type '" ++ show typ2 ++ "' does not conform to the typeclasses") utilData)
    where
        c'' = replaceTypeVar c' var typ2

unify ((FuncType s1 s2, FuncType t1 t2, utilData):c') sigma = unify c'' sigma
    where
        c'' = c' ++ [(s1, t1, utilData), (s2, t2, utilData)] 

unify ((TuplType typs1, TuplType typs2 , utilData):c') sigma =
    case unifyList typs1 typs2 c' sigma utilData of
        (Left msg)         -> Left msg
        (Right (sub, c'')) ->
            case unify c'' sigma of
                err@(Left _) -> err
                (Right sub')  -> Right (sub `Map.union` sub')

-- catch all
unify ((typ1, typ2, utilData):_) _ = Left (formatErr ("type mismatch, could not match expected type '" ++ show typ1 ++ "' with actual type '" ++ show typ2 ++ "'") utilData)

replaceTypeVar :: [Constraint] -> String -> Types -> [Constraint]
replaceTypeVar [] _ _                              = []
replaceTypeVar ((typ1, typ2, utilData):c') var typ = ((typ1', typ2', utilData):(replaceTypeVar c' var typ))
    where
        typ1' = substituteTypeVar typ1 var typ
        typ2' = substituteTypeVar typ2 var typ

substituteTypeVar :: Types -> String -> Types -> Types
substituteTypeVar (FuncType typ1' typ2') var typ  = FuncType (substituteTypeVar typ1' var typ) (substituteTypeVar typ2' var typ)
substituteTypeVar (TuplType typs') var typ        = TuplType [substituteTypeVar typ' var typ | typ' <- typs']
substituteTypeVar (ListType typ') var typ         = ListType (substituteTypeVar typ' var typ)
substituteTypeVar (AlgePoly typeId typs') var typ = AlgePoly typeId [substituteTypeVar typ' var typ | typ' <- typs']
substituteTypeVar (UniqType typ' valid) var typ   = UniqType (substituteTypeVar typ' var typ) valid
substituteTypeVar (PolyClss var' classes) var typ =
    if var' == var
        then typ
        else PolyClss var' classes

substituteTypeVar typ _ _ = typ

checkClasses :: [String] -> Types -> Bool
checkClasses [] _       = True
checkClasses (c:cs) typ =
    if (getClassFun c) typ
        then checkClasses cs typ
        else False

numClss  = UnbdPoly "a0" ["Num"]
eqClss   = UnbdPoly "a0" ["Eq"]
ordClss  = UnbdPoly "a0" ["Ord"]
showClss = UnbdPoly "a0" ["Show"]
biClss   = UnbdPoly "a0" ["Bi"]

getClassFun :: String -> (Types -> Bool)
getClassFun "Num"  = numFun
getClassFun "Eq"   = eqFun
getClassFun "Ord"  = ordFun
getClassFun "Show" = showFun
getClassFun "Bi"   = biFun
getClassFun _      = error "unknown typeclass."

legalTypeClasses :: [String] -> Bool
legalTypeClasses []     = True
legalTypeClasses (c:cs) =
    if legalTypeClass c
        then legalTypeClasses cs
        else False

legalTypeClass :: String -> Bool
legalTypeClass "Num"  = True
legalTypeClass "Eq"   = True
legalTypeClass "Ord"  = True
legalTypeClass "Show" = True
legalTypeClass "Bi"   = True
legalTypeClass _      = False

numFun :: Types -> Bool
numFun (PrimType IntPrim)   = True
numFun (PrimType FloatPrim) = True
numFun (PrimType CharPrim)  = True
numFun (UniqType typ _)     = numFun typ
numFun (PolyClss _ typs)    = elem "Num" typs
numFun (UnbdPoly _ typs)    = elem "Num" typs
numFun _                    = False

eqFun :: Types -> Bool
eqFun (PrimType _)      = True
eqFun (FuncType _ _)    = False
eqFun (TuplType typs)  = checkMultiple typs eqFun
eqFun (ListType typ)    = eqFun typ
eqFun EmptList          = True
eqFun (AlgeType _)      = True
eqFun (AlgePoly _ typs) = checkMultiple typs eqFun
eqFun (PolyClss _ typs) = elem "Eq" typs
eqFun (UnbdPoly _ typs) = elem "Eq" typs
eqFun (UniqType typ _)  = eqFun typ
eqFun _                 = False

ordFun :: Types -> Bool
ordFun (PrimType IntPrim)   = True
ordFun (PrimType FloatPrim) = True
ordFun (PrimType CharPrim)  = True
ordFun (ListType typ)       = ordFun typ
ordFun (UniqType typ _)     = ordFun typ
ordFun (PolyClss _ typs)    = elem "Ord" typs
ordFun (UnbdPoly _ typs)    = elem "Ord" typs
ordFun _                    = False

showFun :: Types -> Bool
showFun (PrimType _)      = True
showFun (FuncType _ _)    = False
showFun (TuplType typs)  = checkMultiple typs showFun
showFun (ListType typ)    = eqFun typ
showFun EmptList          = True
showFun (AlgeType _)      = True
showFun (AlgePoly _ typs) = checkMultiple typs showFun
showFun (PolyClss _ typs) = elem "Show" typs
showFun (UnbdPoly _ typs) = elem "Show" typs
showFun (UniqType typ _)  = showFun typ
showFun _                 = False

biFun :: Types -> Bool
biFun (PrimType IntPrim)  = True
biFun (PrimType CharPrim) = True
biFun (UniqType typ _)    = biFun typ
biFun (PolyClss _ typs)   = elem "Bi" typs
biFun (UnbdPoly _ typs)   = elem "Bi" typs
biFun _                   = False

checkMultiple :: [Types] -> (Types -> Bool) -> Bool
checkMultiple [] _           = True
checkMultiple (typ:typs) fun =
    if fun typ
        then checkMultiple typs fun
        else False

-- constraint rules begin

