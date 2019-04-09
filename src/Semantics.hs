module Semantics
    (
    ) where

import Ast
import Data.Map.Strict as Map
import Data.Set as Set

type Sort = String

data Signature = ConstSig TypeId
               | FuncSig Sort TypeId
               deriving (Eq, Ord)

type TermConstructor = (TypeId, Signature)
type Env = Map VarId Values
type Sig = Set TermConstructor

data Values = ConstValue ConstAST
            | ClosureValue VarId ExprAST Env Sig
            | RecClosureValue VarId VarId ExprAST Env Sig

sorts :: CompTypeAST -> Sort
sorts (CompSimpleAST typeId) = typeName typeId
sorts (CompListAST comp') = "[" ++ sorts comp' ++ "]"
sorts (CompTupleAST comps') = "(" ++ ([sorts comp' | comp' <- comps'] >>= (++ ", ")) ++ ")"
sorts (CompFuncAST comp1' comp2') = sorts comp1' ++ "->" ++ sorts comp2'

except :: Env -> (VarId, Values) -> Env
except env (var, value) = Map.insertWith const var value env

union :: Sig -> Sig -> Sig
union sigma1 sigma2 = Set.union sigma1 sigma2

getVar :: Env -> VarId -> Maybe Values
getVar env var = Map.lookup var env

member :: TermConstructor -> Sig -> Bool
member termCon sigma = Set.Member termCon sigma

interpret :: ProgAST -> IO ()
interpret (ProgAST dt dv) = do
    sigma <- typeDcl dt (Map.empty)
    env <- varDcl dv (Set.empty) sigma
    case main of
        (Just var) -> prog var world env sigma
        None       -> putStrLn "error: main is not defined" -- TODO: use Alex handledError ..
    where
        main = env `getVar` (VarId "main")
        world = -- TODO!

typeDcl :: [TypeDclAST] -> Env -> IO Sig
-- TODO!

varDcl :: [VarDclAST] -> Env -> Sig -> IO Env
-- TODO!

prog :: Values -> Values -> Env -> Sig -> IO Values
-- TODO!