module Semantics
    ( interpret
    ) where

import Ast
import Data.Map.Strict as Map
import Data.Set as Set

type Sort = String

data Signature = ConstSig Sort
               | FuncSig Sort Sort
               deriving (Eq, Ord)

type TermConstructor = (TypeId, Signature)
type Env = Map VarId Values
type Sig = Set TermConstructor

data Values = ConstValue ConstAST
            | ClosureValue VarId ExprAST Env Sig
            | RecClosureValue VarId VarId ExprAST Env Sig
            | SystemValue Integer --TODO: add Files, tuples and lists

sortsType :: TypeId -> Sort
sortsType typeId = typeName typeId

sorts :: CompTypeAST -> Sort
sorts (CompSimpleAST typeId) = sortsType typeId
sorts (CompListAST comp') = "[" ++ sorts comp' ++ "]"
sorts (CompTupleAST comps') = "(" ++ ([sorts comp' | comp' <- comps'] >>= (++ ", ")) ++ ")"
sorts (CompFuncAST comp1' comp2') = sorts comp1' ++ "->" ++ sorts comp2'

except :: Env -> (VarId, Values) -> Env
except env (var, value) = Map.insertWith const var value env

unionSig :: Sig -> Sig -> Sig
unionSig sigma1 sigma2 = Set.union sigma1 sigma2

getVar :: Env -> VarId -> Maybe Values
getVar env var = Map.lookup var env

member :: TermConstructor -> Sig -> Bool
member termCon sigma = Set.member termCon sigma

evaluateTermCons :: ConsAST -> TypeId -> TermConstructor
evaluateTermCons SingleConsAST t typeId          = (t, ConstSig (sortsType typeId))
evaluateTermCons DoubleConsAST t compType typeId = (t, FuncSig (sorts compType) (sortsType typeId))

interpret :: ProgAST -> IO ()
interpret (ProgAST dt dv) = do
    sigma <- evalTypeDcl dt (Set.empty)
    env <- evalVarDcl dv (Map.empty) sigma
    case maybeMain of
        (Just main) -> evalExpr main env' sigma
        Nothing    -> putStrLn "error: main is not defined" -- TODO: use Alex handledError ..
    where
        maybeMain = env `getVar` (VarId "main")
        env' = env `except` (VarId "system", SystemValue 0)

evalTypeDcl :: TypeDclAST -> Sig -> IO Sig
evalTypeDcl dt sigma = do
    case dt of
        TypeDclAST typeId cons dt' -> do
            sigma' <- evalTypeDcl dt' sigma
            return $ sigma' `unionSig` (Set.fromList [evaluateTermCons ts typeId | ts <- cons]) 
        EpsTypeDclAST              -> return sigma

evalVarDcl :: VarDclAST -> Env -> Sig -> IO Env
evalVarDcl dv env sigma = do
    case dv of
        VarDclAST xt expr dv' -> do
            env' <- evalVarDcl dv' env sigma
            value <- evalExpr expr env sigma
            return $ env' `except` (x, value)
        EpsVarDclAST          -> return env
    where
        x = case xt of
                UntypedVarAST varId -> varId
                TypedVarAST varId _ -> varId

evalExpr :: ExprAST -> Env -> Sig -> IO Values
evalExpr expr env sigma = do
    --TODO!
