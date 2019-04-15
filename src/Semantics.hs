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
            | TerValue TypeId
            | ClosureValue VarId ExprAST Env Sig
            | RecClosureValue VarId VarId ExprAST Env Sig
            | SystemValue Integer --TODO: add Files, tuples and lists
            | TupleValue [Values]
            | ListValue [Values]

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

memberSig :: TermConstructor -> Sig -> Bool
memberSig termCon sigma = Set.member termCon sigma

evaluateTermCons :: ConsAST -> TypeId -> TermConstructor
evaluateTermCons (SingleConsAST t) typeId          = (t, ConstSig (sortsType typeId))
evaluateTermCons (DoubleConsAST t compType) typeId = (t, FuncSig (sorts compType) (sortsType typeId))

interpret :: ProgAST -> IO ()
interpret (ProgAST dt dv) = do
    sigma <- evalTypeDcl dt (Set.empty)
    env <- evalVarDcl dv (Map.empty) sigma
    let maybeMain = env `getVar` (VarId "main")
      in case maybeMain of
            Nothing     -> putStrLn "error: main is not defined" -- TODO: use Alex handledError ..
            (Just main) -> let env' = env `except` (VarId "system", SystemValue 0)
                             in evalExpr main env' sigma

evalTypeDcl :: TypeDclAST -> Sig -> IO Sig
evalTypeDcl dt sigma =
    case dt of
        EpsTypeDclAST              -> return sigma
        TypeDclAST typeId cons dt' -> do
            sigma' <- evalTypeDcl dt' sigma
            return $ sigma' `unionSig` (Set.fromList [evaluateTermCons ts typeId | ts <- cons]) 

evalVarDcl :: VarDclAST -> Env -> Sig -> IO Env
evalVarDcl dv env sigma =
    case dv of
        EpsVarDclAST          -> return env
        VarDclAST xt expr dv' -> do
            env' <- evalVarDcl dv' env sigma
            value <- evalExpr expr env sigma
            return $ env' `except` (x, value) 
            where 
                x = case xt of
                        UntypedVarAST varId -> varId
                        TypedVarAST varId _ -> varId

evalExpr :: ExprAST -> Env -> Sig -> IO Values
evalExpr expr env sigma = do
    case expr of
        (VarExprAST varId)            -> evalVarExpr varId env sigma
        (TypeExprAST typeId)          -> return $ TerValue typeId
        (ConstExprAST const)          -> return $ ConstValue const
        (ParenExprAST expr')          -> evalExpr expr' env sigma
        (LambdaExprAST varId expr')   -> return $ ClosureValue varId expr' env sigma
        (FunAppExprAST expr1 expr2)   -> evalFunApp expr1 expr2 env sigma
        (TupleExprAST exprs)          -> evalTuple exprs env sigma
        (ListExprAST exprs)           -> evalList exprs env sigma
        (MatchExprAST expr' branches) -> evalMatch expr' branches env sigma
        (CaseExprAST branches)        -> evalCase branches env sigma
        (LetInExprAST xt expr1 expr2) -> evalLetIn xt expr1 expr2 env sigma

evalVarExpr :: VarId -> Env -> Sig -> IO Values
evalVarExpr varId env sigma =
    case maybeValue of
        Nothing      -> error "error: variable '" ++ varName varId ++ "' is out of scope." -- TODO! (consider Either?)
        (Just value) -> return value
    where
        maybeValue = env `getVar` varId

evalTuple :: [ExprAST] -> Env -> Sig -> IO Values
evalTuple exprs env sigma = do
    body <- evalExprs exprs env sigma
    return $ TupleValue body

evalList :: [ExprAST] -> Env -> Sig -> IO Values
evalList exprs env sigma = do
    body <- evalExprs exprs env sigma
    return $ ListValue body

evalExprs :: [ExprAST] -> Env -> Sig -> IO [Values]
evalExprs [] _ _           = return []
evalExprs (e:es) env sigma = do 
    value <- evalExpr e env sigma
    values <- evalExprs es env sigma
    return (value:values)

evalLetIn :: TypeVarAST -> ExprAST -> ExprAST -> Env -> Sig -> IO Values