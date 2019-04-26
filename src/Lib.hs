module Lib
    ( 
    ) where

import Ast
-- this file is used to store functions that will be used elsewhere later!

-- storage type for sets of termconstructor names and associated signatures
type TermConstructor = (TypeId, Signature)

-- types are stored as formated strings
type Sort = String

-- signatures for termconstructors
data Signature = ConstSig Sort -- T
               | FuncSig Sort Sort -- Composite -> T
               deriving (Eq, Ord)

-- implementation of the sorts function
-- recursively constructs a formated string (Sort)
sorts :: CompTypeAST -> Sort
sorts (CompSimpleAST typeId _) = typeName typeId
sorts (CompSimplePolyAST varId _) = varName varId
sorts (CompPolyAST typeId comps' _) = typeName typeId ++ "<" ++ ([sorts comp' | comp' <- comps'] >>= (++ ", ")) ++ ">"
sorts (CompListAST comp' _) = "[" ++ sorts comp' ++ "]"
sorts (CompTupleAST comps' _) = "(" ++ ([sorts comp' | comp' <- comps'] >>= (++ ", ")) ++ ")"
sorts (CompFuncAST comp1' comp2' _) = sorts comp1' ++ "->" ++ sorts comp2'



-- implementation of (varErk-1) and (varErk-2)
-- returns an error message if:
--  1. the same variable name is declared more than once
-- otherwise, a recursively defined variabel environment is returned 
evalVarDcl :: VarDclAST -> Env -> Sig -> IO (Either String Env)
evalVarDcl dv env sigma =
    case dv of
        EpsVarDclAST                   -> return $ Right env
        VarDclAST xt expr dv' utilData ->
            case Map.lookup x env of
                (Just _) -> return $ Left (formatErr ("cannot redeclare variable '" ++ (varName x) ++ "'") utilData)
                Nothing  -> do
                    maybeValue <- evalExpr expr env sigma
                    case maybeValue of
                        (Left msg)    -> return $ Left msg
                        (Right value) ->
                            case value of
                                (ClosureValue x' e' env2 sigma') -> evalVarDcl dv' (env `except` (x, RecClosureValue x x' e' env2 sigma')) sigma
                                _                                -> evalVarDcl dv' (env `except` (x, value)) sigma
            where
                -- extract 'x' from the AST
                x = case xt of
                    UntypedVarAST varId _ -> varId
                    TypedVarAST varId _ _ -> varId