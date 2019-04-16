module Semantics
    ( interpret
    ) where

import Ast
import Data.Map.Strict as Map
import Data.Set as Set
import System.IO

type Sort = String

data Signature = ConstSig Sort
               | FuncSig Sort Sort
               deriving (Eq, Ord)

type Binding = (VarId, Values)

data Bindings = MatchFail
              | Bindings [(VarId, Values)]

type TermConstructor = (TypeId, Signature)
type Env = Map VarId Values
type Sig = Set TermConstructor

data Values = ConstValue ConstAST
            | TerValue TypeId
            | TerConsValue TypeId Values
            | ClosureValue VarId ExprAST Env Sig
            | RecClosureValue VarId VarId ExprAST Env Sig
            | SystemValue Integer
            | FileValue Handle
            | TupleValue [Values]
            | ListValue [Values]
            | PartialValue (Values -> IO Values)

sortsType :: TypeId -> Sort
sortsType typeId = typeName typeId

sorts :: CompTypeAST -> Sort
sorts (CompSimpleAST typeId) = sortsType typeId
sorts (CompListAST comp') = "[" ++ sorts comp' ++ "]"
sorts (CompTupleAST comps') = "(" ++ ([sorts comp' | comp' <- comps'] >>= (++ ", ")) ++ ")"
sorts (CompFuncAST comp1' comp2') = sorts comp1' ++ "->" ++ sorts comp2'

except :: Env -> Binding -> Env
except env (var, value) = Map.insertWith const var value env

unionSig :: Sig -> Sig -> Sig
unionSig sigma1 sigma2 = Set.union sigma1 sigma2

getVar :: Env -> VarId -> Maybe Values
getVar env var = Map.lookup var env

memberSig :: TermConstructor -> Sig -> Bool
memberSig termCon sigma = Set.member termCon sigma

has :: Sig -> TypeId -> Bool
has sigma t = let termCons = Set.toList sigma
                in hasRec termCons t

hasRec :: [TermConstructor] -> TypeId -> Bool
hasRec [] _            = False
hasRec ((t', _):rem) t = 
    if t' == t
        then True
        else hasRec rem t

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

evalFunApp :: ExprAST -> ExprAST -> Env -> Sig -> IO Values
evalFunApp expr1 expr2 env sigma = do
    v <- evalExpr expr1 env sigma
    v' <- evalExpr expr2 env sigma
    case v of
        (ConstValue c) -> partiallyApply c v'
        (PartialValue f) -> f v'
        (ClosureValue x e env' sigma') -> evalExpr e (env' `except` (x, v')) sigma'
        body@(RecClosureValue f x e env' sigma') -> evalExpr e ((env' `except` (x, v')) `except` (f, body)) sigma'   
        (TerValue t) -> return $ TerConsValue t v'

partiallyApply :: ConstAST -> Values -> IO Values
partiallyApply UnaryMinusConstAST value     = return $ apply UnaryMinusConstAST [value]
partiallyApply NotConstAST value            = return $ apply NotConstAST [value]
partiallyApply PlusConstAST value           = return $ PartialValue (\y -> apply PlusConstAST [value, y])
partiallyApply MinusConstAST value          = return $ PartialValue (\y -> apply MinusConstAST [value, y])
partiallyApply TimesConstAST value          = return $ PartialValue (\y -> apply TimesConstAST [value, y])
partiallyApply DivideConstAST value         = return $ PartialValue (\y -> apply DivideConstAST [value, y])
partiallyApply ModuloConstAST value         = return $ PartialValue (\y -> apply ModuloConstAST [value, y])
partiallyApply EqualsConstAST value         = return $ PartialValue (\y -> apply EqualsConstAST [value, y])
partiallyApply GreaterConstAST value        = return $ PartialValue (\y -> apply GreaterConstAST [value, y])
partiallyApply LessConstAST value           = return $ PartialValue (\y -> apply LessConstAST [value, y])
partiallyApply GreaterOrEqualConstAST value = return $ PartialValue (\y -> apply GreaterOrEqualConstAST [value, y])
partiallyApply LessOrEqualConstAST value    = return $ PartialValue (\y -> apply LessOrEqualConstAST [value, y])
partiallyApply AppenConstAST value          = return $ PartialValue (\y -> apply AppenConstAST [value, y])
partiallyApply ConcatenateConstAST value    = return $ PartialValue (\y -> apply ConcatenateConstAST [value, y])
partiallyApply AndConstAST value            = return $ PartialValue (\y -> apply AndConstAST [value, y])
partiallyApply OrConstAST value             = return $ PartialValue (\y -> apply OrConstAST [value, y])
partiallyApply OpenConstAST value           = return $ PartialValue (\y -> apply OpenConstAST [value, y])
partiallyApply CloseConstAST value          = return $ PartialValue (\y -> apply CloseConstAST [value, y])
partiallyApply ReadConstAST value           = return $ PartialValue (\y -> apply ReadConstAST [value, y])
partiallyApply WriteConstAST value          = return $ PartialValue (\y -> apply WriteConstAST [value, y])

evalLetIn :: TypeVarAST -> ExprAST -> ExprAST -> Env -> Sig -> IO Values
evalLetIn xt expr1 expr2 env sigma = do
    value <- evalExpr expr1 env sigma
    case value of
        (ClosureValue x' expr' env' sigma') -> evalExpr expr2 (env `except` (x, RecClosureValue x x' env' sigma')) sigma
        _                  -> evalExpr expr2 (env `except` (x, value)) sigma
    where
        x = case xt of
            UntypedVarAST varId -> varId
            TypedVarAST varId _ -> varId

evalMatch :: ExprAST -> [(PatternAST, ExprAST)] -> Env -> Sig -> IO Values
evalMatch _ [] _ _                                 = error "error: non-exhaustive match branches."
evalMatch expr ((pat', expr'):branches') env sigma = do
    bindings <- match expr pat' env sigma
    case bindings of
        MatchFail      -> evalMatch expr branches' env sigma
        Bindings delta -> evalExpr expr' (applyBindings env delta) sigma

applyBindings :: Env -> [Binding] -> Env
applyBindings env []     = env
applyBindings env (b:bs) = let env' = env `except` b 
                             in applyBindings env' bs

match :: ExprAST -> PatternAST -> Env -> Sig -> IO Bindings
match expr (VarPatternAST varId) env sigma = do 
    value <- evalExpr expr env sigma
    return $ Bindings [(varId, value)]

match _ WildPatternAST _ _ = return $ Bindings []

match (ConstExprAST c1) (ConstPatternAST c2) _ _ = 
    return $ if c1 == c2 
                then Bindings [] 
                else MatchFail

match (TypeExprAST t1) (TypePatternAST t2) _ sigma =
    if (sigma `has` t1) && (sigma `has` t2)
        then if t1 == t2 
                then return $ Bindings []
                else return $ MatchFail
        else error "error: unknown term constructor." 

match (ListExprAST (e:es)) (DecompPatternAST pat' varId) env sigma = do
    value <- evalExpr (ListExprAST es) env sigma
    delta <- match e pat' env sigma
    return $ case delta of
                MatchFail      -> MatchFail
                Bindings binds -> Bindings ((varId, value):binds)

match (TupleExprAST es) (TuplePatternAST ps) env sigma = matchMultiple es ps env sigma

match (ListExprAST es) (ListPatternAST ps) env sigma = matchMultiple es ps env sigma                

match (FunAppExprAST (TypeExprAST t1) expr') (TypeConsPatternAST t2 pat') env sigma =
    if (sigma `has` t1) && (sigma `has` t2)
        then if t1 == t2 
                then match expr' pat' env sigma
                else return $ MatchFail
        else error "error: unknown term constructor."

match _ _ _ _ = return MatchFail


matchMultiple :: [ExprAST] -> [PatternAST] -> Env -> Sig -> IO Bindings
matchMultiple [] [] _ _               = return $ Bindings []
matchMultiple (e:es) (p:ps) env sigma =
    if (length es) /= (length ps)
        then return $ MatchFail
        else do
            binds <- matchMultiple es ps env sigma
            bind  <- match e p env sigma
            return $ case (bind, binds) of
                        (MatchFail, _)             -> MatchFail
                        (_, MatchFail)             -> MatchFail
                        (Bindings l1, Bindings l2) -> Bindings (l1 ++ l2) 
    
-- apply

apply PlusConstAST [ConstValue (IntConstAST v1), ConstValue (IntConstAST v2)] = ConstValue (IntConstAST (v1 + v2))
apply PlusConstAST [ConstValue (FloatConstAST v1), ConstValue (FloatConstAST v2)] = ConstValue (FloatConstAST (v1 + v2))