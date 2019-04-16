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
            | FileValue Handle Integer
            | PredefinedFileValue String Integer
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
hasRec ((t', _):remainder) t = 
    if t' == t
        then True
        else hasRec remainder t

evaluateTermCons :: ConsAST -> TypeId -> TermConstructor
evaluateTermCons (SingleConsAST t) typeId          = (t, ConstSig (sortsType typeId))
evaluateTermCons (DoubleConsAST t compType) typeId = (t, FuncSig (sorts compType) (sortsType typeId))

interpret :: ProgAST -> IO Values
interpret (ProgAST dt dv) = do
    sigma <- evalTypeDcl dt (Set.empty)
    env <- evalVarDcl dv initEnv sigma
    let (maybeMain) = env `getVar` (VarId "main")
      in case maybeMain of
            Nothing -> error "error: main is not defined"
            (Just (ClosureValue x e env2 sigma2)) -> let env' = env2 `except` (x, SystemValue 0)
                             in evalExpr e env' sigma2
    where
        stdin' = PredefinedFileValue "stdin" 0
        stdout' = PredefinedFileValue "stdout" 0
        initEnv = Map.fromList [(VarId "stdin", stdin'), (VarId "stdout", stdout')] 

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
        (ConstExprAST c)          -> return $ ConstValue c
        (ParenExprAST expr')          -> evalExpr expr' env sigma
        (LambdaExprAST varId expr')   -> return $ ClosureValue varId expr' env sigma
        (FunAppExprAST expr1 expr2)   -> evalFunApp expr1 expr2 env sigma
        (TupleExprAST exprs)          -> evalTuple exprs env sigma
        (ListExprAST exprs)           -> evalList exprs env sigma
        (MatchExprAST expr' branches) -> evalMatch expr' branches env sigma
        (CaseExprAST branches)        -> evalCase branches env sigma
        (LetInExprAST xt expr1 expr2) -> evalLetIn xt expr1 expr2 env sigma

evalVarExpr :: VarId -> Env -> Sig -> IO Values
evalVarExpr varId env _ =
    case maybeValue of
        Nothing      -> error $ "error: variable '" ++ varName varId ++ "' is out of scope."
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
partiallyApply UnaryMinusConstAST value     = apply UnaryMinusConstAST [value]
partiallyApply NotConstAST value            = apply NotConstAST [value]
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
        (ClosureValue x' expr' env' sigma') -> evalExpr expr2 (env `except` (x, RecClosureValue x x' expr' env' sigma')) sigma
        _                  -> evalExpr expr2 (env `except` (x, value)) sigma
    where
        x = case xt of
            UntypedVarAST varId -> varId
            TypedVarAST varId _ -> varId

evalCase :: [(PredAST, ExprAST)] -> Env -> Sig -> IO Values
evalCase [] _ _ = error "error: non-exhaustive case branches."
evalCase ((pred, expr'):branches') env sigma = do
    res <- handlePred pred env sigma
    if res
        then evalExpr expr' env sigma
        else evalCase branches' env sigma

handlePred :: PredAST -> Env -> Sig -> IO Bool
handlePred PredWildAST _ _              = return True
handlePred (PredExprAST expr) env sigma = do 
    res <- evalExpr expr env sigma
    case res of
        (ConstValue (BoolConstAST b)) -> return b


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


apply :: ConstAST -> [Values] -> IO Values
apply UnaryMinusConstAST [ConstValue (IntConstAST v1)]                                    = return $ ConstValue (IntConstAST (-v1))
apply UnaryMinusConstAST [ConstValue (FloatConstAST v1)]                                  = return $ ConstValue (FloatConstAST (-v1))
apply PlusConstAST [ConstValue (IntConstAST v1), ConstValue (IntConstAST v2)]             = return $ ConstValue (IntConstAST (v1 + v2))
apply PlusConstAST [ConstValue (FloatConstAST v1), ConstValue (FloatConstAST v2)]         = return $ ConstValue (FloatConstAST (v1 + v2))
apply MinusConstAST [ConstValue (IntConstAST v1), ConstValue (IntConstAST v2)]            = return $ ConstValue (IntConstAST (v1 - v2))
apply MinusConstAST [ConstValue (FloatConstAST v1), ConstValue (FloatConstAST v2)]        = return $ ConstValue (FloatConstAST (v1 - v2))
apply TimesConstAST [ConstValue (IntConstAST v1), ConstValue (IntConstAST v2)]            = return $ ConstValue (IntConstAST (v1 * v2))
apply TimesConstAST [ConstValue (FloatConstAST v1), ConstValue (FloatConstAST v2)]        = return $ ConstValue (FloatConstAST (v1 * v2))
apply DivideConstAST [ConstValue (IntConstAST v1), ConstValue (IntConstAST v2)]           = return $ ConstValue (IntConstAST (v1 `div` v2))
apply DivideConstAST [ConstValue (FloatConstAST v1), ConstValue (FloatConstAST v2)]       = return $ ConstValue (FloatConstAST (v1 / v2))
apply ModuloConstAST [ConstValue (IntConstAST v1), ConstValue (IntConstAST v2)]           = return $ ConstValue (IntConstAST (v1 `mod` v2))
apply EqualsConstAST [ConstValue (BoolConstAST v1), ConstValue (BoolConstAST v2)]         = return $ ConstValue (BoolConstAST (v1 == v2))
apply NotConstAST [ConstValue (BoolConstAST v1)]                                          = return $ ConstValue (BoolConstAST (not v1))
apply GreaterConstAST [ConstValue (BoolConstAST v1), ConstValue (BoolConstAST v2)]        = return $ ConstValue (BoolConstAST (v1 > v2))
apply LessConstAST [ConstValue (BoolConstAST v1), ConstValue (BoolConstAST v2)]           = return $ ConstValue (BoolConstAST (v1 < v2))
apply GreaterOrEqualConstAST [ConstValue (BoolConstAST v1), ConstValue (BoolConstAST v2)] = return $ ConstValue (BoolConstAST (v1 >= v2))
apply LessOrEqualConstAST [ConstValue (BoolConstAST v1), ConstValue (BoolConstAST v2)]    = return $ ConstValue (BoolConstAST (v1 <= v2))
apply AppenConstAST [v1, ListValue v2]                                                    = return $ ListValue (v1:v2)
apply ConcatenateConstAST [ListValue v1, ListValue v2]                                    = return $ ListValue (v1 ++ v2)
apply AndConstAST [ConstValue (BoolConstAST v1), ConstValue (BoolConstAST v2)]            = return $ ConstValue (BoolConstAST (v1 && v2))
apply OrConstAST [ConstValue (BoolConstAST v1), ConstValue (BoolConstAST v2)]             = return $ ConstValue (BoolConstAST (v1 || v2))


--TODO !! udvid apply for boolske operatorer: hvad med tal osv?