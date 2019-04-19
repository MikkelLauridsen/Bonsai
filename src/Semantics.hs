module Semantics
    ( interpret
    ) where

import Ast
import Data.Map.Strict as Map
import Data.Set as Set
import System.IO
import System.Directory
import Control.Exception
import Text.Read

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
            | FileValue (Maybe Handle) Integer
            | PredefinedFileValue String Integer
            | TupleValue [Values]
            | ListValue [Values]
            | PartialValue (Values -> IO Values)

sortsType :: TypeId -> Sort
sortsType typeId = typeName typeId

sorts :: CompTypeAST -> Sort
sorts (CompSimpleAST typeId _) = sortsType typeId
sorts (CompListAST comp' _) = "[" ++ sorts comp' ++ "]"
sorts (CompTupleAST comps' _) = "(" ++ ([sorts comp' | comp' <- comps'] >>= (++ ", ")) ++ ")"
sorts (CompFuncAST comp1' comp2' _) = sorts comp1' ++ "->" ++ sorts comp2'

except :: Env -> Binding -> Env
except env (var, value) = Map.insertWith const var value env

unionSig :: Sig -> Sig -> Sig
unionSig sigma1 sigma2 = Set.union sigma1 sigma2

getVar :: Env -> VarId -> Maybe Values
getVar env var = Map.lookup var env

has :: Sig -> TypeId -> Bool
has sigma t = let termCons = Set.toList sigma
                in hasRec termCons t

hasRec :: [TermConstructor] -> TypeId -> Bool
hasRec [] _            = False
hasRec ((t', _):remainder) t = 
    if t' == t
        then True
        else hasRec remainder t

conflicts :: Sig -> Sig -> Bool
conflicts sigma1 sigma2 = conflictsHelper (Set.toList sigma1) sigma2

conflictsHelper :: [TermConstructor] -> Sig -> Bool
conflictsHelper [] _ = False
conflictsHelper ((t1, _):sigma1') sigma2 =
    if sigma2 `has` t1
        then True
        else conflictsHelper sigma1' sigma2

evaluateTermCons :: ConsAST -> TypeId -> TermConstructor
evaluateTermCons (SingleConsAST t _) typeId          = (t, ConstSig (sortsType typeId))
evaluateTermCons (DoubleConsAST t compType _) typeId = (t, FuncSig (sorts compType) (sortsType typeId))

interpret :: ProgAST -> IO (Either String Values)
interpret (ProgAST dt dv utilData) = do
    maybeSigma <- evalTypeDcl dt (Set.empty)
    case maybeSigma of
        (Left msg)    -> return $ Left msg
        (Right sigma) -> do
            maybeEnv <- evalVarDcl dv initEnv sigma
            case maybeEnv of
                (Left msg)  -> return $ Left msg
                (Right env) -> do
                    let maybeMain = env `getVar` (VarId "main" Untyped)
                        in case maybeMain of
                            Nothing -> return $ Left "error: main is not defined" -- TODO: format!
                            (Just (RecClosureValue _ x e env2 sigma2)) -> 
                                let env' = env2 `except` (x, SystemValue 0)
                                    in evalExpr e env' sigma2
                            _ -> return $ Left "error: invalid main signature." -- TODO: format!
            
    where
        stdin' = PredefinedFileValue "stdin" 0
        stdout' = PredefinedFileValue "stdout" 0
        initEnv = Map.fromList [(VarId "stdin" Untyped, stdin'), (VarId "stdout" Untyped, stdout')] 

evalTypeDcl :: TypeDclAST -> Sig -> IO (Either String Sig) -- TODO: what if the Type is redeclared?
evalTypeDcl dt sigma =
    case dt of
        EpsTypeDclAST                       -> return $ Right sigma
        TypeDclAST typeId cons dt' utilData -> do
            if sigma `conflicts` sigma2
                then return $ Left "error: cannot redefine termconstructor." -- TODO: format!
                else evalTypeDcl dt' (sigma `unionSig` sigma2)
            where
                sigma2 = (Set.fromList [evaluateTermCons ts typeId | ts <- cons])

evalVarDcl :: VarDclAST -> Env -> Sig -> IO (Either String Env)
evalVarDcl dv env sigma =
    case dv of
        EpsVarDclAST                   -> return $ Right env
        VarDclAST xt expr dv' utilData ->
            case Map.lookup x env of
                (Just _) -> return $ Left "error: cannot redeclare variable." -- TODO: format!
                Nothing  -> do
                    maybeValue <- evalExpr expr env sigma
                    case maybeValue of
                        (Left msg)    -> return $ Left msg
                        (Right value) ->
                            case value of
                                (ClosureValue x' e' env2 sigma') -> evalVarDcl dv' (env `except` (x, RecClosureValue x x' e' env2 sigma')) sigma
                                _                                -> evalVarDcl dv' (env `except` (x, value)) sigma
            where
                x = case xt of
                    UntypedVarAST varId _ -> varId
                    TypedVarAST varId _ _ -> varId

evalExpr :: ExprAST -> Env -> Sig -> IO (Either String Values)
evalExpr expr env sigma = do
    case expr of
        (VarExprAST varId _)            -> evalVarExpr varId env sigma
        (TypeExprAST typeId _)          -> return $ Right (TerValue typeId)
        (ConstExprAST c _)              -> return $ Right (ConstValue c)
        (ParenExprAST expr' _)          -> evalExpr expr' env sigma
        (LambdaExprAST varId expr' _)   -> return $ Right (ClosureValue varId expr' env sigma)
        (FunAppExprAST expr1 expr2 _)   -> evalFunApp expr1 expr2 env sigma
        (TupleExprAST exprs _)          -> evalTuple exprs env sigma
        (ListExprAST exprs _)           -> evalList exprs env sigma
        (CaseExprAST branches _)        -> evalCase branches env sigma
        (LetInExprAST xt expr1 expr2 _) -> evalLetIn xt expr1 expr2 env sigma
        (MatchExprAST expr' branches _) -> do
            maybeValue <- evalExpr expr' env sigma
            case maybeValue of
                err@(Left _) -> return err 
                (Right value) -> evalMatch value branches env sigma

evalVarExpr :: VarId -> Env -> Sig -> IO (Either String Values)
evalVarExpr varId env _ =
    case maybeValue of
        Nothing      -> return $ Left ("error: variable '" ++ varName varId ++ "' is out of scope.") -- TODO: format!
        (Just value) -> return $ Right value
    where
        maybeValue = env `getVar` varId

evalTuple :: [ExprAST] -> Env -> Sig -> IO (Either String Values)
evalTuple exprs env sigma = do
    maybeBody <- evalExprs exprs env sigma
    case maybeBody of
        (Left msg)   -> return $ Left msg
        (Right body) -> return $ Right (TupleValue body)

evalList :: [ExprAST] -> Env -> Sig -> IO (Either String Values)
evalList exprs env sigma = do
    maybeBody <- evalExprs exprs env sigma
    case maybeBody of
        (Left msg)   -> return $ Left msg
        (Right body) -> return $ Right (ListValue body)

evalExprs :: [ExprAST] -> Env -> Sig -> IO (Either String [Values])
evalExprs [] _ _           = return $ Right []
evalExprs (e:es) env sigma = do 
    maybeValue <- evalExpr e env sigma
    case maybeValue of
        (Left msg)    -> return $ Left msg
        (Right value) -> do
            maybeValues <- evalExprs es env sigma
            case maybeValues of
                (Left msg)     -> return $ Left msg
                (Right values) -> return $ Right (value:values)

evalFunApp :: ExprAST -> ExprAST -> Env -> Sig -> IO (Either String Values)
evalFunApp expr1 expr2 env sigma = do
    maybeValue <- evalExpr expr1 env sigma
    case maybeValue of
        (Left msg) -> return $ Left msg
        (Right v)  -> do
            maybeValue' <- evalExpr expr2 env sigma
            case maybeValue' of
                (Left msg) -> return $ Left msg
                (Right v') -> 
                    case v of
                        (ConstValue c)                      -> do 
                            applied <- partiallyApply c v'
                            return $ Right applied
                        (PartialValue f)                    -> do 
                            applied <- f v'
                            return $ Right applied
                        (ClosureValue x e env' sigma')      -> evalExpr e (env' `except` (x, v')) sigma'
                        (RecClosureValue f x e env' sigma') -> evalExpr e ((env' `except` (x, v')) `except` (f, v)) sigma'   
                        (TerValue t)                        -> return $ Right (TerConsValue t v')
                        _                                   -> error "error: invalid function type." -- should be prevented by typesystem

partiallyApply :: ConstAST -> Values -> IO Values
partiallyApply fun@(UnaryMinusConstAST _) value     = apply fun [value]
partiallyApply fun@(NotConstAST _) value            = apply fun [value]
partiallyApply fun@(ReadConstAST _) value           = apply fun [value]
partiallyApply fun@(ShowConstAST _) value           = apply fun [value]
partiallyApply fun@(ToIntConstAST _) value          = apply fun [value]
partiallyApply fun@(ToFloatConstAST _) value        = apply fun [value]
partiallyApply fun@(PlusConstAST _) value           = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(MinusConstAST _) value          = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(TimesConstAST _) value          = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(DivideConstAST _) value         = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(ModuloConstAST _) value         = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(EqualsConstAST _) value         = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(GreaterConstAST _) value        = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(LessConstAST _) value           = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(GreaterOrEqualConstAST _) value = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(LessOrEqualConstAST _) value    = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(AppenConstAST _) value          = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(ConcatenateConstAST _) value    = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(AndConstAST _) value            = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(OrConstAST _) value             = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(OpenReadConstAST _) value       = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(OpenWriteConstAST _) value      = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(CloseConstAST _) value          = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(WriteConstAST _) value          = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply fun@(DeleteConstAST _) value         = return $ PartialValue (\y -> apply fun [value, y])
partiallyApply _ _                          = error "error: cannot partially apply a non-function constant."

evalLetIn :: TypeVarAST -> ExprAST -> ExprAST -> Env -> Sig -> IO (Either String Values)
evalLetIn xt expr1 expr2 env sigma = do
    maybeValue <- evalExpr expr1 env sigma
    case maybeValue of
        err@(Left _)  -> return err
        (Right value) ->
            case value of
                (ClosureValue x' expr' env' sigma') -> evalExpr expr2 (env `except` (x, RecClosureValue x x' expr' env' sigma')) sigma
                _                  -> evalExpr expr2 (env `except` (x, value)) sigma
            where
                x = case xt of
                    UntypedVarAST varId _ -> varId
                    TypedVarAST varId _ _ -> varId

evalCase :: [(PredAST, ExprAST)] -> Env -> Sig -> IO (Either String Values)
evalCase [] _ _ = return $ Left "error: non-exhaustive case branches." -- TODO: format!
evalCase ((pred', expr'):branches') env sigma = do
    maybeRes <- handlePred pred' env sigma
    case maybeRes of
        (Left msg)  -> return $ Left msg
        (Right res) ->
            if res
                then evalExpr expr' env sigma
                else evalCase branches' env sigma

handlePred :: PredAST -> Env -> Sig -> IO (Either String Bool)
handlePred (PredWildAST _) _ _            = return $ Right True
handlePred (PredExprAST expr _) env sigma = do 
    maybeValue <- evalExpr expr env sigma
    case maybeValue of
        (Left msg) -> return $ Left msg
        (Right value) ->
            case value of
                (ConstValue (BoolConstAST b _)) -> return $ Right b
                _                             -> error "error: case condition must be a predicate or wildcard."


evalMatch :: Values -> [(PatternAST, ExprAST)] -> Env -> Sig -> IO (Either String Values)
evalMatch _ [] _ _                                 = return $ Left "error: non-exhaustive match branches." -- TODO: format!
evalMatch value ((pat', expr'):branches') env sigma =
    case match value pat' sigma of
        MatchFail      -> evalMatch value branches' env sigma
        Bindings delta -> evalExpr expr' (applyBindings env delta) sigma

applyBindings :: Env -> [Binding] -> Env -- TODO! test intersection ..
applyBindings env []     = env
applyBindings env (b:bs) = applyBindings (env `except` b) bs

match :: Values -> PatternAST -> Sig -> Bindings
match value (VarPatternAST varId _) _ = Bindings [(varId, value)]

match _ (WildPatternAST _) _ = Bindings []

match (ConstValue c1) (ConstPatternAST c2 _) _ = 
    if c1 == c2 
        then Bindings [] 
        else MatchFail

match (TerValue t1) (TypePatternAST t2 utilData) sigma =
    if (sigma `has` t1) && (sigma `has` t2)
        then if t1 == t2 
                then Bindings []
                else MatchFail
        else error "error: unknown term constructor." 

match (ListValue (v:vs)) (DecompPatternAST pat' varId _) sigma =
    case delta of
       MatchFail      -> MatchFail
       Bindings binds -> Bindings ((varId, v'):binds)
    where
        v' = ListValue vs
        delta = match v pat' sigma

match (TupleValue vs) (TuplePatternAST ps _) sigma = matchMultiple vs ps sigma

match (ListValue []) (ListPatternAST [] _) _ = Bindings []
match (ListValue vs) (ListPatternAST ps _) sigma = matchMultiple vs ps sigma                

match (TerConsValue t1 value) (TypeConsPatternAST t2 pat' utilData) sigma =
    if (sigma `has` t1) && (sigma `has` t2)
        then if t1 == t2 
                then match value pat' sigma
                else MatchFail
        else error "error: unknown term constructor."

match _ _ _ = MatchFail


matchMultiple :: [Values] -> [PatternAST] -> Sig -> Bindings
matchMultiple [] [] _             = Bindings []
matchMultiple (v:vs) (p:ps) sigma =
    if (length vs) /= (length ps)
        then MatchFail
        else case (bind, binds) of
                (MatchFail, _)             -> MatchFail
                (_, MatchFail)             -> MatchFail
                (Bindings l1, Bindings l2) -> Bindings (l1 ++ l2)
            where
                bind  = match v p sigma
                binds = matchMultiple vs ps sigma

matchMultiple _ _ _ = MatchFail

valueListToString :: [Values] -> String
valueListToString [] = ""
valueListToString ((ConstValue (CharConstAST c _)):cs) = (c:(valueListToString cs))
valueListToString _ = error "error: list must be of chars."

stringToValueList :: String -> [Values]
stringToValueList []     = []
stringToValueList (c:cs) = ((ConstValue (CharConstAST c initUtilData)):(stringToValueList cs))

advanceSystem :: Values -> Values
advanceSystem (SystemValue sys) = SystemValue (sys + 1)
advanceSystem _                 = error "error: cannot advance a non-system value."

advanceFile :: Values -> Values
advanceFile (FileValue h id')          = FileValue h (id' + 1)
advanceFile (PredefinedFileValue s f) = PredefinedFileValue s (f + 1)
advanceFile _                         = error "error: cannot advance a non-file value."

trueValue = ConstValue (BoolConstAST True initUtilData)
falseValue = ConstValue (BoolConstAST False initUtilData)
emptyCharValue = ConstValue (CharConstAST ' ' initUtilData)

apply :: ConstAST -> [Values] -> IO Values

-- arithmetic operators
apply (UnaryMinusConstAST utilData) [ConstValue (IntConstAST v1 _)]                                      = return $ ConstValue (IntConstAST (-v1) utilData)
apply (UnaryMinusConstAST utilData) [ConstValue (FloatConstAST v1 _)]                                    = return $ ConstValue (FloatConstAST (-v1) utilData)
apply (PlusConstAST utilData) [ConstValue (IntConstAST v1 _), ConstValue (IntConstAST v2 _)]             = return $ ConstValue (IntConstAST (v1 + v2) utilData)
apply (PlusConstAST utilData) [ConstValue (FloatConstAST v1 _), ConstValue (FloatConstAST v2 _)]         = return $ ConstValue (FloatConstAST (v1 + v2) utilData)
apply (MinusConstAST utilData) [ConstValue (IntConstAST v1 _), ConstValue (IntConstAST v2 _)]            = return $ ConstValue (IntConstAST (v1 - v2) utilData)
apply (MinusConstAST utilData) [ConstValue (FloatConstAST v1 _), ConstValue (FloatConstAST v2 _)]        = return $ ConstValue (FloatConstAST (v1 - v2) utilData)
apply (TimesConstAST utilData) [ConstValue (IntConstAST v1 _), ConstValue (IntConstAST v2 _)]            = return $ ConstValue (IntConstAST (v1 * v2) utilData)
apply (TimesConstAST utilData) [ConstValue (FloatConstAST v1 _), ConstValue (FloatConstAST v2 _)]        = return $ ConstValue (FloatConstAST (v1 * v2) utilData)
apply (DivideConstAST utilData) [ConstValue (IntConstAST v1 _), ConstValue (IntConstAST v2 _)]           = return $ ConstValue (IntConstAST (v1 `div` v2) utilData)
apply (DivideConstAST utilData) [ConstValue (FloatConstAST v1 _), ConstValue (FloatConstAST v2 _)]       = return $ ConstValue (FloatConstAST (v1 / v2) utilData)
apply (ModuloConstAST utilData) [ConstValue (IntConstAST v1 _), ConstValue (IntConstAST v2 _)]           = return $ ConstValue (IntConstAST (v1 `mod` v2) utilData)

-- boolean operators
apply (EqualsConstAST utilData) [ConstValue (BoolConstAST v1 _), ConstValue (BoolConstAST v2 _)]         = return $ ConstValue (BoolConstAST (v1 == v2) utilData)
apply (NotConstAST utilData) [ConstValue (BoolConstAST v1 _)]                                            = return $ ConstValue (BoolConstAST (not v1) utilData)
apply (GreaterConstAST utilData) [ConstValue (BoolConstAST v1 _), ConstValue (BoolConstAST v2 _)]        = return $ ConstValue (BoolConstAST (v1 > v2) utilData)
apply (LessConstAST utilData) [ConstValue (BoolConstAST v1 _), ConstValue (BoolConstAST v2 _)]           = return $ ConstValue (BoolConstAST (v1 < v2) utilData)
apply (GreaterOrEqualConstAST utilData) [ConstValue (BoolConstAST v1 _), ConstValue (BoolConstAST v2 _)] = return $ ConstValue (BoolConstAST (v1 >= v2) utilData)
apply (LessOrEqualConstAST utilData) [ConstValue (BoolConstAST v1 _), ConstValue (BoolConstAST v2 _)]    = return $ ConstValue (BoolConstAST (v1 <= v2) utilData)
apply (AndConstAST utilData) [ConstValue (BoolConstAST v1 _), ConstValue (BoolConstAST v2 _)]            = return $ ConstValue (BoolConstAST (v1 && v2) utilData)
apply (OrConstAST utilData) [ConstValue (BoolConstAST v1 _), ConstValue (BoolConstAST v2 _)]             = return $ ConstValue (BoolConstAST (v1 || v2) utilData)

-- list operations
apply (AppenConstAST _) [v1, ListValue v2]                                                    = return $ ListValue (v1:v2)
apply (ConcatenateConstAST _) [ListValue v1, ListValue v2]                                    = return $ ListValue (v1 ++ v2)

-- IO operations
apply (OpenReadConstAST _) [sys, (ListValue pathList)] = do
    e <- try (openFile path ReadMode) :: IO (Either IOException Handle)
    case e of
        (Left e)  -> return $ TupleValue [falseValue, sys', FileValue Nothing 0]
        (Right h) -> return $ TupleValue [trueValue, sys', FileValue (Just h) 0]
    where
        path = valueListToString pathList
        sys' = advanceSystem sys

apply (OpenWriteConstAST _) [sys, (ListValue pathList)] = do
    e <- try (openFile path WriteMode) :: IO (Either IOException Handle)
    case e of
        (Left e)  -> return $ TupleValue [falseValue, sys', FileValue Nothing 0]
        (Right h) -> return $ TupleValue [trueValue, sys', FileValue (Just h) 0]
    where
        path = valueListToString pathList
        sys' = advanceSystem sys

apply (CloseConstAST _) [sys, (FileValue Nothing _)] = do
    return $ TupleValue [falseValue, advanceSystem sys]

apply (CloseConstAST _) [sys, (FileValue (Just handle) _)] = do
    e <- try (hClose handle) :: IO (Either IOException ())
    case e of
        (Left e)  -> return $ TupleValue [falseValue, sys']
        (Right _) -> return $ TupleValue [trueValue, sys']
    where
        sys' = advanceSystem sys

apply (DeleteConstAST _) [sys, (FileValue Nothing _)] = do
    return $ TupleValue [falseValue, advanceSystem sys]

apply (DeleteConstAST _) [sys, (ListValue pathList)] = do
    e <- try (removeFile path) :: IO (Either IOException ())
    case e of
        (Left e)  -> return $ TupleValue [falseValue, sys']
        (Right _) -> return $ TupleValue [trueValue, sys']
    where
        path = valueListToString pathList
        sys' = advanceSystem sys

apply (ReadConstAST _) [file@(FileValue Nothing _)] = do
    return $ TupleValue [falseValue, emptyCharValue, advanceFile file]

apply (ReadConstAST _) [file@(FileValue (Just handle) _)] = do
    e <- try (hGetChar handle) :: IO (Either IOException Char)
    case e of
        (Left e)   -> return $ TupleValue [falseValue, emptyCharValue, f]
        (Right ch) -> return $ TupleValue [trueValue, ConstValue (CharConstAST ch initUtilData), f]
    where
        f = advanceFile file

apply (WriteConstAST _) [_, file@(FileValue Nothing _)] = do
    return $ TupleValue [falseValue, advanceFile file]

apply (WriteConstAST _) [(ConstValue (CharConstAST ch _)), file@(FileValue (Just handle) _)] = do
    e <- try (hPutChar handle ch) :: IO (Either IOException ())
    case e of
        (Left e)  -> return $ TupleValue [falseValue, f]
        (Right _) -> return $ TupleValue [trueValue, f]
    where
        f = advanceFile file

apply (WriteConstAST _) [(ConstValue (CharConstAST ch _)), file@(PredefinedFileValue "stdout" _)] = do
    e <- try (putChar ch) :: IO (Either IOException ())
    hFlush stdout
    case e of
        (Left e)  -> return $ TupleValue [falseValue, f]
        (Right _) -> return $ TupleValue [trueValue, f]
    where
        f = advanceFile file

apply (ReadConstAST _) [file@(PredefinedFileValue "stdin" _)] = do
    e <- try getChar :: IO (Either IOException Char)
    case e of
        (Left e)  -> return $ TupleValue [falseValue, emptyCharValue, f]
        (Right c) -> return $ TupleValue [trueValue, (ConstValue (CharConstAST c initUtilData)), f]
    where
        f = advanceFile file

-- string conversion operations
apply (ShowConstAST _) [ConstValue (CharConstAST c _)] = return $ ListValue [ConstValue (CharConstAST c initUtilData)]
apply (ShowConstAST _) [ConstValue (BoolConstAST b _)] = return $ ListValue (stringToValueList (show b))
apply (ShowConstAST _) [ConstValue (FloatConstAST f _)] = return $ ListValue (stringToValueList (show f))
apply (ShowConstAST _) [ConstValue (IntConstAST i _)] = return $ ListValue (stringToValueList (show i))
apply (ShowConstAST _) [TerValue t] = return $ ListValue (stringToValueList (typeName t))
apply (ShowConstAST _) [TerConsValue t v] = do
    res <- apply (ShowConstAST initUtilData) [v]
    case res of
        (ListValue l) -> return $ ListValue (stringToValueList (typeName t) ++ space ++ l)
        _             -> error "error: must be a list."
        where
            space = [ConstValue (CharConstAST ' ' initUtilData)]

apply (ShowConstAST _) [ListValue l] = do 
    l' <- showList' l
    return $ ListValue ([ConstValue (CharConstAST '[' initUtilData)] ++ getValueList l' ++ [ConstValue (CharConstAST ']' initUtilData)])

apply (ShowConstAST _) [TupleValue l] = do 
    l' <- showList' l
    return $ ListValue ([ConstValue (CharConstAST '(' initUtilData)] ++ getValueList l' ++ [ConstValue (CharConstAST ')' initUtilData)])

apply (ToIntConstAST _) [ListValue cs] =
    return $ case readMaybe string of
                Nothing  -> TupleValue [ConstValue (BoolConstAST False initUtilData), ConstValue (IntConstAST (-1) initUtilData)]
                (Just i) -> TupleValue [ConstValue (BoolConstAST True initUtilData), ConstValue (IntConstAST i initUtilData)]
        where
            string = valueListToString cs

apply (ToFloatConstAST _) [ListValue cs] =
    return $ case readMaybe string of
        Nothing  -> TupleValue [ConstValue (BoolConstAST False initUtilData), ConstValue (FloatConstAST (-1.0) initUtilData)]
        (Just f) -> TupleValue [ConstValue (BoolConstAST True initUtilData), ConstValue (FloatConstAST f initUtilData)]
        where
            string = valueListToString cs

apply _ _ = error "error: invalid arguments for apply."

getValueList :: Values -> [Values]
getValueList (ListValue l) = l
getValueList _ = error "error: value is not a list."

showList' :: [Values] -> IO Values
showList' []     = return $ ListValue []
showList' [v]    = apply (ShowConstAST initUtilData) [v]
showList' (v:vs) = do
    v' <- apply (ShowConstAST initUtilData) [v]
    vs' <- showList' vs
    case (v', vs') of
        (ListValue l1, ListValue l2) -> return $ ListValue (l1 ++ spacer ++ l2)
        _ -> error "error: must be a list."
        where
            spacer = [ConstValue (CharConstAST ',' initUtilData), ConstValue (CharConstAST ' ' initUtilData)]