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
import Data.List

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

instance Eq Values where
    ConstValue c1 == ConstValue c2 = c1 == c2
    TerValue t1 == TerValue t2 = t1 == t2
    TerConsValue t1 v1 == TerConsValue t2 v2 = t1 == t2 && v1 == v2
    SystemValue i1 == SystemValue i2 = i1 == i2
    FileValue m1 i1 == FileValue m2 i2 = m1 == m2 && i1 == i2
    PredefinedFileValue s1 i1 == PredefinedFileValue s2 i2 = s1 == s2 && i1 == i2
    TupleValue t1 == TupleValue t2 = t1 == t2
    ListValue l1 == ListValue l2 = l1 == l2
    _ == _ = False -- it is impossible to check whether two functions are equivalent, as their environments may vary


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
has sigma t = hasRec (Set.toList sigma) t

hasRec :: [TermConstructor] -> TypeId -> Bool
hasRec [] _                  = False
hasRec ((t', _):remainder) t = 
    if t' == t
        then True
        else hasRec remainder t

hasType :: Sig -> Sort -> Bool
hasType sigma s = hasTypeRec (Set.toList sigma) s

hasTypeRec :: [TermConstructor] -> Sort -> Bool
hasTypeRec [] _             = False
hasTypeRec ((_, s2):tcs) s1 =
    if (findType s2) == s1
        then True
        else hasTypeRec tcs s1

findType :: Signature -> Sort
findType (ConstSig s)  = s
findType (FuncSig _ s) = s

conflicts :: Sig -> Sig -> Maybe String
conflicts sigma1 sigma2 = 
    case conflictsHelper (Set.toList sigma1) sigma2 of
        err@(Just _) -> err
        Nothing      -> testTypes (Set.toList sigma1) sigma2                        

conflictsHelper :: [TermConstructor] -> Sig -> Maybe String
conflictsHelper [] _                     = Nothing
conflictsHelper ((t1, _):sigma1') sigma2 =
    if sigma2 `has` t1
        then Just (typeName t1)
        else conflictsHelper sigma1' sigma2

testTypes :: [TermConstructor] -> Sig -> Maybe String
testTypes [] _                = Nothing
testTypes ((_, s):tcs) sigma2 =
    if sigma2 `hasType` (findType s)
        then Just (findType s)
        else testTypes tcs sigma2

hasConflicts :: Sig -> Maybe String
hasConflicts sigma = hasConflictsHelper (Set.toList sigma)

hasConflictsHelper :: [TermConstructor] -> Maybe String
hasConflictsHelper []           = Nothing
hasConflictsHelper ((t, _):tcs) =
    if hasRec tcs t
        then Just (typeName t)
        else hasConflictsHelper tcs

evaluateTermCons :: ConsAST -> TypeId -> TermConstructor
evaluateTermCons (SingleConsAST t _) typeId          = (t, ConstSig (sortsType typeId))
evaluateTermCons (DoubleConsAST t compType _) typeId = (t, FuncSig (sorts compType) (sortsType typeId))

formatErr :: String -> UtilData -> String
formatErr err UtilData{position=pos, sourceLine=line} = 
    let (l, c, o) = pos
        in (show l ++ ":" ++ show c ++ ": error: " ++ 
            err ++ " in:\n" ++
            (Prelude.take (o - 1) (repeat ' ')) ++ line ++ "\n" ++ 
            (getIndicator (o - 1) (length line)))

getIndicator :: Int -> Int -> String
getIndicator offset len = Prelude.take offset (repeat ' ') ++ Prelude.take len (repeat '^')

interpret :: FilePath -> ProgAST -> IO (Either String Values)
interpret path (ProgAST dt dv _) = do
    maybeSigma <- evalTypeDcl dt (Set.empty)
    case maybeSigma of
        (Left msg)    -> return $ Left (path ++ ":" ++ msg)
        (Right sigma) -> do
            maybeEnv <- evalVarDcl dv initEnv sigma
            case maybeEnv of
                (Left msg)  -> return $ Left (path ++ ":" ++ msg)
                (Right env) -> do
                    let maybeMain = env `getVar` (VarId "main" Untyped)
                        in case maybeMain of
                            Nothing -> return $ Left (path ++ ":--:--: error: main is not defined")
                            (Just (RecClosureValue _ x e env2 sigma2)) -> do
                                let env' = env2 `except` (x, SystemValue 0)
                                res <- evalExpr e env' sigma2
                                case res of
                                    (Left msg)      -> return $ Left (path ++ ":" ++ msg)
                                    value@(Right _) -> return value
                            _ -> return $ Left (path ++ ":--:--: error: invalid main signature")       
    where
        stdin'  = PredefinedFileValue "stdin" 0
        stdout' = PredefinedFileValue "stdout" 0
        initEnv = Map.fromList [(VarId "stdin" Untyped, stdin'), (VarId "stdout" Untyped, stdout')] 

evalTypeDcl :: TypeDclAST -> Sig -> IO (Either String Sig)
evalTypeDcl dt sigma =
    case dt of
        EpsTypeDclAST                       -> return $ Right sigma
        TypeDclAST typeId cons dt' utilData ->
            case sigma `conflicts` sigma2 of
                (Just name) -> return $ Left (formatErr ("cannot redefine '" ++ name ++ "'") utilData)
                Nothing     -> evalTypeDcl dt' (sigma `unionSig` sigma2)
            where
                sigma2 = (Set.fromList [evaluateTermCons ts typeId | ts <- cons]) -- TODO: check internal conflicts! (a set cannot contain multiple instances...)

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
                x = case xt of
                    UntypedVarAST varId _ -> varId
                    TypedVarAST varId _ _ -> varId

evalExpr :: ExprAST -> Env -> Sig -> IO (Either String Values)
evalExpr expr env sigma = do
    case expr of
        (VarExprAST varId utilData)            -> evalVarExpr varId env sigma utilData
        (TypeExprAST typeId _)                 -> return $ Right (TerValue typeId)
        (ConstExprAST c _)                     -> return $ Right (ConstValue c)
        (ParenExprAST expr' _)                 -> evalExpr expr' env sigma
        (LambdaExprAST varId expr' _)          -> return $ Right (ClosureValue varId expr' env sigma)
        (FunAppExprAST expr1 expr2 _)          -> evalFunApp expr1 expr2 env sigma
        (TupleExprAST exprs _)                 -> evalTuple exprs env sigma
        (ListExprAST exprs _)                  -> evalList exprs env sigma
        (CaseExprAST branches utilData)        -> evalCase branches env sigma utilData
        (LetInExprAST xt expr1 expr2 _)        -> evalLetIn xt expr1 expr2 env sigma
        (MatchExprAST expr' branches utilData) -> do
            maybeValue <- evalExpr expr' env sigma
            case maybeValue of
                err@(Left _) -> return err 
                (Right value) -> evalMatch value branches env sigma utilData

evalVarExpr :: VarId -> Env -> Sig -> UtilData -> IO (Either String Values)
evalVarExpr varId env _ utilData =
    case maybeValue of
        Nothing      -> return $ Left (formatErr ("variable '" ++ varName varId ++ "' is out of scope") utilData)
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
                        _                                   -> error "invalid function type." -- should be prevented by typesystem

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
partiallyApply _ _                                  = error "cannot partially apply a non-function constant." -- should be prevented by typesystem

evalLetIn :: TypeVarAST -> ExprAST -> ExprAST -> Env -> Sig -> IO (Either String Values)
evalLetIn xt expr1 expr2 env sigma = do
    maybeValue <- evalExpr expr1 env sigma
    case maybeValue of
        err@(Left _)  -> return err
        (Right value) ->
            case value of
                (ClosureValue x' expr' env' sigma') -> evalExpr expr2 (env `except` (x, RecClosureValue x x' expr' env' sigma')) sigma
                _                                   -> evalExpr expr2 (env `except` (x, value)) sigma
            where
                x = case xt of
                    UntypedVarAST varId _ -> varId
                    TypedVarAST varId _ _ -> varId

evalCase :: [(PredAST, ExprAST)] -> Env -> Sig -> UtilData -> IO (Either String Values)
evalCase [] _ _ utilData = return $ Left (formatErr "non-exhaustive case branches" utilData)
evalCase ((pred', expr'):branches') env sigma utilData = do
    maybeRes <- handlePred pred' env sigma
    case maybeRes of
        (Left msg)  -> return $ Left msg
        (Right res) ->
            if res
                then evalExpr expr' env sigma
                else evalCase branches' env sigma utilData

handlePred :: PredAST -> Env -> Sig -> IO (Either String Bool)
handlePred (PredWildAST _) _ _            = return $ Right True
handlePred (PredExprAST expr _) env sigma = do 
    maybeValue <- evalExpr expr env sigma
    case maybeValue of
        (Left msg) -> return $ Left msg
        (Right value) ->
            case value of
                (ConstValue (BoolConstAST b _)) -> return $ Right b
                _                               -> error "case condition must be a predicate or wildcard." -- should be prevented by typesystem


evalMatch :: Values -> [(PatternAST, ExprAST)] -> Env -> Sig -> UtilData -> IO (Either String Values)
evalMatch _ [] _ _ utilData                                  = return $ Left (formatErr "non-exhaustive match branches" utilData)
evalMatch value ((pat', expr'):branches') env sigma utilData =
    case match value pat' sigma of
        (Left msg)  -> return $ Left msg
        (Right res) ->  
            case res of
                MatchFail      -> evalMatch value branches' env sigma utilData
                Bindings delta -> 
                    case findConflicts delta of
                        (Just msg) -> return $ Left (formatErr ("variable '" ++ msg ++ "' cannot be bound more than once in the same pattern") utilData)
                        Nothing    -> evalExpr expr' (applyBindings env delta) sigma

findConflicts :: [Binding] -> Maybe String
findConflicts []          = Nothing
findConflicts ((x, _):bs) = 
    if bs `hasVar` x
        then Just (varName x)
        else findConflicts bs

hasVar :: [Binding] -> VarId -> Bool
hasVar [] _          = False
hasVar ((y, _):bs) x = 
    if x == y
        then True
        else hasVar bs x 

applyBindings :: Env -> [Binding] -> Env
applyBindings env []     = env
applyBindings env (b:bs) = applyBindings (env `except` b) bs

match :: Values -> PatternAST -> Sig -> Either String Bindings
match value (VarPatternAST varId _) _ = Right (Bindings [(varId, value)])

match _ (WildPatternAST _) _ = Right (Bindings [])

match (ConstValue c1) (ConstPatternAST c2 _) _ = 
    if c1 == c2 
        then Right (Bindings []) 
        else Right MatchFail

match (TerValue t1) (TypePatternAST t2 utilData) sigma =
    if (sigma `has` t1) && (sigma `has` t2)
        then if t1 == t2 
                then Right (Bindings [])
                else Right MatchFail
        else Left (formatErr ("unknown term constructor '" ++ typeName t1 ++ "'") utilData)

match (ListValue (v:vs)) (DecompPatternAST pat' varId _) sigma =
    case maybeDelta of
        err@(Left _)  -> err
        (Right delta) ->
            case delta of
               MatchFail      -> Right MatchFail
               Bindings binds -> Right (Bindings ((varId, v'):binds))
    where
        v' = ListValue vs
        maybeDelta = match v pat' sigma

match (TupleValue vs) (TuplePatternAST ps _) sigma = matchMultiple vs ps sigma

match (ListValue []) (ListPatternAST [] _) _ = Right (Bindings [])
match (ListValue vs) (ListPatternAST ps _) sigma = matchMultiple vs ps sigma                

match (TerConsValue t1 value) (TypeConsPatternAST t2 pat' utilData) sigma =
    if (sigma `has` t1) && (sigma `has` t2)
        then if t1 == t2 
                then match value pat' sigma
                else Right MatchFail
        else Left (formatErr ("unknown term constructor '" ++ typeName t1 ++ "'") utilData)

match _ _ _ = Right MatchFail


matchMultiple :: [Values] -> [PatternAST] -> Sig -> Either String Bindings
matchMultiple [] [] _             = Right (Bindings [])
matchMultiple (v:vs) (p:ps) sigma =
    if (length vs) /= (length ps)
        then Right MatchFail
        else case maybeBind of
                err@(Left _) -> err
                (Right bind) ->
                    case maybeBinds of
                        err@(Left _)  -> err
                        (Right binds) -> 
                            case (bind, binds) of
                                (MatchFail, _)             -> Right MatchFail
                                (_, MatchFail)             -> Right MatchFail
                                (Bindings l1, Bindings l2) -> Right (Bindings (l1 ++ l2))
            where
                maybeBind  = match v p sigma
                maybeBinds = matchMultiple vs ps sigma

matchMultiple _ _ _ = Right MatchFail

valueListToString :: [Values] -> String
valueListToString [] = ""
valueListToString ((ConstValue (CharConstAST c _)):cs) = (c:(valueListToString cs))
valueListToString _ = error "list must be of chars."

stringToValueList :: String -> [Values]
stringToValueList []     = []
stringToValueList (c:cs) = ((ConstValue (CharConstAST c initUtilData)):(stringToValueList cs))

advanceSystem :: Values -> Values
advanceSystem (SystemValue sys) = SystemValue (sys + 1)
advanceSystem _                 = error "cannot advance a non-system value."

advanceFile :: Values -> Values
advanceFile (FileValue h id')          = FileValue h (id' + 1)
advanceFile (PredefinedFileValue s f) = PredefinedFileValue s (f + 1)
advanceFile _                         = error "cannot advance a non-file value."

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
apply (EqualsConstAST utilData) [v1, v2]                                                                 = return $ ConstValue (BoolConstAST (v1 == v2) utilData)
apply (NotConstAST utilData) [ConstValue (BoolConstAST v1 _)]                                            = return $ ConstValue (BoolConstAST (not v1) utilData)
apply (GreaterConstAST utilData) [ConstValue v1, ConstValue v2]                                          = return $ ConstValue (BoolConstAST (v1 > v2) utilData)
apply (LessConstAST utilData) [ConstValue v1, ConstValue v2]                                             = return $ ConstValue (BoolConstAST (v1 < v2) utilData)
apply (GreaterOrEqualConstAST utilData) [ConstValue v1, ConstValue v2]                                   = return $ ConstValue (BoolConstAST (v1 >= v2) utilData)
apply (LessOrEqualConstAST utilData) [ConstValue v1, ConstValue v2]                                      = return $ ConstValue (BoolConstAST (v1 <= v2) utilData)
apply (AndConstAST utilData) [ConstValue (BoolConstAST v1 _), ConstValue (BoolConstAST v2 _)]            = return $ ConstValue (BoolConstAST (v1 && v2) utilData)
apply (OrConstAST utilData) [ConstValue (BoolConstAST v1 _), ConstValue (BoolConstAST v2 _)]             = return $ ConstValue (BoolConstAST (v1 || v2) utilData)

-- list operations
apply (AppenConstAST _) [v1, ListValue v2]                                                               = return $ ListValue (v1:v2)
apply (ConcatenateConstAST _) [ListValue v1, ListValue v2]                                               = return $ ListValue (v1 ++ v2)

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
        _             -> error "must be a list."
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

apply _ _ = error "invalid arguments for apply." -- should be prevented by typesystem

getValueList :: Values -> [Values]
getValueList (ListValue l) = l
getValueList _ = error "value is not a list."

showList' :: [Values] -> IO Values
showList' []     = return $ ListValue []
showList' [v]    = apply (ShowConstAST initUtilData) [v]
showList' (v:vs) = do
    v' <- apply (ShowConstAST initUtilData) [v]
    vs' <- showList' vs
    case (v', vs') of
        (ListValue l1, ListValue l2) -> return $ ListValue (l1 ++ spacer ++ l2)
        _ -> error "must be a list."
        where
            spacer = [ConstValue (CharConstAST ',' initUtilData), ConstValue (CharConstAST ' ' initUtilData)]