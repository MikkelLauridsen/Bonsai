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

-- types are stored as formated strings
type Sort = String

-- signatures for termconstructors
data Signature = ConstSig Sort -- T
               | FuncSig Sort Sort -- Composite -> T
               deriving (Eq, Ord)

-- storage type for variabel environments
type Binding = (VarId, Values)

-- match results, either failure or a list of bindings
data Bindings = MatchFail
              | Bindings [Binding]

-- storage type for sets of termconstructor names and associated signatures
type TermConstructor = (TypeId, Signature)

-- variabel environment type
type Env = Map VarId Values

-- type for sets of termconstructor names and associated signatures
type Sig = Set TermConstructor

-- Bonsai value type
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

-- an instance of Eq for Values must be declared for the == operator
-- Not all types can be compared, specifically functions
instance Eq Values where
    ConstValue c1 == ConstValue c2 = c1 == c2
    TerValue t1 == TerValue t2 = t1 == t2
    TerConsValue t1 v1 == TerConsValue t2 v2 = t1 == t2 && v1 == v2
    SystemValue i1 == SystemValue i2 = i1 == i2
    FileValue m1 i1 == FileValue m2 i2 = m1 == m2 && i1 == i2
    PredefinedFileValue s1 i1 == PredefinedFileValue s2 i2 = s1 == s2 && i1 == i2
    TupleValue t1 == TupleValue t2 = t1 == t2
    ListValue l1 == ListValue l2 = l1 == l2
    _ == _ = error "unsupported types for equality checking." -- it is impossible to check whether two functions are equivalent, as their environments may vary

instance Ord Values where
    ConstValue c1 `compare` ConstValue c2 = c1 `compare` c2
    ListValue l1 `compare` ListValue l2 = l1 `compareLists` l2
    _ `compare` _ = error "unsupported types for comparison."

compareLists :: [Values] -> [Values] -> Ordering
compareLists [] [] = EQ
compareLists [] _  = LT
compareLists _ []  = GT
compareLists ((ConstValue c1):l1') ((ConstValue c2):l2') =
    case c1 `compare` c2 of
        EQ -> compareLists l1' l2'
        LT -> LT
        GT -> GT
compareLists _ _ = error "unsupported list value type for comparison."

-- implementation of the sorts function
-- recursively constructs a formated string (Sort)
sorts :: CompTypeAST -> Sort
sorts (CompSimpleAST typeId _) = typeName typeId
sorts (CompListAST comp' _) = "[" ++ sorts comp' ++ "]"
sorts (CompTupleAST comps' _) = "(" ++ ([sorts comp' | comp' <- comps'] >>= (++ ", ")) ++ ")"
sorts (CompFuncAST comp1' comp2' _) = sorts comp1' ++ "->" ++ sorts comp2'

-- Convenience functions for variabel environments
-- and sets of termconstructor names and associated signatures
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

-- checks whether two sets from Sig share Type names 
-- or termconstructor names
-- returns the type or constructor name if so
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

-- checks whether a set from Sig has multiple instances
-- of the same termconstructor
-- returns the name if so
hasConflicts :: [TermConstructor] -> Maybe String
hasConflicts []           = Nothing
hasConflicts ((t, _):tcs) =
    if hasRec tcs t
        then Just (typeName t)
        else hasConflicts tcs

-- constructs a TermConstructor value from an AST and a type id
evaluateTermCons :: ConsAST -> TypeId -> TermConstructor
evaluateTermCons (SingleConsAST t _) typeId          = (t, ConstSig (typeName typeId))
evaluateTermCons (DoubleConsAST t compType _) typeId = (t, FuncSig (sorts compType) (typeName typeId))

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

-- start of transition rules
-- interpret corresponds to (prog)
-- requires an additional file path for error messages
-- all transition rule functions either return an error message 
-- or the specified type from the rule
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
                    case maybeMain of
                        Nothing -> return $ Left (path ++ ":--:--: error: main is not defined")
                        (Just (RecClosureValue _ x e env2 sigma2)) -> do
                            let env' = env2 `except` (x, SystemValue 0)
                            res <- evalExpr e env' sigma2
                            case res of
                                (Left msg)      -> return $ Left (path ++ ":" ++ msg)
                                value@(Right _) -> return value
                        _ -> return $ Left (path ++ ":--:--: error: invalid main signature")       
    where
        -- set up the initial variabel environment containing I/O variables
        stdin'  = PredefinedFileValue "stdin" 0
        stdout' = PredefinedFileValue "stdout" 0
        initEnv = Map.fromList [(VarId "stdin" Untyped, stdin'), (VarId "stdout" Untyped, stdout')] 

-- implementation of (typeErk-1) and (typeErk-2)
-- returns an error message if:
--  1. the AST declares the same type more than once
--  2. the same termconstructor name is declared in separate type declaractions
--  3. the same type declaration has multiple declarations of a termconstructor name
-- otherwise, a recursively defined set from Sig is returned
evalTypeDcl :: TypeDclAST -> Sig -> IO (Either String Sig)
evalTypeDcl dt sigma =
    case dt of
        EpsTypeDclAST                       -> return $ Right sigma
        TypeDclAST typeId cons dt' utilData ->
            case sigma `conflicts` sigma2 of
                (Just name) -> return $ Left (formatErr ("cannot redefine '" ++ name ++ "'") utilData)
                Nothing     -> 
                    case hasConflicts list2 of
                        (Just name) -> return $ Left (formatErr ("multiple instances of termconstructor '" ++ name ++ "'") utilData)
                        Nothing     -> evalTypeDcl dt' (sigma `unionSig` sigma2)
            where
                -- set up the set from Sig in which the termconstructors of 'typeId' are declared 
                list2  = [evaluateTermCons ts typeId | ts <- cons]
                sigma2 = Set.fromList list2

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

-- collection of expression transition rules
-- implementation of rules (const), (lambda), (ter)
-- returns an error message if:
--  1. any of the transition rule implementations returns an error message
-- otherwise, returns a Bonsai value
evalExpr :: ExprAST -> Env -> Sig -> IO (Either String Values)
evalExpr expr env sigma = do
    case expr of
        (VarExprAST varId utilData)            -> evalVarExpr varId env sigma utilData
        (TypeExprAST typeId utilData)          -> evalTer typeId sigma utilData
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

-- implementation of transition rule (var)
-- returns an error message if:
--  1. the specified variable is not defined in the known variable environment
-- otherwise, returns the bound value
evalVarExpr :: VarId -> Env -> Sig -> UtilData -> IO (Either String Values)
evalVarExpr varId env _ utilData =
    case maybeValue of
        Nothing      -> return $ Left (formatErr ("variable '" ++ varName varId ++ "' is out of scope") utilData)
        (Just value) -> return $ Right value
    where
        maybeValue = env `getVar` varId

-- implementation of transition rule (ter)
-- return an error message if:
--  1. the termconstructor is not known in Sigma
-- otherwise, returns the termconstructor as a Bonsai value
evalTer :: TypeId -> Sig -> UtilData -> IO (Either String Values)
evalTer t sigma utilData = 
    if sigma `has` t
        then return $ Right (TerValue t)
        else return $ Left (formatErr ("unknown term-constructor '" ++ typeName t ++ "'") utilData)

-- implementation of transition rule (tupe-1) and (type-2)
-- returns an error message if:
--  1. any of the immediate constituents result in an error message
-- otherwise, returns a Bonsai tuple value
evalTuple :: [ExprAST] -> Env -> Sig -> IO (Either String Values)
evalTuple exprs env sigma = do
    maybeBody <- evalExprs exprs env sigma
    case maybeBody of
        (Left msg)   -> return $ Left msg
        (Right body) -> return $ Right (TupleValue body)

-- implementation of transition rule (list-1) and (list-2)
-- returns an error message if:
--  1. any of the immediate constituents result in an error message
-- otherwise, returns a Bonsai list value
evalList :: [ExprAST] -> Env -> Sig -> IO (Either String Values)
evalList exprs env sigma = do
    maybeBody <- evalExprs exprs env sigma
    case maybeBody of
        (Left msg)   -> return $ Left msg
        (Right body) -> return $ Right (ListValue body)

-- helper function for evalTuple and evalList
-- evaluates each expression AST in input list
-- returns an error message if:
--  1. any of the elements result in an error message
-- otherwise, returns a list of Bonsai values
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

-- implementation of transition rules (andv-1), (andv-2), (andv-3), (andv-4) and (andv-5)
-- returns an error message if:
--  1. expr1 results in an error message when evaluated
--  2. expr2 results in an error message when evaluated
-- otherwise, returns a Bonsai value corresponding to the applied function
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

-- helper function for evalFunApp
-- returns the result of apply(c,v)
-- if c's definition of apply only requires one value
-- otherwise, returns a partial value
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

-- implementation of transition rule (let-1) and (let-2)
-- returns an error message if:
--  1. either expr1 or expr2 results in an error message when evaluated
-- otherwise, returns a Bonsai value
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
                -- extract x from AST
                x = case xt of
                    UntypedVarAST varId _ -> varId
                    TypedVarAST varId _ _ -> varId

-- implementation of transition rule (case)
-- chooses the first acceptable branch
-- returns an error message if:
--  1. the chosen branch results in an error message
--  2. no branch is acceptable - that is: non-exhaustive branches
--  3. any evaluated 'predicate' results in an error message 
-- otherwise, returns the value the chosen branch evaluates to
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

-- helper function for evalCase
-- returns an error message if:
--  1. the specified 'predicate' results in an error message when evaluated
-- otherwise, returns a boolean value indicating whether the branch is accepted
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

-- implementation of transition rule (match)
-- chooses the first acceptable branch
-- returns an error message if:
--  1. an unknown termconstructor is used in any pattern
--  2. the expression to be matched upon results in an error message
--  3. the chosen branch results in an error message
--  4. no branch is acceptable - that is: non-exhaustive branches
--  5. the same variable name is bound more than once in a pattern 
-- otherwise, returns the value the chosen branch evaluates to
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

-- helper function for evalMatch
-- returns the name of a bounded variable,
-- if it is bound more than once in input list
findConflicts :: [Binding] -> Maybe String
findConflicts []          = Nothing
findConflicts ((x, _):bs) = 
    if bs `hasVar` x
        then Just (varName x)
        else findConflicts bs

-- helper function for findConflicts
-- returns a boolean value indicating
-- whether the input varId is bound in input list
hasVar :: [Binding] -> VarId -> Bool
hasVar [] _          = False
hasVar ((y, _):bs) x = 
    if x == y
        then True
        else hasVar bs x 

-- helper function for evalMatch
-- returns an updated variable environment
-- with bounds in input variable environment
-- updated with input list of bindings
applyBindings :: Env -> [Binding] -> Env
applyBindings env []     = env
applyBindings env (b:bs) = applyBindings (env `except` b) bs

-- implementation of the match function
-- returns an error message if:
--  1. an undefined termconstructor is used in a pattern
-- otherwise, either returns a match-fail or a list of bindings
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
        else Left (formatErr ("unknown term-constructor '" ++ typeName t1 ++ "'") utilData)

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

-- helper function for match
-- it is used to evaluate immediate constituents of tuple/list value pattern pairs
-- returns an error message if:
--  1. any of the recursive matches result in an error message
-- otherwise, returns either a match-fail or a list of bindings
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
                maybeBind  = match v p sigma           -- evaluate immediate pair
                maybeBinds = matchMultiple vs ps sigma -- evaluate remaining pairs

matchMultiple _ _ _ = Right MatchFail


-- implementation of apply function
-- returns a Bonsai value based on input predefined function 
-- and value parameters (represented by value list)
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
apply (GreaterConstAST utilData) [v1, v2]                                                                = return $ ConstValue (BoolConstAST (v1 > v2) utilData)
apply (LessConstAST utilData) [v1, v2]                                                                   = return $ ConstValue (BoolConstAST (v1 < v2) utilData)
apply (GreaterOrEqualConstAST utilData) [v1, v2]                                                         = return $ ConstValue (BoolConstAST (v1 >= v2) utilData)
apply (LessOrEqualConstAST utilData) [v1, v2]                                                            = return $ ConstValue (BoolConstAST (v1 <= v2) utilData)
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

-- helper functions for apply

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