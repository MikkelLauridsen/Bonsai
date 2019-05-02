module Typesystem
    (
        
    ) where

import Ast
import Data.List
import Actions
import Prettifier
import Data.Map.Strict as Map
import Data.Set as Set

-- type-environment type
type Env = Map VarId Types

-- type for sets of algebraic types
type Sig = Set AlgebraicType

type Binding = (VarId, Types)

extractConsName :: TermConstructor -> TypeId
extractConsName (ConstConstructor typeId)  = typeId
extractConsName (CompConstructor typeId _) = typeId

getTermType :: Sig -> TypeId -> String
getTermType sigma t =
    case maybeTyp of
        Nothing         -> error "unknown termconstructor"
        (Just (typ, _)) -> typeName typ
    where
        maybeTyp = find fun (Set.toList sigma)
        fun = \(_, tcs) ->
            case find (\tc -> extractConsName tc == t) tcs of
                Nothing  -> False
                (Just _) -> True

getSignature :: Sig -> TypeId -> Maybe Types
getSignature sigma t =
    case getCons (Set.toList sigma) t of
        (ConstConstructor _)    -> Nothing
        (CompConstructor _ sig) -> Just sig

getCons :: [AlgebraicType] -> TypeId -> TermConstructor
getCons [] t       = error "unknown termconstructor"
getCons ((_, tcs):typs) t =
    case find (\tc -> extractConsName tc == t) tcs of
        Nothing   -> getCons typs t
        (Just tc) -> tc

getAlgebraicType :: Sig -> TypeId -> Maybe Types
getAlgebraicType sigma typeId = find (\(typ, _) -> typ == typeId) (Set.toList sigma)

-- checks whether the input termconstructor (by name)
-- is defined in input set of Algebraic types
has :: Sig -> TypeId -> Bool
has sigma t = hasRec (Set.toList sigma) t

hasRec :: [AlgebraicType] -> TypeId -> Bool
hasRec [] _ = False
hasRec ((_, tcs):typs) t =
    case find (\tc -> extractConsName tc == t) tcs of
        Nothing   -> hasRec typs t
        (Just _)  -> True

-- Convenience functions for variable environments
-- and sets of termconstructor names and associated signatures
except :: Env -> Binding -> Env
except env (varId, typ) = Map.insertWith const varId typ env

getVar :: Env -> VarId -> Maybe Types
getVar env var = Map.lookup var env

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

types :: CompTypeAST -> Sig-> Either String Types
types (CompSimpleAST typeId _) sigma = idToTypes typeId sigma
types (CompSimplePolyAST varId _) _  = Right $ PolyType (varName varId)

types (CompPolyAST typeId comps' utilData) sigma = 
    case idToTypes typeId sigma of
        (Left id) -> Left (formatErr ("unknown type '" ++ typeName id ++ "'") utilData)
        (Right typ@(AlgePoly name polys cons)) ->
            case typesList comps' sigma of
                (Left msg)   -> Left msg
                (Right typs) ->
                    if length polys == length typs
                        then AlgePoly name typs cons
                        else Left (formatErr ("algebraic type '" ++ show typ ++ "' cannot be applied to " ++ length typs ++ "types") utilData)
        (Right typ) -> Left (formatErr ("type '" ++ show typ ++ "' cannot be used polymorphically") utilData)

types (CompListAST comp' _) sigma =
    case types comp' sigma of
        err@(Left _) -> err
        (Right typ)  -> Right $ ListType typ

types (CompTupleAST comps' utilData) sigma =
    case typesList comps' sigma utilData of
        (Left msg)   -> Left msg
        (Right typs) -> Right $ TuplType typs

types (CompFuncAST comp1' comp2' _) sigma = 
    case types comp1' sigma of
        err@(Left _) -> err
        (Right typ1) ->
            case types comp2' sigma of
                err@(Left _) -> err
                (Right typ2) -> Right (FuncType typ1 typ2)

idToTypes :: TypeId -> Sig -> Either TypeId Types
idToTypes id sigma =
    if last (typeName id) == '*'
        then case stringToNonUniquePrim (init (typeName id)) of
            (Just typ) -> Right (UniqType typ True)
            Nothing    ->
                case getAlgebraicType sigma id of
                    (Just typ) -> Right (UniqType typ True)
                    Nothing    -> Left id
        else case stringToNonUniquePrim (typeName id) of
            (Just typ) -> Right typ
            Nothing    ->
                case getAlgebraicType sigma id of
                    (Just typ) -> Right typ
                    Nothing    -> Left id

stringToNonUniquePrim :: String -> Maybe Types
stringToNonUniquePrim "Int"   = Just $ PrimType IntPrim
stringToNonUniquePrim "Float" = Just $ PrimType FloatPrim
stringToNonUniquePrim "Bool"  = Just $ PrimType BoolPrim
stringToNonUniquePrim "Char"  = Just $ PrimType CharPrim
stringToNonUniquePrim _       = Nothing

typesList :: [CompTypeAST] -> Sig -> UtilData -> Either String [Types]
typesList [] _ _                       = Right []
typesList (comp:comps') sigma utilData =
    case types comp sigma of
        (Left msg)   -> Left msg
        (Right typ) ->
            case typesList comps' sigma of
                err@(Left _) -> err
                (Right typs) -> Right (typ:typs)

-- collection of expression transition rules
-- implementation of rules (const), (lambda), (ter)
-- returns an error message if:
--  1. any of the transition rule implementations returns an error message
-- otherwise, returns a Bonsai value and the updated global variable environment
evalExpr :: ExprAST -> Env -> Sig -> Either String (Types, [Binding])
evalExpr expr env sigma = do
    case expr of
        (VarExprAST varId utilData)            -> evalVarExpr varId env sigma utilData
        (TypeExprAST typeId utilData)          -> evalTer typeId env sigma utilData
        (ConstExprAST c _)                     -> Right (evalConst c, [])
        (ParenExprAST expr' _)                 -> evalExpr expr' env sigma
        (LambdaExprAST xt expr' _)             -> evalLambda xt expr' env sigma
        (FunAppExprAST expr1 expr2 _)          -> evalFunApp expr1 expr2 env sigma
        (TupleExprAST exprs utilData)          -> evalTuple exprs env sigma utilData
        (ListExprAST exprs utilData)           -> evalList exprs env sigma utilData
        (CaseExprAST branches utilData)        -> evalCase branches env sigma utilData
        (LetInExprAST xt expr1 expr2 _)        -> evalLetIn xt expr1 expr2 env sigma
        (MatchExprAST expr' branches utilData) ->
            case evalExpr expr' env sigma of
                err@(Left _) -> err 
                (Right (typ, binds)) -> evalMatch typ branches (applyBindings env binds) sigma utilData

evalVarExpr :: VarId -> Env -> Sig -> UtilData -> Either String (Types, [Binding])
evalVarExpr varId env sigma utilData =
    case getVar env varId of
        Nothing                       -> Left (formatErr ("variable '" ++ varName varId ++ "' is out of scope") utilData)
        (Just typ@(UniqType _ False)) -> Left (formatErr ("unique variable '" ++ varName varId ++ "::" ++ show typ ++ "' cannot be used twice") utilData)
        (Just (UniqType typ True))    -> Right (typ, [(varId, (UniqType typ False))])
        (Just (LazyType expr))        ->
            case evalExpr expr env sigma of
                err@(Left _)     -> err
                (Right (typ, _)) -> Right (typ, [(varId, typ)])
        (Just (LazyPair expr typ))    ->
            case evalExpr expr env sigma of
                err@(Left _)      -> err
                (Right (typ', _)) ->
                    if typ' == typ
                        then Right (typ, [(varId, typ)])
                        else Left (formatErr ("variable '" ++ varName varId ++ "' was annotated as '" ++ show typ ++ "' but has type '" ++ show typ' ++ "'") utilData)
        (Just typ)                    -> Right (typ, []) 

evalTer :: TypeId -> Env -> Sig -> UtilData -> Either String (Types, [Binding])
evalTer t env sigma utilData =
    case getSignature sigma t of
        Nothing    -> Left (formatErr ("unknown term-constructor '" ++ typeName t ++ "'") utilData)
        (Just typ) -> Right (typ, [])

evalConst :: ConstAST -> Types
evalConst (IntConstAST _ _)          = PrimType IntPrim
evalConst (BoolConstAST _ _)         = PrimType BoolPrim
evalConst (FloatConstAST _ _)        = PrimType FloatPrim
evalConst (CharConstAST _ _)         = PrimType CharPrim
evalConst (UnaryMinusConstAST _)     = FuncType (PolyType "a0") (PolyType "a0")
evalConst (PlusConstAST _)           = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PolyType "a0"))
evalConst (MinusConstAST _)          = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PolyType "a0"))
evalConst (TimesConstAST _)          = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PolyType "a0"))
evalConst (DivideConstAST _)         = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PolyType "a0"))
evalConst (ModuloConstAST _)         = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PolyType "a0"))
evalConst (EqualsConstAST _)         = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PrimType BoolPrim))
evalConst (NotConstAST _)            = FuncType (PrimType BoolPrim) (PrimType BoolPrim)
evalConst (GreaterConstAST _)        = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PrimType BoolPrim))
evalConst (LessConstAST _)           = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PrimType BoolPrim))
evalConst (GreaterOrEqualConstAST _) = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PrimType BoolPrim))
evalConst (LessOrEqualConstAST _)    = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PrimType BoolPrim))
evalConst (AppenConstAST _)          = FuncType (PolyType "a0") (FuncType (ListType (PolyType "a0")) (ListType (PolyType "a0")))
evalConst (ConcatenateConstAST _)    = FuncType (ListType (PolyType "a0")) (FuncType (ListType (PolyType "a0")) (ListType (PolyType "a0")))
evalConst (AndConstAST _)            = FuncType (PrimType BoolPrim) (FuncType (PrimType BoolPrim) (PrimType BoolPrim))
evalConst (OrConstAST _)             = FuncType (PrimType BoolPrim) (FuncType (PrimType BoolPrim) (PrimType BoolPrim))
evalConst (BiLShiftConstAST _)       = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PolyType "a0"))
evalConst (BiRShiftConstAST _)       = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PolyType "a0"))
evalConst (BiNotConstAST _)          = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PolyType "a0"))
evalConst (BiAndConstAST _)          = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PolyType "a0"))
evalConst (BiXorConstAST _)          = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PolyType "a0"))
evalConst (BiOrConstAST _)           = FuncType (PolyType "a0") (FuncType (PolyType "a0") (PolyType "a0"))
evalConst (OpenReadConstAST _)       = FuncType (UniqType (PrimType SystemPrim) True) (FuncType (ListType (PrimType CharPrim)) (TuplType [PrimType BoolPrim, UniqType (PrimType SystemPrim) True, UniqType (PrimType FilePrim) True]))
evalConst (OpenWriteConstAST _)      = FuncType (UniqType (PrimType SystemPrim) True) (FuncType (ListType (PrimType CharPrim)) (TuplType [PrimType BoolPrim, UniqType (PrimType SystemPrim) True, UniqType (PrimType FilePrim) True]))
evalConst (CloseConstAST _)          = FuncType (UniqType (PrimType SystemPrim) True) (FuncType (UniqType (PrimType FilePrim) True) (TuplType [PrimType BoolPrim, UniqType (PrimType SystemPrim) True]))
evalConst (ReadConstAST _)           = FuncType (UniqType (PrimType FilePrim) True) (TuplType [PrimType BoolPrim, PrimType CharPrim, UniqType (PrimType FilePrim) True])
evalConst (WriteConstAST _)          = FuncType (UniqType (PrimType FilePrim) True) (FuncType (PrimType CharPrim) (TuplType [PrimType BoolPrim, UniqType (PrimType FilePrim) True]))
evalConst (DeleteConstAST _)         = FuncType (UniqType (PrimType SystemPrim) True) (FuncType (UniqType (PrimType FilePrim) True) (TuplType [PrimType BoolPrim, UniqType (PrimType SystemPrim) True]))
evalConst (ShowConstAST _)           = FuncType (PolyType "a0") (ListType (PrimType CharPrim))
evalConst (ToIntConstAST _)          = FuncType (ListType (PrimType CharPrim)) (TuplType [PrimType BoolPrim, PrimType IntPrim])
evalConst (ToFloatConstAST _)        = FuncType (ListType (PrimType CharPrim)) (TuplType [PrimType BoolPrim, PrimType FloatPrim])

evalLambda :: TypeVarAST -> ExprAST -> Env -> Sig -> Either String (Types, [Binding])
evalLambda (TypedVarAST x s _) expr env sigma = 
    case types s sigma of
        (Left msg) -> msg
        (Right typ) -> evalExpr expr env' sigma
            where
                env' = env `except` (x, typ)

evalTuple :: [ExprAST] -> Env -> Sig -> Either String (Types, [Binding])
evalTuple exprs env sigma =
    case handleTupleExprs exprs [] env sigma of
        (Left msg)            -> Left msg
        (Right (body, binds)) -> Right (TuplType body, binds) 

handleTupleExprs :: [ExprAST] -> [Binding] -> Env -> Sig -> Either String ([Types], [Binding])
handleTupleExprs [expr1, expr2] binds env sigma =
    case evalExpr expr1 env' sigma of
        (Left msg)             -> Left msg
        (Right (typ1, binds1')) ->
            case evalExpr expr2 (applyBindings env' binds1') sigma of
                (Left msg) -> Left msg
                (Right (typ2, binds2')) -> Right ([typ1, typ2], binds ++ binds1' ++ binds2')
    where
        env' = applyBindings env binds

handleTupleExprs (expr:exprs') binds env sigma =
    case evalExpr expr env' sigma of
        (Left msg)            -> Left msg
        (Right (typ, binds')) ->
            case handleTupleExprs exprs' (binds ++ binds') env sigma of
                err@(Left _)            -> err
                (Right (typs, binds'')) -> Right ((typ:typs), binds ++ binds' ++ binds'')
    where
        env' = applyBindings env binds

evalList :: [ExprAST] -> Env -> Sig -> UtilData -> Either String (Types, [Binding])
evalList (expr:exprs') env sigma utilData =
    case evalExpr expr env sigma of
        err@(Left _)         -> err
        (Right (typ, binds)) ->
            case handleListExprs typ exprs' [] env' sigma utilData of
                (Left msg)     -> Left msg
                (Right binds') -> Right (ListType typ, binds ++ binds') 
            where
                env' = applyBindings env binds

handleListExprs :: Types -> [ExprAST] -> [Binding] -> Env -> Sig -> UtilData -> Either String [Binding]
handleListExprs _ [] binds _ _ _ = Right binds
handleListExprs typ (expr:exprs') binds env sigma utilData =
    case evalExpr expr env' sigma of
        (Left msg)             -> Left msg
        (Right (typ', binds')) ->
            if typ' == typ
                then handleListExprs typ exprs' (binds ++ binds') env sigma utilData
                else Left (formatErr ("list elements type mismatch: actual '" ++ show typ' ++ "' expected '" ++ show typ ++ "'") utilData)
    where
        env' = applyBindings env binds

evalCase :: [(PredAST, ExprAST)] -> Env -> Sig -> UtilData -> Either String (Types, [Binding])
evalCase [] _ _ utilData = Left (formatErr ("cannot inference type of a case expression of zero branches") utilData)
evalCase ((pred1, expr1):branches') env sigma utilData =
    case handlePred pred1 env sigma of
        (Left msg)    -> Left msg
        (Right binds) ->
            case evalExpr expr1 (applyBindings env binds) sigma of
                err@(Left _)     -> err
                (Right (typ, _)) -> handleCaseBranches typ branches' env sigma utilData

handleCaseBranches :: Types -> [(PredAST, ExprAST)] -> Env -> Sig -> UtilData -> Either String (Types, [Binding])
handleCaseBranches typ [] _ _ _ = Right (typ, [])
handleCaseBranches typ ((pred, expr):branches') env sigma utilData =
    case handlePred pred env sigma of
        (Left msg)    -> Left msg
        (Right binds) ->
            case evalExpr expr (applyBindings env binds) sigma of
                err@(Left _)      -> err
                (Right (typ', _)) -> 
                    if typ' == typ
                        then handleCaseBranches typ branches' env sigma utilData
                        else Left (formatErr ("case branch type mismatch '" ++ show typ' ++ "' expected '" ++ show typ ++ "'") utilData)           

handlePred :: PredAST -> Env -> Sig -> Either String [Binding]
handlePred (PredWildAST _) _ _            = Right []
handlePred (PredExprAST expr utilData) env sigma =
    case evalExpr expr env sigma of
        (Left msg)                         -> Left msg
        (Right (PrimType BoolPrim, binds)) -> Right binds
        (Right (typ, _))                   -> Left (formatErr ("predicate type mismatch '" ++ show typ ++ "' expected 'Bool'") utilData)

evalMatch :: Types -> [(PatternAST, ExprAST)] -> Env -> Sig -> UtilData -> Either String (Types, [Binding])
evalMatch _ [] _ _ utilData = Left (formatErr ("cannot inference type of a match expression of zero branches") utilData)
evalMatch typ1 ((pat1, expr1):branches') env sigma utilData =
    case match typ1 pat1 sigma of
        (Left msg)       -> Left msg
        (Right bindings) ->
            case evalExpr expr1 (applyBindings env bindings) sigma of
                err@(Left _) -> err
                (Right (typ2, _)) -> handleMatchBranches typ1 branches' typ2 env sigma utilData

handleMatchBranches :: Types -> [(PatternAST, ExprAST)] -> Types -> Env -> Sig -> UtilData -> Either String (Types, [Binding])
handleMatchBranches _ [] typ2 _ _ _ = Right (typ2, [])
handleMatchBranches typ1 ((pat, expr):branches') typ2 env sigma utilData =
    case match typ1 pat sigma of
        (Left msg)       -> Left msg
        (Right bindings) ->
            case evalExpr expr (applyBindings env bindings) sigma of
                err@(Left _) -> err
                (Right (typ3, _)) ->
                    if typ2 == typ3
                        then handleMatchBranches typ1 branches' typ2 env sigma utilData
                        else Left (formatErr ("match branch type mismatch '" ++ show typ3 ++ "' expected '" ++ show typ2 ++ "'") utilData)

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
--  2. a mismatch of types is detected 
-- otherwise, returns a list of bindings
match :: Types -> PatternAST -> Sig -> Either String [Binding]
match typ (VarPatternAST varId _) _ = Right ([(varId, typ)])

match _ (WildPatternAST _) _ = Right []

match (PrimType (IntPrim)) (ConstPatternAST (IntConstAST _ _) _) _ = Right []
match (PrimType (FloatPrim)) (ConstPatternAST (FloatConstAST _ _) _) _ = Right []
match (PrimType (BoolPrim)) (ConstPatternAST (BoolConstAST _ _) _) _ = Right []
match (PrimType (CharPrim)) (ConstPatternAST (CharConstAST _ _) _) _ = Right []

match (AlgeType typ sigma') (TypePatternAST t utilData) sigma =
    if sigma `has` t
        then if sigma' `has` t
            then Right []
            else Left (formatErr ("mismatched type '" ++ getTermType sigma t ++ "' expected '" ++ typeName typ ++ "'") utilData)
        else Left (formatErr ("unknown term-constructor '" ++ typeName t ++ "'") utilData)

match (ListType typ) (DecompPatternAST pat' varId _) sigma =
    case maybeDelta of
        err@(Left _)      -> err
        (Right bindings) -> Right ((varId, typ):bindings)
    where
        maybeDelta = match typ pat' sigma

match expected@(AlgeType typ sigma') pat@(TypeConsPatternAST t pat' utilData) sigma =
    if sigma `has` t
        then if sigma' `has` t
            then case getSignature sigma' t of
                    Nothing    -> Left (formatErr ("termconstructor '" ++ typeName t ++ "' is not compound") utilData)
                    (Just sig) -> match sig pat' sigma
            else Left (formatErr ("mismatched type '" ++ getTermType sigma t ++ "' expected '" ++ typeName typ ++ "'") utilData)
        else Left (formatErr ("unknown term-constructor '" ++ typeName t ++ "'") utilData)

match (TuplType typs) (TuplePatternAST ps utilData) sigma = matchMultiple typs ps sigma utilData

match (ListType _) (ListPatternAST [] _) _ = Right []
match (ListType typ') (ListPatternAST ps utilData) sigma = matchMultiple ((Prelude.take (length ps)) (repeat typ')) ps sigma utilData               

match typ pat _ = Left (formatErr ("could not match pattern '" ++ prettyShow pat 0 ++ "' with type '" ++ show typ ++ "'") (getUtilDataPat pat))

-- helper function for match
-- it is used to evaluate immediate constituents of tuple/list Type pattern pairs
-- returns an error message if:
--  1. any of the recursive matches result in an error message
-- otherwise, returns a list of bindings
matchMultiple :: [Types] -> [PatternAST] -> Sig -> UtilData -> Either String [Binding]
matchMultiple [] [] _ _                        = Right []
matchMultiple (typ:typs) (p:ps) sigma utilData =
    if (length typs) /= (length ps)
        then Left (formatErr ("mismatched type and pattern lengths, expected immediates '" ++ ([show typ' | typ' <- (typ:typs)] >>= (++ ", "))  ++ "'") utilData)
        else case maybeBind of
                err@(Left _) -> err
                (Right bind) ->
                    case maybeBinds of
                        err@(Left _)  -> err
                        (Right binds) -> Right (bind ++ binds)
            where
                maybeBind  = match typ p sigma                    -- evaluate immediate pair
                maybeBinds = matchMultiple typs ps sigma utilData -- evaluate remaining pairs

matchMultiple _ _ _ _ = error "illegal tuple size" -- this should not happen!