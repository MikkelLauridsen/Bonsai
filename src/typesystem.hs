module Typesystem
    (
      typeBonsai
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
type Sig = Set TermConstructor

type Binding = (VarId, Types)

extractConsName :: TermConstructor -> TypeId
extractConsName (typeId, _, _)  = typeId

extractTypeName :: Types -> TypeId
extractTypeName (AlgeType t)   = t
extractTypeName (AlgePoly t _) = t
extractTypeName _              = error "type must be algebraic"

getTermType :: Sig -> TypeId -> String
getTermType sigma t =
    case find (\(t', _, _) -> t' == t) (Set.toList sigma) of
        Nothing               -> error "unknown termconstructor"
        (Just (_, typ, _)) -> show typ

getTermConstructor :: Sig -> TypeId -> Maybe TermConstructor
getTermConstructor sigma t = find (\(t', _, _) -> t' == t) (Set.toList sigma)

getSignature :: Sig -> TypeId -> Maybe Types
getSignature sigma t =
    case find (\(t', _, _) -> t' == t) (Set.toList sigma) of
        Nothing            -> Nothing
        (Just (_, _, typ)) -> Just typ

getAlgebraicType :: Sig -> TypeId -> Maybe Types
getAlgebraicType sigma t =
    case find (\(_, typ, _) -> extractTypeName typ == t) (Set.toList sigma) of
        Nothing            -> Nothing
        (Just (_, typ, _)) -> Just typ

-- checks whether the input termconstructor (by name)
-- is defined in input set of Algebraic types
has :: Sig -> TypeId -> Bool
has sigma t =
    case find (\(t', _, _) -> t' == t) (Set.toList sigma) of
        Nothing            -> False
        (Just (_, _, _)) -> True

lazyHas :: LazySig -> TypeId -> Bool
lazyHas ls t =
    case find (\(t', _) -> t' == t) (Set.toList ls) of
        Nothing  -> False
        (Just _) -> True

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

buildSignature :: CompTypeAST -> TypeId -> [String] -> LazySig -> Either String Types
buildSignature (CompSimpleAST typeId utilData) _ _ ls =
    case lazyIdToTypes typeId ls of
        Nothing    -> Left (formatErr ("unknown type '" ++ typeName typeId ++ "'") utilData)
        (Just typ) -> Right typ

buildSignature (CompSimplePolyAST varId utilData) memberType typeVars _ =
    if elem (varName varId) typeVars
        then Right $ PolyType (varName varId)
        else Left (formatErr ("algebraic type '" ++ typeName memberType ++ "' does not have type-variable '" ++ varName varId ++ "' in its signature") utilData)

buildSignature (CompPolyAST typeId comps' utilData) memberType typeVars ls =
    case lazyIdToTypes typeId ls of
        Nothing                   -> Left (formatErr ("unknown type '" ++ typeName typeId ++ "'") utilData)
        (Just (AlgePoly _ polys)) ->
            if length polys /= length comps'
                then Left (formatErr ("algebraic type '" ++ typeName typeId ++ "' cannot be applied to " ++ show (length comps') ++ "type(s)") utilData)
                else case buildSignatureList comps' memberType typeVars ls of
                    (Left msg)   -> Left msg
                    (Right typs) -> Right (AlgePoly typeId typs)
        (Just typ)                -> Left (formatErr ("algebraic type '" ++ show typ ++ "' is not polymorphic") utilData)

buildSignature (CompListAST comp' _) memberType typeVars ls =
    case buildSignature comp' memberType typeVars ls of
        err@(Left _) -> err
        (Right typ)  -> Right $ ListType typ

buildSignature (CompTupleAST comps' _) memberType typeVars ls =
    case buildSignatureList comps' memberType typeVars ls of
        (Left msg)   -> Left msg
        (Right typs) -> Right $ TuplType typs

buildSignature (CompFuncAST comp1' comp2' _) memberType typeVars ls =
    case buildSignature comp1' memberType typeVars ls of
        err@(Left _)  -> err
        (Right typ1)  ->
            case buildSignature comp2' memberType typeVars ls of
                err@(Left _) -> err
                (Right typ2) -> Right (FuncType typ1 typ2)

buildSignatureList :: [CompTypeAST] -> TypeId -> [String] -> LazySig -> Either String [Types]
buildSignatureList [] _ _ _ = Right []
buildSignatureList (comp:comps') memberType typeVars ls =
    case buildSignature comp memberType typeVars ls of
        (Left msg)  -> Left msg
        (Right typ) ->
            case buildSignatureList comps' memberType typeVars ls of
                err@(Left _) -> err
                (Right typs) -> Right (typ:typs)

types :: CompTypeAST -> Sig -> Either String Types
types (CompSimpleAST typeId utilData) sigma =
    case idToTypes typeId sigma of
        (Left id)   -> Left (formatErr ("unknown type '" ++ typeName id ++ "'") utilData)
        (Right typ) -> Right typ

types (CompSimplePolyAST varId _) _  = Right $ PolyType (varName varId)

types (CompPolyAST typeId comps' utilData) sigma = 
    case idToTypes typeId sigma of
        (Left id) -> Left (formatErr ("unknown type '" ++ typeName id ++ "'") utilData)
        (Right typ@(AlgePoly name polys)) ->
            case typesList comps' sigma of
                (Left msg)   -> Left msg
                (Right typs) ->
                    if length polys == length typs
                        then Right (AlgePoly name typs)
                        else Left (formatErr ("algebraic type '" ++ show typ ++ "' cannot be applied to " ++ show (length typs) ++ "type(s)") utilData)
        (Right typ) -> Left (formatErr ("type '" ++ show typ ++ "' cannot be used polymorphically") utilData)

types (CompListAST comp' _) sigma =
    case types comp' sigma of
        err@(Left _) -> err
        (Right typ)  -> Right $ ListType typ

types (CompTupleAST comps' _) sigma =
    case typesList comps' sigma of
        (Left msg)   -> Left msg
        (Right typs) -> Right $ TuplType typs

types (CompFuncAST comp1' comp2' _) sigma = 
    case types comp1' sigma of
        err@(Left _) -> err
        (Right typ1) ->
            case types comp2' sigma of
                err@(Left _) -> err
                (Right typ2) -> Right (FuncType typ1 typ2)

lazyIdToTypes :: TypeId -> LazySig -> Maybe Types
lazyIdToTypes id ls =
    if last (typeName id) == '*'
        then case stringToNonUniquePrim (init (typeName id)) of
            (Just typ) -> Just (UniqType typ True)
            Nothing    ->
                case find (\(id', _) -> typeName id' == init (typeName id)) (Set.toList ls) of
                    (Just (typ, polys)) -> 
                        if length polys == 0
                            then Just (UniqType (AlgeType typ) True)
                            else Just (UniqType (AlgePoly typ (stringsToPolyTypes polys)) True)
                    Nothing             -> Nothing
        else case stringToNonUniquePrim (typeName id) of
            (Just typ) -> Just typ
            Nothing    ->
                case find (\(id', _) -> id' == id) (Set.toList ls) of
                    (Just (typ, polys)) -> 
                        if length polys == 0
                            then Just $ AlgeType typ
                            else Just (AlgePoly typ (stringsToPolyTypes polys))
                    Nothing             -> Nothing

stringsToPolyTypes :: [String] -> [Types]
stringsToPolyTypes = Data.List.map (\s -> PolyType s)

idToTypes :: TypeId -> Sig -> Either TypeId Types
idToTypes id sigma =
    if last (typeName id) == '*'
        then case stringToNonUniquePrim (init (typeName id)) of
            (Just typ) -> Right (UniqType typ True)
            Nothing    ->
                case getAlgebraicType sigma (TypeId (init (typeName id))) of
                    (Just typ) -> Right (UniqType typ True)
                    Nothing    -> Left id
        else case stringToNonUniquePrim (typeName id) of
            (Just typ) -> Right typ
            Nothing    ->
                case getAlgebraicType sigma id of
                    (Just typ) -> Right typ
                    Nothing    -> Left id

stringToNonUniquePrim :: String -> Maybe Types
stringToNonUniquePrim "Int"    = Just $ PrimType IntPrim
stringToNonUniquePrim "Float"  = Just $ PrimType FloatPrim
stringToNonUniquePrim "Bool"   = Just $ PrimType BoolPrim
stringToNonUniquePrim "Char"   = Just $ PrimType CharPrim
stringToNonUniquePrim "File"   = Just $ PrimType FilePrim
stringToNonUniquePrim "System" = Just $ PrimType SystemPrim
stringToNonUniquePrim "String" = Just $ ListType (PrimType CharPrim)
stringToNonUniquePrim _        = Nothing

typesList :: [CompTypeAST] -> Sig -> Either String [Types]
typesList [] _                = Right []
typesList (comp:comps') sigma =
    case types comp sigma of
        (Left msg)  -> Left msg
        (Right typ) ->
            case typesList comps' sigma of
                err@(Left _) -> err
                (Right typs) -> Right (typ:typs)

typeBonsai :: FilePath -> ProgAST -> Maybe String
typeBonsai path (ProgAST dt dv _) =
    case evalTypeDclLazily dt Set.empty of
        (Left msg)        -> Just (path ++ ":" ++ msg)
        (Right lazySigma) ->
            case evalTypeDcl dt Set.empty lazySigma of
                (Left msg)    -> Just (path ++ ":" ++ msg)
                (Right sigma) ->
                    case evalVarDclLazily dv initEnv sigma of
                        (Left msg)  -> Just (path ++ ":" ++ msg)
                        (Right env) ->
                            case evalVarDcl dv env sigma of
                                (Just msg) -> Just (path ++ ":" ++ msg)
                                Nothing  -> Nothing
    where
        stdin'  = (VarId "stdin" Untyped, UniqType (PrimType FilePrim) True)
        stdout' = (VarId "stdout" Untyped, UniqType (PrimType FilePrim) True)
        initEnv = Map.fromList [stdin', stdout']

evalVarDclLazily :: VarDclAST -> Env -> Sig -> Either String Env
evalVarDclLazily EpsVarDclAST env _ = Right env
evalVarDclLazily (VarDclAST (UntypedVarAST x _) expr dv' utilData) env sigma = error "not yet implemented."

evalVarDclLazily (VarDclAST (TypedVarAST x s _) _ dv' utilData) env sigma =
    case getVar env x of
        (Just _) -> Left (formatErr ("cannot redeclare global variable '" ++ varName x ++ "'") utilData)
        Nothing  ->
            case types s sigma of
                (Left msg)  -> Left msg
                (Right typ) -> evalVarDclLazily dv' (env `except` (x, typ)) sigma

evalVarDcl :: VarDclAST -> Env -> Sig -> Maybe String
evalVarDcl EpsVarDclAST _ _ = Nothing
evalVarDcl (VarDclAST (UntypedVarAST x _) expr dv' utilData) env sigma = error "not yet implemented."

evalVarDcl (VarDclAST (TypedVarAST x s _) expr dv' utilData) env sigma =
    case evalExpr expr env sigma of
        (Left msg)        -> Just msg
        (Right (typ', _)) -> 
            if typ' == typ
                then evalVarDcl dv' env sigma
                else Just (formatErr ("variable declaration type mismatch, variable '" ++ varName x ++ "' was annotated as '" ++ show typ ++ "' but has type '" ++ show typ' ++ "'") utilData)
    where
        typ = 
            case getVar env x of
                Nothing    -> error "annotated variable has no type." -- should not happen
                (Just typ) -> typ



evalTypeDclLazily :: TypeDclAST -> LazySig -> Either String LazySig
evalTypeDclLazily EpsTypeDclAST lazySigma = Right lazySigma

evalTypeDclLazily (TypeDclAST typeId _ dt' utilData) lazySigma =
    if lazySigma `lazyHas` typeId
        then Left (formatErr ("algebraic type '" ++ typeName typeId ++ "' cannot be redefined") utilData)
        else evalTypeDclLazily dt' (Set.insert (typeId, []) lazySigma)

evalTypeDclLazily (TypePolyDclAST typeId polys _ dt' utilData) lazySigma =
    if lazySigma `lazyHas` typeId
        then Left (formatErr ("algebraic type '" ++ typeName typeId ++ "' cannot be redefined") utilData)
        else evalTypeDclLazily dt' (Set.insert (typeId, vars) lazySigma)
    where
        vars = Data.List.map varName polys

evalTypeDcl :: TypeDclAST -> Sig -> LazySig -> Either String Sig
evalTypeDcl EpsTypeDclAST sigma lazySigma = Right sigma

evalTypeDcl (TypeDclAST typeId cons dt' utilData) sigma lazySigma = -- TODO: check conflicts!!
    case evalCons cons typeId [] lazySigma of
        (Left msg)  -> Left msg
        (Right tcs) -> evalTypeDcl dt' (sigma `Set.union` (Set.fromList tcs)) lazySigma

evalTypeDcl (TypePolyDclAST typeId polys cons dt' utilData) sigma lazySigma =
    case evalCons cons typeId vars lazySigma of
        (Left msg)  -> Left msg
        (Right tcs) -> evalTypeDcl dt' (sigma `Set.union` (Set.fromList tcs)) lazySigma
    where
        vars = Data.List.map (\x -> varName x) polys

evalCons :: [ConsAST] -> TypeId -> [String] -> LazySig -> Either String [TermConstructor]
evalCons [] _ _ _ = Right []
evalCons (tc:tcs') typeId typeVars lazySigma =
    case tc of
        (SingleConsAST name _) ->
            case evalCons tcs' typeId typeVars lazySigma of
                err@(Left _) -> err
                (Right res)  ->
                    if length typeVars == 0
                        then Right ((name, AlgeType typeId, AlgeType typeId):res)
                        else Right ((name, AlgePoly typeId (stringsToPolyTypes typeVars), AlgePoly typeId (stringsToPolyTypes typeVars)):res)
        (DoubleConsAST name s utilData) ->
            case buildSignature s typeId typeVars lazySigma of
                (Left msg)  -> Left msg
                (Right typ) ->
                    case evalCons tcs' typeId typeVars lazySigma of
                        err@(Left _) -> err
                        (Right res)  ->
                            if length typeVars == 0
                                then Right ((name, AlgeType typeId, FuncType typ (AlgeType typeId)):res)
                                else Right ((name, AlgePoly typeId (stringsToPolyTypes typeVars), FuncType typ (AlgePoly typeId (stringsToPolyTypes typeVars))):res)

-- collection of expression type rules
-- implementation of rules (const), (lambda), (ter)
-- returns an error message if:
--  1. any of the type rule implementations return an error message
-- otherwise, returns a type and a list of linear-type bindings
evalExpr :: ExprAST -> Env -> Sig -> Either String (Types, [Binding])
evalExpr (VarExprAST varId utilData) env sigma   = evalVarExpr varId env sigma utilData
evalExpr (TypeExprAST typeId utilData) env sigma = evalTer typeId env sigma utilData

evalExpr (ConstExprAST c utilData) env sigma = Right (evalConst c, [])

evalExpr (ParenExprAST expr' _) env sigma               = evalExpr expr' env sigma
evalExpr (LambdaExprAST xt expr' _) env sigma           = evalLambda xt expr' env sigma
evalExpr (FunAppExprAST expr1 expr2 utilData) env sigma = evalFunApp expr1 expr2 env sigma utilData
evalExpr (TupleExprAST exprs _) env sigma               = evalTuple exprs env sigma
evalExpr (ListExprAST exprs utilData) env sigma         = evalList exprs env sigma utilData
evalExpr (CaseExprAST branches utilData) env sigma      = evalCase branches env sigma utilData
evalExpr (LetInExprAST xt expr1 expr2 _) env sigma      = evalLetIn xt expr1 expr2 env sigma

evalExpr (MatchExprAST expr' branches utilData) env sigma =
    case evalExpr expr' env sigma of
        err@(Left _)         -> err 
        (Right (typ, binds)) -> evalMatch typ branches (applyBindings env binds) sigma utilData

evalFunApp :: ExprAST -> ExprAST -> Env -> Sig -> UtilData -> Either String (Types, [Binding])
evalFunApp expr1 expr2 env sigma utilData =
    case evalExpr expr2 env sigma of
        err@(Left _)         -> err
        (Right (typ, binds)) ->
            case evalExpr expr1 env' sigma of
                err@(Left _)                            -> err
                (Right (FuncType typ1' typ2', binds'))  ->
                    case analyzeTypevars typ1' typ Map.empty of
                        (True, typeBinds) -> Right (applyTypeBindings typ2' typeBinds, binds ++ binds')
                        (False, _)        -> Left (formatErr ("mismatched parameter types in function application, expected '" ++ show typ1' ++ "' but actual type is '" ++ show typ ++ "'") utilData)
                (Right (LazyFunc x body env'', binds')) ->
                    case evalExpr body (applyBindings env'' (binds ++ binds' ++ [(x, typ)])) sigma of
                        err@(Left _)             -> err
                        (Right (typ2', binds'')) -> Right (typ2', binds ++ binds' ++ binds'')
                (Right (wrongType, _)) -> Left (formatErr ("mismatched types in function application, expected a function but actual type is '" ++ show wrongType ++ "'") utilData)
            where
                env' = applyBindings env binds

applyTypeBindings :: Types -> Map String Types -> Types
applyTypeBindings typ@(PrimType _) _               = typ
applyTypeBindings (FuncType typ1 typ2) typeBinds   = FuncType (applyTypeBindings typ1 typeBinds) (applyTypeBindings typ2 typeBinds)
applyTypeBindings (TuplType typs) typeBinds        = TuplType [applyTypeBindings typ typeBinds | typ <- typs]
applyTypeBindings (ListType typ) typeBinds         = ListType (applyTypeBindings typ typeBinds)
applyTypeBindings EmptList _                       = EmptList 
applyTypeBindings typ@(AlgeType _) _               = typ
applyTypeBindings (AlgePoly typeId typs) typeBinds = AlgePoly typeId [applyTypeBindings typ typeBinds | typ <- typs]

applyTypeBindings typ@(PolyType var) typeBinds =
    case Map.lookup var typeBinds of
        Nothing     -> typ
        (Just typ') -> typ'

applyTypeBindings (UniqType typ valid) typeBinds = UniqType (applyTypeBindings typ typeBinds) valid
applyTypeBindings _ _                            = error "cannot apply type bindings to a lazy type."

analyzeTypevars :: Types -> Types -> Map String Types -> (Bool, Map String Types)
analyzeTypevars (PrimType prim1) (PrimType prim2) typeBinds = (prim1 == prim2, typeBinds)

analyzeTypevars (FuncType typa1 typb1) (FuncType typa2 typb2) typeBinds =
    case analyzeTypevars typa1 typa2 typeBinds of
        (True, typeBinds') -> analyzeTypevars typb1 typb2 typeBinds'
        res@(False, _)     -> res

analyzeTypevars (TuplType typs1) (TuplType typs2) typeBinds     = analyzeTypevarsList typs1 typs2 typeBinds
analyzeTypevars (ListType typ1) (ListType typ2) typeBinds       = analyzeTypevars typ1 typ2 typeBinds
analyzeTypevars EmptList EmptList typeBinds                     = (True, typeBinds)
analyzeTypevars EmptList (ListType _) typeBinds                 = (True, typeBinds)
analyzeTypevars (ListType _) EmptList typeBinds                 = (True, typeBinds)
analyzeTypevars (AlgeType typeId1) (AlgeType typeId2) typeBinds = (typeId1 == typeId2, typeBinds)

analyzeTypevars (AlgePoly typeId1 typs1) (AlgePoly typeId2 typs2) typeBinds =
    if typeId1 == typeId2
        then analyzeTypevarsList typs1 typs2 typeBinds
        else (False, typeBinds)

analyzeTypevars (AlgePoly typeId1 _) (AlgeType typeId2) typeBinds = (typeId1 == typeId2, typeBinds)
analyzeTypevars (AlgeType typeId1) (AlgePoly typeId2 _) typeBinds = (typeId1 == typeId2, typeBinds)

analyzeTypevars typ1@(PolyType var) typ2 typeBinds =
    if typ2 == typ1
        then (True, typeBinds)
        else case Map.lookup var typeBinds of
            Nothing                  -> (True, Map.insert var typ2 typeBinds)
            (Just typ3@(PolyType _)) -> (typ2 == typ3, typeBinds)
            (Just typ1)              -> analyzeTypevars typ1 typ2 typeBinds

analyzeTypevars (UniqType typ1 _) (UniqType typ2 _) typeBinds = analyzeTypevars typ1 typ2 typeBinds

-- catch all pattern
analyzeTypevars expected actual typeBinds = (False, typeBinds)

analyzeTypevarsList :: [Types] -> [Types] -> Map String Types -> (Bool, Map String Types) 
analyzeTypevarsList [] [] typeBinds = (True, typeBinds)
analyzeTypevarsList [] _ typeBinds  = (False, typeBinds)
analyzeTypevarsList _ [] typeBinds  = (False, typeBinds)
analyzeTypevarsList (t1:ts1) (t2:ts2) typeBinds =
    case analyzeTypevars t1 t2 typeBinds of
        (True, typeBinds')  -> analyzeTypevarsList ts1 ts2 typeBinds'
        res@(False, _)      -> res

evalLetIn :: TypeVarAST -> ExprAST -> ExprAST -> Env -> Sig -> Either String (Types, [Binding])
evalLetIn (TypedVarAST x s utilData) expr1 expr2 env sigma =
    case evalExpr expr1 env sigma of
        err@(Left _)         -> err
        (Right (typ, binds)) ->
            case types s sigma of
                (Left msg)   -> Left msg
                (Right typ') ->
                    if typ == typ'
                        then evalExpr expr2 env' sigma
                        else Left (formatErr ("mismatched types in let-in, variable '" ++ varName x ++ "' is annotated as '" ++ show typ' ++ "' but has type '" ++ show typ ++ "'") utilData) 
                        where
                            env' = applyBindings env (binds ++ [(x, typ')])

evalLetIn (UntypedVarAST x _) expr1 expr2 env sigma =
    case evalExpr expr1 env sigma of
        err@(Left _)         -> err
        (Right (typ, binds)) -> evalExpr expr2 env' sigma
            where
                env' = applyBindings env (binds ++ [(x, typ)])

evalVarExpr :: VarId -> Env -> Sig -> UtilData -> Either String (Types, [Binding])
evalVarExpr varId env sigma utilData =
    case getVar env varId of
        Nothing                         -> Left (formatErr ("variable '" ++ varName varId ++ "' is out of scope") utilData)
        (Just typ@(UniqType _ False))   -> Left (formatErr ("unique variable '" ++ varName varId ++ "::" ++ show typ ++ "' cannot be used twice") utilData)
        (Just uniq@(UniqType typ True)) -> Right (uniq, [(varId, (UniqType typ False))])
        (Just (LazyType expr))          ->
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
    case getTermConstructor sigma t of
        Nothing -> Left (formatErr ("unknown term-constructor '" ++ typeName t ++ "'") utilData)
        (Just (_, mem@(AlgePoly name _), sig)) ->
            if mem == sig
                then Right (AlgeType name, [])
                else Right (sig, [])
        (Just (_, mem, sig)) -> Right (sig, [])

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
evalConst (WriteConstAST _)          = FuncType (PrimType CharPrim) (FuncType (UniqType (PrimType FilePrim) True) (TuplType [PrimType BoolPrim, UniqType (PrimType FilePrim) True]))
evalConst (DeleteConstAST _)         = FuncType (UniqType (PrimType SystemPrim) True) (FuncType (UniqType (PrimType FilePrim) True) (TuplType [PrimType BoolPrim, UniqType (PrimType SystemPrim) True]))
evalConst (ShowConstAST _)           = FuncType (PolyType "a0") (ListType (PrimType CharPrim))
evalConst (ToIntConstAST _)          = FuncType (ListType (PrimType CharPrim)) (TuplType [PrimType BoolPrim, PrimType IntPrim])
evalConst (ToFloatConstAST _)        = FuncType (ListType (PrimType CharPrim)) (TuplType [PrimType BoolPrim, PrimType FloatPrim])
evalConst (IntToCharAST _)           = FuncType (PrimType IntPrim) (PrimType CharPrim)
evalConst (CharToIntAST _)           = FuncType (PrimType CharPrim) (PrimType IntPrim)

evalLambda :: TypeVarAST -> ExprAST -> Env -> Sig -> Either String (Types, [Binding])
evalLambda (TypedVarAST x s _) expr env sigma = 
    case types s sigma of
        (Left msg)   -> Left msg
        (Right typ1) ->
            case evalExpr expr env' sigma of
                err@(Left _)          -> err
                (Right (typ2, binds)) -> Right (FuncType typ1 typ2, binds)
            where
                env' = env `except` (x, typ1)

evalLambda (UntypedVarAST x _) expr env _ = Right (LazyFunc x expr env, []) 

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
evalList [] _ _ _ = Right (EmptList, [])
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
                (Right (typ, _)) -> handleCaseBranches typ branches' env sigma Map.empty utilData

handleCaseBranches :: Types -> [(PredAST, ExprAST)] -> Env -> Sig -> Map String Types -> UtilData -> Either String (Types, [Binding])
handleCaseBranches typ [] _ _ typeBinds _ = Right (applyTypeBindings typ typeBinds, [])
handleCaseBranches typ ((pred, expr):branches') env sigma typeBinds utilData =
    case handlePred pred env sigma of
        (Left msg)    -> Left msg
        (Right binds) ->
            case evalExpr expr (applyBindings env binds) sigma of
                err@(Left _)      -> err
                (Right (typ', _)) ->
                    case analyzeTypevars typ typ' typeBinds of
                        (True, typeBinds') -> handleCaseBranches typ'' branches' env sigma typeBinds' utilData
                        (False, _)        -> Left (formatErr ("case branch type mismatch '" ++ show typ' ++ "' expected '" ++ show typ ++ "'") utilData)
                    where
                        typ'' = 
                            case typ of
                                EmptList     -> typ'
                                (AlgeType _) -> typ'
                                _            -> typ


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
                (Right (typ2, _)) -> handleMatchBranches typ1 branches' typ2 env sigma Map.empty utilData

handleMatchBranches :: Types -> [(PatternAST, ExprAST)] -> Types -> Env -> Sig -> Map String Types -> UtilData -> Either String (Types, [Binding])
handleMatchBranches _ [] typ2 _ _ typeBinds _ = Right (applyTypeBindings typ2 typeBinds, [])
handleMatchBranches typ1 ((pat, expr):branches') typ2 env sigma typeBinds utilData =
    case match typ1 pat sigma of
        (Left msg)       -> Left msg
        (Right bindings) ->
            case evalExpr expr (applyBindings env bindings) sigma of
                err@(Left _)      -> err
                (Right (typ3, _)) ->
                    case analyzeTypevars typ2 typ3 typeBinds of
                        (True, typeBinds') -> handleMatchBranches typ1 branches' typ'' env sigma typeBinds' utilData
                        (False, _)        -> Left (formatErr ("mismatched types in match expression, expected '" ++ show typ2 ++ "' but actual type is '" ++ show typ3 ++ "'") utilData)
                    where
                        typ'' = 
                            case typ2 of
                                EmptList     -> typ3
                                (AlgeType _) -> typ3
                                _            -> typ2

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

match expected@(AlgePoly typ typs) (TypePatternAST t utilData) sigma =
    case getTermConstructor sigma t of
        Nothing             -> Left (formatErr ("unknown term-constructor '" ++ typeName t ++ "'") utilData)
        (Just (_, typ', _)) ->
            if extractTypeName typ' == typ
                then Right []
                else Left (formatErr ("mismatched type '"++ show typ' ++ "' expected '" ++ show expected ++ "'") utilData)

match expected@(AlgeType typ) (TypePatternAST t utilData) sigma =
    case getTermConstructor sigma t of
        Nothing             -> Left (formatErr ("unknown term-constructor '" ++ typeName t ++ "'") utilData)
        (Just (_, typ', _)) ->
            if extractTypeName typ' == typ
                then Right []
                else Left (formatErr ("mismatched type '"++ show typ' ++ "' expected '" ++ show expected ++ "'") utilData)

match (ListType typ) (DecompPatternAST pat' varId _) sigma =
    case maybeDelta of
        err@(Left _)      -> err
        (Right bindings) -> Right ((varId, ListType typ):bindings)
    where
        maybeDelta = match typ pat' sigma

match expected@(AlgePoly typ typs) pat@(TypeConsPatternAST t pat' utilData) sigma =
    case getTermConstructor sigma t of
        Nothing                                           -> Left (formatErr ("unknown term-constructor '" ++ typeName t ++ "'") utilData)
        (Just (_, AlgePoly typ' typs', (FuncType sig _))) ->
            let (_, typeBinds) = analyzeTypevarsList typs' typs Map.empty
                in match (applyTypeBindings sig typeBinds) pat' sigma
        (Just (_, typ', _))                               -> Left (formatErr ("mismatched type '"++ show typ' ++ "' expected '" ++ show expected ++ "'") utilData)

match expected@(AlgeType typ) pat@(TypeConsPatternAST t pat' utilData) sigma =
    case getTermConstructor sigma t of
        Nothing               -> Left (formatErr ("unknown term-constructor '" ++ typeName t ++ "'") utilData)
        (Just (_, typ', sig)) ->
            if extractTypeName typ' == typ
                then match sig pat' sigma
                else Left (formatErr ("mismatched type '" ++ show typ' ++ "' expected '" ++ show expected ++ "'") utilData)

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
        then Left (formatErr ("mismatched type and pattern lengths, expected immediates '" ++ ([show typ' | typ' <- init (typ:typs)] >>= (++ ", ")) ++ show (last typs)  ++ "'") utilData)
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