module Actions
    (
    ) where

import Ast

--rule Prog: 1
prog :: [TypeDclAST] -> [VarDclAST] -> ProgAST
prog typeDecs varDecs = ProgAST typeDecs varDecs

--rule Type_decs: 1
eps_type_decs :: [TypeDclAST]
eps_type_decs = []

--rule Type_decs: 2
type_decs :: [TypeDclAST] -> TypeDclAST -> [TypeDclAST]
type_decs decs dec = decs ++ [dec]

--rule Type_dec: 1
type_dec :: TypeId -> [ConsAST] -> TypeDclAST
type_dec type_id cons_list = TypeDclAST type_id cons_list 

--rule Cons_list: 1
cons_list :: [ConsAST] -> ConsAST -> [ConsAST]
cons_list cons_list cons = cons_list ++ [cons]

--rule Cons_list: 2
simple_cons_list :: ConsAST -> [ConsAST]
simple_cons_list cons = [cons]

--rule Cons: 1
comp_cons :: TypeId -> CompTypeAST -> ConsAST
comp_cons type_id comp_type = DoubleConsAST type_id comp_type

--rule Cons: 2
simple_cons :: TypeId -> ConsAST
simple_cons type_id = SingleConsAST type_id


--rule Var_decs: 1
eps_var_decs :: [VarDclAST]
eps_var_decs = []

--rule Var_decs: 2
var_decs :: [VarDclAST] -> VarDclAST -> [VarDclAST]
var_decs decs dec = decs ++ [dec]

--rule Var_dec: 1
var_dec :: TypeVarAST -> ExprAST -> VarDclAST
var_dec var expr = VarDclAST var expr

--rule Match: 1
match :: ExprAST -> [(PatternAST, ExprAST)] -> ExprAST
match expr body = MatchExprAST expr body

--rule Match_body: 1
match_body :: [(PatternAST, ExprAST)] -> PatternAST -> ExprAST -> [(PatternAST, ExprAST)]
match_body list pattern expr = list ++ [(pattern, expr)]

--rule Match_body: 2
single_match_body :: PatternAST -> ExprAST -> [(PatternAST, ExprAST)]
single_match_body pattern expr = [(pattern, expr)]

--rule Let_in: 1
let_in :: [TypeVarAST] -> ExprAST -> ExprAST -> ExprAST
let_in [var] expr1 expr2 = LetInExprAST var expr1 expr2
let_in tuple expr1 expr2 = MatchExprAST expr1 [(tuple_to_pattern tuple, expr2)]

tuple_to_pattern :: [TypeVarAST] -> PatternAST
tuple_to_pattern vars = TuplePatternAST (map type_var_to_pat vars) 

type_var_to_pat :: TypeVarAST -> PatternAST
type_var_to_pat (UnTypedVarAST name) = VarPatternAST name
type_var_to_pat (TypedVarAST name _) = VarPatternAST name

--rule Case: 1
case_expr :: [(PredAST, ExprAST)] -> ExprAST
case_expr body = CaseExprAST body

--rule Case_body: 1
case_body :: [(PredAST, ExprAST)] -> PredAST -> ExprAST -> [(PredAST, ExprAST)]
case_body body pred' expr = body ++ [(pred', expr)]

--rule Case_body: 2
single_case_body :: PredAST -> ExprAST -> [(PredAST, ExprAST)]
single_case_body pred' expr = [(pred', expr)]