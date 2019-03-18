module Actions
    ( let_in
    , decomp_pat
    , func_left_expr
    , convert_one_op
    , convert_two_op
    , convert_three_op
    , convert_unary_op
    ) where

import Ast
import Lexer

--rule Let_in: 1
let_in :: [TypeVarAST] -> ExprAST -> ExprAST -> ExprAST
let_in [var] expr1 expr2 = LetInExprAST var expr1 expr2
let_in tuple expr1 expr2 = MatchExprAST expr1 [(tuple_to_pattern tuple, expr2)]

tuple_to_pattern :: [TypeVarAST] -> PatternAST
tuple_to_pattern vars = TuplePatternAST (map type_var_to_pat vars) 

type_var_to_pat :: TypeVarAST -> PatternAST
type_var_to_pat (UntypedVarAST name) = VarPatternAST name
type_var_to_pat (TypedVarAST name _) = VarPatternAST name

--rule Struc_pat: 4
decomp_pat :: PatternAST -> String -> VarId -> PatternAST
decomp_pat pat op var = case op of
                          ":" -> DecompPatternAST pat var
                          _   -> error "only the decomposition operator may be used in a pattern."

--rule Left_expr: 4
func_left_expr :: [ExprAST] -> ExprAST
func_left_expr []     = error "Cannot apply a function to zero arguments."
func_left_expr [expr] = expr
func_left_expr (x:xs) = FunAppExprAST x (func_left_expr xs)

convert_one_op :: String -> ConstAST
convert_one_op "+"  = PlusConstAST
convert_one_op "-"  = MinusConstAST
convert_one_op "++" = ConcatenateConstAST
convert_one_op "&&" = AndConstAST
convert_one_op "||" = OrConstAST
convert_one_op _    = error "undefined operator."

convert_two_op :: String -> ConstAST
convert_two_op "*"  = TimesConstAST
convert_two_op "/"  = DivideConstAST
convert_two_op "%"  = ModuloConstAST
convert_two_op "==" = EqualsConstAST
convert_two_op _    = error "undefined operator."

convert_three_op :: String -> ConstAST
convert_three_op "<"  = LessConstAST
convert_three_op ">"  = GreaterConstAST
convert_three_op "<=" = LessOrEqualConstAST
convert_three_op ">=" = GreaterOrEqualConstAST
convert_three_op ":"  = AppenConstAST
convert_three_op _    = error "undefined operator."

convert_unary_op :: String -> ConstAST
convert_unary_op "!" = NotConstAST
convert_unary_op "~" = UnaryMinusConstAST
convert_unary_op _   = error "undefined operator."