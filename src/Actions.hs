module Actions
    ( let_in
    , func_left_expr
    , convert_one_op
    , convert_two_op
    , convert_three_op
    , convert_unary_op
    , convert_io_op
    , handle_paren
    , handle_string_expr
    , handle_string_pat
    ) where

import Ast

--rule Let_in: 1
let_in :: [PatternAST] -> ExprAST -> ExprAST -> ExprAST
let_in tuple expr1 expr2 = MatchExprAST expr1 [(TuplePatternAST tuple, expr2)]

--rule Left_expr: 4
func_left_expr :: [ExprAST] -> ExprAST
func_left_expr []     = error "Cannot apply a function to zero arguments."
func_left_expr [expr] = expr
func_left_expr (x:xs) = FunAppExprAST x (func_left_expr xs)

--rule Lit_expr: 6
handle_paren :: [ExprAST] -> ExprAST
handle_paren []       = error "Bonsai does not allow use of the unit type."
handle_paren [single] = ParenExprAST single
handle_paren multiple = TupleExprAST multiple

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
convert_three_op _    = error "undefined operator."

convert_unary_op :: String -> ConstAST
convert_unary_op "!" = NotConstAST
convert_unary_op "~" = UnaryMinusConstAST
convert_unary_op _   = error "undefined operator."

convert_io_op :: String -> ConstAST
convert_io_op "open_read" = OpenReadConstAST
convert_io_op "open_write" = OpenWriteConstAST
convert_io_op "close" = CloseConstAST
convert_io_op "read" = ReadConstAST
convert_io_op "write" = WriteConstAST
convert_io_op "delete" = DeleteConstAST
convert_io_op "show" = ShowConstAST
convert_io_op "to_int" = ToIntConstAST
convert_io_op "to_float" = ToFloatConstAST
convert_io_op _ = error "undefined IO operation."

handle_string_expr :: String -> [ExprAST]
handle_string_expr []     = []
handle_string_expr (c:cs) = ((ConstExprAST (CharConstAST c)):(handle_string_expr cs)) 

handle_string_pat :: String -> [PatternAST]
handle_string_pat []     = []
handle_string_pat (c:cs) = ((ConstPatternAST (CharConstAST c)):(handle_string_pat cs)) 