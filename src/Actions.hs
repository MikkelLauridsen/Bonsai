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
    , getUtilData
    , getVarId
    , getTypeId
    , getCharVal
    , getIntVal
    , getFloatVal
    , getBoolVal
    , getUtilDataProg
    , getUtilDataExpr
    , getUtilDataConst
    ) where

import Ast
import Lexer
import Data.Word (Word8) 
import Unsafe.Coerce (unsafeCoerce) 

getUtilData :: Token -> UtilData
getUtilData (Token (AlexUserState {currentLine=l, Lexer.position=p, lineNumber=n}) _) = UtilData Untyped (getPosition p n) l

getPosition :: AlexPosn -> (Int, Int) -> (Int, Int, Int)
getPosition (AlexPn _ line column) (_, off) = (line, column, off)

getUtilDataProg :: TypeDclAST -> VarDclAST -> UtilData
getUtilDataProg (TypeDclAST _ _ _ utilData) _ = utilData
getUtilDataProg _ (VarDclAST _ _ _ utilData)  = utilData
getUtilDataProg _ _                           = UtilData Untyped (0, 0, 0) ""

getUtilDataExpr :: ExprAST -> UtilData
getUtilDataExpr (VarExprAST _ utilData)       = utilData
getUtilDataExpr (TypeExprAST _ utilData)      = utilData
getUtilDataExpr (ConstExprAST _ utilData)     = utilData
getUtilDataExpr (ParenExprAST _ utilData)     = utilData
getUtilDataExpr (LambdaExprAST _ _ utilData)  = utilData
getUtilDataExpr (FunAppExprAST _ _ utilData)  = utilData
getUtilDataExpr (TupleExprAST _ utilData)     = utilData
getUtilDataExpr (ListExprAST _ utilData)      = utilData
getUtilDataExpr (MatchExprAST _ _ utilData)   = utilData
getUtilDataExpr (CaseExprAST _ utilData)      = utilData
getUtilDataExpr (LetInExprAST _ _ _ utilData) = utilData

getUtilDataConst :: ConstAST -> UtilData
getUtilDataConst (IntConstAST _ utilData) = utilData
getUtilDataConst (BoolConstAST _ utilData) = utilData
getUtilDataConst (FloatConstAST _ utilData) = utilData
getUtilDataConst (CharConstAST _ utilData) = utilData
getUtilDataConst (UnaryMinusConstAST utilData) = utilData
getUtilDataConst (PlusConstAST utilData) = utilData
getUtilDataConst (MinusConstAST utilData) = utilData
getUtilDataConst (TimesConstAST utilData) = utilData
getUtilDataConst (DivideConstAST utilData) = utilData
getUtilDataConst (ModuloConstAST utilData) = utilData
getUtilDataConst (EqualsConstAST utilData) = utilData
getUtilDataConst (NotConstAST utilData) = utilData
getUtilDataConst (GreaterConstAST utilData) = utilData
getUtilDataConst (LessConstAST utilData) = utilData
getUtilDataConst (GreaterOrEqualConstAST utilData) = utilData
getUtilDataConst (LessOrEqualConstAST utilData) = utilData
getUtilDataConst (AppenConstAST utilData) = utilData
getUtilDataConst (ConcatenateConstAST utilData) = utilData
getUtilDataConst (AndConstAST utilData) = utilData
getUtilDataConst (OrConstAST utilData) = utilData
getUtilDataConst (OpenReadConstAST utilData) = utilData
getUtilDataConst (OpenWriteConstAST utilData) = utilData
getUtilDataConst (CloseConstAST utilData) = utilData
getUtilDataConst (ReadConstAST utilData) = utilData
getUtilDataConst (WriteConstAST utilData) = utilData
getUtilDataConst (DeleteConstAST utilData) = utilData
getUtilDataConst (ShowConstAST utilData) = utilData
getUtilDataConst (ToIntConstAST utilData) = utilData
getUtilDataConst (ToFloatConstAST utilData) = utilData
getUtilDataConst (BiLShiftConstAST utilData) = utilData
getUtilDataConst (BiRShiftConstAST utilData) = utilData
getUtilDataConst (BiNotConstAST utilData) = utilData
getUtilDataConst (BiAndConstAST utilData) = utilData
getUtilDataConst (BiXorConstAST utilData) = utilData
getUtilDataConst (BiOrConstAST utilData) = utilData

getVarId :: Token -> VarId
getVarId (Token _ (VarIdToken name)) = VarId name Untyped
getVarId _ = error "Token must be asociated with a var id."

getTypeId :: Token -> TypeId
getTypeId (Token _ (TypeIdToken name)) = TypeId name
getTypeId _ = error "Token must be associated with a type id."

-- we convert the 4 byte Char into a 1 byte Word8,
-- which supports typeclasses Ord, Num, Read, Show
-- this is safe because the lexer only supports ASCII characters
toWord8 :: Char -> Word8
toWord8 = unsafeCoerce

getCharVal :: Token -> Word8
getCharVal (Token _ (CharToken c)) = toWord8 c
getCharVal _ = error "Token must be associated with a char value."

getIntVal :: Token -> Int
getIntVal (Token _ (IntToken i)) = i
getIntVal _ = error "Token must be associated with an int value."

getFloatVal :: Token -> Float
getFloatVal (Token _ (FloatToken f)) = f
getFloatVal _ = error "Token must be associated with a float value."

getBoolVal :: Token -> Bool
getBoolVal (Token _ (BoolToken b)) = b
getBoolVal _ = error "Token must be associated with a boolean value."

--rule Let_in: 1
let_in :: [PatternAST] -> ExprAST -> ExprAST -> UtilData -> ExprAST
let_in tuple expr1 expr2 utilData = MatchExprAST expr1 [(TuplePatternAST tuple utilData, expr2)] utilData

--rule Left_expr: 4
func_left_expr :: [ExprAST] -> UtilData -> ExprAST
func_left_expr [] _            = error "Cannot apply a function to zero arguments."
func_left_expr [expr] _        = expr
func_left_expr (x:xs) utilData = FunAppExprAST x (func_left_expr xs utilData) utilData

--rule Lit_expr: 6
handle_paren :: [ExprAST] -> UtilData -> ExprAST
handle_paren [] _              = error "Bonsai does not allow use of the unit type."
handle_paren [single] utilData = ParenExprAST single utilData
handle_paren multiple utilData = TupleExprAST multiple utilData

convert_one_op :: Token -> ConstAST
convert_one_op token@(Token _ (LevelOneOpToken "+"))  = PlusConstAST (getUtilData token)
convert_one_op token@(Token _ (LevelOneOpToken "-"))  = MinusConstAST (getUtilData token)
convert_one_op token@(Token _ (LevelOneOpToken "++")) = ConcatenateConstAST (getUtilData token)
convert_one_op token@(Token _ (LevelOneOpToken "&&")) = AndConstAST (getUtilData token)
convert_one_op token@(Token _ (LevelOneOpToken "||")) = OrConstAST (getUtilData token)
convert_one_op _    = error "undefined operator."

convert_two_op :: Token -> ConstAST
convert_two_op token@(Token _ (LevelTwoOpToken "*"))  = TimesConstAST (getUtilData token)
convert_two_op token@(Token _ (LevelTwoOpToken "/"))  = DivideConstAST (getUtilData token)
convert_two_op token@(Token _ (LevelTwoOpToken "%"))  = ModuloConstAST (getUtilData token)
convert_two_op token@(Token _ (LevelTwoOpToken "==")) = EqualsConstAST (getUtilData token)
convert_two_op token@(Token _ (LevelTwoOpToken "b&")) = BiAndConstAST (getUtilData token)
convert_two_op token@(Token _ (LevelTwoOpToken "b^")) = BiXorConstAST (getUtilData token)
convert_two_op token@(Token _ (LevelTwoOpToken "b|")) = BiOrConstAST (getUtilData token)
convert_two_op _    = error "undefined operator."

convert_three_op :: Token -> ConstAST
convert_three_op token@(Token _ (LevelThreeOpToken "<="))  = LessOrEqualConstAST (getUtilData token)
convert_three_op token@(Token _ (LevelThreeOpToken ">="))  = GreaterOrEqualConstAST (getUtilData token)
convert_three_op token@(Token _ (LevelThreeOpToken "b<<")) = BiLShiftConstAST (getUtilData token)
convert_three_op token@(Token _ (LevelThreeOpToken "b>>")) = BiRShiftConstAST (getUtilData token)
convert_three_op _    = error "undefined operator."

convert_unary_op :: Token -> ConstAST
convert_unary_op token@(Token _ (UnaryOpToken "!"))  = NotConstAST (getUtilData token)
convert_unary_op token@(Token _ (UnaryOpToken "~"))  = UnaryMinusConstAST (getUtilData token)
convert_unary_op token@(Token _ (UnaryOpToken "b~")) = BiNotConstAST (getUtilData token)
convert_unary_op _   = error "undefined operator."

convert_io_op :: Token -> ConstAST
convert_io_op token@(Token _ (IOToken "open_read"))  = OpenReadConstAST (getUtilData token)
convert_io_op token@(Token _ (IOToken "open_write")) = OpenWriteConstAST (getUtilData token)
convert_io_op token@(Token _ (IOToken "close"))      = CloseConstAST (getUtilData token)
convert_io_op token@(Token _ (IOToken "read"))       = ReadConstAST (getUtilData token)
convert_io_op token@(Token _ (IOToken "write"))      = WriteConstAST (getUtilData token)
convert_io_op token@(Token _ (IOToken "delete"))     = DeleteConstAST (getUtilData token)
convert_io_op token@(Token _ (IOToken "show"))       = ShowConstAST (getUtilData token)
convert_io_op token@(Token _ (IOToken "to_int"))     = ToIntConstAST (getUtilData token)
convert_io_op token@(Token _ (IOToken "to_float"))   = ToFloatConstAST (getUtilData token)
convert_io_op _ = error "undefined IO operation."

handle_string_expr :: Token -> [ExprAST]
handle_string_expr (Token _ (StringToken []))         = []
handle_string_expr token@(Token userState (StringToken (c:cs))) = ((ConstExprAST (CharConstAST (toWord8 c) utilData) utilData):(handle_string_expr (Token userState (StringToken cs)))) 
    where
        utilData = getUtilData token
        
handle_string_expr _ = error "unexpected token type."

handle_string_pat :: Token -> [PatternAST]
handle_string_pat (Token _ (StringToken []))         = []
handle_string_pat token@(Token userState (StringToken (c:cs))) = ((ConstPatternAST (CharConstAST (toWord8 c) utilData) utilData):(handle_string_pat (Token userState (StringToken cs)))) 
    where
        utilData = getUtilData token

handle_string_pat _ = error "unexpected token type."