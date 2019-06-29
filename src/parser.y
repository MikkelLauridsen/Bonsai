{

module Parser (parseBonsai) where

import Lexer
import Actions
import Ast

}

%name parse  -- name of parse function
%tokentype { Token } -- return type of specified lexer (Token AlexPosn Terminal)
%monad { Alex } -- monad type of monadic lexer
%lexer { lexwrap } { Token _ EOFToken } -- lexer function and EOF token
%errorhandlertype explist -- use error handler with [String] of expected Tokens on failure
%error { parseError } -- error handler function

%token -- terminals of Bonsai grammar. { CONTENT } CONTENT denotes the value of a given terminal 
    var       { Token _ VarToken }
    let       { Token _ LetToken }
    in        { Token _ InToken }
    match     { Token _ MatchToken }
    case      { Token _ CaseToken }
    type      { Token _ TypeToken }
    bool      { Token _ (BoolToken _) }
    '=>'      { Token _ FollowsToken }
    int       { Token _ (IntToken _) }
    float     { Token _ (FloatToken _) }
    char      { Token _ (CharToken _) }
    string    { Token _ (StringToken _) }
    type_id   { Token _ (TypeIdToken _) }
    var_id    { Token _ (VarIdToken _) }
    '|'       { Token _ GuardToken }
    '.'       { Token _ EscapeToken }
    '<'       { Token _ AngleOpenToken }
    '>'       { Token _ AngleCloseToken }
    '='       { Token _ DeclareToken }
    '{'       { Token _ CurlyOpenToken }
    '}'       { Token _ CurlyCloseToken }
    '::'      { Token _ AnnotateToken }
    ':'       { Token _ ConsToken }
    '['       { Token _ SquareOpenToken }
    ']'       { Token _ SquareCloseToken }
    '('       { Token _ ParenOpenToken }
    ')'       { Token _ ParenCloseToken }
    '->'      { Token _ EntailsToken }
    ','       { Token _ CommaToken }
    '?'       { Token _ WildcardToken }
    '<<'      { Token _ ClassOpenToken }
    '>>'      { Token _ ClassCloseToken }
    one_op    { Token _ (LevelOneOpToken _) }
    two_op    { Token _ (LevelTwoOpToken _) }
    three_op  { Token _ (LevelThreeOpToken _) }
    unary_op  { Token _ (UnaryOpToken _) }
    io_op     { Token _ (IOToken _) }

%%

-- Grammar variables and associated production rules 
-- Semantic actions of a given rule is specified in { }
-- Semantic actions are used to construct an abstract syntax tree
-- $i denotes "value of term i" in the corresponding production

Single      : type type_id '=' '{' Cons_list '}'                            { TypeSingle (getTypeId $2) $5 (getUtilData $1)  }
            | type type_id '<' Poly_body '>' '=' '{' Cons_list '}'          { TypePolySingle (getTypeId $2) $4 $8 (getUtilData $1) }
            | var Typed_var '=' Expr                                        { VarSingle $2 $4 (getUtilData $1) }
            | Expr                                                          { ExprSingle $1 }
                    
-- Types                    
                    
Type_dec    :                                                               { EpsTypeDclAST }
            | type type_id '=' '{' Cons_list '}' Type_dec                   { TypeDclAST (getTypeId $2) $5 $7 (getUtilData $1) }
            | type type_id '<' Poly_body '>' '=' '{' Cons_list '}' Type_dec { TypePolyDclAST (getTypeId $2) $4 $8 $10 (getUtilData $1) }
                    
Cons_list   : Cons_list Cons                                                { $1 ++ [$2] }
            | Cons                                                          { [$1] }
                    
Cons        : '|' type_id Comp_type                                         { DoubleConsAST (getTypeId $2) $3 (getUtilData $1) }
            | '|' type_id                                                   { SingleConsAST (getTypeId $2) (getUtilData $1) }
                    
Poly_body   : Poly_body ',' var_id                                          { $1 ++ [getVarId $3] }
            | var_id                                                        { [getVarId $1] }
                    
Comp_type   : type_id                                                       { CompSimpleAST (getTypeId $1) (getUtilData $1) }
            | var_id                                                        { CompSimplePolyAST (getVarId $1) (getUtilData $1) }
            | var_id '<<' Class_rep '>>'                                    { CompClssAST (getVarId $1) $3 (getUtilData $1)}
            | type_id '<' Comp_rep '>'                                      { CompPolyAST (getTypeId $1) $3 (getUtilData $1) }
            | '[' Comp_type ']'                                             { CompListAST $2 (getUtilData $1) }
            | '(' Comp_rep ')'                                              { handleCompParen $2 (getUtilData $1) }
            | '(' Comp_type '->' Comp_type ')'                              { CompFuncAST $2 $4 (getUtilData $1) }

Class_rep   : Class_rep ',' type_id                                         { $1 ++ [(getTypeId $3)] }
            | type_id                                                       { [(getTypeId $1)] }

Comp_rep    : Comp_rep ',' Comp_type                                        { $1 ++ [$3] }
            | Comp_type                                                     { [$1] }
                    
Type_spec   : '::' Comp_type                                                { $2 }
                    
-- Variable declarations                    
                    
Var_dec     :                                                               { EpsVarDclAST }
            | var Typed_var '=' Expr Var_dec                                { VarDclAST $2 $4 $5 (getUtilData $1) }
                    
-- Control structures                       
                    
Match       : match Expr '{' Match_body '}'                                 { MatchExprAST $2 $4 (getUtilData $1) }
                    
Match_body  : Match_body '|' Pattern '->' Expr                              { $1 ++ [($3, $5)] }
            | '|' Pattern '->' Expr                                         { [($2, $4)] }
                    
Let_in      : let Typed_var '=' Expr in '(' Expr ')'                        { LetInExprAST $2 $4 $7 (getUtilData $1) }
            | let Vars '=' Expr in '(' Expr ')'                             { let_in $2 $4 $7 (getUtilData $1) }
                    
Case        : case '{' Case_body '}'                                        { CaseExprAST $3 (getUtilData $1) }
                    
Case_body   : Case_body '|' Pred '->' Expr                                  { $1 ++ [($3, $5)] }
            | '|' Pred '->' Expr                                            { [($2, $4)] }
                    
Pred        : Expr                                                          { PredExprAST $1 (getUtilDataExpr $1) }
            | '?'                                                           { PredWildAST (getUtilData $1) }
                    
-- Utilities                        
                    
Vars        : '(' Vars_body ')'                                             { $2 }
                    
Vars_body   : Vars_body ',' Vars_joint                                      { $1 ++ [$3] }
            | Vars_joint ',' Vars_joint                                     { [$1, $3] }
                    
Vars_joint  : var_id                                                        { VarPatternAST (getVarId $1) (getUtilData $1) }
            | '?'                                                           { WildPatternAST (getUtilData $1) }
                    
Typed_var   : var_id                                                        { UntypedVarAST (getVarId $1) (getUtilData $1) }
            | var_id Type_spec                                              { TypedVarAST (getVarId $1) $2 (getUtilData $1) }
                    
Literal     : char                                                          { CharConstAST (getCharVal $1) (getUtilData $1) }
            | int                                                           { IntConstAST (getIntVal $1) (getUtilData $1) }
            | float                                                         { FloatConstAST (getFloatVal $1) (getUtilData $1) }
            | bool                                                          { BoolConstAST (getBoolVal $1) (getUtilData $1) }
                    
Three_op    : three_op                                                      { convert_three_op $1 }
            | '<'                                                           { LessConstAST (getUtilData $1) }
            | '>'                                                           { GreaterConstAST (getUtilData $1) }
                    
ConstFun    : one_op                                                        { convert_one_op $1 }
            | two_op                                                        { convert_two_op $1 }
            | Three_op                                                      { $1 }
            | unary_op                                                      { convert_unary_op $1 }
                    
-- Patterns                         
                    
Pattern     : Struc_pat                                                     { $1 }
            | Literal                                                       { ConstPatternAST $1 (getUtilDataConst $1) }
            | string                                                        { ListPatternAST (handle_string_pat $1) (getUtilData $1) }
            | var_id                                                        { VarPatternAST (getVarId $1) (getUtilData $1) }
            | '?'                                                           { WildPatternAST (getUtilData $1) }
            | type_id Pattern                                               { TypeConsPatternAST (getTypeId $1) $2 (getUtilData $1) }
            | type_id                                                       { TypePatternAST (getTypeId $1) (getUtilData $1) }
                    
Struc_pat   : '(' Pat_body ')'                                              { TuplePatternAST $2 (getUtilData $1) }
            | '['']'                                                        { ListPatternAST [] (getUtilData $1) }
            | '[' Pat_body ']'                                              { ListPatternAST $2 (getUtilData $1) }
            | '(' Pattern ':' var_id ')'                                    { DecompPatternAST $2 (getVarId $4) (getUtilData $1) }
                    
Pat_body    : Pat_body ',' Pattern                                          { $1 ++ [$3] }
            | Pattern                                                       { [$1] }
                    
-- Expr                     
                    
Expr        : Expr one_op Two_infix                                         { FunAppExprAST (FunAppExprAST (ConstExprAST (convert_one_op $2) (getUtilData $2)) $1 (getUtilData $2)) $3 (getUtilData $2) }
            | Two_infix                                                     { $1 }
                    
Two_infix   : Two_infix two_op Three_infix                                  { FunAppExprAST (FunAppExprAST (ConstExprAST (convert_two_op $2) (getUtilData $2)) $1 (getUtilData $2)) $3 (getUtilData $2) }
            | Three_infix                                                   { $1 }
                    
Three_infix : Three_infix Three_op Unary_infix                              { FunAppExprAST (FunAppExprAST (ConstExprAST $2 (getUtilDataConst $2)) $1 (getUtilDataConst $2)) $3 (getUtilDataConst $2) }
            | Unary_infix                                                   { $1 }
                    
Unary_infix : unary_op Left_expr                                            { FunAppExprAST (ConstExprAST (convert_unary_op $1) (getUtilData $1)) $2 (getUtilData $1) }
            | Left_expr                                                     { $1 }
                    
Left_expr   : Match                                                         { $1 }
            | Let_in                                                        { $1 }
            | Case                                                          { $1 }
            | Func_expr                                                     { $1 }
                    
Func_expr   : Func_expr Lit_expr                                            { FunAppExprAST $1 $2 (getUtilDataExpr $1) }
            | Lit_expr                                                      { $1 }
                    
Lit_expr    : Lambda                                                        { $1 }
            | Literal                                                       { ConstExprAST $1 (getUtilDataConst $1) }
            | string                                                        { ListExprAST (handle_string_expr $1) (getUtilData $1) }
            | type_id                                                       { TypeExprAST (getTypeId $1) (getUtilData $1) }
            | var_id                                                        { VarExprAST (getVarId $1) (getUtilData $1) }
            | '.' ConstFun                                                  { ConstExprAST $2 (getUtilData $1) }
            | io_op                                                         { ConstExprAST (convert_io_op $1) (getUtilData $1) }
            | '(' Expr ':' Expr ')'                                         { FunAppExprAST (FunAppExprAST (ConstExprAST (AppenConstAST (getUtilData $3)) (getUtilData $3)) $2 (getUtilData $1)) $4 (getUtilData $1) }
            | '(' Tuple_body ')'                                            { handle_paren $2 (getUtilData $1) }
            | '[' List_body ']'                                             { ListExprAST $2 (getUtilData $1) }
                    
Lambda      : Typed_var '=>' '{' Expr '}'                                   { LambdaExprAST $1 $4 (getUtilData $2) }
                    
Tuple_body  : Tuple_body ',' Expr                                           { $1 ++ [$3] }
            | Expr                                                          { [$1] }
                    
List_body   :                                                               { [] }
            | Tuple_body                                                    { $1 }
       
-- end grammar

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (scanBonsai >>=)

-- parseError is called when no production rule can handle the current token
-- calls the lexer handleError function with a parser error message
parseError :: (Token, [String]) -> Alex a
parseError ((Token (AlexUserState _ _ _ _ _ _ pos) token), expected) = do
        line <- getCurrentLine
        (_, offset) <- getLineNumber
        handleError pos (getErrorMessage pos line token expected offset)

-- getErrorMessage returns an error message showcasing the position of the current token,
-- as well as all token types that would have been legal
getErrorMessage :: AlexPosn -> String -> Terminal -> [String] -> Int -> String
getErrorMessage (AlexPn _ _ column) line current expected offset = 
        "unexpected token ``" ++ 
        terminalString current ++ 
        "`` at:```Haskell\n" ++
        line ++
        "\n" ++
        getErrorIndicator (column - offset) (length (terminalString current)) ++
        "```" ++
        handleExpected expected

-- handleExpected recursively constructs a string of comma separated token names in input list
handleExpected :: [String] -> String
handleExpected expected = "expected: ``" ++
        (case map convertWord expected of
            []       -> "\n"
            [s]      -> s ++ "\n"
            (s:ss)   -> (reverse ss >>= (++ ", ")) ++ s) ++ "``" 

-- convertWord formats a selected few possible token names to a more pleasant format
convertWord :: String -> String
convertWord "unary_op" = "a unary operator"
convertWord "var_id" = "a variable name"
convertWord "type_id" = "a type name"
convertWord "io_op" = "an I/O operation"
convertWord "string" = "a string"
convertWord "char" = "a char"
convertWord "float" = "a float"
convertWord "int" = "an int"
convertWord "bool" = "a boolean"
convertWord "case" = "a case expression"
convertWord "match" = "a match expression"
convertWord "let" = "a let expression"
convertWord string = string

-- getErrorIndicator returns a string of the format ' ' * num + '^' * length
-- it is used to indicate the position and length of a token in a given string
getErrorIndicator :: Int -> Int -> String
getErrorIndicator num length = take num (repeat ' ') ++ take length (repeat '^')

-- parseBonsai is the finished monadic lexer/parser driven by Alex
-- it takes a filepath which is used to specify which file is responsible for given errors
-- also takes a string which will be scanned and parsed
parseBonsai :: FilePath -> String -> Either String SingleAST
parseBonsai = setFileAndRun parse

}
