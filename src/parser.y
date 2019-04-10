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
    var       { Token _ VarToken}
    let       { Token _ LetToken }
    in        { Token _ InToken }
    match     { Token _ MatchToken }
    case      { Token _ CaseToken }
    type      { Token _ TypeToken }
    bool      { Token _ (BoolToken $$) }
    '=>'      { Token _ FollowsToken }
    int       { Token _ (IntToken $$) }
    float     { Token _ (FloatToken $$) }
    char      { Token _ (CharToken $$) }
    string    { Token _ (StringToken $$) }
    type_id   { Token _ (TypeIdToken $$) }
    var_id    { Token _ (VarIdToken $$) }
    '|'       { Token _ GuardToken }
    '='       { Token _ DeclareToken }
    '{'       { Token _ CurlyOpenToken }
    '}'       { Token _ CurlyCloseToken }
    '::'      { Token _ AnnotateToken }
    '['       { Token _ SquareOpenToken }
    ']'       { Token _ SquareCloseToken }
    '('       { Token _ ParenOpenToken }
    ')'       { Token _ ParenCloseToken }
    '->'      { Token _ EntailsToken }
    ','       { Token _ CommaToken }
    '?'       { Token _ WildcardToken }
    one_op    { Token _ (LevelOneOpToken $$) }
    two_op    { Token _ (LevelTwoOpToken $$) }
    three_op  { Token _ (LevelThreeOpToken $$) }
    unary_op  { Token _ (UnaryOpToken $$) }

%%

-- Grammar variables and associated production rules 
-- Semantic actions of a given rule is specified in { }
-- Semantic actions are used to construct an abstract syntax tree
-- $i denotes "value of term i" in the corresponding production

Prog    : Type_dec Var_dec                           { ProgAST $1 $2 }

-- Types

Type_dec    :                                             { EpsTypeDclAST }
            | type type_id '=' '{' Cons_list '}' Type_dec { TypeDclAST (TypeId $2) $5 $7 }

Cons_list   : Cons_list Cons                         { $1 ++ [$2] }
            | Cons                                   { [$1] }

Cons        : '|' type_id Comp_type                  { DoubleConsAST (TypeId $2) $3 }
            | '|' type_id                            { SingleConsAST (TypeId $2) }

Comp_type   : type_id                                { CompSimpleAST (TypeId $1) }
            | '[' Comp_type ']'                      { CompListAST $2 }
            | '(' Comp_rep ')'                       { CompTupleAST $2 }
            | '(' Comp_type '->' Comp_type ')'       { CompFuncAST $2 $4 }

Comp_rep    : Comp_rep ',' Comp_type                 { $1 ++ [$3] }
            | Comp_type                              { [$1] }

Type_spec   : '::' Comp_type                         { $2 }

-- Variable declarations

Var_dec     :                                        { EpsVarDclAST }
            | var Typed_var '=' Expr Var_dec         { VarDclAST $2 $4 $5 }

-- Control structures

Match       : match Expr '{' Match_body '}'          { MatchExprAST $2 $4 }

Match_body  : Match_body '|' Pattern '->' Expr       { $1 ++ [($3, $5)] }
            | '|' Pattern '->' Expr                  { [($2, $4)] }

Let_in      : let Vars '=' Expr in '(' Expr ')'      { let_in $2 $4 $7 }

Case        : case '{' Case_body '}'                 { CaseExprAST $3 }

Case_body   : Case_body '|' Pred '->' Expr           { $1 ++ [($3, $5)] }
            | '|' Pred '->' Expr                     { [($2, $4)] }

Pred        : Expr                                   { PredExprAST $1 }
            | '?'                                    { PredWildAST }

-- Utilities

Vars        : Typed_var                              { [$1] }
            | '(' Vars_body ')'                      { $2 }

Vars_body   : Vars_body ',' Typed_var                { $1 ++ [$3] }
            | Typed_var                              { [$1] }

Typed_var   : var_id                                 { UntypedVarAST (VarId $1) }
            | var_id Type_spec                       { TypedVarAST (VarId $1) $2 }

Literal     : string                                 { StringConstAST $1 }
            | char                                   { CharConstAST $1 }
            | int                                    { IntConstAST $1 }
            | float                                  { FloatConstAST $1 }
            | bool                                   { BoolConstAST $1 }

-- Patterns

Pattern     : Struc_pat                              { $1 }
            | Literal                                { ConstPatternAST $1 }
            | var_id                                 { VarPatternAST (VarId $1) }
            | '?'                                    { WildPatternAST }
            | type_id Pattern                        { TypeConsPatternAST (TypeId $1) $2 }
            | type_id                                { TypePatternAST (TypeId $1) }

Struc_pat   : '(' Pat_body ')'                       { TuplePatternAST $2 }
            | '['']'                                 { ListPatternAST [] }
            | '[' Pat_body ']'                       { ListPatternAST $2 }
            | '(' Pattern three_op var_id ')'        { decomp_pat $2 $3 (VarId $4) }

Pat_body    : Pat_body ',' Pattern                   { $1 ++ [$3] }
            | Pattern                                { [$1] }

-- Expr

Expr        : Expr one_op Two_infix                  { FunAppExprAST (FunAppExprAST (ConstExprAST (convert_one_op $2)) $1) $3 }
            | Two_infix                              { $1 }

Two_infix   : Two_infix two_op Three_infix           { FunAppExprAST (FunAppExprAST (ConstExprAST (convert_two_op $2)) $1) $3 }
            | Three_infix                            { $1 }

Three_infix : Three_infix three_op Unary_infix       { FunAppExprAST (FunAppExprAST (ConstExprAST (convert_three_op $2)) $1) $3 }
            | Unary_infix                            { $1 }

Unary_infix : unary_op Left_expr                     { FunAppExprAST (ConstExprAST (convert_unary_op $1)) $2 }
            | Left_expr                              { $1 }

Left_expr   : Match                                  { $1 }
            | Let_in                                 { $1 }
            | Case                                   { $1 }
            | Func_expr                              { func_left_expr $1 }

Func_expr   : Func_expr Lit_expr                     { $1 ++ [$2] }
            | Lit_expr                               { [$1] }

Lit_expr    : Lambda                                 { $1 }
            | Literal                                { ConstExprAST $1 }
            | type_id                                { TypeExprAST (TypeId $1) }
            | var_id                                 { VarExprAST (VarId $1) }
            | '(' Tuple_body ')'                     { TupleExprAST $2 }
            | '[' List_body ']'                      { ListExprAST $2 }

Lambda      : Typed_var '=>' '{' Expr '}'            { LambdaExprAST $1 $4 }

Tuple_body  : Tuple_body ',' Expr                    { $1 ++ [$3] }
            | Expr                                   { [$1] }

List_body   :                                        { [] }
            | Tuple_body                             { $1 }

-- end grammar

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

-- parseError is called when no production rule can handle the current token
-- calls the lexer handleError function with a parser error message
parseError :: (Token, [String]) -> Alex a
parseError ((Token pos token), expected) = do
        line <- getCurrentLine
        (_, offset) <- getLineNumber
        handleError pos (getErrorMessage pos line token expected offset)

-- getErrorMessage returns an error message showcasing the position of the current token,
-- as well as all token types that would have been legal
getErrorMessage :: AlexPosn -> String -> Terminal -> [String] -> Int -> String
getErrorMessage (AlexPn _ _ column) line current expected offset = 
        "unexpected token '" ++ 
        terminalString current ++ 
        "' at:\n  " ++
        line ++
        "\n  " ++
        getErrorIndicator (column - offset) (length (terminalString current)) ++
        "\n  " ++
        handleExpected expected

-- handleExpected recursively constructs a string of comma separated token names in input list
handleExpected :: [String] -> String
handleExpected expected = "expected: " ++
        case map convertWord expected of
            []       -> "\n"
            [s]      -> s ++ "\n"
            [s2, s1] -> s1 ++ " or " ++ s2 ++ "\n"
            (s:ss)   -> (reverse ss >>= (++ ", ")) ++ s ++ "\n" 

-- convertWord formats a selected few possible token names to a more pleasant format
convertWord :: String -> String
convertWord "unary_op" = "a unary operator"
convertWord "var_id" = "a variable name"
convertWord "type_id" = "a type name"
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
parseBonsai :: FilePath -> String -> Either String ProgAST
parseBonsai = runAlex' parse

}
