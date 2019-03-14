{

{-# OPTIONS -w #-}
module Parser (parseBonsai) where

import Lexer

}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ EOFToken }
%error { happyError }

%token
    var       { Token _ VarToken}
    let       { Token _ LetToken }
    in        { Token _ InToken }
    match     { Token _ MatchToken }
    case      { Token _ CaseToken }
    type      { Token _ TypeToken }
    true      { Token _ TrueToken }
    false     { Token _ FalseToken }
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

-- Start grammar

Prog    : Type_decs Var_decs                         { }

-- Types

Type_decs   :                                        { }
            | Type_decs Type_dec                     { }

Type_dec    : type type_id '=' '{' Cons_list '}'     { }

Cons_list   : Cons_list Cons                         { }
            | Cons                                   { }

Cons        : '|' type_id Comp_type                  { }
            | '|' type_id                            { }

Comp_type   : type_id                                { }
            | '[' Comp_type ']'                      { }
            | '(' Comp_type Comp_rep ')'             { }
            | '(' Comp_type '->' Comp_type ')'       { }

Comp_rep    : Comp_rep ',' Comp_type                 { }
            | Comp_type                              { }

Type_spec   : '::' Comp_type                         { }

-- Variable declarations

Var_decs    :                                        { }
            | Var_decs Var_dec                       { }

Var_dec     : var Typed_var '=' Expr                 { }

-- Control structures

Match       : match Expr '{' Match_body '}'          { }

Match_body  : Match_body '|' Pattern '->' Expr       { }
            | '|' Pattern '->' Expr                  { }

Let_in      : let Vars '=' Expr in '(' Expr ')'      { }

Case        : case '{' Case_body '}'                 { }

Case_body   : Case_body '|' Expr '->' Expr           { }
            | '|' Expr '->' Expr                     { }

-- Utilities

Vars        : Typed_var                              { }
            | '(' Vars_body ')'                      { }

Vars_body   : Vars_body ',' Typed_var                { }
            | Typed_var                              { }

Typed_var   : var_id                                 { }
            | var_id Type_spec                       { }

Literal     : string                                 { }
            | char                                   { }
            | int                                    { }
            | float                                  { }
            | true                                   { }
            | false                                  { }

-- Patterns

Pattern     : Struc_pat                              { }
            | Literal                                { }
            | var_id                                 { }
            | '?'                                    { }
            | type_id Pattern                        { }
            | type_id                                { }

Struc_pat   : '(' Pat_body ')'                       { }
            | '['']'                                 { }
            | '[' Pat_body ']'                       { }
            | '(' Pattern three_op var_id ')'        { }

Pat_body    : Pat_body ',' Pattern                   { }
            | Pattern                                { }

-- Expr

Expr        : Expr one_op Two_infix                  { }
            | Two_infix                              { }

Two_infix   : Two_infix two_op Three_infix           { }
            | Three_infix                            { }

Three_infix : Three_infix three_op Unary_infix       { }
            | Unary_infix                            { }

Unary_infix : unary_op Left_expr                     { }
            | Left_expr                              { }

Left_expr   : Match                                  { }
            | Let_in                                 { }
            | Case                                   { }
            | Func_expr                              { }

Func_expr   : Func_expr Lit_expr                     { }
            | Lit_expr                               { }

Lit_expr    : Lambda                                 { }
            | Literal                                { }
            | type_id                                { }
            | var_id                                 { }
            | '(' Tuple_body ')'                     { }
            | '[' List_body ']'                      { }

Lambda      : Typed_var '=>' '{' Expr '}'            { }

Tuple_body  : Tuple_body ',' Expr                    { }
            | Expr                                   { }

List_body   :                                        { }
            | Tuple_body                             { }

--end grammar

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexError' p ("Parse error at token '" ++ terminalString t ++ "'")

parseBonsai :: FilePath -> String -> Either String ()
parseBonsai = runAlex' parse

}
