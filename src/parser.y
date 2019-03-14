{
    {-# OPTIONS -w #-}
    module Parser 
    ( parseBonsai
    ) where

    import Lexer
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
    var       { Token _ VarToken}
    let       { Token _ LetToken }
    in        { Token _ InToken }
    match     { Token _ MatchToken }
    case      { Token _ CaseToken }
    type      { Token _ TypeToken }
    otherwise { Token _ OtherwiseToken }
    true      { Token _ TrueToken }
    false     { Token _ FalseToken }
    '=''>'    { Token _ FollowsToken }
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
    ':'':'    { Token _ AnnotateToken }
    '['       { Token _ SquareOpenToken }
    ']'       { Token _ SquareCloseToken }
    '('       { Token _ ParenOpenToken }
    ')'       { Token _ ParenCloseToken }
    '-''>'    { Token _ EntailsToken }
    ','       { Token _ CommaToken}
    one_op    { Token _ (LevelOneOpToken $$) }
    two_op    { Token _ (LevelTwoOpToken $$) }
    three_op  { Token _ (LevelThreeOpToken $$) }
    unary_op  { Token _ (UnaryOpToken $$) }

%%

-- Start variable

Prog    : Type_decs Var_decs {}

-- Types

Type_decs   : | Type_decs Type_dec {}

Type_dec    : type type_id '=' '{' Cons_list '}'

Cons_list   : Cons_list Cons
            | Cons

Cons        : '|' type_id Comp_type
            | '|' type_id

Comp_type   : type_id
            | '[' Comp_type ']'
            | '(' Comp_type Comp_type_rep ')'
            | '(' Comp_type '-''>' Comp_type ')'

Type_spec   : ':'':' Comp_type

-- Variable declarations

Var_decs    : | Var_decs Var_dec {}

Var_dec     : var Typed_var '=' Expr

-- Control structures

Match       : match Expr

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexError' p ("Parse error at token '" ++ printTerminal t ++ "'")

parseBonsai :: FilePath -> String -> Either String Prog
parseBonsai = runAlex' parse
}
