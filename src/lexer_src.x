{
    {-# OPTIONS -w #-}
    module Lexer 
    ( Token(..)
    , AlexPosn(..)
    , Terminal(..)
    , terminalString
    , Alex(..)
    , runAlex'
    , alexMonadScan'
    , alexError'
    ) where

    import Prelude hiding (lex)
    import Control.Monad (liftM)
}

%wrapper "monadUserState"

$digit   = 0-9
$lower   = [a-z]
$upper   = [A-Z]
$alpha   = [a-zA-Z]
$stringb = $printable # \"

tokens :-

    $white+                         ;
    \#.*                            ;
    var                             { \s -> VarToken}
    let                             { \s -> LetToken}
    in                              { \s -> InToken}
    match                           { \s -> MatchToken}
    case                            { \s -> CaseToken}
    type                            { \s -> TypeToken}
    true                            { \s -> TrueToken}
    false                           { \s -> FalseToken}
    \=\>                            { \s -> FollowsToken}
    $digit+                         { \s -> IntToken (read s)}
    $digit+\.$digit+                { \s -> FloatToken (read s)}
    \'.\'                           { \s -> CharToken (head (tail s)) }
    \"$stringb*\"                   { \s -> StringToken (tail (init s))}
    $upper [$alpha $digit \_ \']*   { \s -> TypeIdToken s}
    $lower [$alpha $digit \_ \']*   { \s -> VarIdToken s}
    \|                              { \s -> GuardToken}
    \=                              { \s -> DeclareToken}
    \{                              { \s -> CurlyOpenToken}
    \}                              { \s -> CurlyCloseToken}
    \:\:                            { \s -> AnnotateToken}
    \[                              { \s -> SquareOpenToken}
    \]                              { \s -> SquareCloseToken}
    \(                              { \s -> ParenOpenToken}
    \)                              { \s -> ParenCloseToken}
    \-\>                            { \s -> EntailsToken}
    \,                              { \s -> CommaToken}
    \?                              { \s -> WildcardToken}
    \+                              { \s -> LevelOneOpToken s} 
    \-                              { \s -> LevelOneOpToken s}
    \+\+                            { \s -> LevelOneOpToken s}
    \&\&                            { \s -> LevelOneOpToken s}
    \|\|                            { \s -> LevelOneOpToken s}
    \*                              { \s -> LevelTwoOpToken s}
    \/                              { \s -> LevelTwoOpToken s}
    \%                              { \s -> LevelTwoOpToken s}
    \=\=                            { \s -> LevelTwoOpToken s}
    \!\=                            { \s -> LevelTwoOpToken s}
    \<                              { \s -> LevelThreeOpToken s} 
    \>                              { \s -> LevelThreeOpToken s}
    \<\=                            { \s -> LevelThreeOpToken s}
    \>\=                            { \s -> LevelThreeOpToken s}
    \:                              { \s -> LevelThreeOpToken s}
    \!                              { \s -> UnaryOpToken s}
    \~                              { \s -> UnaryOpToken s}


{
data Terminal = 
        VarToken                 |
        LetToken                 |
        InToken                  |
        MatchToken               |
        CaseToken                |
        TypeToken                |
        TrueToken                |
        FalseToken               |
        FollowsToken             |
        IntToken Int             |
        FloatToken Float         |
        CharToken Char           |
        StringToken String       |
        TypeIdToken String       |
        VarIdToken String        |
        GuardToken               |
        DeclareToken             |
        CurlyOpenToken           |
        CurlyCloseToken          |
        AnnotateToken            |
        SquareOpenToken          |
        SquareCloseToken         |
        ParenOpenToken           |
        ParenCloseToken          |
        EntailsToken             |
        CommaToken               |
        WildcardToken            |
        LevelOneOpToken String   |
        LevelTwoOpToken String   |
        LevelThreeOpToken String |
        UnaryOpToken String      |
        EOFToken
        deriving (Eq, Show)

data AlexUserState = AlexUserState { path :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM path alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

terminalString :: Terminal -> String
terminalString VarToken                   = "var"
terminalString LetToken                   = "let"
terminalString InToken                    = "in"
terminalString MatchToken                 = "match"
terminalString CaseToken                  = "case"
terminalString TypeToken                  = "type"
terminalString TrueToken                  = "true"
terminalString FalseToken                 = "false"
terminalString FollowsToken               = "=>"
terminalString (IntToken int)             = show int
terminalString (FloatToken float)         = show float
terminalString (CharToken char)           = show char
terminalString (StringToken string)       = show string
terminalString (TypeIdToken string)       = show string
terminalString (VarIdToken string)        = show string
terminalString GuardToken                 = "|"
terminalString DeclareToken               = "="
terminalString CurlyOpenToken             = "{"
terminalString CurlyCloseToken            = "}"
terminalString AnnotateToken              = "::"
terminalString SquareOpenToken            = "["
terminalString SquareCloseToken           = "]"
terminalString ParenOpenToken             = "("
terminalString ParenCloseToken            = ")"
terminalString EntailsToken               = "->"
terminalString CommaToken                 = ","
terminalString WildcardToken              = "?"
terminalString (LevelOneOpToken string)   = show string
terminalString (LevelTwoOpToken string)   = show string
terminalString (LevelThreeOpToken string) = show string
terminalString (UnaryOpToken string)      = show string
terminalString EOFToken                   = "$"

alexEOF :: Alex Token
alexEOF = do
    (p,_,_,_) <- alexGetInput
    return (Token p TokenEOF)

lex :: (String -> Terminal) -> AlexAction Token
lex func = \(p,_,_,s) i -> return (Token p (f (take i s)))

lex' :: TokenClass -> AlexAction Token
lex' = lex . const

alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
    path <- getFilePath
    alexError (path ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a path input = runAlex input (setFilePath path >> a)

alexMonadScan' :: Alex Token
alexMonadScan' = do
    input <- alexGetInput
    start <- alexGetStartCode
    case alexScan input start of
        AlexEOF -> alexEOF
        AlexError (p,_,_,s) -> alexError' p ("Lexical error at character '" ++ take 1 s ++ "'")
        AlexSkip input' length -> do
            alexSetInput input'
            alexMonadScan'
        AlexToken input' length action -> do
            alexSetInput input'
            action (ignorePendingBytes input) length

}