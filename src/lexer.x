{
    {-# OPTIONS -w #-}
    module Lexer 
    ( Token(..)
    , AlexPosn(..)
    , Terminal(..)
    , printTerminal
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
    otherwise                       { \s -> OtherwiseToken}
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
        OtherwiseToken           |
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

printTerminal :: Terminal -> String
printTerminal VarToken                   = "var"
printTerminal LetToken                   = "let"
printTerminal InToken                    = "in"
printTerminal MatchToken                 = "match"
printTerminal CaseToken                  = "case"
printTerminal TypeToken                  = "type"
printTerminal OtherwiseToken             = "otherwise"
printTerminal TrueToken                  = "true"
printTerminal FalseToken                 = "false"
printTerminal FollowsToken               = "=>"
printTerminal (IntToken int)             = show int
printTerminal (FloatToken float)         = show float
printTerminal (CharToken char)           = show char
printTerminal (StringToken string)       = show string
printTerminal (TypeIdToken string)       = show string
printTerminal (VarIdToken string)        = show string
printTerminal GuardToken                 = "|"
printTerminal DeclareToken               = "="
printTerminal CurlyOpenToken             = "{"
printTerminal CurlyCloseToken            = "}"
printTerminal AnnotateToken              = "::"
printTerminal SquareOpenToken            = "["
printTerminal SquareCloseToken           = "]"
printTerminal ParenOpenToken             = "("
printTerminal ParenCloseToken            = ")"
printTerminal EntailsToken               = "->"
printTerminal CommaToken                 = ","
printTerminal (LevelOneOpToken string)   = show string
printTerminal (LevelTwoOpToken string)   = show string
printTerminal (LevelThreeOpToken string) = show string
printTerminal (UnaryOpToken string)      = show string
printTerminal EOFToken                   = "$"

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