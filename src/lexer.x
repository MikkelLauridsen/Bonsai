{

{-# OPTIONS -w  #-}
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

      $white+                           ;
    \#.*                            ;
    var                             { lex' VarToken}
    let                             { lex' LetToken}
    in                              { lex' InToken}
    match                           { lex' MatchToken}
    case                            { lex' CaseToken}
    type                            { lex' TypeToken}
    true                            { lex' TrueToken}
    false                           { lex' FalseToken}
    \=\>                            { lex' FollowsToken}
    $digit+                         { lex (IntToken . read)}
    $digit+\.$digit+                { lex (FloatToken . read)}
    \'.\'                           { lex (CharToken . read) }
    \"$stringb*\"                   { lex StringToken }
    $upper [$alpha $digit \_ \']*   { lex TypeIdToken }
    $lower [$alpha $digit \_ \']*   { lex VarIdToken }
    \|                              { lex' GuardToken}
    \=                              { lex' DeclareToken}
    \{                              { lex' CurlyOpenToken}
    \}                              { lex' CurlyCloseToken}
    \:\:                            { lex' AnnotateToken}
    \[                              { lex' SquareOpenToken}
    \]                              { lex' SquareCloseToken}
    \(                              { lex' ParenOpenToken}
    \)                              { lex' ParenCloseToken}
    \-\>                            { lex' EntailsToken}
    \,                              { lex' CommaToken}
    \?                              { lex' WildcardToken}
    \+                              { lex LevelOneOpToken} 
    \-                              { lex LevelOneOpToken}
    \+\+                            { lex LevelOneOpToken}
    \&\&                            { lex LevelOneOpToken}
    \|\|                            { lex LevelOneOpToken}
    \*                              { lex LevelTwoOpToken}
    \/                              { lex LevelTwoOpToken}
    \%                              { lex LevelTwoOpToken}
    \=\=                            { lex LevelTwoOpToken}
    \!\=                            { lex LevelTwoOpToken}
    \<                              { lex LevelThreeOpToken} 
    \>                              { lex LevelThreeOpToken}
    \<\=                            { lex LevelThreeOpToken}
    \>\=                            { lex LevelThreeOpToken}
    \:                              { lex LevelThreeOpToken}
    \!                              { lex UnaryOpToken}
    \~                              { lex UnaryOpToken}


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

data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

data Token = Token AlexPosn Terminal deriving (Show)

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
  return $ Token p EOFToken

lex :: (String -> Terminal) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

lex' :: Terminal -> AlexAction Token
lex' = lex . const

alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
    path <- getFilePath
    alexError (path ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a path input = runAlex input (setFilePath path >> a)

}