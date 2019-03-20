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
, handleError
) where

import Prelude hiding (lex)
import Control.Monad (liftM)

}

%wrapper "monadUserState"

$digit   = 0-9
$lower   = [a-z]
$upper   = [A-Z]
$alpha   = [a-zA-Z]
$regchar = [\x20-\x7E] # \\
$spechar = [t b n r f \' \" \\]
$stringb = $printable # \"

tokens :-

    $white+                                                         ;
    \#.*                                                            ;
    var                                                             { lex' VarToken}
    let                                                             { lex' LetToken}
    in                                                              { lex' InToken}
    match                                                           { lex' MatchToken}
    case                                                            { lex' CaseToken}
    type                                                            { lex' TypeToken}
    true                                                            { lex' (BoolToken True)}
    false                                                           { lex' (BoolToken False)}
    \=\>                                                            { lex' FollowsToken}
    $digit+                                                         { lex (IntToken . read)}
    $digit+\.$digit+                                                { lex (FloatToken . read)}
    \'($regchar | \\ $spechar)\'                                    { lex (CharToken . read)}
    \"$stringb*\"                                                   { lex (StringToken . tail . init)}
    ($upper | $upper [$alpha $digit \_]* [$alpha $digit]) \'? \*?   { lex TypeIdToken}
    ($lower | $lower [$alpha $digit \_]* [$alpha $digit]) \'?       { lex VarIdToken}
    \|                                                              { lex' GuardToken}
    \=                                                              { lex' DeclareToken}
    \{                                                              { lex' CurlyOpenToken}
    \}                                                              { lex' CurlyCloseToken}
    \:\:                                                            { lex' AnnotateToken}
    \[                                                              { lex' SquareOpenToken}
    \]                                                              { lex' SquareCloseToken}
    \(                                                              { lex' ParenOpenToken}
    \)                                                              { lex' ParenCloseToken}
    \-\>                                                            { lex' EntailsToken}
    \,                                                              { lex' CommaToken}
    \?                                                              { lex' WildcardToken}
    \+                                                              { lex LevelOneOpToken} 
    \-                                                              { lex LevelOneOpToken}
    \+\+                                                            { lex LevelOneOpToken}
    \&\&                                                            { lex LevelOneOpToken}
    \|\|                                                            { lex LevelOneOpToken}
    \*                                                              { lex LevelTwoOpToken}
    \/                                                              { lex LevelTwoOpToken}
    \%                                                              { lex LevelTwoOpToken}
    \=\=                                                            { lex LevelTwoOpToken}
    \<                                                              { lex LevelThreeOpToken} 
    \>                                                              { lex LevelThreeOpToken}
    \<\=                                                            { lex LevelThreeOpToken}
    \>\=                                                            { lex LevelThreeOpToken}
    \:                                                              { lex LevelThreeOpToken}
    \!                                                              { lex UnaryOpToken}
    \~                                                              { lex UnaryOpToken}


{

data Terminal = 
    VarToken                 |
    LetToken                 |
    InToken                  |
    MatchToken               |
    CaseToken                |
    TypeToken                |
    BoolToken Bool           |
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
terminalString (BoolToken bool)           = show bool
terminalString FollowsToken               = "=>"
terminalString (IntToken int)             = show int
terminalString (FloatToken float)         = show float
terminalString (CharToken char)           = show char
terminalString (StringToken string)       = string
terminalString (TypeIdToken string)       = string
terminalString (VarIdToken string)        = string
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
terminalString (LevelOneOpToken string)   = string
terminalString (LevelTwoOpToken string)   = string
terminalString (LevelThreeOpToken string) = string
terminalString (UnaryOpToken string)      = string
terminalString EOFToken                   = "$"

alexEOF :: Alex Token
alexEOF = do
  (pos, _, _, _) <- alexGetInput
  return (Token pos EOFToken)

lex :: (String -> Terminal) -> AlexAction Token
lex func = \(pos, _, _, string) count -> return (Token pos (func (take count string)))

lex' :: Terminal -> AlexAction Token
lex' = lex . const

alexMonadScan' :: Alex Token
alexMonadScan' = do
    input <- alexGetInput
    sCode <- alexGetStartCode
    case alexScan input sCode of
      AlexEOF -> alexEOF
      AlexError (pos, _, _, string) ->
          handleError pos (getErrorMessage string)
      AlexSkip  input' length -> do
          alexSetInput input'
          alexMonadScan'
      AlexToken input' length action -> do
          alexSetInput input'
          action (ignorePendingBytes input) length

getErrorMessage :: String -> String
getErrorMessage string = "unexpected character '" ++ 
                         [head string] ++ 
                         "'\n   " ++
                         getPositionString string ++
                         "\n   ^"


getPositionString :: String -> String
getPositionString string = takeWhile (not . (flip elem) "\r\n") string

handleError :: AlexPosn -> String -> Alex a
handleError (AlexPn _ line column) err = do 
    path <- getFilePath
    alexError (path ++ ":" ++ show line ++ ":" ++ show column ++ ": error: " ++ err)

runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a path input = runAlex input (setFilePath path >> a)

}