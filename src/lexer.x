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

<0>            $white+                                                         ;
<0>            \#.*                                                            ;
<0>            var                                                             { lex' VarToken}
<0>            let                                                             { lex' LetToken}
<0>            in                                                              { lex' InToken}
<0>            match                                                           { lex' MatchToken}
<0>            case                                                            { lex' CaseToken}
<0>            type                                                            { lex' TypeToken}
<0>            true                                                            { lex' (BoolToken True)}
<0>            false                                                           { lex' (BoolToken False)}
<0>            \=\>                                                            { lex' FollowsToken}
<0>            $digit+                                                         { lex (IntToken . read)}
<0>            $digit+\.$digit+                                                { lex (FloatToken . read)}
<0>            ($upper | $upper [$alpha $digit \_]* [$alpha $digit]) \'? \*?   { lex TypeIdToken}
<0>            ($lower | $lower [$alpha $digit \_]* [$alpha $digit]) \'?       { lex VarIdToken}
<0>            \|                                                              { lex' GuardToken}
<0>            \=                                                              { lex' DeclareToken}
<0>            \{                                                              { lex' CurlyOpenToken}
<0>            \}                                                              { lex' CurlyCloseToken}
<0>            \:\:                                                            { lex' AnnotateToken}
<0>            \[                                                              { lex' SquareOpenToken}
<0>            \]                                                              { lex' SquareCloseToken}
<0>            \(                                                              { lex' ParenOpenToken}
<0>            \)                                                              { lex' ParenCloseToken}
<0>            \-\>                                                            { lex' EntailsToken}
<0>            \,                                                              { lex' CommaToken}
<0>            \?                                                              { lex' WildcardToken}
<0>            \+                                                              { lex LevelOneOpToken} 
<0>            \-                                                              { lex LevelOneOpToken}
<0>            \+\+                                                            { lex LevelOneOpToken}
<0>            \&\&                                                            { lex LevelOneOpToken}
<0>            \|\|                                                            { lex LevelOneOpToken}
<0>            \*                                                              { lex LevelTwoOpToken}
<0>            \/                                                              { lex LevelTwoOpToken}
<0>            \%                                                              { lex LevelTwoOpToken}
<0>            \=\=                                                            { lex LevelTwoOpToken}
<0>            \<                                                              { lex LevelThreeOpToken} 
<0>            \>                                                              { lex LevelThreeOpToken}
<0>            \<\=                                                            { lex LevelThreeOpToken}
<0>            \>\=                                                            { lex LevelThreeOpToken}
<0>            \:                                                              { lex LevelThreeOpToken}
<0>            \!                                                              { lex UnaryOpToken}
<0>            \~                                                              { lex UnaryOpToken}
<0>            \"                                                              { enterString `andBegin` state_string}
<0>            \'                                                              { enterChar `andBegin` state_char}
<state_string> \"                                                              { finishString `andBegin` state_default}
<state_string> \\$spechar                                                      { addToString}
<state_string> $regchar                                                        { addToString}
<state_string> \n                                                              { specialError newlineError} 
<state_string> \\                                                              { specialError illegalEscapeError}
<state_char>   \'                                                              { finishChar `andBegin` state_default}
<state_char>   $regchar                                                        { addToChar}
<state_char>   \\$spechar                                                      { addToChar}
<state_char>   \n                                                              { specialError newlineError} 
<state_char>   \\                                                              { specialError illegalEscapeError}

{

state_default = 0

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

data LexState = DefaultState | StringState | CharState

data AlexUserState = AlexUserState { 
                                     filePath    :: FilePath
                                   , stringValue :: String
                                   , state     :: LexState
                                   , charValue   :: (Char, Bool) 
                                   , currentLine :: String
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>" "" DefaultState (' ', False) ""

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

getStringValue :: Alex String
getStringValue = liftM stringValue alexGetUserState

getState :: Alex LexState
getState = liftM state alexGetUserState

getCharValue :: Alex (Char, Bool)
getCharValue = liftM charValue alexGetUserState

getCurrentLine :: Alex String
getCurrentLine = liftM currentLine alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath path = do 
    string <- getStringValue
    state <- getState
    char <- getCharValue
    current <- getCurrentLine
    alexSetUserState (AlexUserState path string state char current)

setStringValue :: String -> Alex ()
setStringValue string = do 
    path <- getFilePath
    state <- getState
    char <- getCharValue
    current <- getCurrentLine
    alexSetUserState (AlexUserState path string state char current)

setState :: LexState -> Alex ()
setState state = do
    path <- getFilePath
    string <- getStringValue
    char <- getCharValue
    current <- getCurrentLine
    alexSetUserState (AlexUserState path string state char current)

setCharValue :: (Char, Bool) -> Alex ()
setCharValue char = do 
    path <- getFilePath
    string <- getStringValue
    state <- getState
    current <- getCurrentLine
    alexSetUserState (AlexUserState path string state char current)

setCurrentLine :: String -> Alex ()
setCurrentLine current = do
    path <- getFilePath
    string <- getStringValue
    state <- getState
    char <- getCharValue
    alexSetUserState (AlexUserState path string state char current)

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

-- actions

lex :: (String -> Terminal) -> AlexAction Token
lex func = \(pos, _, _, string) count -> return (Token pos (func (take count string)))

lex' :: Terminal -> AlexAction Token
lex' = lex . const

enterString :: AlexAction Token
enterString = \(pos, _, _, string) count -> do
    setState StringState
    setStringValue ""
    alexMonadScan'

addToString :: AlexAction Token
addToString = \(pos, _, _, string) count -> do
    current <- getStringValue
    setStringValue (current ++ [(convert_to_char (take count string))])
    alexMonadScan'

finishString :: AlexAction Token
finishString = \(pos, _, _, string) count -> do
    string <- getStringValue
    setState DefaultState
    return (Token pos (StringToken string))

enterChar :: AlexAction Token
enterChar = \(pos, _, _, string) count -> do
    setState CharState
    setCharValue (' ', False)
    alexMonadScan'

addToChar :: AlexAction Token
addToChar = \(pos, _, _, string) count -> do 
    setCharValue (convert_to_char (take count string), True)
    alexMonadScan'

finishChar :: AlexAction Token
finishChar = \(pos, _, _, string) count -> do
    (char, hasChar) <- getCharValue
    setState DefaultState
    if hasChar == False
      then (handleError pos ("missing char value at:\n  ")) --TODO!
      else return (Token pos (CharToken char))

-- utility

convert_to_char :: String -> Char
convert_to_char "\\t"  = '\t'
convert_to_char "\\b"  = '\b'
convert_to_char "\\n"  = '\n'
convert_to_char "\\r"  = '\r'
convert_to_char "\\f"  = '\f'
convert_to_char "\\\'" = '\''
convert_to_char "\\\"" = '\"'
convert_to_char "\\"   = '\\'
convert_to_char (x:xs) = x

-- error handling

alexEOF :: Alex Token
alexEOF = do
  state <- getState
  (pos, _, _, _) <- alexGetInput
  case state of
    StringState  -> handleError pos "string not closed at end of file"
    CharState    -> handleError pos "char not closed at end of file"
    DefaultState -> return (Token pos EOFToken)

alexMonadScan' :: Alex Token
alexMonadScan' = do
    input <- alexGetInput
    sCode <- alexGetStartCode
    case alexScan input sCode of
      AlexEOF -> alexEOF
      AlexError (pos, _, _, string) -> do
          current <- getCurrentLine
          handleError pos (getErrorMessage current pos string)
      AlexSkip  input' length -> do
          current <- getCurrentLine
          setCurrentLine (findCurrentLine input' current)
          alexSetInput input'
          alexMonadScan'
      AlexToken input' length action -> do
          alexSetInput input'
          action (ignorePendingBytes input) length

findCurrentLine :: AlexInput -> String -> String
findCurrentLine ((AlexPn _ line column), _, _, string) current = if column == 1
                                                                    then takeWhile (not . (flip elem) "\n\r") string
                                                                    else current

getErrorMessage :: String -> AlexPosn -> String -> String
getErrorMessage current (AlexPn _ line column) string = "unexpected character '" ++ 
                                                        [head string] ++ 
                                                        "' in:\n   " ++
                                                        take column current ++
                                                        getPositionString string ++
                                                        "\n   " ++
                                                        getErrorIndicator (column - 1)

getErrorIndicator :: Int -> String
getErrorIndicator 0    = "^"
getErrorIndicator size = ' ':(getErrorIndicator (size - 1))

getPositionString :: String -> String
getPositionString string = tail (takeWhile (not . (flip elem) "\r\n") string)

handleError :: AlexPosn -> String -> Alex a
handleError (AlexPn _ line column) err = do 
    path <- getFilePath
    alexError (path ++ ":" ++ show line ++ ":" ++ show column ++ ": error: " ++ err)

-- special error cases

specialError :: (String -> AlexPosn -> String -> String) -> AlexAction Token
specialError err = \(pos, _, _, string) count -> do 
    current <- getCurrentLine
    handleError pos (err current pos string)

newlineError current (AlexPn _ line column) (x:xs) = "illegal newline in string or char literal at:\n  " ++ 
                                                      take (column - 1) current ++
                                                      " " ++ 
                                                      getPositionString (' ':xs) ++
                                                      "\n   " ++
                                                      getErrorIndicator (column - 2)

illegalEscapeError current (AlexPn _ line column) string = "illegal escape sequence at:\n  " ++ 
                                                           take (column - 1) current ++
                                                           "\\" ++ 
                                                           getPositionString string ++
                                                           "\n   " ++
                                                           getErrorIndicator (column - 2)

runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a path input = runAlex input (setFilePath path >> a)

}