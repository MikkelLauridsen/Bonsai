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
, getCurrentLine
, getLineNumber
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
<0>            \-? $digit+                                                     { lex (IntToken . read)}
<0>            \-? $digit+\.$digit+                                            { lex (FloatToken . read)}
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
-- ensures correct HS color coding: "

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
                                      filePath      :: FilePath
                                    , stringValue   :: String
                                    , state         :: LexState
                                    , charValue     :: (Char, Bool) 
                                    , currentLine   :: String
                                    , lineNumber    :: (Int, Int)
                                    , position      :: AlexPosn
                                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>" "" DefaultState (' ', False) "" (-1, 0) (AlexPn 0 0 0)

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

getLineNumber :: Alex (Int, Int)
getLineNumber = liftM lineNumber alexGetUserState

getPosition :: Alex AlexPosn
getPosition = liftM position alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath path = do 
    string <- getStringValue
    state <- getState
    char <- getCharValue
    current <- getCurrentLine
    line <- getLineNumber
    pos <- getPosition
    alexSetUserState (AlexUserState path string state char current line pos)

setStringValue :: String -> Alex ()
setStringValue string = do 
    path <- getFilePath
    state <- getState
    char <- getCharValue
    current <- getCurrentLine
    line <- getLineNumber
    pos <- getPosition
    alexSetUserState (AlexUserState path string state char current line pos)

setState :: LexState -> Alex ()
setState state = do
    path <- getFilePath
    string <- getStringValue
    char <- getCharValue
    current <- getCurrentLine
    line <- getLineNumber
    pos <- getPosition
    alexSetUserState (AlexUserState path string state char current line pos)

setCharValue :: (Char, Bool) -> Alex ()
setCharValue char = do 
    path <- getFilePath
    string <- getStringValue
    state <- getState
    current <- getCurrentLine
    line <- getLineNumber
    pos <- getPosition
    alexSetUserState (AlexUserState path string state char current line pos)

setCurrentLine :: String -> Alex ()
setCurrentLine current = do
    path <- getFilePath
    string <- getStringValue
    state <- getState
    char <- getCharValue
    line <- getLineNumber
    pos <- getPosition
    alexSetUserState (AlexUserState path string state char current line pos)

setLineNumber :: (Int, Int) -> Alex ()
setLineNumber line = do
    path <- getFilePath
    string <- getStringValue
    state <- getState
    char <- getCharValue
    current <- getCurrentLine
    pos <- getPosition
    alexSetUserState (AlexUserState path string state char current line pos)

setPosition :: AlexPosn -> Alex ()
setPosition pos = do
    path <- getFilePath
    string <- getStringValue
    state <- getState
    char <- getCharValue
    current <- getCurrentLine
    line <- getLineNumber
    alexSetUserState (AlexUserState path string state char current line pos)


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
addToChar = \input@(pos, _, _, string) count -> do
    (_, hasChar) <- getCharValue
    if hasChar
      then (specialError charSizeError) input count
      else do
        setCharValue (convert_to_char (take count string), True)
        alexMonadScan'

finishChar :: AlexAction Token
finishChar = \input@(pos, _, _, string) count -> do
    (char, hasChar) <- getCharValue
    setState DefaultState
    if hasChar == False
      then (specialError emptyCharError) input count
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
          (_, offset) <- getLineNumber
          handleError pos (getErrorMessage current pos string offset)
      AlexSkip  input' length -> do
          updateLine input'
          alexSetInput input'
          alexMonadScan'
      AlexToken input' length action -> do
          updateLine input'
          alexSetInput input'
          action (ignorePendingBytes input) length

updateLine :: AlexInput -> Alex ()
updateLine input@(pos, _, _, _) = do
    line <- getLineNumber
    current <- getCurrentLine
    let (upCurrent, upLine) = findCurrentLine input current line
    setCurrentLine upCurrent
    setPosition pos
    setLineNumber upLine


findCurrentLine :: AlexInput -> String -> (Int, Int) -> (String, (Int, Int))
findCurrentLine ((AlexPn _ line column), _, _, string) current (lineN, offset) = 
    if (line /= lineN)
      then (takeWhile (not . (flip elem) "\n\r") string, (line, column))
      else (current, (lineN, offset))

getErrorMessage :: String -> AlexPosn -> String -> Int -> String
getErrorMessage current (AlexPn _ line column) string offset = 
    "unexpected character '" ++ 
    [head string] ++ 
    "' at:\n   " ++
    current ++
    "\n   " ++
    getErrorIndicator (column - offset)

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

specialError :: (String -> AlexPosn -> String -> Int -> String) -> AlexAction Token
specialError err = \(pos, _, _, string) count -> do 
    current <- getCurrentLine
    (_, offset) <- getLineNumber
    handleError pos (err current pos string offset)

emptyCharError current (AlexPn _ line column) string offset = 
    "illegal closing of char sequence at:\n  " ++
    takeWhile (not . (flip elem) "\r\n") current ++
    "\n  " ++
    getErrorIndicator (column - offset) 

charSizeError current (AlexPn _ line column) string offset = 
    "illegally sized char at:\n  " ++
    take (column + 1 - offset) current ++
    getPositionString string ++
    "\n   " ++
    getErrorIndicator (column - 1 - offset)

newlineError current (AlexPn _ line column) (x:xs) offset =  -- TODO: fix this one
    "illegal newline in string or char literal at:\n  " ++
    take (column - offset) current ++
    " " ++ 
    getPositionString (' ':xs) ++
    "\n   " ++
    getErrorIndicator (column - 1 - offset)

illegalEscapeError current (AlexPn _ line column) string offset = 
    "illegal escape sequence at:\n  " ++ 
    take (column - offset) current ++
    "\\" ++ 
    getPositionString string ++
    "\n   " ++
    getErrorIndicator (column - 1 - offset)

runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a path input = runAlex input (setFilePath path >> a)

}