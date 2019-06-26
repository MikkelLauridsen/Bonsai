{

{-# OPTIONS -w  #-}
module Lexer 
( Token(..)
, AlexPosn(..)
, Terminal(..)
, terminalString
, Alex(..)
, setFileAndRun
, scanBonsai
, handleError
, getCurrentLine
, getLineNumber
, AlexUserState(..)
) where

import Prelude hiding (stringTerminalToToken)
import Control.Monad (liftM)

}

%wrapper "monadUserState"

-- character groups for regular expressions
$digit   = 0-9
$lower   = [a-z]
$upper   = [A-Z]
$alpha   = [a-zA-Z]
$regchar = [\x20-\x7E] # \\
$spechar = [t b n r f \' \" \\] --escapable characters"

-- token rules: regular expressions used to identify tokens or change the lexer state.
-- <N> denotes state N, where <0> is the initial state.
-- only token rules associated with the current lexer state will be evaluated.
tokens :-

<0>            $white+                                                         ; -- ignore whitespace
<0>            \#.*                                                            ; -- ignore comments
<0>            var                                                             { terminalToToken VarToken}
<0>            let                                                             { terminalToToken LetToken}
<0>            in                                                              { terminalToToken InToken}
<0>            match                                                           { terminalToToken MatchToken}
<0>            case                                                            { terminalToToken CaseToken}
<0>            type                                                            { terminalToToken TypeToken}
<0>            true                                                            { terminalToToken (BoolToken True)}
<0>            false                                                           { terminalToToken (BoolToken False)}
<0>            close                                                           { stringTerminalToToken IOToken}
<0>            read                                                            { stringTerminalToToken IOToken}
<0>            write                                                           { stringTerminalToToken IOToken}
<0>            show                                                            { stringTerminalToToken IOToken}
<0>            s2i                                                             { stringTerminalToToken IOToken}
<0>            s2f                                                             { stringTerminalToToken IOToken}
<0>            i2c                                                             { stringTerminalToToken IOToken}
<0>            c2i                                                             { stringTerminalToToken IOToken}
<0>            \=\>                                                            { terminalToToken FollowsToken}
<0>            \-? $digit+                                                     { stringTerminalToToken (IntToken . read)}
<0>            \-? $digit+\.$digit+                                            { stringTerminalToToken (FloatToken . read)}
<0>            ($upper | $upper [$alpha $digit \_]* [$alpha $digit]) \'? \*?   { stringTerminalToToken TypeIdToken}
<0>            ($lower | $lower [$alpha $digit \_]* [$alpha $digit]) \'?       { stringTerminalToToken VarIdToken}
<0>            0x [$digit $upper]+                                             { stringTerminalToToken (IntToken . read)}
<0>            \|                                                              { terminalToToken GuardToken}
<0>            \.                                                              { terminalToToken EscapeToken}
<0>            \=                                                              { terminalToToken DeclareToken}
<0>            \{                                                              { terminalToToken CurlyOpenToken}
<0>            \}                                                              { terminalToToken CurlyCloseToken}
<0>            \:\:                                                            { terminalToToken AnnotateToken}
<0>            \[                                                              { terminalToToken SquareOpenToken}
<0>            \]                                                              { terminalToToken SquareCloseToken}
<0>            \(                                                              { terminalToToken ParenOpenToken}
<0>            \)                                                              { terminalToToken ParenCloseToken}
<0>            \-\>                                                            { terminalToToken EntailsToken}
<0>            \,                                                              { terminalToToken CommaToken}
<0>            \?                                                              { terminalToToken WildcardToken}
<0>            \<\<                                                            { terminalToToken ClassOpenToken}
<0>            \>\>                                                            { terminalToToken ClassCloseToken}
<0>            \+                                                              { stringTerminalToToken LevelOneOpToken} 
<0>            \-                                                              { stringTerminalToToken LevelOneOpToken}
<0>            \+\+                                                            { stringTerminalToToken LevelOneOpToken}
<0>            \&\&                                                            { stringTerminalToToken LevelOneOpToken}
<0>            \|\|                                                            { stringTerminalToToken LevelOneOpToken}
<0>            \*                                                              { stringTerminalToToken LevelTwoOpToken}
<0>            \/                                                              { stringTerminalToToken LevelTwoOpToken}
<0>            \%                                                              { stringTerminalToToken LevelTwoOpToken}
<0>            \=\=                                                            { stringTerminalToToken LevelTwoOpToken}
<0>            b\&                                                             { stringTerminalToToken LevelTwoOpToken}
<0>            b\^                                                             { stringTerminalToToken LevelTwoOpToken}
<0>            b\|                                                             { stringTerminalToToken LevelTwoOpToken}
<0>            \<                                                              { terminalToToken AngleOpenToken} 
<0>            \>                                                              { terminalToToken AngleCloseToken}
<0>            \<\=                                                            { stringTerminalToToken LevelThreeOpToken}
<0>            \>\=                                                            { stringTerminalToToken LevelThreeOpToken}
<0>            b\<\<                                                           { stringTerminalToToken LevelThreeOpToken}
<0>            b\>\>                                                           { stringTerminalToToken LevelThreeOpToken}
<0>            \:                                                              { terminalToToken ConsToken}
<0>            \!                                                              { stringTerminalToToken UnaryOpToken}
<0>            b\~                                                             { stringTerminalToToken UnaryOpToken}
<0>            \~                                                              { stringTerminalToToken UnaryOpToken}
<0>            \"                                                              { enterString `andBegin` state_string} --" switch to the string state
<0>            \'                                                              { enterChar `andBegin` state_char} -- switch to the char state
-- token rules associated with string state
<state_string> \"                                                              { finishString `andBegin` state_default} -- finish string on "
<state_string> \\$spechar                                                      { addToString}
<state_string> $regchar                                                        { addToString}
<state_string> \n                                                              { specialError newlineError} -- report error on string split on multiple lines
<state_string> \\                                                              { specialError illegalEscapeError} -- report error on unescaped \
-- token rules associated with char state
<state_char>   \'                                                              { finishChar `andBegin` state_default} -- finish char on '
<state_char>   $regchar                                                        { addToChar}
<state_char>   \\$spechar                                                      { addToChar}
<state_char>   \n                                                              { specialError newlineError} -- report error on char split on multiple lines
<state_char>   \\                                                              { specialError illegalEscapeError} -- report error on unescaped \

-- end of token rules
{

-- alias for default lexer state
state_default = 0

-- terminals (tokens) used by the lexer
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
    EscapeToken              |
    AngleOpenToken           |
    AngleCloseToken          |
    DeclareToken             |
    CurlyOpenToken           |
    CurlyCloseToken          |
    AnnotateToken            |
    SquareOpenToken          |
    SquareCloseToken         |
    ParenOpenToken           |
    ParenCloseToken          |
    ConsToken                |
    EntailsToken             |
    CommaToken               |
    WildcardToken            |
    ClassOpenToken           |
    ClassCloseToken          |
    LevelOneOpToken String   |
    LevelTwoOpToken String   |
    LevelThreeOpToken String |
    UnaryOpToken String      |
    IOToken String           |
    EOFToken
    deriving (Eq, Show)

-- lexing states
data LexState = DefaultState | StringState | CharState

-- state of the lexer, which is updated during lexing/parsing
data AlexUserState = AlexUserState { 
                                      filePath      :: FilePath     -- path of the file being lexed/parsed
                                    , stringValue   :: String       -- current string body (only valid during string_state)
                                    , state         :: LexState     -- the current lexer state (lexing string, char or default)
                                    , charValue     :: (Char, Bool) -- as a char cannot be empty, we use a bool to see whether the char body is valid (only used during char_state)
                                    , currentLine   :: String       -- current line of source file, used for error messages
                                    , lineNumber    :: (Int, Int)   -- current line number and indentation, used for error messages
                                    , position      :: AlexPosn     -- current line and column numbers used for error messages
                                   }

-- initial state of the lexer
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>" "" DefaultState (' ', False) "" (-1, 0) (AlexPn 0 0 0)

-- functions used to access individual parts of the lexer state
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

-- functions used to update individual parts of the lexer state
setFilePath :: FilePath -> Alex ()
setFilePath path = do 
    userState <- alexGetUserState
    alexSetUserState userState{ filePath = path }

setStringValue :: String -> Alex ()
setStringValue string = do 
    userState <- alexGetUserState
    alexSetUserState userState{ stringValue = string }

setState :: LexState -> Alex ()
setState state' = do
    userState <- alexGetUserState
    alexSetUserState userState{ state = state' }

setCharValue :: (Char, Bool) -> Alex ()
setCharValue char = do 
    userState <- alexGetUserState
    alexSetUserState userState{ charValue = char }

setCurrentLine :: String -> Alex ()
setCurrentLine current = do
    userState <- alexGetUserState
    alexSetUserState userState{ currentLine = current }

setLineNumber :: (Int, Int) -> Alex ()
setLineNumber line = do
    userState <- alexGetUserState
    alexSetUserState userState{ lineNumber = line }

setPosition :: AlexPosn -> Alex ()
setPosition pos = do
    userState <- alexGetUserState
    alexSetUserState userState{ position = pos }

-- Token format used by the lexer and parser:
-- a terminal and the current position
data Token = Token AlexUserState Terminal

-- terminalString returns the string representation of input Terminal
-- Used for error messages
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
terminalString EscapeToken                = "."
terminalString AngleOpenToken             = "<"
terminalString AngleCloseToken            = ">"
terminalString DeclareToken               = "="
terminalString CurlyOpenToken             = "{"
terminalString CurlyCloseToken            = "}"
terminalString AnnotateToken              = "::"
terminalString SquareOpenToken            = "["
terminalString SquareCloseToken           = "]"
terminalString ParenOpenToken             = "("
terminalString ParenCloseToken            = ")"
terminalString ConsToken                  = ":"
terminalString EntailsToken               = "->"
terminalString CommaToken                 = ","
terminalString WildcardToken              = "?"
terminalString ClassOpenToken             = "<<"
terminalString ClassCloseToken            = ">>"
terminalString (LevelOneOpToken string)   = string
terminalString (LevelTwoOpToken string)   = string
terminalString (LevelThreeOpToken string) = string
terminalString (UnaryOpToken string)      = string
terminalString (IOToken string)           = string
terminalString EOFToken                   = "$"

-- actions

-- convert string to terminal function to token
stringTerminalToToken :: (String -> Terminal) -> AlexAction Token
stringTerminalToToken func = \(_, _, _, string) count -> do
     userState <- alexGetUserState
     return (Token userState (func (take count string)))

-- convert terminal to token
terminalToToken :: Terminal -> AlexAction Token
terminalToToken = stringTerminalToToken . const

-- enter the string_state
enterString :: AlexAction Token
enterString = \(pos, _, _, string) count -> do
    setState StringState
    setStringValue ""
    scanBonsai

-- add to the current string body,
-- changing the user state
addToString :: AlexAction Token
addToString = \(pos, _, _, string) count -> do
    current <- getStringValue
    setStringValue (current ++ [(convert_to_char (take count string))])
    scanBonsai

-- exit the string_state and return the new string token
finishString :: AlexAction Token
finishString = \(_, _, _, string) count -> do
    string <- getStringValue
    setState DefaultState
    userState <- alexGetUserState
    return (Token userState (StringToken string))

-- enter the char_state
enterChar :: AlexAction Token
enterChar = \(pos, _, _, string) count -> do
    setState CharState
    setCharValue (' ', False)
    scanBonsai

-- set the char body
-- if a body is already present,
-- output an error message
addToChar :: AlexAction Token
addToChar = \input@(pos, _, _, string) count -> do
    (_, hasChar) <- getCharValue
    if hasChar
      then (specialError charSizeError) input count
      else do
        setCharValue (convert_to_char (take count string), True)
        scanBonsai

-- exit the char_state and return the new char token
finishChar :: AlexAction Token
finishChar = \input@(pos, _, _, string) count -> do
    (char, hasChar) <- getCharValue
    setState DefaultState
    userState <- alexGetUserState
    if hasChar == False
      then (specialError emptyCharError) input count
      else return (Token userState (CharToken char))

-- convert string sequence to escaped char
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

-- this function is required by Alex
-- It is called when no more characters remain to be scanned
-- Errors messages are returned if the lexer is not in the default state
alexEOF :: Alex Token
alexEOF = do
  state <- getState
  (pos, _, _, _) <- alexGetInput
  case state of
    StringState  -> handleError pos "string not closed at end of file"
    CharState    -> handleError pos "char not closed at end of file"
    DefaultState -> do 
        userState <- alexGetUserState
        return (Token userState EOFToken)

-- scanBonsai recursively scans the specified file,
-- until a regular expression recognizes the sequence
-- if no regular expression recognizes, an error message is returned 
scanBonsai :: Alex Token
scanBonsai = do
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
          scanBonsai
      AlexToken input' length action -> do
          updateLine input'
          alexSetInput input'
          action (ignorePendingBytes input) length

-- updates the current source file line
-- as well as the saved file position and indent level          
updateLine :: AlexInput -> Alex ()
updateLine input@(pos, _, _, _) = do
    line <- getLineNumber
    current <- getCurrentLine
    let (upCurrent, upLine) = findCurrentLine input current line
    setCurrentLine upCurrent
    setPosition pos
    setLineNumber upLine

-- reads the source file from the current position until a line break is found,
-- then returns a tuple containing the read characters and a new line number and indent level
findCurrentLine :: AlexInput -> String -> (Int, Int) -> (String, (Int, Int))
findCurrentLine ((AlexPn _ line column), _, _, string) current (lineN, offset) = 
    if (line /= lineN)
      then (takeWhile (not . (flip elem) "\n\r") string, (line, column))
      else (current, (lineN, offset))

-- returns a formated error message,
-- based on input source file line, position and indent level      
getErrorMessage :: String -> AlexPosn -> String -> Int -> String
getErrorMessage current (AlexPn _ line column) string offset = 
    "unexpected character '" ++ 
    [head string] ++ 
    "' at:\n   " ++
    current ++
    "\n   " ++
    getErrorIndicator (column - offset)

-- returns a string of 'size' spaces and one ^
getErrorIndicator :: Int -> String
getErrorIndicator 0    = "^"
getErrorIndicator size = ' ':(getErrorIndicator (size - 1))

-- returns the input string, 
-- shortened down to the sequence found before the first line break
getPositionString :: String -> String
getPositionString string = tail (takeWhile (not . (flip elem) "\r\n") string)

-- used by the scanBonsai function upon lexer or parse error
-- a formated error message is output to stdout
handleError :: AlexPosn -> String -> Alex a
handleError (AlexPn _ line column) err = do 
    path <- getFilePath
    alexError (path ++ ":" ++ show line ++ ":" ++ show column ++ ": error: " ++ err)

-- handles special error cases such as newline in string/char state
specialError :: (String -> AlexPosn -> String -> Int -> String) -> AlexAction Token
specialError err = \(pos, _, _, string) count -> do 
    current <- getCurrentLine
    (_, offset) <- getLineNumber
    handleError pos (err current pos string offset)

-- special error case formats

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

-- sets the current filepath and runs the lexer
-- returns the result which may be an error message
setFileAndRun :: Alex a -> FilePath -> String -> Either String a
setFileAndRun a path input = runAlex input (setFilePath path >> a)
}