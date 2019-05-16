module Main (main) where

import System.Environment (getArgs)
import Parser (parseBonsai)
import Ast
import Prettifier
import Inference
import Semantics
import System.IO

-- prettify formats the result of parsing a given file
-- if the result is an error message, it is returned
-- if the result is an AST, it is formatted to conform to Bonsai's abstract syntax
prettify :: Either String ProgAST -> String
prettify (Left err) = err
prettify (Right prog) = prettyShow prog 0

-- main starts the interactive interpreter,
-- interprets a file or outputs an error message
-- based on program arguments
main :: IO ()
main = do
    args <- getArgs
    case args of
        []           -> interactive
        [file]       -> fromFile file
        [file, "@P"] -> prettifyFile file
        _            -> error "expected a single file."

-- prettifyFile tries to read the file at input path
-- then parses it and prints an abstract representation
-- of the program on success
prettifyFile :: String -> IO ()
prettifyFile file = do
    result <- fmap (parseBonsai file) (readFile file)
    putStrLn $ prettify result

-- fromFile tries to read the file at input path
-- and interprets it on success
fromFile :: String -> IO ()
fromFile file = do
    result <- fmap (parseBonsai file) (readFile file)
    runInterpret file result

-- UserAction is used by the interactive interpreter to specify user actions,
-- such as interpreting or prettifying the user's input
data UserAction = RunUser String
                | ExitUser
                | PrettifyUser String
                | ErrorUser String

-- getSource recursively chooses a UserAction
-- based stdin input
getSource :: [String] -> IO UserAction
getSource current = do
    line <- cmdPrompt
    case line of
        "@EXIT"     -> return ExitUser
        "@RUN"      -> return $ RunUser (current >>= (++ "")) -- TODO: try const instead of (++ "") !!
        "@PRETTIFY" -> return $ PrettifyUser (current >>= (++ ""))
        "@RESET"    -> getSource []
        "@UNDO"     -> case current of
                            [] -> return $ ErrorUser "cannot delete an empty string"
                            _  -> getSource $ init current
        _           -> getSource $ current ++ [line]

-- interactive recursively interprets or pretty prints user input
-- until an ExitUser UserAction is returned by getSource
interactive :: IO ()
interactive = do
    res <- getSource []
    case res of
        ExitUser           -> putStrLn "\n"
        ErrorUser err      -> do
            putStrLn $ "ERROR: " ++ err ++ "\n"
            interactive
        RunUser input      -> do
            runInterpret "<stdin>" (parse input)
            interactive
        PrettifyUser input -> do
            putStrLn $ prettify (parse input)
            interactive
    where parse = parseBonsai "<stdin>"

runInterpret :: FilePath -> Either String ProgAST -> IO ()
runInterpret _ (Left err) = putStrLn err
runInterpret path (Right ast) =
    case infer path ast of
        (Just msg) -> putStrLn msg
        Nothing    -> do
            res <- interpret path ast
            case res of
                (Left msg) -> putStrLn msg
                (Right _)  -> putChar '\n'

-- prompts the user for a line in stdin
-- and returns the result
cmdPrompt :: IO String
cmdPrompt = do
    putStr "; "
    hFlush stdout
    getLine
