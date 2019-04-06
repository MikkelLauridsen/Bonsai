module Main (main) where

import System.Environment (getArgs)
import Parser (parseBonsai)
import Ast
import Prettifier
import System.IO

prettify :: Either String ProgAST -> String
prettify (Left err) = err
prettify (Right prog) = prettyShow prog 0

main :: IO ()
main = do
    args <- getArgs
    case args of
        []     -> interactive
        [file] -> fromFile file
        _      -> error "expected a single file."


fromFile :: String -> IO ()
fromFile file = do
    result <- fmap (parseBonsai file) (readFile file)
    putStrLn (prettify result)

data UserAction = RunUser String
                | ExitUser
                | PrettifyUser String

getSource :: String -> IO UserAction
getSource current = do
    line <- cmdPrompt
    case line of
        "@EXIT"     -> return ExitUser
        "@RUN"      -> return $ RunUser current
        "@PRETTIFY" -> return $ PrettifyUser current
        _           -> getSource $ current ++ line

interactive :: IO ()
interactive = do
    res <- getSource ""
    case res of
        ExitUser           -> putStrLn "Aborting.."
        RunUser input      -> do
            putStrLn $ prettify ((parseBonsai "<stdin>") input)
            interactive
        PrettifyUser input -> do
            putStrLn $ prettify ((parseBonsai "<stdin>") input)
            interactive


cmdPrompt :: IO String
cmdPrompt = do
    putStr "; "
    hFlush stdout
    getLine
