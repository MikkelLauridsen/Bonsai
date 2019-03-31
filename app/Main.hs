module Main (main) where

import System.Environment (getArgs)
import Parser (parseBonsai)
import Ast
import Prettifier

showResult :: Either String ProgAST -> String
showResult (Left err) = err
showResult (Right prog) = prettyShow prog 0

main :: IO ()
main = do
    args <- getArgs
    result <- case args of
                []     -> fmap (parseBonsai "<stdin>") getLine
                [file] -> fmap (parseBonsai file) (readFile file)
                _      -> error "expected a single file." 
    putStrLn (showResult result)
