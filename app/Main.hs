module Main (main) where

import System.Environment (getArgs)
import Parser (parseBonsai)
import Prettifier

code = "var boolVal = (x && y) || ((32 <= 42) && x) || ((val1 == val2) && y)" 
ast = parseBonsai "test" code

main :: IO ()
--main = putStrLn (prettify ast)

main = do
    args <- getArgs
    result <- case args of
                []     -> fmap (parseBonsai "<stdin>") getLine
                [file] -> fmap (parseBonsai file) (readFile file)
                _      -> error "expected a single file." 
    putStrLn (prettify result)