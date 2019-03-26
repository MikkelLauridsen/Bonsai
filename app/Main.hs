module Main (main) where

import System.Environment (getArgs)
import Parser (parseBonsai)
import Prettifier

main :: IO ()
main = do
    args <- getArgs
    result <- case args of
                []     -> fmap (parseBonsai "<stdin>") getLine
                [file] -> fmap (parseBonsai file) (readFile file)
                _      -> error "expected a single file." 
    putStrLn (prettify result)