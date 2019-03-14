module Main (main) where

import System.Environment (getArgs)
import Parser (parseBonsai)

main :: IO ()
main = do
    args <- getArgs
    result <- case args of
                []     -> fmap (parseBonsai "<stdin>") getContents
                [file] -> fmap (parseBonsai file) (readFile file)
                _      -> error "Expected a single file." 
    either putStrLn (print . eval []) result

--junk der skal ud
eval :: [(String,Int)] -> () -> Int
eval p () = 1
eval p () = 0