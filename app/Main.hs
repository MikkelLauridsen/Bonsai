    
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import System.Environment (getArgs)
import Parser (parseBonsai)
import Ast
import Prettifier
import Inference
import Semantics
import System.IO
import Discord
import Control.Exception (finally)
import Control.Monad (when)
import Data.Char (toLower)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Disc z = (ChannelId, (RestChan, Gateway, z))

-- prettify formats the result of parsing a given file
-- if the result is an error message, it is returned
-- if the result is an AST, it is formatted to conform to Bonsai's abstract syntax
prettify :: Either String ProgAST -> String
prettify (Left err)   = err
prettify (Right prog) = prettyShow prog 0

toAst :: Either String ProgAST -> String
toAst (Left err)   = err
toAst (Right prog) = show prog

-- main starts the interactive interpreter,
-- interprets a file or outputs an error message
-- based on program arguments
main :: IO ()
main = do
    tok <- T.strip <$> TIO.readFile "auth-token"
    dis <- loginRestGateway (Auth tok)
    finally (loopDiscord dis) (stopDiscord dis)

loopDiscord :: (RestChan, Gateway, z) -> IO ()
loopDiscord dis = do
    e <- nextEvent dis
    case e of
        Left err -> putStrLn ("Event error: " ++ show err)
        Right (MessageCreate m) -> do
            when ((T.isPrefixOf ";" (messageText m)) && not (fromBot m)) $ do
                runInterpret (messageChannel m, dis) (parseBonsai "" (T.unpack (T.tail (messageText m))))
            loopDiscord dis
        _ -> loopDiscord dis

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

runInterpret :: Disc z -> Either String ProgAST -> IO ()
runInterpret dis@(chan, dis') (Left err) = do 
    _ <- restCall dis' (CreateMessage chan (T.pack err))
    putStrLn err
runInterpret dis@(chan, dis') (Right ast) =
    case infer "" ast of
        (Just msg) -> do 
            _ <- restCall dis' (CreateMessage chan (T.pack msg))
            putStrLn msg
        Nothing    -> do
            res <- interpret dis ast
            case res of
                (Left msg) -> do
                    _ <- restCall dis' (CreateMessage chan (T.pack msg))
                    putStrLn msg
                (Right _)  -> do
                    _ <- restCall dis' (CreateMessage chan "fin 🐮")
                    putChar '\n'