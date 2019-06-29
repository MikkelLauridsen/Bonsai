    
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

import System.Environment (getArgs)
import Data.Map.Strict as Map
import Data.Set as Set
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
import Control.Concurrent as CC

type Disc z = (ChannelId, (RestChan, Gateway, z))

type Env = Map VarId Values

type LazySig = Set LazyAlgebraicType 

type LazyAlgebraicType = (TypeId, [String])

type Ups = Map TypeId Type

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
    finally (loopDiscord dis initEnvs) (stopDiscord dis)

data Envs = Envs {
                   typeEnv     :: TypeEnv
                 , varEnv      :: Env
                 , typeSig     :: Set (TypeId, Type, Type)
                 , typeUpsilon :: Ups
                 , typeLsigma  :: LazySig
                 , varSig      :: Set TypeId
                 }

initEnvs = Envs (TypeEnv Map.empty) Map.empty Set.empty Map.empty Set.empty Set.empty

loopDiscord :: (RestChan, Gateway, z) -> Envs -> IO ()
loopDiscord dis envs = do
    e <- nextEvent dis
    case e of
        Left err -> putStrLn ("Event error: " ++ show err)
        Right (MessageCreate m) -> do
            when ((T.isPrefixOf ";" (messageText m)) && not (fromBot m)) $ do
                let res = parseBonsai "" (T.unpack (T.tail (messageText m)))
                case res of
                    Left err -> do
                        _ <- restCall dis (CreateMessage (messageChannel m) (T.pack err))
                        putStrLn "Parser/lexer error\n"
                    (Right (ExprSingle expr)) -> runInterpret (messageChannel m, dis) envs expr
                    Right var@(VarSingle (UntypedVarAST x _) expr _) -> do
                        _ <- restCall dis (CreateMessage (messageChannel m) (T.pack (creationMsg var)))
                        putStrLn (creationMsg var)
                        loopDiscord dis (envs{ varEnv = Map.insert x (LazyValue expr) (varEnv envs), typeEnv = TypeEnv (Map.insert x (LazyT expr) (extractTypeEnv (typeEnv envs))) })
                    Right var@(VarSingle (TypedVarAST x s _) expr _) -> do
                        _ <- restCall dis (CreateMessage (messageChannel m) (T.pack (creationMsg var)))
                        putStrLn (creationMsg var)
                        loopDiscord dis (envs{ varEnv = Map.insert x (LazyValue expr) (varEnv envs), typeEnv = TypeEnv (Map.insert x (LazyS expr s) (extractTypeEnv (typeEnv envs))) })
                    Right typ ->
                        case inferMakeType (typeSig envs) (typeUpsilon envs) (typeLsigma envs) typ of
                            Left msg -> do
                                _ <- restCall dis (CreateMessage (messageChannel m) (T.pack msg))
                                putStrLn (msg ++ "\n")
                            Right (sigma', upsilon', lsigma') -> do
                                _ <- restCall dis (CreateMessage (messageChannel m) (T.pack (creationMsg typ)))
                                putStrLn (creationMsg typ)
                                loopDiscord dis (envs{ typeSig = sigma', typeUpsilon = upsilon', typeLsigma = lsigma', varSig = (Set.union (varSig envs) (Set.fromList (singleTypeToIds typ))) })
            loopDiscord dis envs
        _ -> loopDiscord dis envs

compete :: [IO a] -> IO a
compete actions = do
    mvar <- newEmptyMVar
    tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) actions
    result <- takeMVar mvar
    mapM_ killThread tids
    return result

timeout :: Int -> IO a -> IO (Maybe a)
timeout usec action = compete [fmap Just action, threadDelay usec >> return Nothing]

singleTypeToIds :: SingleAST -> [TypeId]
singleTypeToIds (TypeSingle _ cons _)       = Prelude.map extractId cons
singleTypeToIds (TypePolySingle _ _ cons _) = Prelude.map extractId cons

extractId :: ConsAST -> TypeId
extractId (SingleConsAST id _)   = id
extractId (DoubleConsAST id _ _) = id

creationMsg :: SingleAST -> String
creationMsg (VarSingle (UntypedVarAST x _) expr _) = "variable ``" ++ varName x ++ "`` was saved as:```Haskell\n" ++ prettyShow expr 0 ++ "```"
creationMsg (VarSingle (TypedVarAST x s _) expr _) = "variable ``" ++ varName x ++ "::" ++ prettyShow s 0 ++ "`` was saved as:```Haskell\n" ++ prettyShow expr 0 ++ "```"
creationMsg (TypeSingle t cons _) = "type ``" ++ typeName t ++ "`` was saved with termconstructors:```Haskell\n" ++ ([prettyShow con 0 | con <- cons] >>= (++ "\n")) ++ "```"
creationMsg (TypePolySingle t vars cons _) = "type ``" ++ typeName t ++ "<" ++ prettyShowList vars 0 ", " ++ ">" ++ "`` was saved with termconstructors:```Haskell\n" ++ ([prettyShow con 0 | con <- cons] >>= (++ "\n")) ++ "```"

extractTypeEnv :: TypeEnv -> Map VarId Scheme
extractTypeEnv (TypeEnv m) = m

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

runInterpret :: Disc z -> Envs -> ExprAST -> IO ()
runInterpret dis@(chan, dis') envs expr = do
    let action = case inferRun (typeEnv envs) (typeSig envs) (typeUpsilon envs) (typeLsigma envs) expr of
            (Just msg) -> return $ Left msg
            Nothing    -> evalExpr expr Map.empty (varEnv envs) (varSig envs) dis

    res <- timeout 1000 action
    case res of
        Nothing -> do
            _ <- restCall dis' (CreateMessage chan (T.pack "time limit exceeded"))
            putStrLn "time limit exceeded, killed thread\n\n"
        Just (Left msg) -> do
            _ <- restCall dis' (CreateMessage chan (T.pack msg))
            putStrLn msg
        Just (Right (v, _)) -> do
            _ <- restCall dis' (CreateMessage chan (T.pack ("``" ++ show v ++ "``")))
            putStrLn ("``" ++ show v ++ "``")


    