module Main where

import Options.Applicative (execParser)
import Data.Text (pack)

import Control.Concurrent.MVar  (MVar, takeMVar, putMVar)

import CLI.CommandLine
import CLI.Setup

import Zwirn.Language.Compiler

main :: IO ()
main = do
    putStrLn $ "Starting Zwirn CLI.."
    config <- execParser conf
    envMV <- setup config
    loop envMV

loop :: MVar Environment -> IO ()
loop envMV = do
  env <- takeMVar envMV
  env' <- job env
  putMVar envMV env'
  loop envMV

job :: Environment -> IO Environment
job env = do
     putStr ">>> "
     cont <- getLine
     res <- runCI env (compilerInterpreterBlock 0 0 (pack cont))
     case res of
           Left (CIError err (Just (CurrentBlock _ _))) -> (putStrLn $ "Error: " ++ show err) >> (return env)
           Left (CIError err Nothing) -> (putStrLn $ "Error: " ++ show err) >> (return env)
           Right (resp, newEnv, _, _) -> (putStrLn resp) >> (return newEnv)
