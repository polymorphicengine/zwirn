module Listener.Listen (listen) where

import Sound.OSC.FD as O

import Control.Concurrent.MVar  (MVar, takeMVar, putMVar)

import Data.Text (Text, pack)

import Zwirn.Language.Compiler
import Listener.Setup

data EvalMode
  = EvalBlock
  | EvalLine
  | EvalWhole
  deriving (Eq, Show)

listen :: Remote -> Local -> MVar Environment -> IO ()
listen remote local envMV = do
       env <- takeMVar envMV
       m <- recvMessage local
       env' <- act remote local env m
       putMVar envMV env'
       listen remote local envMV

act :: Remote -> Local -> Environment -> Maybe O.Message -> IO Environment
act remote local env (Just (Message "/eval/block" [ASCII_String st])) = ciAction 0 0 EvalBlock remote local env (pack $ ascii_to_string st)
act remote local env (Just (Message "/eval/block" [ASCII_String st, Int64 line])) = ciAction (fromIntegral line) 0 EvalBlock remote local env (pack $ ascii_to_string st)
act remote local env (Just (Message "/eval/block" [ASCII_String st, Int64 line, Int64 ed])) = ciAction (fromIntegral line) (fromIntegral ed) EvalBlock remote local env (pack $ ascii_to_string st)
act remote local env (Just (Message "/eval/line" [ASCII_String st])) = ciAction 0 0 EvalLine remote local env (pack $ ascii_to_string st)
act remote local env (Just (Message "/eval/line" [ASCII_String st, Int64 line])) = ciAction (fromIntegral line) 0 EvalLine remote local env (pack $ ascii_to_string st)
act remote local env (Just (Message "/eval/line" [ASCII_String st, Int64 line, Int64 ed])) = ciAction (fromIntegral line) (fromIntegral ed) EvalLine remote local env (pack $ ascii_to_string st)
act remote local env (Just (Message "/eval" [ASCII_String st])) = ciAction 0 0 EvalWhole remote local env (pack $ ascii_to_string st)
act remote local env (Just (Message "/eval" [ASCII_String st, Int64 ed])) = ciAction 0 (fromIntegral ed) EvalWhole remote local env (pack $ ascii_to_string st)
act _ _ env Nothing = putStrLn "not a message?" >> return env
act _ _ env (Just m) = (putStrLn $ "Unhandled message: " ++ show m) >> return env

ciAction :: Int -> Int -> EvalMode -> Remote -> Local -> Environment -> Text -> IO Environment
ciAction line editorNum mode remote local env st = do
  let ci = case mode of
              EvalBlock -> compilerInterpreterBlock line editorNum st
              EvalLine -> compilerInterpreterLine line editorNum st
              EvalWhole -> compilerInterpreterWhole editorNum st
  res <- runCI env ci
  case res of
        Left (CIError err (Just (CurrentBlock strt end))) -> do
              O.sendTo local (O.p_message "/eval/error" [string err, float strt, float end]) remote
              return env
        Left (CIError err Nothing) -> do
              O.sendTo local (O.p_message "/eval/error" [string err]) remote
              return env
        Right (resp, newEnv, _, _) -> do
              O.sendTo local (O.p_message "/eval/ok" [string resp]) remote
              return newEnv
