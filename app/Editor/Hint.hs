module Editor.Hint where

--import Control.Exception  (SomeException)
import Control.Monad.Catch (catch)
import Control.Concurrent.MVar  (MVar, putMVar, takeMVar)


import Language.Haskell.Interpreter as Hint

import Data.List (intercalate)

import Sound.Tidal.Context (ControlPattern)

type InterpreterMessage = String

data InterpreterResponse = RMini ControlPattern
                         | RError String
                         deriving Show

exts :: [Extension]
exts = [OverloadedStrings, BangPatterns, MonadComprehensions, LambdaCase]

hintJob :: MVar InterpreterMessage -> MVar InterpreterResponse -> IO ()
hintJob mMV rMV = do
                result <- catch (Hint.runInterpreter $ staticInterpreter >> (interpreterLoop mMV rMV))
                          (\e -> return (Left $ UnknownError $ show (parseError e)))
                -- can this happen? If it happens all definitions made interactively are lost...
                let response = case result of
                        Left err -> RError (parseError err)
                        Right p  -> RError (show p)
                putMVar rMV response
                hintJob mMV rMV

staticInterpreter :: Interpreter ()
staticInterpreter = Hint.set [languageExtensions := exts] >> Hint.loadModules ["src/Functional.hs","src/MiniPrelude.hs"] >> Hint.setTopLevelModules ["Functional","MiniPrelude"]

interpreterLoop :: MVar InterpreterMessage -> MVar InterpreterResponse -> Interpreter ()
interpreterLoop mMV rMV = do
                    cont <- liftIO $ takeMVar mMV
                    catch (interpretMini cont rMV) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)
                    interpreterLoop mMV rMV


interpretMini :: String -> MVar InterpreterResponse -> Interpreter ()
interpretMini s rMV = do
                      p <- Hint.interpret s (Hint.as :: ControlPattern)
                      liftIO $ putMVar rMV $ RMini p



parseError:: InterpreterError -> String
parseError (UnknownError s) = "Unknown error: " ++ s
parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
parseError (NotAllowed s) = "NotAllowed error: " ++ s
parseError (GhcException s) = "GHC Exception: " ++ s
