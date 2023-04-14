module Editor.Hint where

import Control.Exception  (SomeException)
import Control.Monad.Catch (catch)
import Control.Concurrent.MVar  (MVar, putMVar, takeMVar)


import Language.Haskell.Interpreter as Hint

import Data.List (intercalate)

import Sound.Tidal.Context (Pattern)

import qualified Functional as F

type InterpreterMessage = String

data InterpreterResponse = RMini (Pattern Int)
                         | RError String
                         deriving Show

hintJob :: MVar InterpreterMessage -> MVar InterpreterResponse -> IO ()
hintJob mMV rMV = do
                result <- catch (Hint.runInterpreter $ staticInterpreter >> (interpreterLoop mMV rMV))
                          (\e -> return (Left $ UnknownError $ show (e :: SomeException)))
                -- can this happen? If it happens all definitions made interactively are lost...
                let response = case result of
                        Left err -> RError (parseError err)
                        Right p  -> RError (show p)
                putMVar rMV response
                hintJob mMV rMV

staticInterpreter :: Interpreter ()
staticInterpreter = Hint.loadModules ["src/Functional.hs","src/MiniPrelude.hs", "src/Tidal.hs"] >> Hint.setTopLevelModules ["Functional","MiniPrelude","Tidal"]

interpreterLoop :: MVar InterpreterMessage -> MVar InterpreterResponse -> Interpreter ()
interpreterLoop mMV rMV = do
                    cont <- liftIO $ takeMVar mMV
                    catch (interpretMini cont rMV) (\e -> liftIO $ putMVar rMV $ RError $ show (e :: SomeException))
                    interpreterLoop mMV rMV


interpretMini :: String -> MVar InterpreterResponse -> Interpreter ()
interpretMini s rMV = do
                      p <- Hint.interpret s (Hint.as :: Pattern Int)
                      liftIO $ putMVar rMV $ RMini p



parseError:: InterpreterError -> String
parseError (UnknownError s) = "Unknown error: " ++ s
parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
parseError (NotAllowed s) = "NotAllowed error: " ++ s
parseError (GhcException s) = "GHC Exception: " ++ s
