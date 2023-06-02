module Editor.Hint where

--import Control.Exception  (SomeException)
import Control.Monad.Catch (catch)
import Control.Concurrent.MVar  (MVar, putMVar, takeMVar)


import Language.Haskell.Interpreter as Hint

import Data.List (intercalate)

import Sound.Tidal.Context (ControlPattern)

data InterpreterMessage = MMini String
                        | MDef String
                        | MType String

data InterpreterResponse = RMini ControlPattern
                         | RType String
                         | RError String
                         | RSucc
                         deriving Show

exts :: [Extension]
exts = [OverloadedStrings, BangPatterns, MonadComprehensions, LambdaCase, ExtendedDefaultRules, NoMonomorphismRestriction]

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
staticInterpreter = do
        Hint.set [languageExtensions := exts]
        Hint.loadModules ["src/Meta.hs","src/Prelude/MiniPrelude.hs"]
        Hint.setTopLevelModules ["Meta","Prelude.MiniPrelude"]
        --Hint.runStmt "default (Pattern Int, Pattern String)"

interpreterLoop :: MVar InterpreterMessage -> MVar InterpreterResponse -> Interpreter ()
interpreterLoop mMV rMV = do
                    cont <- liftIO $ takeMVar mMV
                    case cont of
                      MMini s -> catch (interpretMini s rMV) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)
                      MType s -> catch (interpretType s rMV) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)
                      MDef s -> catch ((Hint.runStmt s) >> (liftIO $ putMVar rMV $ RSucc)) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)
                    interpreterLoop mMV rMV


interpretMini :: String -> MVar InterpreterResponse -> Interpreter ()
interpretMini s rMV = do
                      p <- Hint.interpret s (Hint.as :: ControlPattern)
                      liftIO $ putMVar rMV $ RMini p

interpretType :: String -> MVar InterpreterResponse -> Interpreter ()
interpretType cont rMV = do
                  t <- Hint.typeChecksWithDetails cont
                  case t of
                    Left errors -> liftIO $ putMVar rMV $ RError $ intercalate "\n" $ map errMsg errors
                    Right out -> liftIO $ putMVar rMV $ RType out


parseError:: InterpreterError -> String
parseError (UnknownError s) = "Unknown error: " ++ s
parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
parseError (NotAllowed s) = "NotAllowed error: " ++ s
parseError (GhcException s) = "GHC Exception: " ++ s
