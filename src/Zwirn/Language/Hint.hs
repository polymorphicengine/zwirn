module Zwirn.Language.Hint
    ( HintMode (..)
    , InterpreterMessage (..)
    , InterpreterResponse (..)
    , hintJob
    )where

--import Control.Exception  (SomeException)
import Control.Monad.Catch (catch)
import Control.Concurrent.MVar  (MVar, putMVar, takeMVar)

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint

import Data.List (intercalate)

import Sound.Tidal.Context (Pattern, ControlPattern)

data HintMode = GHC | NoGHC deriving (Eq,Show)

data InterpreterMessage = MPat String
                        | MDef String
                        | MJS String
                        deriving (Show, Eq)

data InterpreterResponse = RPat ControlPattern
                         | RJS (Pattern String)
                         | RError String
                         | RSucc
                         deriving (Show, Eq)

exts :: [Extension]
exts = [OverloadedStrings, BangPatterns, MonadComprehensions, LambdaCase, ExtendedDefaultRules, NoMonomorphismRestriction, NoImplicitPrelude]

modulePaths :: String -> [String]
modulePaths path = map (path ++) ["src/Zwirn/Interactive.hs", "src/Zwirn/Interactive/Meta.hs", "src/Zwirn/Interactive/Generic.hs", "src/Zwirn/Interactive/Prelude/Control.hs", "src/Zwirn/Interactive/Prelude/Hydra.hs", "src/Zwirn/Interactive/Prelude/MiniPrelude.hs", "src/Zwirn/Interactive/Prelude/Params.hs"]

moduleNames :: [String]
moduleNames =  ["Zwirn.Interactive"]

ghcArgs :: String -> [String]
ghcArgs path = ["-clear-package-db", "-package-db", path ++ "haskell-libs/package.conf.d", "-package-db", path ++ "haskell-libs/package.db", "-v"]

runUnsafeInterpreter :: Interpreter a -> IO (Either InterpreterError a)
runUnsafeInterpreter interpreter = do
  execPath <- dropFileName <$> getExecutablePath
  Hint.unsafeRunInterpreterWithArgsLibdir (ghcArgs execPath) (execPath ++ "haskell-libs") interpreter

hintJob :: HintMode -> MVar InterpreterMessage -> MVar InterpreterResponse -> IO ()
hintJob mode mMV rMV = do
                execPath <- dropFileName <$> getExecutablePath
                let (runner, path) = case mode of
                                      GHC -> (Hint.runInterpreter, "")
                                      NoGHC -> (runUnsafeInterpreter, execPath)
                result <- catch (runner $ (staticInterpreter path) >> (interpreterLoop mMV rMV))
                          (\e -> return (Left $ UnknownError $ show (parseError e)))
                -- can this happen? If it happens all definitions made interactively are lost...
                let response = case result of
                        Left err -> RError (parseError err)
                        Right p  -> RError (show p)
                putMVar rMV response
                hintJob mode mMV rMV

staticInterpreter :: String -> Interpreter ()
staticInterpreter path = do
        Hint.set [languageExtensions := exts]
        Hint.loadModules (modulePaths path)
        Hint.setTopLevelModules moduleNames

interpreterLoop :: MVar InterpreterMessage -> MVar InterpreterResponse -> Interpreter ()
interpreterLoop mMV rMV = do
                    cont <- liftIO $ takeMVar mMV
                    case cont of
                      MPat s -> catch (interpretPat s rMV) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)
                      MDef s -> catch ((Hint.runStmt s) >> (liftIO $ putMVar rMV $ RSucc)) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)
                      MJS s -> catch (interpretJS s rMV) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)
                    interpreterLoop mMV rMV


interpretPat :: String -> MVar InterpreterResponse -> Interpreter ()
interpretPat s rMV = do
                p <- Hint.interpret s (Hint.as :: ControlPattern)
                liftIO $ putMVar rMV $ RPat p


interpretJS :: String -> MVar InterpreterResponse -> Interpreter ()
interpretJS s rMV = do
                p <- Hint.interpret s (Hint.as :: Pattern String)
                liftIO $ putMVar rMV $ RJS p

parseError :: InterpreterError -> String
parseError (UnknownError s) = "Unknown error: " ++ s
parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
parseError (NotAllowed s) = "NotAllowed error: " ++ s
parseError (GhcException s) = "GHC Exception: " ++ s
