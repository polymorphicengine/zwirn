{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Zwirn.Language.Hint
  ( HintMode (..),
    InterpreterMessage (..),
    InterpreterResponse (..),
    MessageType (..),
    FromResponse,
    hintJob,
    fromResponse,
  )
where

{-
    Hint.hs - interactive haskell interpreter, adapted from
    https://github.com/tidalcycles/Tidal/tree/main/tidal-listener
    Copyright (C) 2023, Martin Gius

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

-- import Control.Exception  (SomeException)

import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.Monad.Catch (catch)
import Data.List (intercalate)
import Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe as Hint
import Sound.Tidal.Context (ControlPattern)
import System.Environment (getExecutablePath)
import System.FilePath (dropFileName)
import Zwirn.Interactive.Types (NumberPattern, TextPattern)

data HintMode = GHC | NoGHC deriving (Eq, Show)

data MessageType = AsNum | AsText | AsVM | AsDef deriving (Eq, Show)

data InterpreterMessage = Message MessageType String
  deriving (Show, Eq)

data InterpreterResponse
  = RVM ControlPattern
  | RText TextPattern
  | RNum NumberPattern
  | RError String
  | RSucc

-- deriving (Show, Eq)

class FromResponse a where
  fromResponse :: InterpreterResponse -> Either String a

instance FromResponse ControlPattern where
  fromResponse (RVM p) = return p
  fromResponse (RError err) = Left err
  fromResponse _ = Left "Unkown Hint Error"

instance FromResponse TextPattern where
  fromResponse (RText p) = return p
  fromResponse (RError err) = Left err
  fromResponse _ = Left "Unkown Hint Error"

instance FromResponse NumberPattern where
  fromResponse (RNum p) = return p
  fromResponse (RError err) = Left err
  fromResponse _ = Left "Unkown Hint Error"

instance FromResponse () where
  fromResponse RSucc = return ()
  fromResponse (RError err) = Left err
  fromResponse _ = Left "Unkown Hint Error"

exts :: [Extension]
exts = [NoImplicitPrelude, ExtendedDefaultRules, NoMonomorphismRestriction]

modulePaths :: String -> [String]
modulePaths path =
  map
    (path ++)
    [ "src/Zwirn/Interactive.hs",
      "src/Zwirn/Interactive/Types.hs",
      "src/Zwirn/Interactive/Convert.hs",
      "src/Zwirn/Interactive/HydraT.hs",
      "src/Zwirn/Interactive/TidalT.hs",
      "src/Zwirn/Interactive/Transform.hs",
      "src/Zwirn/Interactive/Generic.hs",
      "src/Zwirn/Interactive/Prelude.hs",
      "src/Zwirn/Interactive/Prelude/Chords.hs",
      "src/Zwirn/Interactive/Prelude/Core.hs",
      "src/Zwirn/Interactive/Prelude/Control.hs",
      "src/Zwirn/Interactive/Prelude/Hydra.hs",
      "src/Zwirn/Interactive/Prelude/MiniPrelude.hs",
      "src/Zwirn/Interactive/Prelude/Params.hs"
    ]

moduleNames :: [String]
moduleNames = ["Zwirn.Interactive"]

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
  result <-
    catch
      (runner $ (staticInterpreter path) >> (interpreterLoop mMV rMV))
      (\e -> return (Left $ UnknownError $ show (parseError e)))
  -- can this happen? If it happens all definitions made interactively are lost...
  let response = case result of
        Left err -> RError (parseError err)
        Right p -> RError (show p)
  putMVar rMV response
  hintJob mode mMV rMV

staticInterpreter :: String -> Interpreter ()
staticInterpreter path = do
  Hint.set [languageExtensions := exts]
  Hint.loadModules (modulePaths path)
  Hint.setTopLevelModules moduleNames

interpreterLoop :: MVar InterpreterMessage -> MVar InterpreterResponse -> Interpreter ()
interpreterLoop mMV rMV = do
  (Message typ s) <- liftIO $ takeMVar mMV
  case typ of
    AsVM -> catch (interpretVM s rMV) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)
    AsText -> catch (interpretText s rMV) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)
    AsNum -> catch (interpretNum s rMV) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)
    AsDef -> catch ((Hint.runStmt s) >> (liftIO $ putMVar rMV $ RSucc)) (\e -> liftIO $ putMVar rMV $ RError $ parseError e)
  interpreterLoop mMV rMV

interpretVM :: String -> MVar InterpreterResponse -> Interpreter ()
interpretVM s rMV = do
  p <- Hint.interpret s (Hint.as :: ControlPattern)
  liftIO $ putMVar rMV $ RVM p

interpretText :: String -> MVar InterpreterResponse -> Interpreter ()
interpretText s rMV = do
  p <- Hint.interpret s (Hint.as :: TextPattern)
  liftIO $ putMVar rMV $ RText p

interpretNum :: String -> MVar InterpreterResponse -> Interpreter ()
interpretNum s rMV = do
  p <- Hint.interpret s (Hint.as :: NumberPattern)
  liftIO $ putMVar rMV $ RNum p

parseError :: InterpreterError -> String
parseError (UnknownError s) = "Unknown error: " ++ s
parseError (WontCompile es) = "Compile error: " ++ (intercalate "\n" (Prelude.map errMsg es))
parseError (NotAllowed s) = "NotAllowed error: " ++ s
parseError (GhcException s) = "GHC Exception: " ++ s
