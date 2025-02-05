{-# LANGUAGE OverloadedStrings #-}

module Editor.Setup (setup) where

{-
    Setup.hs - setup of the various components of the backend
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

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (void)
import Data.IORef (newIORef)
import qualified Data.Map as Map
import Data.Text (pack)
import Editor.Backend
import Editor.Config
import Editor.Frontend
import Editor.Highlight (highlightLoop, toggleHighlight)
import Editor.UI
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (defaultConfig, text)
import System.Directory.OsPath (doesDirectoryExist, doesFileExist, listDirectory)
import System.OsPath (decodeUtf, encodeUtf)
import Zwirn.Language.Builtin.Prelude
import Zwirn.Language.Compiler
import Zwirn.Stream

setup :: FullConfig -> Window -> UI ()
setup config win = void $ do
  editor <- frontend win
  setupEditors editor

  str <- setupStream config

  setupHighlight config str

  setupBackend (fullConfigEditor config) str
  addFileInputAndSettings
  makeEditor "editor0"

setupStream :: FullConfig -> UI Stream
setupStream config = do
  mv <- liftIO $ newMVar Map.empty
  m <- liftIO $ newMVar Map.empty
  liftIO $ startStream (fullConfigStream config) mv m (toClock $ fullConfigClock config)

setupHighlight :: FullConfig -> Stream -> UI ()
setupHighlight config str = do
  win <- askWindow
  bufMV <- liftIO $ newMVar []
  boolMV <- liftIO $ newMVar False
  void $ liftIO $ forkIO $ highlightLoop win str (toClock $ fullConfigClock config) (sClockRef str) bufMV

  createHaskellFunction "toggleHighlight" (runUI win $ toggleHighlight boolMV bufMV)
  (if editorConfigHighlight $ fullConfigEditor config then return () else toggleHighlight boolMV bufMV)

setupBackend :: EditorConfig -> Stream -> UI ()
setupBackend config str = do
  win <- askWindow
  let env = Environment str builtinEnvironment (Just $ ConfigEnv configPath resetConfig) Nothing

  bootEnv <- checkBoot config env

  envMV <- liftIO $ newMVar bootEnv

  createHaskellFunction "evalBlockAtCursor" (\cm -> runUI win $ evalContentAtCursor EvalBlock cm envMV)
  createHaskellFunction "evalLineAtCursor" (\cm -> runUI win $ evalContentAtCursor EvalLine cm envMV)
  createHaskellFunction "evalWhole" (\cm -> runUI win $ evalContentAtCursor EvalWhole cm envMV)

  createHaskellFunction "evalBlockAtLine" (\cm l -> runUI win $ evalContentAtLine EvalBlock cm l envMV)
  createHaskellFunction "evalLineAtLine" (\cm l -> runUI win $ evalContentAtLine EvalLine cm l envMV)

  createHaskellFunction "hush" (liftIO $ return () :: IO ())

setupEditors :: Element -> UI ()
setupEditors mainEditor = do
  editorsRef <- liftIO $ newIORef [mainEditor]
  win <- askWindow
  createHaskellFunction "addEditor" (runUI win $ addEditor editorsRef)
  createHaskellFunction "removeEditor" (runUI win $ removeEditor editorsRef)

addFileInputAndSettings :: UI ()
addFileInputAndSettings = do
  win <- askWindow
  body <- getBody win
  void $
    element body
      #+ [ fileInput,
           tidalSettings
         ]

checkBoot :: EditorConfig -> Environment -> UI Environment
checkBoot (EditorConfig _ _ "") env = return env
checkBoot (EditorConfig _ _ path) env = do
  ospath <- encodeUtf path
  isfile <- liftIO $ doesFileExist ospath
  ps <-
    if isfile
      then return $ decodeUtf ospath
      else do
        isfolder <- liftIO $ doesDirectoryExist ospath
        if isfolder
          then do
            pss <- liftIO $ listDirectory ospath
            fs <- mapM decodeUtf pss
            return $ map (\f -> path ++ "/" ++ f) fs
          else return []
  res <- liftIO $ runCI env (compilerInterpreterBoot $ map pack ps)
  case res of
    Left (CIError err newEnv) -> getOutputEl # set UI.text ("Error in Bootfile: " ++ err) >> return newEnv
    Right newEnv ->
      if ps /= []
        then getOutputEl # set UI.text ("Successfully loaded Bootfiles from " ++ path) >> return newEnv
        else getOutputEl # set UI.text ("No Bootfiles found at " ++ path) >> return newEnv
