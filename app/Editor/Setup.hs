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
import Editor.Backend
import Editor.Frontend
import Editor.Highlight (highlightLoop)
import Editor.UI
import Graphics.UI.Threepenny.Core as C hiding (defaultConfig, text)
import Sound.Tidal.Clock (defaultConfig)
import Zwirn.Core.Types (silence)
import Zwirn.Language.Compiler
import Zwirn.Language.Default
import Zwirn.Stream

setup :: Window -> UI ()
setup win = void $ do
  editor <- frontend win
  setupEditors editor

  catchJSErrors

  str <- setupStream

  setupBackend str
  addFileInputAndSettings
  makeEditor "editor0"

setupStream :: UI Stream
setupStream = do
  mv <- liftIO $ newMVar silence
  m <- liftIO $ newMVar Map.empty
  let str = Stream mv m
      conf = defaultConfig
  ref <- liftIO $ startStream str conf
  win <- askWindow
  bufMV <- liftIO $ newMVar []
  _ <- liftIO $ forkIO $ highlightLoop win str conf ref bufMV
  return str

setupBackend :: Stream -> UI ()
setupBackend str = do
  win <- askWindow
  let env = Environment str Nothing defaultTypeEnv primitives (Just $ ConfigEnv (setConfig win) (clearConfig win)) Nothing

  envMV <- liftIO $ newMVar env

  createHaskellFunction "evalBlockAtCursor" (\cm -> runUI win $ evalContentAtCursor EvalBlock cm envMV)
  createHaskellFunction "evalLineAtCursor" (\cm -> runUI win $ evalContentAtCursor EvalLine cm envMV)
  createHaskellFunction "evalWhole" (\cm -> runUI win $ evalContentAtCursor EvalWhole cm envMV)

  createHaskellFunction "evalBlockAtLine" (\cm l -> runUI win $ evalContentAtLine EvalBlock cm l envMV)
  createHaskellFunction "evalLineAtLine" (\cm l -> runUI win $ evalContentAtLine EvalLine cm l envMV)

  createHaskellFunction "hush" (liftIO $ return () :: IO ())
  createHaskellFunction "toggleHighlight" (liftIO $ return () :: IO ())

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
