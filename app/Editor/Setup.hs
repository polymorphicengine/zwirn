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

import Control.Monad  (void)

import Sound.Tidal.Context hiding (mute,solo,(#),s)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (MVar, newEmptyMVar, newMVar)

import Data.IORef (newIORef)

import Graphics.UI.Threepenny.Core as C hiding (text, defaultConfig)


import Editor.Backend
import Editor.Frontend
import Editor.UI
import Editor.Highlight
import Editor.Hydra

import Zwirn.Language.Hint
import Zwirn.Language.Compiler
import Zwirn.Language.Default

import Zwirn.Interactive.Types (Text (..))


setup :: Int -> HintMode -> Window -> UI ()
setup dport mode win = void $ do

     editor <- frontend win
     setupEditors editor

     catchJSErrors

     str <- setupStream dport

     setupHighlighter str
     hyd <- setupHydra str
     (mMV, rMV) <- setupHint mode

     createHaskellFunction "hush" (hush str)
     setupBackend str hyd mode mMV rMV
     addFileInputAndSettings
     makeEditor "editor0"

setupStream :: Int -> UI Stream
setupStream dport = liftIO $ startTidal (superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = dport}) (defaultConfig {cVerbose = True, cFrameTimespan = 1/20})

setupHighlighter :: Stream -> UI ()
setupHighlighter str = do
  win <- askWindow
  buf <- liftIO $ newMVar []
  high <- liftIO $ newMVar True
  createHaskellFunction "toggleHighlight" (runUI win $ toggleHighlight high buf)
  void $ liftIO $ forkIO $ highlightLoop win str buf

setupHydra :: Stream -> UI (MVar (Pattern Text))
setupHydra str = do
  startHydra
  win <- askWindow
  hydBuf <- liftIO $ newMVar (Text "")
  hyd <- liftIO $ newMVar (pure $ Text "solid().out()")
  void $ liftIO $ forkIO $ hydraLoop win str hyd hydBuf
  return hyd

setupHint :: HintMode -> UI (MVar InterpreterMessage, MVar InterpreterResponse)
setupHint mode = do
      mMV <- liftIO newEmptyMVar
      rMV <- liftIO newEmptyMVar
      void $ liftIO $ forkIO $ hintJob mode mMV rMV
      return (mMV, rMV)

setupBackend :: Stream -> MVar (Pattern Text) -> HintMode -> MVar InterpreterMessage -> MVar InterpreterResponse -> UI ()
setupBackend str hyd mode mMV rMV = do

       win <- askWindow
       envMV <- liftIO $ newMVar (Environment str (Just $ hyd) defaultTypeEnv (HintEnv mode mMV rMV))

       createHaskellFunction "evaluateBlock" (\cm -> (runUI win $ interpretCommands cm False envMV))
       createHaskellFunction "evaluateLine" (\cm -> (runUI win $ interpretCommands cm True envMV))

       createHaskellFunction "evaluateBlockLine" (\cm l ->  (runUI win $ interpretCommandsLine cm False l envMV))
       createHaskellFunction "evaluateLineLine" (\cm l -> (runUI win $ interpretCommandsLine cm True l envMV))


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
              void $ (element body) #+ [ fileInput
                                       , tidalSettings
                                       ]
