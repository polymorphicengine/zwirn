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
import Control.Concurrent.MVar  (MVar, newEmptyMVar, newMVar, takeMVar, putMVar)

import Data.IORef (newIORef)

import Graphics.UI.Threepenny.Core as C hiding (text, defaultConfig)
import qualified Graphics.UI.Threepenny.Core as C


import Editor.Backend
import Editor.Frontend
import Editor.UI
import Editor.Highlight
import Editor.Hydra

import Zwirn.Language.Hint
import Zwirn.Language.Compiler
import Zwirn.Language.Default

import Zwirn.Interactive.Types (Text (..))


setup :: HintMode -> Window -> UI ()
setup mode win = void $ do

     editor <- frontend win
     setupEditors editor

     catchJSErrors

     str <- setupStream

     setupHighlighter str
     hyd <- setupHydra str
     (mMV, rMV) <- setupHint mode

     createHaskellFunction "hush" (hush str hyd)
     envMV <- setupBackend str hyd mode mMV rMV
     addFileInputAndSettings
     makeEditor "editor0"
     loadBootDefs envMV

setupStream :: UI Stream
setupStream  = do
  target <- configureTarget
  conf <- configureStream
  liftIO $ startTidal target conf

setupHighlighter :: Stream -> UI ()
setupHighlighter str = do
  win <- askWindow
  buf <- liftIO $ newMVar []
  high <- liftIO $ newMVar True
  createHaskellFunction "toggleHighlight" (runUI win $ toggleHighlight high buf)
  void $ liftIO $ forkIO $ highlightLoop win str buf
  bool <- getHighlight
  case bool of
    True -> return ()
    False -> toggleHighlight high buf

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

setupBackend :: Stream -> MVar (Pattern Text) -> HintMode -> MVar InterpreterMessage -> MVar InterpreterResponse -> UI (MVar Environment)
setupBackend str hyd mode mMV rMV = do

       win <- askWindow
       let env = Environment str (Just $ hyd) defaultTypeEnv (HintEnv mode mMV rMV) (Just $ ConfigEnv (setConfig win) (clearConfig win)) Nothing

       envMV <- liftIO $ newMVar env

       createHaskellFunction "evalBlockAtCursor" (\cm -> (runUI win $ evalContentAtCursor EvalBlock cm envMV))
       createHaskellFunction "evalLineAtCursor" (\cm -> (runUI win $ evalContentAtCursor EvalLine cm envMV))
       createHaskellFunction "evalWhole" (\cm -> (runUI win $ evalContentAtCursor EvalWhole cm envMV))

       createHaskellFunction "evalBlockAtLine" (\cm l ->  (runUI win $ evalContentAtLine EvalBlock cm l envMV))
       createHaskellFunction "evalLineAtLine" (\cm l -> (runUI win $ evalContentAtLine EvalLine cm l envMV))
       return envMV

loadBootDefs :: MVar Environment -> UI ()
loadBootDefs envMV = do
  env <- liftIO $ takeMVar envMV
  mps <- getBootPaths
  case mps of
    Just ps -> do
      x <- liftIO $ runCI env $ compilerInterpreterBoot ps
      case x of
        Left (CIError err _ ) -> do
          _ <- (getOutputEl # set C.text err)
          liftIO $ putMVar envMV env
        Right env' -> do
          _ <- (getOutputEl # set C.text "successfully loaded boot file(s)!")
          liftIO $ putMVar envMV env'
    Nothing -> liftIO $ putMVar envMV env

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
