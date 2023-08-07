module Editor.Setup (setup) where

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
import Zwirn.Language.TypeCheck.Infer

setup :: Int -> HintMode -> Window -> UI ()
setup dport mode win = void $ do

     editor <- frontend win
     setupEditors editor

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

setupHydra :: Stream -> UI (MVar (Pattern String))
setupHydra str = do
  startHydra
  win <- askWindow
  hydBuf <- liftIO $ newMVar ""
  hyd <- liftIO $ newMVar (pure "solid().out()")
  void $ liftIO $ forkIO $ hydraLoop win str hyd hydBuf
  return hyd

setupHint :: HintMode -> UI (MVar InterpreterMessage, MVar InterpreterResponse)
setupHint mode = do
      mMV <- liftIO newEmptyMVar
      rMV <- liftIO newEmptyMVar
      void $ liftIO $ forkIO $ hintJob mode mMV rMV
      return (mMV, rMV)

setupBackend :: Stream -> MVar (Pattern String) -> HintMode -> MVar InterpreterMessage -> MVar InterpreterResponse -> UI ()
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
