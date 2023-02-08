module Editor.Setup where

import Control.Monad  (void)

import Sound.Tidal.Context (Stream)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (newEmptyMVar)

import Data.IORef (newIORef)

import Graphics.UI.Threepenny.Core as C hiding (text)

import Editor.Backend
import Editor.UI
import Editor.Hint

setupBackend :: Stream -> UI ()
setupBackend str = do

       env <- startInterpreter str

       win <- askWindow

       createHaskellFunction "evaluateBlock" (\cm -> (runUI win $ interpretCommands cm False env))
       createHaskellFunction "evaluateLine" (\cm -> (runUI win $ interpretCommands cm True env))

       createHaskellFunction "evaluateBlockLine" (\cm l ->  (runUI win $ interpretCommandsLine cm False l env))
       createHaskellFunction "evaluateLineLine" (\cm l -> (runUI win $ interpretCommandsLine cm True l env))


startInterpreter :: Stream -> UI Env
startInterpreter str = do
           win <- askWindow
           mMV <- liftIO newEmptyMVar
           rMV <- liftIO newEmptyMVar
           void $ liftIO $ forkIO $ hintJob mMV rMV
           return $ Env win str mMV rMV

createShortcutFunctions :: Stream -> Element -> UI ()
createShortcutFunctions str mainEditor = do
                       editorsRef <- liftIO $ newIORef [mainEditor]
                       win <- askWindow
                       createHaskellFunction "addEditor" (runUI win $ addEditor editorsRef)
                       createHaskellFunction "removeEditor" (runUI win $ removeEditor editorsRef)

                       createHaskellFunction "hush" (hush str)
