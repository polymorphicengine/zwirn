module Editor.Setup where

import Control.Monad  (void)

import Sound.Tidal.Context (Stream, Pattern)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (MVar, newEmptyMVar, newMVar)

import Data.IORef (newIORef)

import Graphics.UI.Threepenny.Core as C hiding (text)

import Editor.Backend
import Editor.UI

import Zwirn.Language.Hint
import Zwirn.Language.Compiler
import Zwirn.Language.TypeCheck.Infer
-- import Editor.Hint

setupBackend :: Stream -> HintMode -> UI Env
setupBackend str mode = do

       hyd <- liftIO $ newMVar (pure "solid().out()")
       env <- startInterpreter str mode hyd

       win <- askWindow

       createHaskellFunction "evaluateBlock" (\cm -> (runUI win $ interpretCommands cm False env))
       createHaskellFunction "evaluateLine" (\cm -> (runUI win $ interpretCommands cm True env))

       createHaskellFunction "evaluateBlockLine" (\cm l ->  (runUI win $ interpretCommandsLine cm False l env))
       createHaskellFunction "evaluateLineLine" (\cm l -> (runUI win $ interpretCommandsLine cm True l env))
       return env


startInterpreter :: Stream -> HintMode -> MVar (Pattern String) -> UI Env
startInterpreter str mode hyd = do
           win <- askWindow
           mMV <- liftIO newEmptyMVar
           rMV <- liftIO newEmptyMVar
           void $ liftIO $ forkIO $ hintJob mode mMV rMV
           let compEnv = (Environment str (Just $ hyd) defaultEnv (HintEnv mode mMV rMV))
           return $ Env win str compEnv hyd

createShortcutFunctions :: Stream -> Element -> UI ()
createShortcutFunctions str mainEditor = do
                       editorsRef <- liftIO $ newIORef [mainEditor]
                       win <- askWindow
                       createHaskellFunction "addEditor" (runUI win $ addEditor editorsRef)
                       createHaskellFunction "removeEditor" (runUI win $ removeEditor editorsRef)

                       createHaskellFunction "hush" (hush str)
