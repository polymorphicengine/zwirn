{-# LANGUAGE RecordWildCards #-}
module CLI.Setup (setup) where


import Sound.Tidal.Context hiding (mute,solo,(#),s, Config)

import Control.Monad  (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (MVar, newEmptyMVar, newMVar)

import CLI.CommandLine

import Zwirn.Language.Hint
import Zwirn.Language.Compiler
import Zwirn.Language.Default


setup :: Config -> IO (MVar Environment)
setup c = do
  str <- setupStream c
  (mMV, rMV) <- setupHint (hintMode c)
  envMV <- setupEnvironment str (hintMode c) mMV rMV
  return envMV


setupHint :: HintMode -> IO (MVar InterpreterMessage, MVar InterpreterResponse)
setupHint mode = do
      putStrLn $ show mode
      mMV <- newEmptyMVar
      rMV <- newEmptyMVar
      void $ forkIO $ hintJob mode mMV rMV
      return (mMV, rMV)

setupEnvironment :: Stream -> HintMode -> MVar InterpreterMessage -> MVar InterpreterResponse -> IO (MVar Environment)
setupEnvironment str mode mMV rMV = do
       let env = Environment str Nothing Nothing defaultTypeEnv (HintEnv mode mMV rMV) Nothing Nothing
       envMV <- newMVar env
       return envMV

setupStream :: Config -> IO Stream
setupStream Config{..} = startStream defaultConfig {cVerbose = False} [(superdirtTarget {oPort = dirtPort, oLatency = 0.1}, [superdirtShape])]
