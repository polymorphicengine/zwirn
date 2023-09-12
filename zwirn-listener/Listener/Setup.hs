{-# LANGUAGE RecordWildCards #-}
module Listener.Setup (setup, Remote, Local) where


import Sound.Tidal.Context hiding (mute,solo,(#),s, Config)

import Control.Monad  (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (MVar, newEmptyMVar, newMVar)

import qualified Network.Socket as N
import Sound.OSC.FD as O

import Listener.CommandLine

import Zwirn.Language.Hint
import Zwirn.Language.Compiler
import Zwirn.Language.Default

type Remote = N.SockAddr
type Local = UDP

setup :: Config -> IO (Remote, Local, MVar Environment)
setup c = do
  str <- setupStream c
  (mMV, rMV) <- setupHint (hintMode c)
  envMV <- setupEnvironment str (hintMode c) mMV rMV
  (remoteAddr, localAddr) <- setupListener c
  return (remoteAddr, localAddr, envMV)


setupHint :: HintMode -> IO (MVar InterpreterMessage, MVar InterpreterResponse)
setupHint mode = do
      mMV <- newEmptyMVar
      rMV <- newEmptyMVar
      void $ forkIO $ hintJob mode mMV rMV
      return (mMV, rMV)

setupEnvironment :: Stream -> HintMode -> MVar InterpreterMessage -> MVar InterpreterResponse -> IO (MVar Environment)
setupEnvironment str mode mMV rMV = do
       let env = Environment str Nothing defaultTypeEnv (HintEnv mode mMV rMV) Nothing Nothing
       envMV <- newMVar env
       return envMV

setupListener :: Config -> IO (Remote, Local)
setupListener Config{..} = do
        (remote_addr:_) <- N.getAddrInfo Nothing (Just "127.0.0.1") Nothing
        local <- udpServer "127.0.0.1" listenPort
        let (N.SockAddrInet _ a) = N.addrAddress remote_addr
            remote = N.SockAddrInet (fromIntegral remotePort) a
        return (remote,local)


setupStream :: Config -> IO Stream
setupStream Config{..} = startStream defaultConfig
                                        [(superdirtTarget {oPort = dirtPort, oLatency = 0.1}, [superdirtShape])
                                        ,(editorTarget remotePort, [OSCContext "/code/highlight"])
                                        ]

editorTarget :: Int -> Target
editorTarget rPort = Target {oName = "editor"
                            ,oAddress = "127.0.0.1"
                            ,oPort = rPort
                            ,oBusPort = Nothing
                            ,oLatency = 0.1
                            ,oWindow = Nothing
                            ,oSchedule = Live
                            ,oHandshake = False
                            }
