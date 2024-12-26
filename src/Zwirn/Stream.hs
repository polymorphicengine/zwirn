{-# OPTIONS_GHC -Wno-type-defaults #-}

module Zwirn.Stream where

import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.Socket as N
import qualified Sound.Osc.Fd as O
import Sound.Tidal.Clock
import Sound.Tidal.Link
import Sound.Zwirn.Core.Cord
import Sound.Zwirn.Core.Query
import qualified Sound.Zwirn.Time as Z
import Zwirn.Language.Evaluate

data Stream = Stream {sCord :: MVar (Cord ExpressionMap Double), sState :: MVar ExpressionMap}

type RemoteAddress = N.SockAddr

streamReplace :: Stream -> Cord ExpressionMap Double -> IO ()
streamReplace str p = modifyMVar_ (sCord str) (const $ pure p)

streamSet :: Stream -> T.Text -> Expression -> IO ()
streamSet str x ex = modifyMVar_ (sState str) (return . Map.insert x ex)

send :: RemoteAddress -> O.Udp -> Double -> Double -> IO ()
send remote local c x = O.sendTo local (O.p_message "/zwirn" [O.float c, O.float x]) remote

startStream :: Stream -> IO ()
startStream str = do
  let target_address = "127.0.0.1"
      target_port = 2323
  remote <- resolve target_address (show target_port)
  local <-
    O.udp_socket
      ( \sock sockaddr -> do
          N.setSocketOption sock N.Broadcast 1
          N.connect sock sockaddr
      )
      target_address
      target_port

  _ <- clocked defaultConfig (tickAction str (N.addrAddress remote) local)
  return ()

tickAction :: Stream -> RemoteAddress -> O.Udp -> (Time, Time) -> Double -> ClockConfig -> ClockRef -> (SessionState, SessionState) -> IO ()
tickAction str remote local (star, end) _ _ _ _ = do
  p <- readMVar (sCord str)
  st <- readMVar (sState str)
  let vs = map snd $ findAllValuesWithTime (Z.Time (align star) 1, Z.Time (align end) 1) st p
  mapM_ (send remote local (fromIntegral $ floor star)) vs

resolve :: String -> String -> IO N.AddrInfo
resolve host port = do
  let hints = N.defaultHints {N.addrSocketType = N.Stream}
  addr : _ <- N.getAddrInfo (Just hints) (Just host) (Just port)
  return addr

align :: Time -> Time
align t = fromIntegral (floor $ t / 0.001) * 0.001
