{-# OPTIONS_GHC -Wno-type-defaults #-}

module Zwirn.Stream where

import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.Socket as N
import qualified Sound.Osc as O
import qualified Sound.Osc.Transport.Fd.Udp as O
import Sound.Tidal.Clock
import qualified Sound.Tidal.Clock as Clock
import Sound.Tidal.Link
import Zwirn.Core.Query
import qualified Zwirn.Core.Time as Z
import Zwirn.Language.Evaluate

data Stream = Stream {sCord :: MVar (Zwirn Expression), sState :: MVar ExpressionMap}

type RemoteAddress = N.SockAddr

streamReplace :: Stream -> Zwirn Expression -> IO ()
streamReplace str p = modifyMVar_ (sCord str) (const $ pure p)

streamSet :: Stream -> T.Text -> Expression -> IO ()
streamSet str x ex = modifyMVar_ (sState str) (return . Map.insert x ex)

startStream :: Stream -> IO ()
startStream str = do
  let target_address = "127.0.0.1"
      target_port = 57120
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
tickAction str remote local (star, end) nudge cconf cref (ss, _) = do
  p <- readMVar (sCord str)
  st <- readMVar (sState str)
  let qs = findAllValuesWithTimeState (Z.Time (align star) 1, Z.Time (align end) 1) st p
      vs = map (\(t, v, _) -> (t, v)) qs
      sts = map (\(_, _, x) -> x) qs

  -- TODO: what about race conditions?
  updateState (sState str) sts
  mapM_ (processAndSend remote local nudge cconf cref ss) vs

resolve :: String -> String -> IO N.AddrInfo
resolve host port = do
  let hints = N.defaultHints {N.addrSocketType = N.Stream}
  addr : _ <- N.getAddrInfo (Just hints) (Just host) (Just port)
  return addr

align :: Time -> Time
align t = fromIntegral (floor $ t / 0.001) * 0.001

expressionToOSC :: Expression -> [O.Datum]
expressionToOSC (ENum n) = [O.float n]
expressionToOSC (EText n) = [O.string $ T.unpack n]
expressionToOSC (EMap m) = concatMap (\(k, v) -> O.string (T.unpack k) : expressionToOSC v) $ Map.toList m
expressionToOSC _ = []

additionalData :: Double -> [O.Datum]
additionalData cyc = [O.string "cps", O.float 0.525, O.string "cycle", O.float cyc, O.string "delta", O.float 1]

expressionToMessage :: Double -> Expression -> O.Message
expressionToMessage cyc ex = O.message "/dirt/play" (additionalData cyc ++ expressionToOSC ex)

sendMessage :: RemoteAddress -> O.Udp -> Double -> Double -> (Double, O.Message) -> IO ()
sendMessage remote local latency extraLatency (time, m) = sendBndl remote local $ O.Bundle timeWithLatency [m]
  where
    timeWithLatency = time - latency + extraLatency

sendBndl :: RemoteAddress -> O.Udp -> O.Bundle -> IO ()
sendBndl remote local bndl = O.sendTo local (O.Packet_Bundle bndl) remote

defaultLatency :: Double
defaultLatency = 0.2

processAndSend :: RemoteAddress -> O.Udp -> Double -> ClockConfig -> ClockRef -> SessionState -> (Z.Time, Expression) -> IO ()
processAndSend remote local nudge cconf cref ss (t, ex) = do
  let cyc = fromIntegral $ floor t
      msg = expressionToMessage cyc ex

  let onBeat = Clock.cyclesToBeat cconf (realToFrac ((\(Z.Time r _) -> fromRational r) t))

  on <- Clock.timeAtBeat cconf ss onBeat
  onOSC <- Clock.linkToOscTime cref on

  sendMessage remote local defaultLatency nudge (onOSC, msg)

updateState :: MVar ExpressionMap -> [ExpressionMap] -> IO ()
updateState _ [] = return ()
updateState stmv (st : _) = modifyMVar_ stmv (const $ return st)
