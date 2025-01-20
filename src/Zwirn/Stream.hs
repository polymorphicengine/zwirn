{-# OPTIONS_GHC -Wno-type-defaults #-}

module Zwirn.Stream where

import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.Socket as N
import qualified Sound.Osc as O
import qualified Sound.Osc.Transport.Fd.Udp as O
import Sound.Tidal.Clock
import qualified Sound.Tidal.Clock as Clock
import Sound.Tidal.Link
import Zwirn.Core.Cord (stack)
import Zwirn.Core.Query
import qualified Zwirn.Core.Time as Z
import Zwirn.Language.Evaluate

type PlayMap = Map.Map Text (Zwirn Expression)

data Stream = Stream
  { sPlayMap :: MVar PlayMap,
    sState :: MVar ExpressionMap,
    sAddress :: RemoteAddress,
    sClockRef :: ClockRef,
    sClockConfig :: ClockConfig
  }

type RemoteAddress = N.SockAddr

streamReplace :: Stream -> Text -> Zwirn Expression -> IO ()
streamReplace str key p = modifyMVar_ (sPlayMap str) (return . Map.insert key p)

streamSet :: Stream -> T.Text -> Expression -> IO ()
streamSet str x ex = modifyMVar_ (sState str) (return . Map.insert x ex)

streamSetCPS :: Stream -> Time -> IO ()
streamSetCPS s = Clock.setCPS (sClockConfig s) (sClockRef s)

streamSetBPM :: Stream -> Time -> IO ()
streamSetBPM s = Clock.setBPM (sClockRef s)

-- streamFirst but with random cycle instead of always first cicle
-- streamOnce :: Stream -> Zwirn Expression -> IO ()
-- streamOnce st p = do i <- getStdRandom $ randomR (0, 8192)
--                      streamFirst st $ rotL (toRational (i :: Int)) p

-- streamFirst :: Stream -> Zwirn Expression -> IO ()
-- streamFirst str pat = Clock.clockOnce (tickAction str undefined) clockConfig clockRef

startStream :: MVar PlayMap -> MVar ExpressionMap -> ClockConfig -> IO Stream
startStream zMV stMV conf = do
  let target_address = "127.0.0.1"
      target_port = 57120
  remote <- resolve target_address (show target_port)
  local <- O.udp_server 2323

  cref <- clocked conf (tickAction zMV stMV (N.addrAddress remote) local)
  return $ Stream zMV stMV (N.addrAddress remote) cref conf

tickAction :: MVar PlayMap -> MVar ExpressionMap -> RemoteAddress -> O.Udp -> (Time, Time) -> Double -> ClockConfig -> ClockRef -> (SessionState, SessionState) -> IO ()
tickAction zMV stMV remote local (star, end) nudge cconf cref (ss, _) = do
  pm <- readMVar zMV
  let p = playMapToCord pm
  st <- readMVar stMV
  let qs = findAllValuesWithTimeState (Z.Time (align star) 1, Z.Time (align end) 1) st p
      vs = map (\(t, v, _) -> (t, v)) qs
      sts = map (\(_, _, x) -> x) qs

  -- TODO: what about race conditions?
  updateState stMV sts
  mapM_ (processAndSend remote local nudge cconf cref ss) vs

resolve :: String -> String -> IO N.AddrInfo
resolve host port = do
  let hints = N.defaultHints {N.addrSocketType = N.Stream}
  addr : _ <- N.getAddrInfo (Just hints) (Just host) (Just port)
  return addr

playMapToCord :: PlayMap -> Zwirn Expression
playMapToCord = stack . Map.elems

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
