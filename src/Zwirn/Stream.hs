{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Zwirn.Stream where

{-
    Stream.hs - query and send messages, code adapted from
    https://github.com/tidalcycles/Tidal/tree/dev/src/Sound/Tidal/Stream
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

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar, swapMVar)
import Control.Monad (when)
import Data.Bifunctor (second)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text, pack)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Network.Socket as N
import qualified Sound.Osc as O
import Sound.Osc.Time.Timeout (recvPacketTimeout)
import qualified Sound.Osc.Transport.Fd.Udp as O
import Sound.Tidal.Clock
import qualified Sound.Tidal.Clock as Clock
import Sound.Tidal.Link
import Zwirn.Core.Cord (stack)
import Zwirn.Core.Query
import qualified Zwirn.Core.Time as Z
import Zwirn.Language.Evaluate

type PlayMap = Map.Map Text (Zwirn Expression)

type BusMap = Map.Map Int (Zwirn Expression)

data StreamConfig = StreamConfig
  { streamConfigPort :: Int,
    streamConfigBusPort :: Int,
    streamConfigAddress :: String
  }
  deriving (Generic)

data Stream = Stream
  { sPlayMap :: MVar PlayMap,
    sBusMap :: MVar BusMap,
    sState :: MVar ExpressionMap,
    sBusses :: MVar [Int],
    sAddress :: RemoteAddress,
    sBusAddress :: RemoteAddress,
    sLocal :: O.Udp,
    sClockRef :: ClockRef,
    sClockConfig :: ClockConfig
  }

type RemoteAddress = N.SockAddr

streamReplace :: Stream -> Text -> Zwirn Expression -> IO ()
streamReplace str key p = modifyMVar_ (sPlayMap str) (return . Map.insert key p)

streamReplaceBus :: Stream -> Int -> Zwirn Expression -> IO ()
streamReplaceBus str key p = modifyMVar_ (sBusMap str) (return . Map.insert key p)

streamSet :: Stream -> T.Text -> Expression -> IO ()
streamSet str x ex = modifyMVar_ (sState str) (return . Map.insert x ex)

streamSetCPS :: Stream -> Time -> IO ()
streamSetCPS s = Clock.setCPS (sClockConfig s) (sClockRef s)

streamSetBPM :: Stream -> Time -> IO ()
streamSetBPM s = Clock.setBPM (sClockRef s)

streamFirst :: Stream -> Zwirn Expression -> IO ()
streamFirst str z = do
  dummy <- newMVar $ Map.singleton (pack "_streamOnceDummy_") z
  Clock.clockOnce (tickAction dummy (sBusMap str) (sState str) (sBusses str) (sAddress str) (sBusAddress str) (sLocal str)) (sClockConfig str) (sClockRef str)

startStream :: StreamConfig -> MVar PlayMap -> MVar ExpressionMap -> ClockConfig -> IO Stream
startStream config zMV stMV conf = do
  let target_address = streamConfigAddress config
      target_port = streamConfigPort config
      target_bus_port = streamConfigBusPort config
  remote <- resolve target_address target_port
  remoteBus <- resolve target_address target_bus_port
  local <- O.udp_server 2323

  busMapMV <- newMVar Map.empty
  bussesMV <- newMVar []

  _ <- forkIO $ handshake (N.addrAddress remote) local bussesMV

  cref <- clocked conf (tickAction zMV busMapMV stMV bussesMV (N.addrAddress remote) (N.addrAddress remoteBus) local)
  return $ Stream zMV busMapMV stMV bussesMV (N.addrAddress remote) (N.addrAddress remoteBus) local cref conf

tickAction :: MVar PlayMap -> MVar BusMap -> MVar ExpressionMap -> MVar [Int] -> RemoteAddress -> RemoteAddress -> O.Udp -> (Time, Time) -> Double -> ClockConfig -> ClockRef -> (SessionState, SessionState) -> IO ()
tickAction zMV busMapMV stMV bussesMV remote remoteBus local (star, end) nudge cconf cref (ss, _) = do
  vs <- processPlayMap (star, end) zMV stMV
  bs <- processBusMap (star, end) busMapMV stMV bussesMV
  mapM_ (processAndSend remote local nudge cconf cref ss) vs
  mapM_ (processAndSend remoteBus local nudge cconf cref ss) bs

processPlayMap :: (Time, Time) -> MVar PlayMap -> MVar ExpressionMap -> IO [(Z.Time, O.Message)]
processPlayMap (star, end) zMV stMV = do
  pm <- readMVar zMV
  let p = playMapToCord pm
  st <- readMVar stMV
  let qs = findAllValuesWithTimeState (Z.Time (align star) 1, Z.Time (align end) 1) st p
      vs = map (\(t, v, _) -> (t, v)) qs
      sts = map (\(_, _, x) -> x) qs

  -- TODO: what about race conditions?
  updateState stMV sts

  return $ (\(t, ex) -> (t, expressionToMessage (fromIntegral $ floor t) ex)) <$> vs

processBusMap :: (Time, Time) -> MVar BusMap -> MVar ExpressionMap -> MVar [Int] -> IO [(Z.Time, O.Message)]
processBusMap (star, end) busMV stMV bussesMV = do
  bm <- readMVar busMV
  let bs = Map.toList bm
  busses <- readMVar bussesMV
  st <- readMVar stMV
  return $ concatMap (\(i, p) -> second (busExpressionToMessage $ toBus busses i) <$> findAllValuesWithTime (Z.Time (align star) 1, Z.Time (align end) 1) st p) bs

toBus :: [Int] -> Int -> Int
toBus [] i = i
toBus xs i = xs !! (i `mod` length xs)

resolve :: String -> Int -> IO N.AddrInfo
resolve host port = do
  let hints = N.defaultHints {N.addrSocketType = N.Stream}
  addr : _ <- N.getAddrInfo (Just hints) (Just host) (Just $ show port)
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
additionalData cyc = [O.string "cps", O.float 0.5625, O.string "cycle", O.float cyc]

expressionToMessage :: Double -> Expression -> O.Message
expressionToMessage cyc ex = O.message "/dirt/play" (additionalData cyc ++ expressionToOSC ex)

busExpressionToMessage :: Int -> Expression -> O.Message
busExpressionToMessage bus ex = O.message "/c_set" (O.int32 bus : expressionToOSC ex)

sendMessage :: RemoteAddress -> O.Udp -> Double -> Double -> (Double, O.Message) -> IO ()
sendMessage remote local latency extraLatency (time, m) = sendBndl remote local $ O.Bundle timeWithLatency [m]
  where
    timeWithLatency = time - latency + extraLatency

sendBndl :: RemoteAddress -> O.Udp -> O.Bundle -> IO ()
sendBndl remote local bndl = O.sendTo local (O.Packet_Bundle bndl) remote

defaultLatency :: Double
defaultLatency = 0.2

processAndSend :: RemoteAddress -> O.Udp -> Double -> ClockConfig -> ClockRef -> SessionState -> (Z.Time, O.Message) -> IO ()
processAndSend remote local nudge cconf cref ss (t, msg) = do
  let onBeat = Clock.cyclesToBeat cconf (realToFrac ((\(Z.Time r _) -> fromRational r) t))

  on <- Clock.timeAtBeat cconf ss onBeat
  onOSC <- Clock.linkToOscTime cref on

  sendMessage remote local defaultLatency nudge (onOSC, msg)

updateState :: MVar ExpressionMap -> [ExpressionMap] -> IO ()
updateState _ [] = return ()
updateState stmv (st : _) = modifyMVar_ stmv (const $ return st)

handshake :: RemoteAddress -> O.Udp -> MVar [Int] -> IO ()
handshake addr udp bussesMV = sendHandshake >> listen 0
  where
    sendHandshake :: IO ()
    sendHandshake = O.sendTo udp (O.Packet_Message $ O.Message "/dirt/handshake" []) addr
    listen :: Int -> IO ()
    listen waits = do
      ms <- recvMessagesTimeout 2 udp
      if null ms
        then do
          checkHandshake waits -- there was a timeout, check handshake
          listen (waits + 1)
        else do
          mapM_ respond ms
          listen 0
    checkHandshake :: Int -> IO ()
    checkHandshake waits = do
      busses <- readMVar bussesMV
      when (null busses) $ do
        -- when (waits == 0) $ print "Waiting for SuperDirt (v.1.7.2 or higher).."
        sendHandshake
    respond :: O.Message -> IO ()
    respond (O.Message "/dirt/hello" _) = sendHandshake
    respond (O.Message "/dirt/handshake/reply" xs) = do
      prev <- swapMVar bussesMV $ bufferIndices xs
      return ()
    -- Only report the first time..
    -- when (null prev) $ print "Connected to SuperDirt."
    respond _ = return ()
    bufferIndices :: [O.Datum] -> [Int]
    bufferIndices [] = []
    bufferIndices (x : xs')
      | x == O.AsciiString (O.ascii "&controlBusIndices") = catMaybes $ takeWhile isJust $ map O.datum_integral xs'
      | otherwise = bufferIndices xs'

recvMessagesTimeout :: Double -> O.Udp -> IO [O.Message]
recvMessagesTimeout n sock = maybe [] O.packetMessages <$> recvPacketTimeout n sock
