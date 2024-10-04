module Zwirn.Stream where

import Control.Concurrent
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans
import Data.Array.Storable (writeArray)
import Data.IORef
import Data.Word
import qualified Sound.JACK as Jack
import Sound.JACK.Audio
import qualified Sound.JACK.Exception as JackExc
import Sound.Zwirn.Nested
import Zwirn.Interactive.Types (NumberPattern)

type Stream = MVar NumberPattern

type JackPort = Jack.Port Sample

type ByteBeat = Int -> Word8

streamReplace :: Stream -> NumberPattern -> IO ()
streamReplace str p = modifyMVar_ str (const $ pure p)

convert :: NumberPattern -> ByteBeat
convert p i = floor $ sVal $ zwirnAt p (fromIntegral i / 8000)

getSampleNumber :: IORef Int -> IO Int
getSampleNumber ref = modifyIORef' ref (+ 1) >> readIORef ref

startStream :: Stream -> IO ()
startStream str = do
  ref <- newIORef (0 :: Int)
  Jack.handleExceptions $
    Jack.withClientDefault "zwirn" $ \client -> do
      Jack.withPort client "output" $ \output ->
        Jack.withProcess client (processBeat output ref str) $
          Jack.withActivation client $
            Trans.lift Jack.waitForBreak

processBeat :: (JackExc.ThrowsErrno e) => JackPort Jack.Output -> IORef Int -> Stream -> Jack.NFrames -> Sync.ExceptionalT e IO ()
processBeat output ref str nframes = Trans.lift $ do
  outArr <- getBufferArray output nframes
  mapM_
    ( \i -> do
        n <- getSampleNumber ref
        be <- convert <$> readMVar str
        writeArray outArr (Jack.nframesIndices nframes !! i) (fromIntegral (be $ floor $ fromIntegral n / 6) / 127 - 1)
    )
    [0 .. length (Jack.nframesIndices nframes) - 1]
