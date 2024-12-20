module Zwirn.Jack where

import Control.Concurrent.MVar (readMVar)
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans
import Data.Array.Storable (writeArray)
import Data.IORef
import Data.Word
import qualified Sound.JACK as Jack
import Sound.JACK.Audio
import qualified Sound.JACK.Exception as JackExc
import Sound.Zwirn.Core.Cord
import Sound.Zwirn.Core.Types
import Zwirn.Stream

type JackPort = Jack.Port Sample

type ByteBeat = Int -> Word8

convert :: Cord Int Double -> Int -> Double
convert p n = average $ map (value . fst) $ asList $ zwirn p (fromIntegral n / 48000) 0

getSampleNumber :: IORef Int -> IO Int
getSampleNumber ref = modifyIORef' ref (+ 1) >> readIORef ref

startJack :: Stream -> IO ()
startJack str = do
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
        p <- readMVar str
        writeArray outArr (Jack.nframesIndices nframes !! i) (realToFrac $ convert p n)
    )
    [0 .. length (Jack.nframesIndices nframes) - 1]

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)
