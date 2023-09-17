module Editor.Tomato (playMV) where

import Control.Concurrent.MVar
import Control.Concurrent (forkIO)

import Sound.Tomato.Speakers

import Sound.Tidal.Context hiding (s, n)

import qualified Data.Vector.Storable as V

import Zwirn.Interactive.Types (Number (..), NumberPattern)

void :: IO a -> IO ()
void x = x >> return ()

playMV :: MVar NumberPattern -> IO ()
playMV bmv = void $ forkIO $ withSpeakers (realToFrac sampleRate) 512 $ \s -> playSamplesIO s (samps bmv sampleRate)
    where
    sampleRate  = 44100

oneSampleAt :: MVar NumberPattern -> Int -> Int -> IO Number
oneSampleAt bMV r t = do
            p <- readMVar bMV
            return $  reduce $ map value $ queryArc p (Arc ((fromIntegral t)/(fromIntegral r)) ((fromIntegral t)/(fromIntegral r)))
            where reduce [] = 0
                  reduce (x:_) = x

samps :: MVar NumberPattern -> Int -> [IO Number]
samps bMV r = map (oneSampleAt bMV r) [0..]

playSamplesIO :: Speakers -> [IO Number] -> IO ()
playSamplesIO s sa = do
  let cs = chunkSamps 512 sa
      ss = map (\xs -> fmap (\ys -> V.fromList $ map (\(Num x)-> realToFrac x) ys) $ sequence xs) cs
  playAll s ss

playAll :: Speakers -> [IO AudioBlock] -> IO ()
playAll _ [] = return ()
playAll s (a:as) = a >>= (playBlock s) >> playAll s as


chunkSamps :: Int -> [a] -> [[a]]
chunkSamps _ [] = []
chunkSamps n xs = y : chunkSamps n xs'
    where (y,xs') = splitAt n xs
