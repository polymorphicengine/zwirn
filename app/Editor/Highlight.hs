{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
module Editor.Highlight where

import Sound.Tidal.Context hiding (end, start)
import Sound.Tidal.Link as Link

import Graphics.UI.Threepenny.Core as C hiding (text)
import Foreign.JavaScript (JSObject)

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar  (MVar, takeMVar, putMVar, readMVar)

import Data.Map as Map  (elems)
import Data.List  ((\\))
import Data.Coerce (coerce)

import Foreign.C.Types

-- location of a value in the code specified by line, start and end
type Location = (Int,Int,Int)

type Buffer = [(Location, JSObject)]

highlight :: Location -> UI JSObject
highlight (line, start, end) = callFunction $ ffi "(editor0cm.markText({line: %1, ch: %2}, {line: %1, ch: %3}, {css: \"outline: 2px solid blue;\"}))" line start end


highlightMany :: [Location] -> UI [JSObject]
highlightMany [] = return []
highlightMany (x:xs) = do
                    mark <- highlight x
                    marks <- highlightMany  xs
                    return (mark:marks)

unHighlight :: JSObject -> UI ()
unHighlight mark = runFunction $ ffi "if (typeof %1 !== 'undefined'){%1.clear()};" mark

unhighlightMany :: [JSObject] -> UI ()
unhighlightMany [] = return ()
unhighlightMany (x:xs) = unHighlight x >> unhighlightMany xs

-- queries the pattern at time t and gets the locations of active events
locs :: Double -> ControlPattern -> [Location]
locs t pat = concatMap evToLocs $ queryArc pat (Arc (toRational t) (toRational t) )
        where evToLocs (Event {context = Context xs}) = map toLoc xs
              -- assume an event doesn't span more than one line
              toLoc ((by, bx), (_, ex )) = (by-1,bx-1,ex-1)

locsMany :: Double -> [ControlPattern] -> [Location]
locsMany t = concatMap (locs t)

updateBuf :: Buffer -> [Location] -> UI Buffer
updateBuf buf ls = do
                marks <- highlightMany newLocs
                unhighlightMany unmark
                return $ newBuf ++ (zip newLocs marks)
                   where newLocs = ls \\ (map fst buf) -- locations that are not marked yet, but should be marked now
                         unmark = [x | (l,x) <- buf, not (elem l ls)] -- locations that are marked but should be unmarked now
                         newBuf = [(l,x) | (l,x) <- buf, elem l ls]

getPats :: Stream -> IO [ControlPattern]
getPats stream = do
              pMap <- readMVar $ sPMapMV stream
              let pStates = Map.elems pMap
              return $ concatMap filterPS pStates
              where filterPS (PlayState _ True _ _) = []
                    filterPS (PlayState p False _ _) = [p]

highlightOnce :: SessionState -> Stream -> MVar Buffer -> UI ()
highlightOnce ss stream buffMV = do
                ps <- liftIO $ getPats stream
                c <- liftIO $ streamGetnow' ss stream
                buffer <- liftIO $ takeMVar buffMV
                newBuf <- updateBuf buffer (locsMany c ps)
                liftIO $ threadDelay 100
                liftIO $ putMVar buffMV newBuf

highlightLoopInner :: Window -> SessionState -> Stream -> MVar Buffer -> IO ()
highlightLoopInner win ss stream buf = do
                            runUI win $ highlightOnce ss stream buf
                            highlightLoopInner win ss stream buf--runUI win $ runFunction $ ffi "requestAnimationFrame(highlightLoop)"

highlightLoopOuter :: Window -> Stream -> MVar Buffer -> IO ()
highlightLoopOuter win str buf = do
                            ss <- createAndCaptureAppSessionState (sLink str)
                            highlightLoopInner win ss str buf


streamGetnow' :: SessionState -> Stream -> IO Double
streamGetnow' ss str = do
  now <- Link.clock (sLink str)
  beat <- Link.beatAtTime ss now (cQuantum $! sConfig str)
  return $ coerce $! beat / (cBeatsPerCycle $! sConfig str)
