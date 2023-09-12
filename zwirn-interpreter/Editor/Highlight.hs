{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
module Editor.Highlight where

{-
    Highlight.hs - Logic for pattern highlighting
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

-- location of a value in the code specified by line, start, end and editor number
type Location = (Int,Int,Int,Int)

type Buffer = [(Location, JSObject)]

highlight :: Location -> UI JSObject
highlight (line, start, end, editorNum) = callFunction $ ffi ("(editor" ++ show editorNum ++ "cm.markText({line: %1, ch: %2}, {line: %1, ch: %3}, {css: \"outline: 2px solid blue;\"}))") line start end


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
locs :: ValueMap -> Double -> ControlPattern -> [Location]
locs vm t pat = concatMap evToLocs $ query pat (State (Arc (toRational t) (toRational t)) vm)
        where evToLocs (Event {context = Context xs}) = map toLoc xs
              -- assume an event doesn't span more than one line
              toLoc ((by, bx), (editorNum, ex)) = (by,bx-1,ex-1,editorNum)

locsMany :: ValueMap -> Double -> [ControlPattern] -> [Location]
locsMany vm t = concatMap (locs vm t)

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

highlightOnce :: Stream -> MVar Buffer -> UI ()
highlightOnce stream buffMV = do
                ps <- liftIO $ getPats stream
                c <- liftIO $ streamGetnow' stream
                sMap <- liftIO $ readMVar $ sStateMV stream
                buffer <- liftIO $ takeMVar buffMV
                newBuf <- updateBuf buffer (locsMany sMap c ps)
                liftIO $ threadDelay 10000
                liftIO $ putMVar buffMV newBuf

highlightLoop :: Window -> Stream -> MVar Buffer -> IO ()
highlightLoop win stream buf = do
                            runUI win $ highlightOnce stream buf
                            highlightLoop win stream buf--runUI win $ runFunction $ ffi "requestAnimationFrame(highlightLoop)"

processAhead :: Stream -> Link.Micros
processAhead str = round $ (cProcessAhead $ sConfig str) * 1000000

streamGetnow' :: Stream -> IO Double
streamGetnow' str = do
  ss <- createAndCaptureAppSessionState (sLink str)
  now <- Link.clock (sLink str)
  beat <- Link.beatAtTime ss (now + (processAhead str)) (cQuantum $! sConfig str)
  Link.destroySessionState ss
  return $ coerce $! beat / (cBeatsPerCycle $! sConfig str)


-- to turn highlighting on/off

highlightOff :: MVar Buffer -> UI ()
highlightOff buffMV = do
              buffer <- liftIO $ takeMVar buffMV
              unhighlightMany [x | (_,x) <- buffer]

highlightOn :: MVar Buffer -> IO ()
highlightOn buffMV = putMVar buffMV []

toggleHighlight :: MVar Bool -> MVar Buffer -> UI ()
toggleHighlight boolMV buffMV = do
                    bool <- liftIO $ takeMVar boolMV
                    case bool of
                      True -> highlightOff buffMV
                      False -> liftIO $ highlightOn buffMV
                    liftIO $ putMVar boolMV (not bool)
