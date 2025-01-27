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

-- import Sound.Tidal.Context hiding (end, start)
-- import Sound.Tidal.Link as Link

import Control.Concurrent (readMVar, threadDelay)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Data.List ((\\))
import Foreign.JavaScript (JSObject)
import Graphics.UI.Threepenny.Core as C hiding (text)
import Sound.Tidal.Clock
import Zwirn.Core.Time (Time (..))
import Zwirn.Core.Types
import Zwirn.Language.Evaluate.Expression
import Zwirn.Language.Syntax
import Zwirn.Stream

type Buffer = [(Position, JSObject)]

highlight :: Position -> UI JSObject
highlight (Pos line strt end editorNum) = callFunction $ ffi ("(editor" ++ show editorNum ++ "cm.markText({line: %1, ch: %2}, {line: %1, ch: %3}, {css: \"outline: 2px solid blue;\"}))") line strt end

highlightMany :: [Position] -> UI [JSObject]
highlightMany [] = return []
highlightMany (x : xs) = do
  mark <- highlight x
  marks <- highlightMany xs
  return (mark : marks)

unHighlight :: JSObject -> UI ()
unHighlight mark = runFunction $ ffi "if (typeof %1 !== 'undefined'){%1.clear()};" mark

unhighlightMany :: [JSObject] -> UI ()
unhighlightMany = foldr ((>>) . unHighlight) (return ())

-- queries the pattern at time t and gets the locations of active events
locs :: ExpressionMap -> Double -> Zwirn Expression -> [Position]
locs st t z = concatMap (fixLocs . info . fst) $ toList $ unzwirn z (Time (toRational t) 1) st
  where
    fixLocs = map (\(Pos l xst xen xed) -> Pos l (xst - 1) (xen - 1) xed)

locsMany :: ExpressionMap -> Double -> [Zwirn Expression] -> [Position]
locsMany st t = concatMap (locs st t)

updateBuf :: Buffer -> [Position] -> UI Buffer
updateBuf buf ls = do
  marks <- highlightMany newLocs
  unhighlightMany unmark
  return $ newBuf ++ zip newLocs marks
  where
    newLocs = ls \\ map fst buf -- locations that are not marked yet, but should be marked now
    unmark = [x | (l, x) <- buf, l `notElem` ls] -- locations that are marked but should be unmarked now
    newBuf = [(l, x) | (l, x) <- buf, l `elem` ls]

highlightOnce :: Stream -> ClockConfig -> ClockRef -> MVar Buffer -> UI ()
highlightOnce stream cc cref buffMV = do
  pm <- liftIO $ readMVar $ sPlayMap stream
  let z = playMapToCord pm
  t <- liftIO $ getCycleTime cc cref
  sMap <- liftIO $ readMVar $ sState stream
  buffer <- liftIO $ takeMVar buffMV
  let ls = locs sMap (realToFrac $ align t) z
  newBuf <- updateBuf buffer ls
  liftIO $ threadDelay 10000
  liftIO $ putMVar buffMV newBuf

highlightLoop :: Window -> Stream -> ClockConfig -> ClockRef -> MVar Buffer -> IO ()
highlightLoop win stream cc cref buf = do
  runUI win $ highlightOnce stream cc cref buf
  highlightLoop win stream cc cref buf -- runUI win $ runFunction $ ffi "requestAnimationFrame(highlightLoop)"

-- to turn highlighting on/off

highlightOff :: MVar Buffer -> UI ()
highlightOff buffMV = do
  buffer <- liftIO $ takeMVar buffMV
  unhighlightMany [x | (_, x) <- buffer]

highlightOn :: MVar Buffer -> IO ()
highlightOn buffMV = putMVar buffMV []

toggleHighlight :: MVar Bool -> MVar Buffer -> UI ()
toggleHighlight boolMV buffMV = do
  bool <- liftIO $ takeMVar boolMV
  (if bool then highlightOff buffMV else liftIO $ highlightOn buffMV)
  liftIO $ putMVar boolMV (not bool)
