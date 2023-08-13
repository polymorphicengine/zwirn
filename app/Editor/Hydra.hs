{-# LANGUAGE OverloadedStrings #-}
module Editor.Hydra where

{-
    Hydra.hs - query patterns for javascript (hydra) code and execute it
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

import Control.Concurrent.MVar  (modifyMVar_, MVar, readMVar)
import Control.Concurrent (threadDelay)

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Sound.Tidal.Context hiding ((#))-- (Stream, sPMapMV, Pattern, queryArc, Arc(..))

import Zwirn.Interactive.Types (Text (..))
import Zwirn.Interactive.Convert (_fromTarget)

import Graphics.UI.Threepenny.Core as C hiding (text, value)

import Editor.Highlight (streamGetnow')

startHydra :: UI ()
startHydra = do
  runFunction $ ffi "hydra = new Hydra({canvas: document.getElementById(\"hydraCanvas\"),detectAudio: false})"
  hijackScreen

hijackScreen :: UI ()
hijackScreen = do
        execPath <- liftIO $ dropFileName <$> getExecutablePath
        h <- liftIO $ readFile $ execPath ++ "static/hijackScreen.js"
        runFunction $ ffi h

getWindowWidth :: UI Double
getWindowWidth = callFunction $ ffi "window.innerWidth"

getWindowHeight :: UI Double
getWindowHeight = callFunction $ ffi "window.innerHeight"

hydraLoop :: Window -> Stream -> MVar (Pattern Text) -> MVar Text -> IO ()
hydraLoop win str pM bufM = do
          now <- streamGetnow' str
          sMap <- readMVar (sStateMV str)
          ps <- readMVar pM
          buf <- readMVar bufM
          case query (segment 32 ps) (State (Arc (toRational now) (toRational now)) sMap) of
                        [] -> case (Text "solid().out()") == buf of
                                      False -> do
                                        runUI win $ runFunction $ ffi $ "solid().out()"
                                        modifyMVar_ bufM (const $ pure $ Text "solid().out()")
                                        threadDelay 10000
                                        hydraLoop win str pM bufM
                                      True -> threadDelay 10000 >> hydraLoop win str pM bufM
                        (e:_) -> case value e == buf of
                                      False -> do
                                        runUI win $ runFunction $ ffi $ wrapCatchErr $ _fromTarget $ value e
                                        modifyMVar_ bufM (const $ pure $ value e)
                                        threadDelay 10000
                                        hydraLoop win str pM bufM
                                      True -> threadDelay 10000 >> hydraLoop win str pM bufM

wrapCatchErr :: String -> String
wrapCatchErr st = "try {" ++ st ++ "} catch (err) {}"
