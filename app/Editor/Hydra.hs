module Editor.Hydra where

import Control.Concurrent.MVar  (modifyMVar_, MVar, readMVar)
import Control.Concurrent (threadDelay)

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Sound.Tidal.Context hiding ((#))-- (Stream, sPMapMV, Pattern, queryArc, Arc(..))
import Sound.Tidal.Link as Link

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

hydraLoopInit :: Window -> Stream -> MVar (Pattern String) -> MVar String -> IO ()
hydraLoopInit win str pM bufM = do
              ss <- liftIO $ createAndCaptureAppSessionState (sLink str)
              hydraLoop win str ss pM bufM

hydraLoop :: Window -> Stream -> SessionState -> MVar (Pattern String) -> MVar String -> IO ()
hydraLoop win str ss pM bufM = do
          now <- streamGetnow' ss str
          ps <- readMVar pM
          buf <- readMVar bufM
          case queryArc (segment 32 ps) (Arc (toRational now) (toRational now)) of
                        [] -> case "solid().out()" == buf of
                                      False -> do
                                        runUI win $ runFunction $ ffi $ "solid().out()"
                                        modifyMVar_ bufM (const $ pure $ "solid().out()")
                                        threadDelay 10000
                                        hydraLoop win str ss pM bufM
                                      True -> threadDelay 10000 >> hydraLoop win str ss pM bufM
                        (e:_) -> case value e == buf of
                                      False -> do
                                        runUI win $ runFunction $ ffi $ wrapCatchErr $ value e
                                        modifyMVar_ bufM (const $ pure $ value e)
                                        threadDelay 10000
                                        hydraLoop win str ss pM bufM
                                      True -> threadDelay 10000 >> hydraLoop win str ss pM bufM

wrapCatchErr :: String -> String
wrapCatchErr st = "try {" ++ st ++ "} catch (err) {}"
