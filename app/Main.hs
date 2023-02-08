module Main where

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)


import Sound.Tidal.Context as T hiding (mute,solo,(#),s)

import Graphics.UI.Threepenny.Core as C hiding (text)

import Editor.Frontend


main :: IO ()
main = do
    execPath <- dropFileName <$> getExecutablePath
    str <- T.startTidal (T.superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (T.defaultConfig {cVerbose = True, cFrameTimespan = 1/20})

    startGUI C.defaultConfig {
          jsStatic = Just $ execPath ++ "static",
          jsCustomHTML     = Just "tidal.html"
        } $ setup str
