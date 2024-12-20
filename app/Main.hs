module Main where

{-
    Main.hs - entry point of the editor program
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

import Editor.CommandLine
import Editor.Setup
import Graphics.UI.Threepenny.Core as C hiding (text)
import Options.Applicative (execParser)
import System.Environment (getExecutablePath)
import System.FilePath (dropFileName)

main :: IO ()
main = do
  config <- execParser conf
  execPath <- dropFileName <$> getExecutablePath

  startGUI
    C.defaultConfig
      { jsStatic = Just $ execPath ++ "static",
        jsCustomHTML = Just "tidal.html",
        jsPort = Just (tpPort config)
      }
    setup
