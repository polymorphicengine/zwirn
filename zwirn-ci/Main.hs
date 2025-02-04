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

import CI.Backend
import CI.Config
import CI.Documentation
import CI.Setup
import Conferer as Conf
import Control.Monad (when)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  config <- getConfig
  fullConfig <- Conf.fetch config
  when (ciConfigDocumentation $ fullConfigCi fullConfig) generateDocumentation
  env <- setup fullConfig
  runZwirnCI env evalInputLoop
