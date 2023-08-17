module Main where

import Zwirn.Language.Parser
import Data.Text (pack)
import System.Process
import System.IO

{-
    gen/Main.hs - short script for automatically generating the
    default type environment used by zwirn, assumes that zwirn
    was previously installed via cabal install --lib
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

main :: IO ()
main = do
  (Just hin, Just hout, _, ph) <- createProcess (shell "ghci -v0") { std_in = CreatePipe, std_out = CreatePipe }
  hSetBuffering hin LineBuffering
  hPutStrLn hin $ "import qualified Prelude as P ()"
  hPutStrLn hin $ "import Zwirn.Interactive"
  hPutStrLn hin $ ":browse Zwirn.Interactive.Prelude"
  hPutStrLn hin $ ":q"
  x <- hGetContents hout
  waitForProcess ph
  --putStrLn x
  --x <- readFile "/home/martin/Schreibtisch/zwirn/browse.txt"
  case parseTypeDecls (pack x) of
    Left err -> putStrLn err
    Right xs -> writeFile "/home/martin/Schreibtisch/zwirn/output.txt" $ show xs
