module Main where

import Zwirn.Language.Parser
import Data.Text (pack)

main :: IO ()
main = do
  x <- readFile "/home/martin/Schreibtisch/zwirn/browse.txt"
  writeFile "/home/martin/Schreibtisch/zwirn/output.txt" $ show (parseTypeDecls (pack x))
