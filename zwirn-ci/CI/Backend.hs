module CI.Backend where

{-
    Backend.hs - Implements the interaction between the compiler-interpreter and the editor
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

import Control.Monad.State (lift, liftIO)
import Data.Text (pack)
import System.Console.Haskeline
import Zwirn.Language.Compiler

type ZwirnCI = InputT CI

runZwirnCI :: Environment -> ZwirnCI () -> IO ()
runZwirnCI env x = do
  ci <- runCI env (runInputT defaultSettings x)
  case ci of
    Left (CIError err envv) -> print err >> runZwirnCI envv x
    Right _ -> return ()

evalInput :: ZwirnCI ()
evalInput = do
  mayinput <- getInputLine ">> "
  case mayinput of
    Just input -> do
      x <- lift $ compilerInterpreterBasic (pack input)
      if x == ""
        then return ()
        else liftIO $ putStrLn x
    Nothing -> return ()

evalInputLoop :: ZwirnCI ()
evalInputLoop = evalInput >> evalInputLoop
