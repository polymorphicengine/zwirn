{-# LANGUAGE OverloadedStrings #-}

module CI.Setup (setup) where

{-
    Setup.hs - setup of the various components of the backend
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

import CI.Config
import Control.Concurrent.MVar (newMVar)
import qualified Data.Map as Map
import Zwirn.Language.Builtin.Prelude
import Zwirn.Language.Compiler
import Zwirn.Stream

setup :: FullConfig -> IO Environment
setup config = do
  str <- setupStream config
  return $ getInitialEnv str

setupStream :: FullConfig -> IO Stream
setupStream config = do
  mv <- newMVar Map.empty
  m <- newMVar Map.empty
  startStream (fullConfigStream config) mv m (toClock $ fullConfigClock config)

getInitialEnv :: Stream -> Environment
getInitialEnv str = Environment str builtinEnvironment (Just $ ConfigEnv configPath resetConfig) Nothing
