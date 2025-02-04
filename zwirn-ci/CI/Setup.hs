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
import qualified Data.Text as T
import System.Directory.OsPath
import System.OsPath
import Zwirn.Language.Builtin.Prelude
import Zwirn.Language.Compiler
import Zwirn.Stream

setup :: FullConfig -> IO Environment
setup config = do
  str <- setupStream config
  let initE = getInitialEnv str
  checkBoot (fullConfigCi config) initE

setupStream :: FullConfig -> IO Stream
setupStream config = do
  mv <- newMVar Map.empty
  m <- newMVar Map.empty
  startStream (fullConfigStream config) mv m (toClock $ fullConfigClock config)

getInitialEnv :: Stream -> Environment
getInitialEnv str = Environment str builtinEnvironment (Just $ ConfigEnv configPath resetConfig) Nothing

checkBoot :: CiConfig -> Environment -> IO Environment
checkBoot (CiConfig "" _ _) env = return env
checkBoot (CiConfig path _ _) env = do
  ospath <- encodeUtf path
  isfile <- doesFileExist ospath
  ps <-
    if isfile
      then return $ decodeUtf ospath
      else do
        isfolder <- doesDirectoryExist ospath
        if isfolder
          then do
            pss <- listDirectory ospath
            fs <- mapM decodeUtf pss
            return $ map (\f -> path ++ "/" ++ f) fs
          else return []
  res <- runCI env (compilerInterpreterBoot $ map T.pack ps)
  case res of
    Left (CIError err newEnv) -> putStrLn ("Error in Bootfile: " ++ err) >> return newEnv
    Right newEnv ->
      if ps /= []
        then putStrLn ("Successfully loaded Bootfiles from " ++ path) >> return newEnv
        else putStrLn ("No Bootfiles found at " ++ path) >> return newEnv
