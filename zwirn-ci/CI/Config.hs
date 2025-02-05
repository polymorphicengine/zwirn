{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CI.Config where

{-
    CommandLine.hs - configuration
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

import Conferer as Conf
import Conferer.Source.CLIArgs as Cli
import Conferer.Source.Env as Env
import Conferer.Source.Yaml as Yaml
import Control.Monad (unless)
import qualified Data.ByteString as B
import Data.FileEmbed
import GHC.Generics (Generic)
import qualified Sound.Tidal.Clock as Clock (ClockConfig (..), defaultConfig)
import System.Directory.OsPath
import System.File.OsPath as F
import System.OsPath
import Zwirn.Stream

defaultConfigFile :: B.ByteString
defaultConfigFile = $(embedFile "zwirn-ci-config.yaml")

data ClockConfig = ClockConfig
  { clockConfigQuantum :: Double,
    clockConfigBeatsPerCycle :: Double,
    clockConfigFrameTimespan :: Double,
    clockConfigEnableLink :: Bool,
    clockConfigSkipTicks :: Int,
    clockConfigProcessAhead :: Double
  }
  deriving (Generic)

data CiConfig = CiConfig
  { ciConfigBootPath :: FilePath,
    ciConfigDocumentation :: Bool,
    ciConfigListener :: Bool
  }
  deriving (Generic)

data FullConfig = FullConfig
  { fullConfigCi :: CiConfig,
    fullConfigClock :: ClockConfig,
    fullConfigStream :: StreamConfig
  }
  deriving (Generic)

instance DefaultConfig CiConfig where
  configDef = CiConfig "" False False

instance DefaultConfig StreamConfig where
  configDef = StreamConfig 57120 57110 "127.0.0.1"

instance DefaultConfig ClockConfig where
  configDef = fromClock Clock.defaultConfig

instance DefaultConfig FullConfig where
  configDef = FullConfig configDef configDef configDef

instance FromConfig CiConfig

instance FromConfig StreamConfig

instance FromConfig ClockConfig

instance FromConfig FullConfig

getConfig :: IO Conf.Config
getConfig = do
  home <- getHomeDirectory
  configDirPath <- (home <>) <$> encodeUtf "/.config/zwirn-ci/"
  path <- (home <>) <$> encodeUtf "/.config/zwirn-ci/config.yaml"
  createDirectoryIfMissing True configDirPath
  exists <- doesFileExist path
  unless exists (F.writeFile path (B.fromStrict defaultConfigFile))
  decoded <- decodeUtf path
  mkConfig'
    []
    [ Cli.fromConfig,
      Env.fromConfig "zwirn",
      Yaml.fromFilePath decoded
    ]

fromClock :: Clock.ClockConfig -> ClockConfig
fromClock (Clock.ClockConfig a b c d e f) = ClockConfig (realToFrac a) (realToFrac b) c d (fromIntegral e) f

toClock :: ClockConfig -> Clock.ClockConfig
toClock (ClockConfig a b c d e f) = Clock.ClockConfig (realToFrac a) (realToFrac b) c d (fromIntegral e) f

configPath :: IO String
configPath = do
  home <- getHomeDirectory
  path <- (home <>) <$> encodeUtf "/.config/zwirn-ci/config.yaml"
  exists <- doesFileExist path
  decoded <- decodeUtf path
  if exists then return decoded else return "Config file not found!"

resetConfig :: IO String
resetConfig = do
  home <- getHomeDirectory
  configDirPath <- (home <>) <$> encodeUtf "/.config/zwirn-ci/"
  path <- (home <>) <$> encodeUtf "/.config/zwirn-ci/config.yaml"
  createDirectoryIfMissing True configDirPath
  F.writeFile path (B.fromStrict defaultConfigFile)
  return "Restored default config."
