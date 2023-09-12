module Listener.CommandLine where

{-
    CommandLine.hs - command line interface for the editor
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

import Options.Applicative
import Zwirn.Language.Hint

data Config = Config { hintMode :: HintMode
                     , listenPort :: Int
                     , remotePort :: Int
                     , dirtPort :: Int
                     , cliMode :: Bool
                     } deriving (Eq, Show)


conf :: ParserInfo Config
conf = info (configParser <**> helper)
  ( fullDesc
  <> progDesc "An interactive interpreter for zwirn"
  <> header "zwirn" )

configParser :: Parser Config
configParser = Config <$> noGhcParser
                      <*> listenPortParser
                      <*> remotePortParser
                      <*> dirtPortParser
                      <*> cliModeParser

listenPortParser :: Parser Int
listenPortParser = option auto
                      ( long "listenport"
                     <> short 'l'
                     <> help "Specify the listening port"
                     <> showDefault
                     <> value 6011
                     <> metavar "INT" )

remotePortParser :: Parser Int
remotePortParser = option auto
                     ( long "remoteport"
                    <> short 'r'
                    <> help "Specify the reply port"
                    <> showDefault
                    <> value 6012
                    <> metavar "INT")

dirtPortParser :: Parser Int
dirtPortParser = option auto
                     ( long "dirtport"
                    <> short 'd'
                    <> help "Specify the dirt port"
                    <> showDefault
                    <> value 57120
                    <> metavar "INT")

noGhcParser :: Parser HintMode
noGhcParser =  flag GHC NoGHC
          ( long "no-ghc"
         <> help "If this flag is active, the interpreter will assume that GHC is not installed on the system" )

cliModeParser :: Parser Bool
cliModeParser =  flag False True
         ( long "cli"
        <> help "Command Line Interpreter Mode" )
