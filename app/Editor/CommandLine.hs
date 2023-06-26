module Editor.CommandLine where

import Options.Applicative
import Editor.Hint

data Config = Config {listenPort :: Int
                     ,dirtPort :: Int
                     ,tpPort :: Int
                     ,hintMode :: HintMode
                     } deriving (Eq,Show)


conf :: ParserInfo Config
conf = info (configParser <**> helper)
  ( fullDesc
  <> progDesc "An interactive interpreter for zwirn"
  <> header "zwirn" )

configParser :: Parser Config
configParser = Config <$> listenPortParser
                      <*> dirtPortParser
                      <*> tpPortParser
                      <*> noGhcParser

listenPortParser :: Parser Int
listenPortParser = option auto
                      ( long "listenport"
                     <> short 'l'
                     <> help "Specify the listening port"
                     <> showDefault
                     <> value 6011
                     <> metavar "INT" )

dirtPortParser :: Parser Int
dirtPortParser = option auto
                     ( long "dirtport"
                    <> short 'd'
                    <> help "Specify the dirt port"
                    <> showDefault
                    <> value 57120
                    <> metavar "INT")

tpPortParser :: Parser Int
tpPortParser = option auto
                     ( long "tp-port"
                    <> short 'p'
                    <> help "Specify the threepenny port"
                    <> showDefault
                    <> value 8023
                    <> metavar "INT")

noGhcParser :: Parser HintMode
noGhcParser =  flag GHC NoGHC
          ( long "no-ghc"
         <> help "If this flag is active, the interpreter will assume that GHC is not installed on the system" )
