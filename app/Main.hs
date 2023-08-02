module Main where

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)


import Sound.Tidal.Context as T hiding (mute,solo,(#),s)

import Graphics.UI.Threepenny.Core as C hiding (text)

import Editor.Frontend
import Editor.CommandLine

import Options.Applicative (execParser)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar  (MVar, newEmptyMVar, newMVar)

import Zwirn.Language.Hint
import Zwirn.Language.Compiler
import Zwirn.Language.TypeCheck.Infer

import Data.Text (pack)


main :: IO ()
main = do
    config <- execParser conf
    execPath <- dropFileName <$> getExecutablePath
    str <- T.startTidal (T.superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = dirtPort config}) (T.defaultConfig {cVerbose = True, cFrameTimespan = 1/20})

    startGUI C.defaultConfig {
          jsStatic = Just $ execPath ++ "static",
          jsCustomHTML     = Just "tidal.html",
          jsPort = Just (tpPort config)
        } $ setup str (hintMode config)


-- main :: IO ()
-- main = do
--        putStrLn "Starting Hint..\n"
--        mMV <- liftIO newEmptyMVar
--        rMV <- liftIO newEmptyMVar
--        _ <- liftIO $ forkIO $ hintJob GHC mMV rMV
--        loop (Environment defaultEnv (HintEnv GHC mMV rMV))
--        return ()
--       where loop env = do
--                    putStrLn "type a term:\n"
--                    input <- getLine
--                    res <- runCI env (compilerInterpreter 1 0 $ pack input)
--                    case res of
--                          Left err -> (putStrLn $ show err ++ "\n") >> loop env
--                          Right (resp, newEnv, _, _) -> (putStrLn $ show resp ++ "\n") >> loop newEnv
