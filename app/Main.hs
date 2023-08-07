module Main where

import System.FilePath  (dropFileName)
import System.Environment (getExecutablePath)

import Graphics.UI.Threepenny.Core as C hiding (text)

import Editor.Setup
import Editor.CommandLine

import Options.Applicative (execParser)


main :: IO ()
main = do
    config <- execParser conf
    execPath <- dropFileName <$> getExecutablePath

    startGUI C.defaultConfig {
          jsStatic = Just $ execPath ++ "static",
          jsCustomHTML     = Just "tidal.html",
          jsPort = Just (tpPort config)
        } $ setup (dirtPort config) (hintMode config)


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
