module Main where

import Listener.CommandLine
import Listener.Setup
import Listener.Listen
import Options.Applicative (execParser)

main :: IO ()
main = do
    config <- execParser conf
    (remote, local, env) <- setup config
    putStrLn $ "Starting Zwirn Listener.."
    putStrLn $ "Listening for OSC commands on port " ++ show (listenPort config)
    putStrLn $ "Sending replies to port " ++ show (remotePort config)
    listen remote local env
