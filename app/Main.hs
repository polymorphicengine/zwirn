module Main where

import Language
import Parser
import Interpreter

run :: Term -> String
run = displayTerm . alphaConv . fromChurch . reduceMany

main :: IO ()
main = do
  putStrLn $ "Please enter a mini lambda program:"
  l <- readLn
  case parseTerm l of
    Left err -> putStrLn $ show err
    Right t -> do
        putStrLn "Parsing the term succeeded!"
        putStrLn "Proceeding to run it.."
        putStrLn $ "The result:"
        putStrLn $ run t
        putStrLn "Proceeding to the next program.."
        main
