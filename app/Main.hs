module Main where

import Language
import Parser
import Functional hiding (($))
import Tidal
import Compiler

import Prelude as P
import qualified Language.Haskell.Interpreter as Hint

eval :: String -> IO (Either Hint.InterpreterError (Mini Functional.Int))
eval s = Hint.runInterpreter $ do
  Hint.loadModules ["src/Functional.hs","src/MiniPrelude.hs"]
  Hint.setTopLevelModules ["Functional","MiniPrelude"]
  Hint.interpret s (Hint.as :: Mini Functional.Int)

main :: IO ()
main = do
  putStrLn $ "Enter a MiniTerm: \n"
  input <- getLine
  case parseTerm input of
    Left err -> putStrLn $ show err
    Right t -> do
            let c = compile t
            putStrLn c
            x <- eval $ c
            case x of
                Left err -> putStrLn $ show err
                Right f -> case toPattern f of
                                  Just p -> (putStrLn $ displayMini f) >> (putStrLn $ show p)
                                  Nothing -> putStrLn "Cannot convert resulting term to pattern!"
