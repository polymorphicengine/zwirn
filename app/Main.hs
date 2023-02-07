module Main where

import Megaparsec
import Functional hiding (($))
import Tidal
import Compiler

import Prelude as P
import qualified Language.Haskell.Interpreter as Hint

eval :: String -> IO (Either Hint.InterpreterError (Mini P.Int))
eval s = Hint.runInterpreter $ do
  Hint.loadModules ["src/Functional.hs","src/MiniPrelude.hs"]
  Hint.setTopLevelModules ["Functional","MiniPrelude"]
  Hint.interpret s (Hint.as :: Mini P.Int)


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
                Right f -> (putStrLn $ displayMini f) >> (putStrLn $ show $ toPattern f)
