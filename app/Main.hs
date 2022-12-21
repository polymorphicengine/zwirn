module Main where

import Language
import Parser
import Interpreter
import Tidal
import Compiler

import Prelude as P
import qualified Language.Haskell.Interpreter as Hint

-- import Sound.Tidal.Pattern (Pattern)
--
-- import Data.List (intercalate)
--
-- run :: Term -> String
-- run = displayTerm
--
-- parseOne :: (String, String) -> String
-- parseOne (x,t) = "("++show x++","++func++")"
--                where func = case parseTerm t of
--                                       Left err -> show err
--                                       Right f -> show f
--
-- prelude :: [(String,String)]
-- prelude = [("t", "\\x.\\y.x")
--           ,("f", "\\x.\\y.y")
--           ,("and", "\\p.\\q.p$q$p")
--           ,("or", "\\p.\\q.p$p$q")
--           ,("not", "\\p.p$f$t")
--           ,("if", "\\p.\\a.\\b.p$a$b")
--           ,("pow", "\\b.\\e.e$b")
--           ,("sub", "\\m.\\n.n$pred$m")
--           ,("iszero","\\n.n$(\\x.f)$t")
--           ,("leq", "\\m.\\n.iszero$(sub$m$n)")
--           ,("eq", "\\m.\\n.and$(leq$m$n)$(leq$n$m)")
--           ,("Y", "\\func.(\\arg.func$(arg$arg))$(\\arg.func$(arg$arg))")
--           ,("Z", "\\func.(\\arg.func$(\\v.arg$arg$v))$(\\arg.func$(\\v.arg$arg$v))")
--           ,("flip", "\\h.\\x.\\y.h$y$x")
--           --minilambda functions
--           ,("force","\\x xs.(x xs)|x.x")
--           ,("isseq", "\\x xs.t|x.f")
--           ,("revrun", "Y$(\\h.\\n.(if$(iszero$(pred$n))$0$((pred$n) h$(pred$n))))")
--           ,("seqlen", "Y$(\\h.\\y ys.(succ$(h$ys))|y.1)")
--           ,("seqdrop", "\\n.\\y.n$seqtail$y")
--           ,("seqtake", "Y$(\\h.\\n.(\\y ys.(if$(leq$n$1)$y$(y h$(pred$n)$ys))|y.y))")
--           ,("seqfront", "\\x.seqtake$(pred$(seqlen$x))$x")
--           ,("seqlast","\\x.(seqlen$x)$seqtail$x")
--           ,("rot", "\\n.\\y.n$(\\z.(seqlast$z seqfront$z))$y")
--           ,("replicate", "(Y$(\\h.\\k.\\x.if$(iszero$(pred$k))$(x)$(x h$(pred$k)$x)))")
--           ,("squeeze", "\\h.\\x.\\y.(replicate$(seqlen$x)$h)$x$(replicate$(seqlen$x)$y)")
--           ]
--
-- createPrelude :: String
-- createPrelude = "[" ++ intercalate "\n," (map parseOne prelude) ++ "\n]"
--

eval :: String -> IO (Either Hint.InterpreterError TermF)
eval s = Hint.runInterpreter $ do
  Hint.loadModules ["src/Interpreter.hs"]
  Hint.setTopLevelModules ["Interpreter"]
  Hint.interpret s (Hint.as :: TermF)

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
                Right f -> case toPatternI f of
                                  Just p -> putStrLn $ show p
                                  Nothing -> case toPatternB f of
                                    Just q -> putStrLn $ show q
                                    Nothing -> putStrLn "Cannot convert resulting term to pattern!"
