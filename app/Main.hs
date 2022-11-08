module Main where

import Language
import Parser
import Interpreter
import Tidal

import Sound.Tidal.Pattern (Pattern)

import Data.List (intercalate)

run :: Term -> String
run = displayTerm . fromChurch . reduceMany

runTidal :: Term -> Pattern Int
runTidal t = case toPattern $ fromChurch $ reduceMany t of
                                      Just x -> x
                                      Nothing -> error "Could not transform to pattern!"

parseOne :: (String, String) -> String
parseOne (x,t) = "("++show x++","++func++")"
               where func = case parseTerm t of
                                      Left err -> show err
                                      Right f -> show f

prelude :: [(String,String)]
prelude = [("t", "\\x.\\y.x")
          ,("f", "\\x.\\y.y")
          ,("and", "\\p.\\q.p$q$p")
          ,("or", "\\p.\\q.p$p$q")
          ,("not", "\\p.p$f$t")
          ,("if", "\\p.\\a.\\b.p$a$b")
          ,("succ", "\\n.\\g.\\x.g$(n$g$x)")
          ,("add", "\\m.\\n.m$succ$n")
          ,("mult", "\\m.\\n.m$(add$n)$0")
          ,("pow", "\\b.\\e.e$b")
          ,("pred", "\\n.\\g.\\x.n$(\\j.\\h.h$(j$g))$(\\u.x)$(\\u.u)")
          ,("sub", "\\m.\\n.n$pred$m")
          ,("iszero","\\n.n$(\\x.f)$t")
          ,("leq", "\\m.\\n.iszero$(sub$m$n)")
          ,("eq", "\\m.\\n.and$(leq$m$n)$(leq$n$m)")
          ,("Y", "\\func.(\\arg.func$(arg$arg))$(\\arg.func$(arg$arg))")
          ,("Z", "\\func.(\\arg.func$(\\v.arg$arg$v))$(\\arg.func$(\\v.arg$arg$v))")
          ,("flip", "\\h.\\x.\\y.h$y$x")
          --minilambda functions
          ,("force","\\x xs.(x xs)|x.x")
          ,("isseq", "\\x xs.t|x.f")
          ,("seqhead", "\\x xs.x|x.x")
          ,("seqtail", "\\x xs.xs|x.x")
          ,("fast", "\\n.\\x.x*n")
          ,("slow", "\\n.\\x.x/n")
          ,("revrun", "Y$(\\h.\\n.(if$(iszero$(pred$n))$0$((pred$n) h$(pred$n))))")
          ,("seqlen", "Y$(\\h.\\y ys.(succ$(h$ys))|y.1)")
          ,("seqdrop", "\\n.\\y.n$seqtail$y")
          ,("seqtake", "Y$(\\h.\\n.(\\y ys.(if$(leq$n$1)$y$(y h$(pred$n)$ys))|y.y))")
          ,("seqfront", "\\x.seqtake$(pred$(seqlen$x))$x")
          ,("seqlast","\\x.(seqlen$x)$seqtail$x")
          ,("rot", "\\n.\\y.n$(\\z.(seqlast$z seqfront$z))$y")
          ,("replicate", "(Y$(\\h.\\k.\\x.if$(iszero$(pred$k))$(x)$(x h$(pred$k)$x)))")
          ,("squeeze", "\\h.\\x.\\y.(replicate$(seqlen$x)$h)$x$(replicate$(seqlen$x)$y)")
          ,("rev", "Y$(\\h.(\\y ys.(h$ys y)|z.z))")
          ]

createPrelude :: String
createPrelude = "[" ++ intercalate "\n," (map parseOne prelude) ++ "\n]"

main :: IO ()
main = do
  putStrLn $ "Creating prelude.."
  putStrLn $ createPrelude
