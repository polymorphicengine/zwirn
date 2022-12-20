module Interpreter where

import Language

import Data.Map as Map (Map, fromList)

type Env = Map String Term

envTry :: Env
envTry = fromList [("t",TLambda [(PVar "x",TLambda [(PVar "y",TVar "x")])])
                  ,("f",TLambda [(PVar "x",TLambda [(PVar "y",TVar "y")])])
                  ,("and",TLambda [(PVar "p",TLambda [(PVar "q",TApp (TApp (TVar "p") (TVar "q")) (TVar "p"))])])
                  ,("or",TLambda [(PVar "p",TLambda [(PVar "q",TApp (TApp (TVar "p") (TVar "p")) (TVar "q"))])])
                  ,("not",TLambda [(PVar "p",TApp (TApp (TVar "p") (TVar "f")) (TVar "t"))])
                  ,("if",TLambda [(PVar "p",TLambda [(PVar "a",TLambda [(PVar "b",TApp (TApp (TVar "p") (TVar "a")) (TVar "b"))])])])
                  ,("succ",TLambda [(PVar "n",TLambda [(PVar "g",TLambda [(PVar "x",TApp (TVar "g") (TApp (TApp (TVar "n") (TVar "g")) (TVar "x")))])])])
                  ,("add",TLambda [(PVar "m",TLambda [(PVar "n",TApp (TApp (TVar "m") (TVar "succ")) (TVar "n"))])])
                  ,("mult",TLambda [(PVar "m",TLambda [(PVar "n",TApp (TApp (TVar "m") (TApp (TVar "add") (TVar "n"))) (TInt 0))])])
                  ,("pow",TLambda [(PVar "b",TLambda [(PVar "e",TApp (TVar "e") (TVar "b"))])])
                  ,("pred",TLambda [(PVar "n",TLambda [(PVar "g",TLambda [(PVar "x",TApp (TApp (TApp (TVar "n") (TLambda [(PVar "j",TLambda [(PVar "h",TApp (TVar "h") (TApp (TVar "j") (TVar "g")))])])) (TLambda [(PVar "u",TVar "x")])) (TLambda [(PVar "u",TVar "u")]))])])])
                  ,("sub",TLambda [(PVar "m",TLambda [(PVar "n",TApp (TApp (TVar "n") (TVar "pred")) (TVar "m"))])])
                  ,("iszero",TLambda [(PVar "n",TApp (TApp (TVar "n") (TLambda [(PVar "x",TVar "f")])) (TVar "t"))])
                  ,("leq",TLambda [(PVar "m",TLambda [(PVar "n",TApp (TVar "iszero") (TApp (TApp (TVar "sub") (TVar "m")) (TVar "n")))])])
                  ,("eq",TLambda [(PVar "m",TLambda [(PVar "n",TApp (TApp (TVar "and") (TApp (TApp (TVar "leq") (TVar "m")) (TVar "n"))) (TApp (TApp (TVar "leq") (TVar "n")) (TVar "m")))])])
                  ,("Y",TLambda [(PVar "func",TApp (TLambda [(PVar "arg",TApp (TVar "func") (TApp (TVar "arg") (TVar "arg")))]) (TLambda [(PVar "arg",TApp (TVar "func") (TApp (TVar "arg") (TVar "arg")))]))])
                  ,("Z",TLambda [(PVar "func",TApp (TLambda [(PVar "arg",TApp (TVar "func") (TLambda [(PVar "v",TApp (TApp (TVar "arg") (TVar "arg")) (TVar "v"))]))]) (TLambda [(PVar "arg",TApp (TVar "func") (TLambda [(PVar "v",TApp (TApp (TVar "arg") (TVar "arg")) (TVar "v"))]))]))])
                  ,("flip",TLambda [(PVar "h",TLambda [(PVar "x",TLambda [(PVar "y",TApp (TApp (TVar "h") (TVar "y")) (TVar "x"))])])])
                  ,("force",TLambda [(PSeq (PVar "x") (PVar "xs"),TSeq (TVar "x") (TVar "xs")),(PVar "x",TVar "x")])
                  ,("isseq",TLambda [(PSeq (PVar "x") (PVar "xs"),TVar "t"),(PVar "x",TVar "f")])
                  ,("seqhead",TLambda [(PSeq (PVar "x") (PVar "xs"),TVar "x"),(PVar "x",TVar "x")])
                  ,("seqtail",TLambda [(PSeq (PVar "x") (PVar "xs"),TVar "xs"),(PVar "x",TVar "x")])
                  ,("fast",TLambda [(PVar "n",TLambda [(PVar "x",TMult (TVar "x") (TVar "n"))])])
                  ,("slow",TLambda [(PVar "n",TLambda [(PVar "x",TDiv (TVar "x") (TVar "n"))])])
                  ,("revrun",TApp (TVar "Y") (TLambda [(PVar "h",TLambda [(PVar "n",TApp (TApp (TApp (TVar "if") (TApp (TVar "iszero") (TApp (TVar "pred") (TVar "n")))) (TInt 0)) (TSeq (TApp (TVar "pred") (TVar "n")) (TApp (TVar "h") (TApp (TVar "pred") (TVar "n")))))])]))
                  ,("seqlen",TApp (TVar "Y") (TLambda [(PVar "h",TLambda [(PSeq (PVar "y") (PVar "ys"),TApp (TVar "succ") (TApp (TVar "h") (TVar "ys"))),(PVar "y",TInt 1)])]))
                  ,("seqdrop",TLambda [(PVar "n",TLambda [(PVar "y",TApp (TApp (TVar "n") (TVar "seqtail")) (TVar "y"))])])
                  ,("seqtake",TApp (TVar "Y") (TLambda [(PVar "h",TLambda [(PVar "n",TLambda [(PSeq (PVar "y") (PVar "ys"),TApp (TApp (TApp (TVar "if") (TApp (TApp (TVar "leq") (TVar "n")) (TInt 1))) (TVar "y")) (TSeq (TVar "y") (TApp (TApp (TVar "h") (TApp (TVar "pred") (TVar "n"))) (TVar "ys")))),(PVar "y",TVar "y")])])]))
                  ,("seqfront",TLambda [(PVar "x",TApp (TApp (TVar "seqtake") (TApp (TVar "pred") (TApp (TVar "seqlen") (TVar "x")))) (TVar "x"))])
                  ,("seqlast",TLambda [(PVar "x",TApp (TApp (TApp (TVar "seqlen") (TVar "x")) (TVar "seqtail")) (TVar "x"))])
                  ,("rot",TLambda [(PVar "n",TLambda [(PVar "y",TApp (TApp (TVar "n") (TLambda [(PVar "z",TSeq (TApp (TVar "seqlast") (TVar "z")) (TApp (TVar "seqfront") (TVar "z")))])) (TVar "y"))])])
                  ,("replicate",TApp (TVar "Y") (TLambda [(PVar "h",TLambda [(PVar "k",TLambda [(PVar "x",TApp (TApp (TApp (TVar "if") (TApp (TVar "iszero") (TApp (TVar "pred") (TVar "k")))) (TVar "x")) (TSeq (TVar "x") (TApp (TApp (TVar "h") (TApp (TVar "pred") (TVar "k"))) (TVar "x"))))])])]))
                  ,("squeeze",TLambda [(PVar "h",TLambda [(PVar "x",TLambda [(PVar "y",TApp (TApp (TApp (TApp (TVar "replicate") (TApp (TVar "seqlen") (TVar "x"))) (TVar "h")) (TVar "x")) (TApp (TApp (TVar "replicate") (TApp (TVar "seqlen") (TVar "x"))) (TVar "y")))])])])
                  ,("rev",TApp (TVar "Y") (TLambda [(PVar "h",TLambda [(PSeq (PVar "y") (PVar "ys"),TSeq (TApp (TVar "h") (TVar "ys")) (TVar "y")),(PVar "z",TVar "z")])]))
                  ]

-- the idea is that the structure will always come from the right term and the values in the left term will be matched against them
applySeqToSeq :: TermF -> TermF -> TermF
applySeqToSeq t1 t2 = toFSeq $ map (\i -> zs!!i) [x*l2 | x <- [0..n2-1]]
                    where s1 = getFSeq t1
                          s2 = getFSeq t2
                          n1 = length s1
                          n2 = length s2
                          l = lcm n1 n2
                          l1 = div l n1
                          l2 = div l n2
                          f1 = concatMap (\x -> take l1 $ repeat x) s1
                          f2 = concatMap (\x -> take l2 $ repeat x) s2
                          zs = zipWith (\x y -> apply x y) f1 f2

apply :: TermF -> TermF -> TermF
apply (FLambda f) t = f t
apply t1 t2 = applySeqToSeq t1 t2
