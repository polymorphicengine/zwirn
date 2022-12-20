module Interpreter where

import Language

import Data.Map as Map (Map, fromList)

import qualified Prelude as P

type Env = Map P.String Term

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
applySeqToSeq t1 t2 = toFSeq P.$ P.map (\i -> (P.!!) zs i) [(P.*) x l2 | x <- [0..(P.-) n2 1]]
                    where s1 = getFSeq t1
                          s2 = getFSeq t2
                          n1 = P.length s1
                          n2 = P.length s2
                          l = P.lcm n1 n2
                          l1 = P.div l n1
                          l2 = P.div l n2
                          f1 = P.concatMap (\x -> P.take l1 (P.repeat x)) s1
                          f2 = P.concatMap (\x -> P.take l2 (P.repeat x)) s2
                          zs = P.zipWith (\x y -> apply x y) f1 f2

apply :: TermF -> TermF -> TermF
apply (FLambda f) t = f t
apply t1 t2 = applySeqToSeq t1 t2

intToTerm :: (P.Int -> P.Int) -> TermF
intToTerm f = FLambda g
            where g (FInt i) = FInt (f i)
                  g (FSeq t1 t2) = FSeq (apply (intToTerm f) t1) (apply (intToTerm f) t2)
                  g (FStack t1 t2) = FStack (apply (intToTerm f) t1) (apply (intToTerm f) t2)
                  g (FDiv t n) = FDiv (apply (intToTerm f) t) n
                  g (FMult t n) = FMult (apply (intToTerm f) t) n
                  g x = x

intToTerm2 :: (P.Int -> P.Int -> P.Int) -> TermF
intToTerm2 f = FLambda g
            where g (FInt i) = FLambda h
                              where h (FInt j) = FInt (f i j)
                                    h x = x
                  g x = x


succ :: TermF
succ = intToTerm P.succ

pred :: TermF
pred = intToTerm P.pred

add :: TermF
add = intToTerm2 (P.+)

lcm :: TermF
lcm = intToTerm2 P.lcm

fast :: TermF
fast = FLambda (\t1 -> FLambda (\t2 -> FMult t2 t1))

slow :: TermF
slow = FLambda (\t1 -> FLambda (\t2 -> FDiv t2 t1))

seqhead :: TermF
seqhead = FLambda (\t -> case t of (FSeq x _) -> x; x -> x)

seqtail :: TermF
seqtail = FLambda (\t -> case t of (FSeq _ xs) -> xs; x -> x)

rev :: TermF
rev = FLambda (\t -> case t of (FSeq x xs) -> (FSeq (apply rev xs) x); x -> x)
