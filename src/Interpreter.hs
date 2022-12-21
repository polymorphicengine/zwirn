module Interpreter where

import Prelude as P

type Var = String

data TermF = FVar Var
           | FInt Int
           | FRest
           | FEmpty
           | FSeq TermF TermF
           | FStack TermF TermF
           | FMult TermF TermF
           | FDiv TermF TermF
           | FLambda (TermF -> TermF)

getFSeq :: TermF -> [TermF]
getFSeq (FSeq t1 t2) = t1:(getFSeq t2)
getFSeq t = [t]

toFSeq :: [TermF] -> TermF
toFSeq [] = FRest
toFSeq [t] = t
toFSeq (t:ts) = FSeq t (toFSeq ts)


-- the idea is that the structure will always come from the right term and the values in the left term will be matched against them
applySeqToSeq :: TermF -> TermF -> TermF
applySeqToSeq t1 t2 = toFSeq P.$ map (\i -> zs!!i) [x*l2 | x <- [0..n2-1]]
                    where s1 = getFSeq t1
                          s2 = getFSeq t2
                          n1 = length s1
                          n2 = length s2
                          l = P.lcm n1 n2
                          l1 = div l n1
                          l2 = div l n2
                          f1 = concatMap (\x -> take l1 (repeat x)) s1
                          f2 = concatMap (\x -> take l2 (repeat x)) s2
                          zs = zipWith (\x y -> apply x y) f1 f2

apply :: TermF -> TermF -> TermF
apply (FLambda f) t = f t
apply t1 t2 = applySeqToSeq t1 t2

intToTerm :: (Int -> Int) -> TermF
intToTerm f = FLambda g
            where g (FInt i) = FInt (f i)
                  g (FSeq t1 t2) = FSeq (apply (intToTerm f) t1) (apply (intToTerm f) t2)
                  g (FStack t1 t2) = FStack (apply (intToTerm f) t1) (apply (intToTerm f) t2)
                  g (FDiv t n) = FDiv (apply (intToTerm f) t) n
                  g (FMult t n) = FMult (apply (intToTerm f) t) n
                  g x = x

intToTerm2 :: (Int -> Int -> Int) -> TermF
intToTerm2 f = FLambda g
            where g (FInt i) = (intToTerm $ f i)
                  g (FSeq t1 t2) = FSeq (apply (intToTerm2 f) t1) (apply (intToTerm2 f) t2)
                  g (FStack t1 t2) = FStack (apply (intToTerm2 f) t1) (apply (intToTerm2 f) t2)
                  g (FDiv t n) = FDiv (apply (intToTerm2 f) t) n
                  g (FMult t n) = FMult (apply (intToTerm2 f) t) n
                  g x = x

id :: TermF
id = FLambda (\x -> x)

succ :: TermF
succ = intToTerm P.succ

pred :: TermF
pred = intToTerm P.pred

add :: TermF
add = intToTerm2 (P.+)

mult :: TermF
mult  = intToTerm2 (P.*)

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
rev = FLambda f
     where f (FSeq x xs) = FSeq (apply rev xs) (apply rev x)
           f (FStack x xs) = FSeq (apply rev x) (apply rev xs)
           f (FDiv x n) = FDiv (apply rev x) n
           f (FMult x n) = FMult (apply rev x) n
           f x = x

_cat :: TermF
_cat = FLambda (\t1 -> FLambda (\t2 -> f t1 t2))
           where f (FSeq x xs) ts = FSeq x (apply (apply _cat xs) ts)
                 f FEmpty t = t
                 f x t = FSeq x t

cat :: TermF
cat = FLambda (\t1 -> FLambda (\t2 -> f t1 t2))
           where f t1 t2 = apply (apply slow s) (apply (apply _cat s1) s2)
                         where p1 = apply period t1
                               p2 = apply period t2
                               s = apply (apply add p1) p2
                               s1 = apply (apply fast p1) t1
                               s2 = apply (apply fast p2) t2

period :: TermF
period = FLambda f
       where f (FSeq x xs) = apply (apply Interpreter.lcm (apply period x)) (apply period xs)
             f (FStack x xs) = apply (apply Interpreter.lcm (apply period x)) (apply period xs)
             f (FDiv x n) = apply (apply mult (apply period x)) n
             f _ = FInt 1

inside :: TermF
inside = FLambda (\f -> FLambda (\t -> apply (apply slow (apply period t)) (apply f (apply (apply fast (apply period t)) t))))
