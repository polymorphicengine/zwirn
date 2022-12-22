module MiniPrelude where

import qualified Prelude as P
import Functional

app :: TermF
app = FLambda (\f -> (FLambda (\t -> apply f t)))

appR :: TermF
appR = FLambda (\f -> (FLambda (\t -> applySeqR f t)))

appL :: TermF
appL = FLambda (\f -> (FLambda (\t -> applySeqL f t)))

id :: TermF
id = FLambda (\x -> x)

const :: TermF
const = FLambda (\x -> FLambda (\y -> x))

--logic

not :: TermF
not = boolToTerm P.not

and :: TermF
and = boolToTerm2 (P.&&)

or :: TermF
or = boolToTerm2 (P.||)


iff :: TermF
iff = (FLambda (\b -> FLambda (\t1 -> FLambda (\t2 -> _iff b t1 t2))))
    where _iff (FBool P.True) t1 t2 = t1
          _iff (FBool P.False) t1 t2 = t2
          _iff b@(FSeq _ _) t1 t2 = apply (apply (applySeqR iff b) t1) t2
          _iff b@(FStack _ _) t1 t2 = apply (apply (applyStack iff b) t1) t2
          _iff x _ _ = x

-- some arithmetic functions

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

-- some tidal functions

-- speed the term up by a number
fast :: TermF
fast = FLambda (\t1 -> FLambda (\t2 -> FMult t2 t1))

-- slow the term down by a number
slow :: TermF
slow = FLambda (\t1 -> FLambda (\t2 -> FDiv t2 t1))

-- ply the elements of a sequence by a number
ply :: TermF
ply = (FLambda (\pat -> (case pat of n -> (FLambda (\pat -> (case pat of x -> (apply (apply map (apply fast n)) x)))))))

-- reverse a pattern cyclewise
_rev :: TermF
_rev = FLambda f
     where f (FSeq x xs) = apply (apply append (apply _rev xs)) (apply _rev x)
           f (FStack x xs) = FStack (apply _rev x) (apply _rev xs)
           f (FDiv x n) = FDiv (apply _rev x) n
           f (FMult x n) = FMult (apply _rev x) n
           f x = x

-- reverse a pattern completely
rev :: TermF
rev = apply inside _rev

-- append a value to a sequence as last element
append :: TermF
append = FLambda (\t1 -> FLambda (\t2 -> f t1 t2))
       where f (FSeq x xs) ts = FSeq x (apply (apply append xs) ts)
             f FEmpty t = FSeq t FEmpty
             f x t = FSeq x (FSeq t FEmpty)

-- concat two sequences into one, cyclewise
fastcat :: TermF
fastcat = FLambda (\t1 -> FLambda (\t2 -> f t1 t2))
           where f (FSeq x xs) ts = FSeq x (apply (apply fastcat xs) ts)
                 f FEmpty t = t
                 f x t = FSeq x t

-- concat two sequences into one, respecting their respective periods
cat :: TermF
cat = FLambda (\t1 -> FLambda (\t2 -> f t1 t2))
           where f t1 t2 = apply (apply slow s) (apply (apply fastcat s1) s2)
                         where p1 = apply period t1
                               p2 = apply period t2
                               s = apply (apply add p1) p2
                               s1 = apply (apply fast p1) t1
                               s2 = apply (apply fast p2) t2

while :: TermF
while = FLambda (\b -> FLambda (\g -> FLambda (\x -> apply (apply (apply iff b) (apply (apply map g) x)) x)))

--some utility functions

-- calculate the period of a pattern, i.e. the number of cycles it needs to repeat itself
period :: TermF
period = FLambda f
       where f (FSeq x xs) = apply (apply lcm (apply period x)) (apply period xs)
             f (FStack x xs) = apply (apply lcm (apply period x)) (apply period xs)
             f (FDiv x n) = apply (apply mult (apply period x)) n
             f _ = FInt 1

-- apply a function that works cycle-wise and the resulting function will respect the period of the pattern
inside :: TermF
inside = FLambda (\f -> FLambda (\t -> apply (apply slow (apply period t)) (apply f (apply (apply fast (apply period t)) t))))

-- maps the function in the first argument into the structure
map :: TermF
map = FLambda (\g -> FLambda (\x -> h g x))
    where h g (FSeq x xs) = FSeq (apply g x) (apply (apply map g) xs)
          h g (FStack x xs) = FStack (apply g x) (apply (apply map g) xs)
          h g (FDiv x n) = FDiv (apply g x) n
          h g (FMult x n) = FMult (apply g x) n
          h g FEmpty = FEmpty
          h g x = apply g x

-- return the head of a sequence
seqhead :: TermF
seqhead = FLambda (\t -> case t of (FSeq x _) -> x; x -> x)

-- return the tail of a sequence
seqtail :: TermF
seqtail = FLambda (\t -> case t of (FSeq _ xs) -> xs; x -> x)

-- calculate the length of a sequence
seqlen :: TermF
seqlen = FLambda g
       where g (FSeq x xs) = apply (apply add (FInt 1)) (apply seqlen xs)
             g FEmpty = FInt 0
             g x = FInt 1
