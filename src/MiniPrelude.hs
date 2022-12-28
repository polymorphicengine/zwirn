module MiniPrelude where

import qualified Prelude as P
import Functional

-- app :: Mini a
-- app = FLambda (\f -> (FLambda (\t -> apply f t)))
--
-- appR :: Mini a
-- appR = FLambda (\f -> (FLambda (\t -> applySeqR f t)))
--
-- appL :: Mini a
-- appL = FLambda (\f -> (FLambda (\t -> applySeqL f t)))
--
id :: Mini (Mini a -> Mini a)
id = FVal (\x -> x)

const :: Mini (Mini a -> Mini (Mini b -> Mini a))
const = FVal (\x -> FVal (\y -> x))

--logic

not :: Mini (Mini Bool -> Mini Bool)
not = pure $ P.fmap P.not

and :: Mini (Mini Bool -> Mini (Mini Bool -> Mini Bool))
and = pure $ lift2 (P.&&)

or :: Mini (Mini Bool -> Mini (Mini Bool -> Mini Bool))
or = pure $ lift2 (P.||)

--TODO fix
iff :: Mini (Mini Bool -> Mini (Mini a -> Mini (Mini a -> Mini a)))
iff = (FVal (\b -> FVal (\t1 -> FVal (\t2 -> _iff b t1 t2))))
    where _iff (FVal P.True) t1 t2 = t1
          _iff (FVal P.False) t1 t2 = t2
          _iff b@(FSeq _ _) t1 t2 = applySSSR (applySSSR (applySeqR iff b) t1) t2
          _iff b@(FStack _ _) t1 t2 = apply (apply (apply iff b) t1) t2
          _iff x _ _ = P.undefined

-- some arithmetic functions

succ :: Mini (Mini Int -> Mini Int)
succ = pure $ fmap P.succ

pred :: Mini (Mini Int -> Mini Int)
pred = pure $ fmap P.pred

add :: Mini (Mini Int -> Mini (Mini Int -> Mini Int))
add = pure $ lift2 (P.+)

mult :: Mini (Mini Int -> Mini (Mini Int -> Mini Int))
mult  = pure $ lift2 (P.*)

lcm :: Mini (Mini Int -> Mini (Mini Int -> Mini Int))
lcm = pure $ lift2 P.lcm

-- some tidal functions

-- speed the term up by a number
fast :: Mini (Mini Int -> Mini (Mini a -> Mini a))
fast = FVal (\x -> FVal (\y -> FMult y x))

-- slow the term down by a number
slow :: Mini (Mini Int -> Mini (Mini a -> Mini a))
slow = FVal (\x -> FVal (\y -> FDiv y x))
--
-- -- ply the elements of a sequence by a number
-- ply :: Mini a
-- ply = (FLambda (\pat -> (case pat of n -> (FLambda (\pat -> (case pat of x -> (apply (apply map (apply fast n)) x)))))))
--
--reverse a pattern cyclewise
_rev :: Mini (Mini a -> Mini a)
_rev = FVal f
     where f (FSeq x xs) = apply (apply append (apply _rev xs)) (apply _rev x)
           f (FStack x xs) = FStack (apply _rev x) (apply _rev xs)
           f (FDiv x n) = FDiv (apply _rev x) n
           f (FMult x n) = FMult (apply _rev x) n
           f x = x

-- reverse a pattern completely
rev :: Mini (Mini a -> Mini a)
rev = apply inside _rev

-- append a value to a sequence as last element
append :: Mini (Mini a -> Mini (Mini a -> Mini a))
append = FVal (\x -> FVal (\y -> f x y))
       where f (FSeq x xs) ts = FSeq x (apply (apply append xs) ts)
             f FEmpty t = FSeq t FEmpty
             f x t = FSeq x (FSeq t FEmpty)

-- concat two sequences into one, cyclewise
fastcat :: Mini (Mini a -> Mini (Mini a -> Mini a))
fastcat = FVal (\t1 -> FVal (\t2 -> f t1 t2))
           where f (FSeq x xs) ts = FSeq x (apply (apply fastcat xs) ts)
                 f FEmpty t = t
                 f x t = FSeq x t

-- concat two sequences into one, respecting their respective periods
cat :: Mini (Mini a -> Mini (Mini a -> Mini a))
cat = FVal (\t1 -> FVal(\t2 -> f t1 t2))
           where f t1 t2 = apply (apply slow s) (apply (apply fastcat s1) s2)
                         where p1 = apply period t1
                               p2 = apply period t2
                               s = apply (apply add p1) p2
                               s1 = apply (apply fast p1) t1
                               s2 = apply (apply fast p2) t2

-- while :: Mini (Mini Bool -> Mini (Mini a -> Mini (Mini a -> Mini a)))
-- while = FVal (\b -> FVal (\g -> FVal (\x -> apply (apply (apply iff b) (apply (apply map g) x)) x)))

--some utility functions

-- calculate the period of a pattern, i.e. the number of cycles it needs to repeat itself
period :: Mini (Mini a -> Mini Int)
period = FVal f
       where f (FSeq x xs) = apply (apply lcm (apply period x)) (apply period xs)
             f (FStack x xs) = apply (apply lcm (apply period x)) (apply period xs)
             f (FDiv x n) = apply (apply mult (apply period x)) n
             f _ = FVal 1

-- apply a function that works cycle-wise and the resulting function will respect the period of the pattern
inside :: Mini (Mini (Mini a -> Mini b) -> Mini (Mini a -> Mini b))
inside = FVal (\f -> FVal (\t -> apply (apply slow (apply period t)) (apply f (apply (apply fast (apply period t)) t))))

-- maps the function in the first argument into the structure
map :: Mini (Mini (Mini a -> Mini b) -> Mini (Mini a -> Mini b))
map = FVal (\g -> FVal (\x -> h g x))
    where h g (FSeq x xs) = FSeq (apply g x) (apply (apply map g) xs)
          h g (FStack x xs) = FStack (apply g x) (apply (apply map g) xs)
          h g (FDiv x n) = FDiv (apply g x) n
          h g (FMult x n) = FMult (apply g x) n
          h g FEmpty = FEmpty
          h g x = apply g x

-- return the head of a sequence
seqhead :: Mini (Mini a -> Mini a)
seqhead = FVal (\t -> case t of (FSeq x _) -> x; x -> x)

-- return the tail of a sequence
seqtail :: Mini (Mini a -> Mini a)
seqtail = FVal (\t -> case t of (FSeq _ xs) -> xs; x -> x)

-- calculate the length of a sequence
seqlen :: Mini (Mini a -> Mini Int)
seqlen = FVal g
       where g (FSeq x xs) = apply (apply add (FVal 1)) (apply seqlen xs)
             g FEmpty = FVal 0
             g x = FVal 1
