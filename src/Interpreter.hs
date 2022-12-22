module Interpreter where

import qualified Prelude as P
import Data.List (intercalate)

type Var = P.String

data TermF = FVar Var
           | FInt P.Int
           | FBool P.Bool
           | FRest
           | FEmpty
           | FSeq TermF TermF
           | FStack TermF TermF
           | FMult TermF TermF
           | FDiv TermF TermF
           | FLambda (TermF -> TermF)

displayTermF :: TermF -> P.String
displayTermF (FVar x) = x
displayTermF (FInt i) = P.show i
displayTermF (FBool P.True) = "t"
displayTermF (FBool P.False) = "f"
displayTermF (FRest) = "~"
displayTermF FEmpty = ""
displayTermF t@(FSeq _ _) = "(" P.++ (intercalate " " P.$  P.map displayTermF (removeEmpty P.$ getFSeq t)) P.++ ")"
displayTermF (FStack t1 t2) = displayTermF t1 P.++ "," P.++ displayTermF t2
displayTermF (FMult t1 t2) = displayTermF t1 P.++ "*" P.++ displayTermF t2
displayTermF (FDiv t1 t2) = displayTermF t1 P.++ "/" P.++ displayTermF t2
displayTermF (FLambda ps) = P.error "Can't display functions"

removeEmpty :: [TermF] -> [TermF]
removeEmpty [] = []
removeEmpty (FEmpty:xs) = xs
removeEmpty (x:xs) = x:removeEmpty xs

getFSeq :: TermF -> [TermF]
getFSeq (FSeq t1 t2) = t1:(getFSeq t2)
getFSeq t = [t]

toFSeq :: [TermF] -> TermF
toFSeq [] = FRest
toFSeq [t] = FSeq t FEmpty
toFSeq (t:ts) = FSeq t (toFSeq ts)


-- the idea is that the structure will always come from the right term and the values in the left term will be matched against them
applySeqR :: TermF -> TermF -> TermF
applySeqR t1 t2 = toFSeq (P.map (\i -> (P.!!) zs i) [(P.*) x l2 | x <- [0..(P.-) n2 1]])
                    where s1 = removeEmpty P.$ getFSeq t1
                          s2 = removeEmpty P.$ getFSeq t2
                          n1 = P.length s1
                          n2 = P.length s2
                          l = P.lcm n1 n2
                          l1 = P.div l n1
                          l2 = P.div l n2
                          f1 = P.concatMap (\x -> P.take l1 (P.repeat x)) s1
                          f2 = P.concatMap (\x -> P.take l2 (P.repeat x)) s2
                          zs = P.zipWith (\x y -> apply x y) f1 f2

applySeqL :: TermF -> TermF -> TermF
applySeqL t1 t2 = toFSeq (P.map (\i -> (P.!!) zs i) [(P.*) x l1 | x <- [0..(P.-) n1 1]])
                    where s1 = removeEmpty P.$ getFSeq t1
                          s2 = removeEmpty P.$ getFSeq t2
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
apply t1@(FSeq _ _) t2 = applySeqR t1 t2
apply FEmpty t2 = t2
apply _ _ = P.error "Cannot apply these terms!"

app :: TermF
app = FLambda (\f -> (FLambda (\t -> apply f t)))

appR :: TermF
appR = FLambda (\f -> (FLambda (\t -> applySeqR f t)))

appL :: TermF
appL = FLambda (\f -> (FLambda (\t -> applySeqL f t)))

intToTerm :: (P.Int -> P.Int) -> TermF
intToTerm f = FLambda g
            where g (FInt i) = FInt (f i)
                  g (FSeq t1 t2) = FSeq (apply (intToTerm f) t1) (apply (intToTerm f) t2)
                  g (FStack t1 t2) = FStack (apply (intToTerm f) t1) (apply (intToTerm f) t2)
                  g (FDiv t n) = FDiv (apply (intToTerm f) t) n
                  g (FMult t n) = FMult (apply (intToTerm f) t) n
                  g x = x -- or throw an error maybe ?

boolToTerm :: (P.Bool -> P.Bool) -> TermF
boolToTerm f = FLambda g
            where g (FBool i) = FBool (f i)
                  g (FSeq t1 t2) = FSeq (apply (boolToTerm f) t1) (apply (boolToTerm f) t2)
                  g (FStack t1 t2) = FStack (apply (boolToTerm f) t1) (apply (boolToTerm f) t2)
                  g (FDiv t n) = FDiv (apply (boolToTerm f) t) n
                  g (FMult t n) = FMult (apply (boolToTerm f) t) n
                  g x = x

intToTerm2 :: (P.Int -> P.Int -> P.Int) -> TermF
intToTerm2 f = FLambda g
            where g (FInt i) = (intToTerm (f i))
                  g (FSeq t1 t2) = FSeq (apply (intToTerm2 f) t1) (apply (intToTerm2 f) t2)
                  g (FStack t1 t2) = FStack (apply (intToTerm2 f) t1) (apply (intToTerm2 f) t2)
                  g (FDiv t n) = FDiv (apply (intToTerm2 f) t) n
                  g (FMult t n) = FMult (apply (intToTerm2 f) t) n
                  g x = x

boolToTerm2 :: (P.Bool -> P.Bool -> P.Bool) -> TermF
boolToTerm2 f = FLambda g
            where g (FBool b) = (boolToTerm (f b))
                  g (FSeq t1 t2) = FSeq (apply (boolToTerm2 f) t1) (apply (boolToTerm2 f) t2)
                  g (FStack t1 t2) = FStack (apply (boolToTerm2 f) t1) (apply (boolToTerm2 f) t2)
                  g (FDiv t n) = FDiv (apply (boolToTerm2 f) t) n
                  g (FMult t n) = FMult (apply (boolToTerm2 f) t) n
                  g x = x

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
          _iff b@(FSeq _ _) t1 t2 = applySeqL (applySeqL (applySeqR iff b) t1) t2
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
           f (FStack x xs) = FSeq (apply _rev x) (apply _rev xs)
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
       where f (FSeq x xs) = apply (apply Interpreter.lcm (apply period x)) (apply period xs)
             f (FStack x xs) = apply (apply Interpreter.lcm (apply period x)) (apply period xs)
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
