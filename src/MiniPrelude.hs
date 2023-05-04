{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, OverloadedStrings #-}

module MiniPrelude where

import qualified Prelude as P

import qualified Sound.Tidal.Context as T

import qualified Data.Map as Map

import Functional (lift2, apply, ($))

type Pattern = T.Pattern
type ControlPattern = T.ControlPattern
type ValueMap = T.ValueMap
type Value = T.Value
type Map = Map.Map
type Time = T.Time
type Int = P.Int
type Double = P.Double
type Char = P.Char
type String = P.String
type Bool = P.Bool


-- type family Pat x where
--   Pat (a -> b) = Pattern (Pat a -> Pat b)
--   Pat (Pattern a) = Pattern a
--   Pat a = Pattern a
--
-- class PatClass a where
--   toPat :: a -> Pat a
--
-- instance PatClass P.Bool where
--   toPat = P.pure

infixr 0 $$
($$) :: Pattern (Pattern (Pattern a -> Pattern b) -> Pattern (Pattern a -> Pattern b))
($$) = P.pure (\x -> P.pure (\y -> apply x y))

t :: Pattern Bool
t = P.pure P.True

f :: Pattern Bool
f = P.pure P.False

id :: Pattern (Pattern a -> Pattern a)
id = P.pure (\x -> x)

const :: Pattern (Pattern a -> Pattern (Pattern b -> Pattern a))
const = P.pure (\x -> P.pure (\_ -> x))

rev :: Pattern (Pattern a -> Pattern a)
rev = P.pure T.rev

fast :: Pattern (Pattern Time -> Pattern (Pattern a -> Pattern a))
fast = lift2 T.fast

slow :: Pattern (Pattern Time -> Pattern (Pattern a -> Pattern a))
slow = lift2 T.slow

ply :: Pattern (Pattern Time -> Pattern (Pattern a -> Pattern a))
ply = lift2 T.ply

rot :: P.Ord a => Pattern (Pattern Int -> Pattern (Pattern a -> Pattern a))
rot = lift2 T.rot

run :: Pattern (Pattern Int -> Pattern Int)
run = P.pure T.run

irand :: Pattern (Pattern Int -> Pattern Int)
irand = P.pure T.irand

rotL :: Pattern (Pattern Time -> Pattern (Pattern a -> Pattern a))
rotL = lift2 (\x y -> x T.<~ y)

rotR :: Pattern (Pattern Time -> Pattern (Pattern a -> Pattern a))
rotR = lift2 (\x y -> x T.~> y)

struct :: Pattern (Pattern Bool -> Pattern (Pattern a -> Pattern a))
struct = lift2 T.struct

toBool :: Pattern (Pattern a -> Pattern Bool)
toBool = P.pure (P.fmap (\_ -> P.True))

structFrom :: Pattern (Pattern b -> Pattern (Pattern a -> Pattern a))
structFrom = P.pure (\b -> P.pure (T.struct (apply toBool b)))

mask :: Pattern (Pattern Bool -> Pattern (Pattern a -> Pattern a))
mask = lift2 T.mask

every :: Pattern (Pattern Int -> Pattern (Pattern (Pattern a -> Pattern a) -> Pattern (Pattern a -> Pattern a)))
every = P.pure (\i -> P.pure (\g -> P.pure (T.every i (apply g))))

while :: Pattern (Pattern Bool -> Pattern (Pattern (Pattern a -> Pattern a) -> Pattern (Pattern a -> Pattern a)))
while = P.pure (\i -> P.pure (\g -> P.pure (T.while i (apply g))))

superimpose :: Pattern (Pattern (Pattern a -> Pattern a) -> Pattern (Pattern a -> Pattern a))
superimpose = P.pure (\g -> P.pure (T.superimpose (apply g)))

sometimes :: Pattern (Pattern (Pattern a -> Pattern a) -> Pattern (Pattern a -> Pattern a))
sometimes = P.pure (\g -> P.pure (T.sometimes (apply g)))

rarely :: Pattern (Pattern (Pattern a -> Pattern a) -> Pattern (Pattern a -> Pattern a))
rarely = P.pure (\g -> P.pure (T.rarely (apply g)))

toInt :: Pattern (Pattern Bool -> Pattern Int)
toInt = P.pure (P.fmap (\x -> if x then 1 else 0))


(+) :: P.Num a => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(+) = lift2 (\x y -> x P.+ y)

(+|) :: P.Num a => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(+|) = right (+)

(|+) :: P.Num a => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(|+) = left (+)


(-) :: P.Num a => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(-) = lift2 (\x y -> x P.- y)

(-|) :: P.Num a => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(-|) = right (-)

(|-) :: P.Num a => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(|-) = left (-)


(|*|) :: P.Num a => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(|*|) = lift2 (\x y -> x P.* y)

(*|) :: P.Num a => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(*|) = right (|*|)

(|*) :: P.Num a => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(|*) = left (|*|)

(//) :: (P.Num a, P.Fractional a) => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(//) = lift2 (\x y -> x P./ y)


-- control pattern stuff

n :: Pattern (Pattern Int -> ControlPattern)
n = P.pure (\m -> T.n $ P.fmap (\x -> T.Note $ P.fromIntegral x) m)

s :: Pattern (Pattern String -> ControlPattern)
s = P.pure T.s

room :: Pattern (Pattern Double -> ControlPattern)
room = P.pure T.room

size :: Pattern (Pattern Double -> ControlPattern)
size = P.pure T.size

(#) :: Pattern (ControlPattern -> Pattern (ControlPattern -> ControlPattern))
(#) = lift2 (\x y -> x T.# y)

-- meta functions to get the structure

right :: Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
right op = P.pure (\x -> P.pure (\y -> apply (apply structFrom y) (apply (apply op x) y)))

left :: Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
left op = P.pure (\x -> P.pure (\y -> apply (apply structFrom x) (apply (apply op x) y)))
