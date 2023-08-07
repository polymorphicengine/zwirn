{-# LANGUAGE TypeApplications #-}
module Zwirn.Interactive.Prelude.MiniPrelude where

import qualified Prelude as P
import qualified Sound.Tidal.Context as T hiding (fromList)

import Zwirn.Interactive.Types
import Zwirn.Interactive.Transform
import Zwirn.Interactive.Convert
import Zwirn.Interactive.TidalT

($) :: Pat b => P ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
($) = toPat apply

(|$|) :: Pat b => P ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
(|$|) = toPat applyBoth

(|$) :: Pat b => P ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
(|$) = toPat applyLeft

($|) :: Pat b => P ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
($|) = toPat applyRight

t :: P NumberPattern
t = toPat $$ toTarget (1 :: P.Int)

f :: P NumberPattern
f = toPat $$ toTarget (0 :: P.Int)

id :: Pat a => P (Pattern a -> Pattern a)
id = toPat (P.id :: Pattern a -> Pattern a)

const :: Pat a => P (Pattern a -> Pattern b -> Pattern a)
const = toPat (P.const :: Pattern a -> Pattern b -> Pattern a)

tick :: Pat b => P (Pattern a -> (Pattern a -> Pattern b) -> Pattern b)
tick = toPat (P.flip apply)

(.) :: (Pat b, Pat d) => P ((Pattern b -> Pattern d) -> (Pattern a -> Pattern b)-> Pattern a -> Pattern d)
(.) = toPat compose
    where compose = (P..) :: ((Pattern b -> Pattern d) -> (Pattern a -> Pattern b)-> Pattern a -> Pattern d)

(++) :: P (TextPattern -> TextPattern -> TextPattern)
(++) = toPat $$ lift2 (toTarget ((P.++) :: P.String -> P.String -> P.String))

rev :: Pat a => P (Pattern a -> Pattern a)
rev = toPat T.rev

fast :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
fast = toPat (\x -> T.fast (fromTarget x))

(*) :: Pat a => P (Pattern a -> NumberPattern -> Pattern a)
(*) = toPat (\x n -> T.fast (fromTarget n) x)

slow :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
slow = toPat (\x -> T.slow (fromTarget x))

(/) ::  Pat a => P (Pattern a -> NumberPattern -> Pattern a)
(/) = toPat (\x n -> T.slow (fromTarget n) x)

ply :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
ply = toPat (\x -> T.ply (fromTarget x))

plyWith :: Pat a => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
plyWith = toPat (\x -> (T.plyWith :: Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) (fromTarget x))

-- rot :: (Pat a, P.Ord a) => P (NumberPattern -> Pattern a -> Pattern a)
-- rot = toPat (\x -> T.rot $$ fromTarget x)

run :: P (NumberPattern -> NumberPattern)
run = toPat $$ toTarget (T.run :: Pattern Double -> Pattern Double)

irand :: P (NumberPattern -> NumberPattern)
irand = toPat $$ toTarget (T.irand :: Pattern Int -> Pattern Double)

rotL :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
rotL = toPat (\x y -> (fromTarget x) T.<~ y)

(<~) :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
(<~) = rotL

rotR :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
rotR = toPat (\x y -> (fromTarget x) T.~> y)

(~>) :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
(~>) = rotR

struct :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
struct = toPat (\n x -> T.struct (fromTarget n) x)

mask :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
mask = toPat (\n x -> T.mask (fromTarget n) x)

every :: Pat a => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
every = toPat (\x -> T.every $$ fromTarget x)

while :: Pat a => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
while = toPat (\x -> T.while $$ fromTarget x)

superimpose :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
superimpose = toPat T.superimpose

jux :: P ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)
jux = toPat T.jux

iter :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
iter = toPat (\x -> T.iter $$ fromTarget x)

sometimes :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
sometimes = toPat T.sometimes

rarely :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
rarely = toPat T.rarely

degrade :: Pat a =>  P (Pattern a -> Pattern a)
degrade = toPat T.degrade

degradeBy :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
degradeBy = toPat (\x -> T.degradeBy $$ fromTarget x)

(?) :: Pat a => P (Pattern a -> NumberPattern -> Pattern a)
(?) = toPat (\x d -> T.degradeBy (fromTarget d) x)

timeLoop :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
timeLoop = toPat (\tm -> T.timeLoop $$ fromTarget tm)

loop :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
loop = timeLoop

-- arithmetic

(+) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(+) = toPat (lift2 (P.+))

(+|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(+|) = right (+)

(|+) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|+) = left (+)


(-) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(-) = toPat (lift2 (P.-))

(-|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(-|) = right (-)

(|-) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|-) = left (-)


(|*|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|*|) = toPat (lift2 (P.*))

(*|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(*|) = right (|*|)

(|*) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|*) = left (|*|)

(//) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(//) = toPat (lift2 (P./))

(/|) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(/|) = right (//)

(|/) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(|/) = left (//)

round :: P (NumberPattern -> NumberPattern)
round = toPat $$ toTarget (lift (P.round :: Double -> Int))

floor :: P (NumberPattern -> NumberPattern)
floor = toPat $$ toTarget (lift (P.floor :: Double -> Int))

show :: Show a => P (Pattern a -> TextPattern)
show = toPat showT



-- samples

bd :: P TextPattern
bd = P.pure $$ Text "bd"

sn :: P TextPattern
sn = P.pure $$ Text "sn"

--

c :: P NumberPattern
c = 0

e :: P NumberPattern
e = 4


-- continous

sine :: P NumberPattern
sine = toTarget (T.sine :: Pattern Double)

rand :: P NumberPattern
rand = toTarget (T.rand :: Pattern Double)

perlin :: P NumberPattern
perlin = toTarget (T.perlin :: Pattern Double)

saw :: P NumberPattern
saw = toTarget (T.saw :: Pattern Double)

tri :: P NumberPattern
tri = toTarget (T.tri :: Pattern Double)

smooth :: P (NumberPattern -> NumberPattern)
smooth = toPat $$ toTarget (T.smooth :: Pattern Double -> Pattern Double)

segment :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
segment = toPat $$ (\x -> T.segment $$ fromTarget x)

range :: P (NumberPattern -> NumberPattern -> NumberPattern -> NumberPattern)
range = toPat $$ toTarget (T.range ::  Pattern Double -> Pattern Double -> Pattern Double -> Pattern Double)


--- comparisons

(>=) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(>=) = toPat $$ toTarget geqT

(<=) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(<=) = toPat $$ toTarget leqT

(==) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(==) = toPat $$ toTarget eqT

(&&) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(&&) = toPat $$ toTarget andT

(||) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(||) = toPat $$ toTarget orT
