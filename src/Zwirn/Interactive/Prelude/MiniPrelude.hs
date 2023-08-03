{-# LANGUAGE TypeApplications #-}
module Zwirn.Interactive.Prelude.MiniPrelude where

import qualified Prelude as P
import qualified Sound.Tidal.Context as T hiding (fromList)

import Zwirn.Interactive.Types
import Zwirn.Interactive.Transform
import Zwirn.Interactive.Convert
import Zwirn.Interactive.TidalT

infixr 0 $
($) :: Pat b => P (Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b)
($) = toPat apply

(|$|) :: Pat b => P (Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b)
(|$|) = toPat applyBoth

(|$) :: Pat b => P (Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b)
(|$) = toPat applyLeft

($|) :: Pat b => P (Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b)
($|) = toPat applyRight

t :: Pattern Number
t = toPat $$ toTarget (1 :: P.Int)

f :: Pattern Number
f = toPat $$ toTarget (0 :: P.Int)

id :: Pat a => P (Pattern a -> Pattern a)
id = toPat (P.id :: Pattern a -> Pattern a)

const :: Pat a => P (Pattern a -> Pattern b -> Pattern a)
const = toPat (P.const :: Pattern a -> Pattern b -> Pattern a)

tick :: Pat b => P (Pattern a -> Pattern (Pattern a -> Pattern b) -> Pattern b)
tick = toPat (P.flip apply)

(.) :: (Pat b, Pat d) => P ((Pattern b -> Pattern d) -> (Pattern a -> Pattern b)-> Pattern a -> Pattern d)
(.) = toPat compose
    where compose = (P..) :: ((Pattern b -> Pattern d) -> (Pattern a -> Pattern b)-> Pattern a -> Pattern d)

leftOp :: Pat c => P (Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)))
leftOp = toPat left

rightOp :: Pat c => P (Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)))
rightOp = toPat right

(++) :: P (Pattern Text -> Pattern Text -> Pattern Text)
(++) = toPat $$ lift2 (toTarget ((P.++) :: P.String -> P.String -> P.String))

rev :: Pat a => P (Pattern a -> Pattern a)
rev = toPat T.rev

fast :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
fast = toPat (\x -> T.fast (fromTarget x))

(*) :: Pat a => P (Pattern a -> Pattern Number -> Pattern a)
(*) = toPat (\x n -> T.fast (fromTarget n) x)

slow :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
slow = toPat (\x -> T.slow (fromTarget x))

(/) ::  Pat a => P (Pattern a -> Pattern Number -> Pattern a)
(/) = toPat (\x n -> T.slow (fromTarget n) x)

ply :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
ply = toPat (\x -> T.ply (fromTarget x))

plyWith :: Pat a => P (Pattern Number -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
plyWith = toPat (\x -> (T.plyWith :: Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) (fromTarget x))

rot :: (Pat a, P.Ord a) => P (Pattern Number -> Pattern a -> Pattern a)
rot = toPat (\x -> T.rot $$ fromTarget x)

run :: P (Pattern Number -> Pattern Number)
run = toPat $$ toTarget (T.run :: Pattern Double -> Pattern Double)

irand :: P (Pattern Number -> Pattern Number)
irand = toPat $$ toTarget (T.irand :: Pattern Int -> Pattern Double)

rotL :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
rotL = toPat (\x y -> (fromTarget x) T.<~ y)

(<~) :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
(<~) = rotL

rotR :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
rotR = toPat (\x y -> (fromTarget x) T.~> y)

(~>) :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
(~>) = rotR

struct :: Pat a => P (Pattern Bool -> Pattern a -> Pattern a)
struct = toPat T.struct

toBool :: P (Pattern a -> Pattern Bool)
toBool = P.pure (P.fmap (\_ -> P.True))

structFrom :: P (Pattern b -> Pattern a -> Pattern a)
structFrom = P.pure (\b -> P.pure (T.struct (apply toBool b)))

mask :: Pat a => P (Pattern Bool -> Pattern a -> Pattern a)
mask = toPat T.mask

every :: Pat a => P (Pattern Number -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
every = toPat (\x -> T.every $$ fromTarget x)

while :: Pat a => P (Pattern Number -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
while = toPat (\x -> T.while $$ fromTarget x)

superimpose :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
superimpose = toPat T.superimpose

jux :: P ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)
jux = toPat T.jux

iter :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
iter = toPat (\x -> T.iter $$ fromTarget x)

sometimes :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
sometimes = toPat T.sometimes

rarely :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
rarely = toPat T.rarely

degrade :: Pat a =>  P (Pattern a -> Pattern a)
degrade = toPat T.degrade

degradeBy :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
degradeBy = toPat (\x -> T.degradeBy $$ fromTarget x)

(?) :: Pat a => P (Pattern a -> Pattern Number -> Pattern a)
(?) = toPat (\x d -> T.degradeBy (fromTarget d) x)

timeLoop :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
timeLoop = toPat (\tm -> T.timeLoop $$ fromTarget tm)

loop :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
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

(//) :: Pattern (Pattern Number -> Pattern (Pattern Number -> Pattern Number))
(//) = toPat (lift2 (P./))

(/|) :: Pattern (Pattern Number -> Pattern (Pattern Number -> Pattern Number))
(/|) = right (//)

(|/) :: Pattern (Pattern Number -> Pattern (Pattern Number -> Pattern Number))
(|/) = left (//)

round :: P (Pattern Number -> Pattern Number)
round = toPat $$ toTarget (lift (P.round :: Double -> Int))

floor :: P (Pattern Number -> Pattern Number)
floor = toPat $$ toTarget (lift (P.floor :: Double -> Int))

show :: Show a => P (Pattern a -> Pattern Text)
show = toPat showT



-- samples

bd :: Pattern String
bd = P.pure "bd"

sn :: Pattern String
sn = P.pure "sn"

--

c :: Pattern Number
c = 0

e :: Pattern Number
e = 4


-- continous

sine :: Pattern Number
sine = toTarget (T.sine :: Pattern Double)

rand :: Pattern Number
rand = toTarget (T.rand :: Pattern Double)

perlin :: Pattern Number
perlin = toTarget (T.perlin :: Pattern Double)

saw :: Pattern Number
saw = toTarget (T.saw :: Pattern Double)

tri :: Pattern Number
tri = toTarget (T.tri :: Pattern Double)

smooth :: P (Pattern Number -> Pattern Number)
smooth = toPat $$ toTarget (T.smooth :: Pattern Double -> Pattern Double)

segment :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
segment = toPat $$ (\x -> T.segment $$ fromTarget x)

range :: P (Pattern Number -> Pattern Number -> Pattern Number -> Pattern Number)
range = toPat $$ toTarget (T.range ::  Pattern Double -> Pattern Double -> Pattern Double -> Pattern Double)


--- comparisons

(>=) :: P (Pattern Number -> Pattern Number -> Pattern Number)
(>=) = toPat $$ toTarget geqT

(<=) :: P (Pattern Number -> Pattern Number -> Pattern Number)
(<=) = toPat $$ toTarget leqT

(==) :: P (Pattern Number -> Pattern Number -> Pattern Number)
(==) = toPat $$ toTarget eqT

(&&) :: P (Pattern Number -> Pattern Number -> Pattern Number)
(&&) = toPat $$ toTarget andT

(||) :: P (Pattern Number -> Pattern Number -> Pattern Number)
(||) = toPat $$ toTarget orT
