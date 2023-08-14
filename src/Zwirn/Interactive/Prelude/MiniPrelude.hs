{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Interactive.Prelude.MiniPrelude where

{-
    MiniPrelude.hs - a collection of functions from the haskell
    prelude and from tidal, adapted for zwirn
    Copyright (C) 2023, Martin Gius

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import qualified Prelude as P
import qualified Sound.Tidal.Context as T hiding (fromList)

import Zwirn.Interactive.Types
import Zwirn.Interactive.Transform
import Zwirn.Interactive.Convert
import Zwirn.Interactive.TidalT

($) :: Pat b => P ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
($) = _toPat _apply

(|$|) :: Pat b => P ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
(|$|) = _toPat _applyBoth

(|$) :: Pat b => P ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
(|$) = _toPat _applyLeft

($|) :: Pat b => P ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
($|) = _toPat _applyRight

silence :: P (Pattern a)
silence = T.silence

t :: P NumberPattern
t = _toPat $$ _toTarget (1 :: P.Int)

f :: P NumberPattern
f = _toPat $$ _toTarget (0 :: P.Int)

id :: Pat a => P (Pattern a -> Pattern a)
id = _toPat (P.id :: Pattern a -> Pattern a)

const :: Pat a => P (Pattern a -> Pattern b -> Pattern a)
const = _toPat (P.const :: Pattern a -> Pattern b -> Pattern a)

tick :: Pat b => P (Pattern a -> (Pattern a -> Pattern b) -> Pattern b)
tick = _toPat (P.flip _apply)

(.) :: (Pat b, Pat d) => P ((Pattern b -> Pattern d) -> (Pattern a -> Pattern b)-> Pattern a -> Pattern d)
(.) = _toPat compose
    where compose = (P..) :: ((Pattern b -> Pattern d) -> (Pattern a -> Pattern b)-> Pattern a -> Pattern d)

(++) :: P (TextPattern -> TextPattern -> TextPattern)
(++) = _toPat $$ _lift2 (_toTarget ((P.++) :: P.String -> P.String -> P.String))

rev :: Pat a => P (Pattern a -> Pattern a)
rev = _toPat T.rev

fast :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
fast = _toPat (\x -> T.fast (_fromTarget x))

(*) :: Pat a => P (Pattern a -> NumberPattern -> Pattern a)
(*) = _toPat (\x n -> T.fast (_fromTarget n) x)

slow :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
slow = _toPat (\x -> T.slow (_fromTarget x))

(/) ::  Pat a => P (Pattern a -> NumberPattern -> Pattern a)
(/) = _toPat (\x n -> T.slow (_fromTarget n) x)

ply :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
ply = _toPat (\x -> T.ply (_fromTarget x))

plyWith :: Pat a => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
plyWith = _toPat (\x -> (T.plyWith :: Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) (_fromTarget x))

-- rot :: (Pat a, P.Ord a) => P (NumberPattern -> Pattern a -> Pattern a)
-- rot = toPat (\x -> T.rot $$ fromTarget x)

run :: P (NumberPattern -> NumberPattern)
run = _toPat $$ _toTarget (T.run :: Pattern Double -> Pattern Double)

irand :: P (NumberPattern -> NumberPattern)
irand = _toPat $$ _toTarget (T.irand :: Pattern Int -> Pattern Double)

rotL :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
rotL = _toPat (\x y -> (_fromTarget x) T.<~ y)

(<~) :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
(<~) = rotL

rotR :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
rotR = _toPat (\x y -> (_fromTarget x) T.~> y)

(~>) :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
(~>) = rotR

struct :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
struct = _toPat (\n x -> T.struct (_fromTarget n) x)

mask :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
mask = _toPat (\n x -> T.mask (_fromTarget n) x)

every :: Pat a => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
every = _toPat (\x -> T.every $$ _fromTarget x)

while :: Pat a => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
while = _toPat (\x -> T.while $$ _fromTarget x)

superimpose :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
superimpose = _toPat T.superimpose

jux :: P ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)
jux = _toPat T.jux

iter :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
iter = _toPat (\x -> T.iter $$ _fromTarget x)

sometimes :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
sometimes = _toPat T.sometimes

rarely :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
rarely = _toPat T.rarely

degrade :: Pat a =>  P (Pattern a -> Pattern a)
degrade = _toPat T.degrade

degradeBy :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
degradeBy = _toPat (\x -> T.degradeBy $$ _fromTarget x)

(?) :: Pat a => P (Pattern a -> NumberPattern -> Pattern a)
(?) = _toPat (\x d -> T.degradeBy (_fromTarget d) x)

timeLoop :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
timeLoop = _toPat (\tm -> T.timeLoop $$ _fromTarget tm)

loop :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
loop = timeLoop

-- arithmetic

(+) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(+) = _toPat (_lift2 (P.+))

(+|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(+|) = _right (+)

(|+) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|+) = _left (+)


(-) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(-) = _toPat (_lift2 (P.-))

(-|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(-|) = _right (-)

(|-) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|-) = _left (-)


(|*|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|*|) = _toPat (_lift2 (P.*))

(*|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(*|) = _right (|*|)

(|*) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|*) = _left (|*|)

(//) ::(Pat a, P.Fractional a) => P (Pattern a -> Pattern a -> Pattern a)
(//) = _toPat (_lift2 (P./))

(/|) :: (Pat a, P.Fractional a) => P (Pattern a -> Pattern a -> Pattern a)
(/|) = _right (//)

(|/) :: (Pat a, P.Fractional a) => P (Pattern a -> Pattern a -> Pattern a)
(|/) = _left (//)

round :: P (NumberPattern -> NumberPattern)
round = _toPat $$ _toTarget (_lift (P.round :: Double -> Int))

floor :: P (NumberPattern -> NumberPattern)
floor = _toPat $$ _toTarget (_lift (P.floor :: Double -> Int))

show :: Show a => P (Pattern a -> TextPattern)
show = _toPat _show



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
sine = _toTarget (T.sine :: Pattern Double)

rand :: P NumberPattern
rand = _toTarget (T.rand :: Pattern Double)

perlin :: P NumberPattern
perlin = _toTarget (T.perlin :: Pattern Double)

saw :: P NumberPattern
saw = _toTarget (T.saw :: Pattern Double)

tri :: P NumberPattern
tri = _toTarget (T.tri :: Pattern Double)

smooth :: P (NumberPattern -> NumberPattern)
smooth = _toPat $$ _toTarget (T.smooth :: Pattern Double -> Pattern Double)

segment :: Pat a => P (NumberPattern -> Pattern a -> Pattern a)
segment = _toPat $$ (\x -> T.segment $$ _fromTarget x)

range :: P (NumberPattern -> NumberPattern -> NumberPattern -> NumberPattern)
range = _toPat $$ _toTarget (T.range ::  Pattern Double -> Pattern Double -> Pattern Double -> Pattern Double)


--- comparisons

(>=) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(>=) = _toPat $$ _toTarget _geq

(<=) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(<=) = _toPat $$ _toTarget _leq

(==) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(==) = _toPat $$ _toTarget _eq

(&&) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(&&) = _toPat $$ _toTarget _and

(||) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(||) = _toPat $$ _toTarget _or

-- list stuff

layer :: Pat b => P ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
layer = _toPat _layer
