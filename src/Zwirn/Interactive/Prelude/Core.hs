module Zwirn.Interactive.Prelude.Core where

{-
    Core.hs - import most of tidals core functions
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

import qualified Pattern as Z
import Zwirn.Interactive.Convert
import Zwirn.Interactive.Transform
import Zwirn.Interactive.Types
import qualified Prelude as P ()

---------------------------------------------
-------------manipulating time---------------
---------------------------------------------

fast :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
fast = _toPat (\n -> Z.fast (_fromTarget n))

(*) :: (Pat a) => P (Pattern a -> NumberPattern -> Pattern a)
(*) = _toPat (\x n -> Z.fast (_fromTarget n) x)

slow :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
slow = _toPat (\n -> Z.slow (_fromTarget n))

(/) :: (Pat a) => P (Pattern a -> NumberPattern -> Pattern a)
(/) = _toPat (\x n -> Z.slow (_fromTarget n) x)

ply :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
ply = _toPat (\n -> Z.ply (_fromTarget n))

rev :: (Pat a) => P (Pattern a -> Pattern a)
rev = _toPat Z.rev

-- plyWith :: (Pat a) => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- plyWith = _toPat ((T.plyWith :: Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) . _fromTarget)

-- rotL :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- rotL = _toPat (Z.shift . _fromTarget)

-- (<~) :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- (<~) = rotL

-- rotR :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- rotR = _toPat (\x y -> (_fromTarget x) T.~> y)

-- (~>) :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- (~>) = rotR

-- slowSqueeze :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- slowSqueeze = _toPat (\x -> T.slowSqueeze (_fromTarget x))

-- fastGap :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- fastGap = _toPat (\x -> T.fastGap (_fromTarget x))

-- repeatCycles :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- repeatCycles = _toPat (\x -> T.repeatCycles (_fromTarget x))

-- timeLoop :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- timeLoop = _toPat (\tm -> T.timeLoop $$ _fromTarget tm)

-- loop :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- loop = timeLoop

-- loopFirst :: (Pat a) => P (Pattern a -> Pattern a)
-- loopFirst = _toPat T.loopFirst

-- inside :: (Pat a) => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- inside = _toPat (\x -> T.inside (_fromTarget x))

-- outside :: (Pat a) => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- outside = _toPat (\x -> T.outside (_fromTarget x))

---------------------------------------------
--------------------signals------------------
---------------------------------------------

-- sine :: P NumberPattern
-- sine = _toTarget (T.sine :: Pattern Double)

-- sine2 :: P NumberPattern
-- sine2 = _toTarget (T.sine2 :: Pattern Double)

-- cosine :: P NumberPattern
-- cosine = _toTarget (T.cosine :: Pattern Double)

-- cosine2 :: P NumberPattern
-- cosine2 = _toTarget (T.cosine2 :: Pattern Double)

-- saw :: P NumberPattern
-- saw = _toTarget (T.saw :: Pattern Double)

-- saw2 :: P NumberPattern
-- saw2 = _toTarget (T.saw2 :: Pattern Double)

-- isaw :: P NumberPattern
-- isaw = _toTarget (T.isaw :: Pattern Double)

-- isaw2 :: P NumberPattern
-- isaw2 = _toTarget (T.isaw2 :: Pattern Double)

-- tri :: P NumberPattern
-- tri = _toTarget (T.tri :: Pattern Double)

-- tri2 :: P NumberPattern
-- tri2 = _toTarget (T.tri2 :: Pattern Double)

-- square :: P NumberPattern
-- square = _toTarget (T.square :: Pattern Double)

-- square2 :: P NumberPattern
-- square2 = _toTarget (T.square2 :: Pattern Double)

-- envL :: P NumberPattern
-- envL = _toTarget T.envL

-- envLR :: P NumberPattern
-- envLR = _toTarget T.envLR

-- envEq :: P NumberPattern
-- envEq = _toTarget T.envEq

-- envEqR :: P NumberPattern
-- envEqR = _toTarget T.envEqR

-- smooth :: P (NumberPattern -> NumberPattern)
-- smooth = _toPat $$ _toTarget (T.smooth :: Pattern Double -> Pattern Double)

-- segment :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- segment = _toPat $$ (\x -> T.segment $$ _fromTarget x)

-- range :: P (NumberPattern -> NumberPattern -> NumberPattern -> NumberPattern)
-- range = _toPat $$ _toTarget (T.range :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern Double)

-- rangex :: P (NumberPattern -> NumberPattern -> NumberPattern -> NumberPattern)
-- rangex = _toPat $$ _toTarget ((T.tParam2 T.rangex) :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern Double)

---------------------------------------------
-----------------randomness------------------
---------------------------------------------

-- rand :: P NumberPattern
-- rand = _toTarget (T.rand :: Pattern Double)

-- brand :: P NumberPattern
-- brand = _toTarget T.brand

-- brandBy :: P (NumberPattern -> NumberPattern)
-- brandBy = _toPat $$ _toTarget T.brandBy

-- irand :: P (NumberPattern -> NumberPattern)
-- irand = _toPat $$ _toTarget (T.irand :: Pattern Int -> Pattern Double)

-- perlin :: P NumberPattern
-- perlin = _toTarget (T.perlin :: Pattern Double)

-- perlin2 :: P (NumberPattern -> NumberPattern)
-- perlin2 = _toPat $$ _toTarget T.perlin2

-- perlinWith :: P (NumberPattern -> NumberPattern)
-- perlinWith = _toPat $$ _toTarget (T.perlinWith :: Pattern Double -> Pattern Double)

-- perlin2With :: P (NumberPattern -> NumberPattern -> NumberPattern)
-- perlin2With = _toPat $$ _toTarget (T.perlin2With)

-- degrade :: (Pat a) => P (Pattern a -> Pattern a)
-- degrade = _toPat T.degrade

-- (?) :: (Pat a) => P (Pattern a -> NumberPattern -> Pattern a)
-- (?) = _toPat (\x d -> T.degradeBy (_fromTarget d) x)

-- degradeBy :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- degradeBy = _toPat (\x -> T.degradeBy $$ _fromTarget x)

-- unDegradeBy :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- unDegradeBy = _toPat (\x -> T.unDegradeBy $$ _fromTarget x)

-- sometimesBy :: (Pat a) => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- sometimesBy = _toPat (\x -> T.sometimesBy (_fromTarget x))

-- sometimes :: (Pat a) => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- sometimes = _toPat T.sometimes

-- rarely :: (Pat a) => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- rarely = _toPat T.rarely

-- often :: (Pat a) => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- often = _toPat T.often

-- almostNever :: (Pat a) => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- almostNever = _toPat T.almostNever

-- almostAlways :: (Pat a) => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- almostAlways = _toPat T.almostAlways

-- never :: (Pat a) => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- never = _toPat T.never

-- always :: (Pat a) => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- always = _toPat T.always

-- someCyclesBy :: (Pat a) => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- someCyclesBy = _toPat (\x -> T.someCyclesBy (_fromTarget x))

-- someCycles :: (Pat a) => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- someCycles = _toPat T.always

---------------------------------------------
----------------conditional------------------
---------------------------------------------

-- every :: (Pat a) => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- every = _toPat (\x -> T.every $$ _fromTarget x)

-- whenmod :: (Pat a) => P (NumberPattern -> NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- whenmod = _toPat (\x y -> T.whenmod (_fromTarget x) (_fromTarget y))

-- while :: (Pat a) => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- while = _toPat (\x -> T.while $$ _fromTarget x)

---------------------------------------------
------------------structure------------------
---------------------------------------------

-- struct :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- struct = _toPat (\n x -> T.struct (_fromTarget n) x)

-- substruct :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- substruct = _toPat (\n x -> T.substruct (_fromTarget n) x)

-- mask :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- mask = _toPat (\n x -> T.mask (_fromTarget n) x)

-- euclid :: (Pat a) => P (NumberPattern -> NumberPattern -> Pattern a -> Pattern a)
-- euclid = _toPat (\n x -> T.euclid (_fromTarget n) (_fromTarget x))

-- euclidInv :: (Pat a) => P (NumberPattern -> NumberPattern -> Pattern a -> Pattern a)
-- euclidInv = _toPat (\n x -> T.euclidInv (_fromTarget n) (_fromTarget x))

-- euclidFull :: (Pat a) => P (NumberPattern -> NumberPattern -> Pattern a -> Pattern a -> Pattern a)
-- euclidFull = _toPat (\n x -> T.euclidFull (_fromTarget n) (_fromTarget x))

-- euclidOff :: (Pat a) => P (NumberPattern -> NumberPattern -> NumberPattern -> Pattern a -> Pattern a)
-- euclidOff = _toPat (\n x y -> T.euclidOff (_fromTarget n) (_fromTarget x) (_fromTarget y))

-- eoff :: (Pat a) => P (NumberPattern -> NumberPattern -> NumberPattern -> Pattern a -> Pattern a)
-- eoff = euclidOff

-- run :: P (NumberPattern -> NumberPattern)
-- run = _toPat $$ _toTarget (T.run :: Pattern Double -> Pattern Double)

-- scan :: P (NumberPattern -> NumberPattern)
-- scan = _toPat $$ _toTarget (T.scan :: Pattern Double -> Pattern Double)

-- append :: (Pat a) => P (Pattern a -> Pattern a -> Pattern a)
-- append = _toPat T.append

-- overlay :: (Pat a) => P (Pattern a -> Pattern a -> Pattern a)
-- overlay = _toPat T.overlay

-- superimpose :: (Pat a) => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- superimpose = _toPat T.superimpose

-- brak :: (Pat a) => P (Pattern a -> Pattern a)
-- brak = _toPat T.brak

-- iter :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- iter = _toPat (\x -> T.iter $$ _fromTarget x)

-- swingBy :: (Pat a) => P (NumberPattern -> NumberPattern -> Pattern a -> Pattern a)
-- swingBy = _toPat (\x y -> T.swingBy (_fromTarget x) (_fromTarget y))

-- swing :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- swing = _toPat (\x -> T.swing $$ _fromTarget x)

-- wedge :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a -> Pattern a)
-- wedge = _toPat (\x -> T.wedge (_fromTarget x))

-- trunc :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- trunc = _toPat (\x -> T.trunc (_fromTarget x))

-- linger :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- linger = _toPat (\x -> T.linger (_fromTarget x))

-- stripe :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- stripe = _toPat (\x -> T.stripe (_fromTarget x))

-- slowstripe :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- slowstripe = _toPat (\x -> T.slowstripe (_fromTarget x))

-- chunk :: (Pat a) => P (NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- chunk = _toPat (\x -> T.chunk (_fromTarget x))

-- shuffle :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- shuffle = _toPat (\x -> T.shuffle (_fromTarget x))

-- scramble :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- scramble = _toPat (\x -> T.scramble (_fromTarget x))

-- arpeggiate :: (Pat a) => P (Pattern a -> Pattern a)
-- arpeggiate = _toPat T.arpeggiate

-- arpg :: (Pat a) => P (Pattern a -> Pattern a)
-- arpg = arpeggiate

-- arp :: (Pat a) => P (TextPattern -> Pattern a -> Pattern a)
-- arp = _toPat (\x -> T.arp (_fromTarget x))

-- rolled :: (Pat a) => P (Pattern a -> Pattern a)
-- rolled = _toPat T.rolled

-- rolledBy :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- rolledBy = _toPat (\x -> T.rolledBy (_fromTarget x))

-- press :: (Pat a) => P (Pattern a -> Pattern a)
-- press = _toPat T.press

-- pressBy :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a)
-- pressBy = _toPat (\x -> T.pressBy (_fromTarget x))

-- sew :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a -> Pattern a)
-- sew = _toPat (\x -> T.sew (_fromTarget x))

-- stitch :: (Pat a) => P (NumberPattern -> Pattern a -> Pattern a -> Pattern a)
-- stitch = _toPat (\x -> T.stitch (_fromTarget x))

-- inv :: P (NumberPattern -> NumberPattern)
-- inv = _toPat $$ _toTarget (T.inv :: Pattern Bool -> Pattern Bool)

-- mono :: (Pat a) => P (Pattern a -> Pattern a)
-- mono = _toPat T.mono

-- bite :: (Pat a) => P (NumberPattern -> NumberPattern -> Pattern a -> Pattern a)
-- bite = _toPat (\x y -> T.bite (_fromTarget x) (_fromTarget y))

-- binary :: P (NumberPattern -> NumberPattern)
-- binary = _toPat $$ _toTarget T.binary

-- ascii :: P (TextPattern -> NumberPattern)
-- ascii = _toPat $$ _toTarget T.ascii

-- rot :: (Pat a, P.Ord a) => P (NumberPattern -> Pattern a -> Pattern a)
-- rot = _toPat (\x -> T.rot $$ _fromTarget x)
