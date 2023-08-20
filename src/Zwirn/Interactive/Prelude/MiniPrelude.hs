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

true :: P NumberPattern
true = _toPat $$ _toTarget (1 :: P.Int)

false :: P NumberPattern
false = _toPat $$ _toTarget (0 :: P.Int)

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

-- rot :: (Pat a, P.Ord a) => P (NumberPattern -> Pattern a -> Pattern a)
-- rot = _toPat (\x -> T.rot $$ _fromTarget x)

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
