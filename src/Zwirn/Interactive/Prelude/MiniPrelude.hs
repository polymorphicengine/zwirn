{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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

-- import qualified Sound.Tidal.Context as T hiding (fromList)

import qualified Sound.Zwirn.Classes as Z
import Zwirn.Interactive.Convert
import Zwirn.Interactive.TidalT
import Zwirn.Interactive.Transform
import Zwirn.Interactive.Types
import qualified Prelude as P

($) :: (Pat b) => P ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
($) = _toPat _apply

-- silence :: P (Pattern a)
-- silence = T.silence

true :: P NumberPattern
true = _toPat $$ _toTarget (1 :: P.Int)

false :: P NumberPattern
false = _toPat $$ _toTarget (0 :: P.Int)

id :: (Pat a) => P (Pattern a -> Pattern a)
id = _toPat (P.id :: Pattern a -> Pattern a)

const :: (Pat a) => P (Pattern a -> Pattern b -> Pattern a)
const = _toPat (P.const :: Pattern a -> Pattern b -> Pattern a)

tick :: (Pat b) => P (Pattern a -> (Pattern a -> Pattern b) -> Pattern b)
tick = _toPat (P.flip _apply)

(.) :: (Pat b, Pat d) => P ((Pattern b -> Pattern d) -> (Pattern a -> Pattern b) -> Pattern a -> Pattern d)
(.) = _toPat compose
  where
    compose = (P..) :: ((Pattern b -> Pattern d) -> (Pattern a -> Pattern b) -> Pattern a -> Pattern d)

(++) :: P (TextPattern -> TextPattern -> TextPattern)
(++) = _toPat (P.liftA2 @Pattern append)

-- arithmetic

(+) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(+) = _toPat ((P.+) :: (P.Num a) => Pattern a -> Pattern a -> Pattern a)

-- (+|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
-- (+|) = _toPat ((T.+|) :: (P.Num a) => Pattern a -> Pattern a -> Pattern a)

-- (|+) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
-- (|+) = _toPat ((T.|+) :: (P.Num a) => Pattern a -> Pattern a -> Pattern a)

(-) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(-) = _toPat ((P.-) :: (P.Num a) => Pattern a -> Pattern a -> Pattern a)

-- (-|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
-- (-|) = _toPat ((T.-|) :: (P.Num a) => Pattern a -> Pattern a -> Pattern a)

-- (|-) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
-- (|-) = _toPat ((T.|-) :: (P.Num a) => Pattern a -> Pattern a -> Pattern a)

-- (|*|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
-- (|*|) = _toPat ((T.|*|) :: (P.Num a) => Pattern a -> Pattern a -> Pattern a)

-- (*|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
-- (*|) = _toPat ((T.*|) :: (P.Num a) => Pattern a -> Pattern a -> Pattern a)

-- (|*) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
-- (|*) = _toPat ((T.|*) :: (P.Num a) => Pattern a -> Pattern a -> Pattern a)

-- (//) :: (Pat a, P.Fractional a) => P (Pattern a -> Pattern a -> Pattern a)
-- (//) = _toPat ((P./) :: (P.Fractional a) => Pattern a -> Pattern a -> Pattern a)

-- (/|) :: (Pat a, P.Fractional a) => P (Pattern a -> Pattern a -> Pattern a)
-- (/|) = _toPat ((T./|) :: (P.Fractional a) => Pattern a -> Pattern a -> Pattern a)

-- (|/) :: (Pat a, P.Fractional a) => P (Pattern a -> Pattern a -> Pattern a)
-- (|/) = _toPat ((T.|/) :: (P.Fractional a) => Pattern a -> Pattern a -> Pattern a)

-- round :: P (NumberPattern -> NumberPattern)
-- round = _toPat $$ _toTarget ((P.round) :: Pattern Double -> Pattern Int)

-- floor :: P (NumberPattern -> NumberPattern)
-- floor = _toPat $$ _toTarget (_lift (P.floor :: Double -> Int))

show :: (Show a) => P (Pattern a -> TextPattern)
show = _toPat _show

-- samples

bd :: P TextPattern
bd = P.pure $$ Text "bd"

sn :: P TextPattern
sn = P.pure $$ Text "sn"

--- comparisons

(>=) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(>=) = _toPat $$ _toTarget ((Z.>=) :: (Pattern Double -> Pattern Double -> Pattern Bool))

(<=) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(<=) = _toPat $$ _toTarget ((Z.<=) :: (Pattern Double -> Pattern Double -> Pattern Bool))

(==) :: P (NumberPattern -> NumberPattern -> NumberPattern)
(==) = _toPat $$ _toTarget ((Z.==) :: (Pattern Double -> Pattern Double -> Pattern Bool))

-- (&&) :: P (NumberPattern -> NumberPattern -> NumberPattern)
-- (&&) = _toPat $$ _toTarget _and

-- (||) :: P (NumberPattern -> NumberPattern -> NumberPattern)
-- (||) = _toPat $$ _toTarget _or
