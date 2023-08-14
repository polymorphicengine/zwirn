{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Zwirn.Interactive.Transform where

{-
    Transform.hs - defines a typeclass to automatically transform
    standard haskell functions to a type that is suitable for zwirn
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

import Zwirn.Interactive.Types
import Zwirn.Interactive.TidalT

class Pat a where
  _toPat :: a -> P a

instance Pat a => (Pat (Pattern a)) where
  _toPat = P.id

instance Pat b => Pat (Pattern a -> b) where
  _toPat g = P.pure (\x -> _toPat $$ g x)

instance (Pat b, Pat c) => Pat ((Pattern a -> Pattern b) -> c) where
  _toPat g = P.pure (\x -> _toPat (g $$ _apply x))

instance Pat ([Pattern a]) where
  _toPat = P.pure

instance Pat Number where
  _toPat = P.pure

instance Pat ValueMap where
  _toPat = P.pure

instance Pat Text where
  _toPat = P.pure
