{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Zwirn.Interactive.Types where

{-
    Types.hs - defines the basic types for zwirn and some
    type class instances
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

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Pattern as Z
import qualified Time as Z
import qualified Prelude as P

type Pattern = Z.Pattern

type Map = Map.Map

type Time = Z.Time

type Int = P.Int

type Double = P.Double

type Char = P.Char

type String = P.String

type Bool = P.Bool

type Maybe = P.Maybe

-- this is the only number type in the system to avoid type ambiguities
newtype Number
  = Num Double
  deriving (P.Eq, P.Num, P.Enum, P.Ord, P.Fractional)

newtype Text
  = Text Text.Text
  deriving (P.Eq)

type TextPattern = Pattern Text

type NumberPattern = Pattern Number

-- this is a helper that transforms some types to types that are useful in the system
-- basically this can be thought of as a transformation given as follows
-- ToPat a == (Pattern a); for any basic type a (like Bool, String, Number etc.)
-- ToPat (a -> b) == Pattern (ToPat a) -> Pattern (ToPat a);
-- in practice this is more difficult..
type family P x where
  -- P  (Pattern a -> b) = Pattern (Pattern a -> P b)
  P (a -> b) = Pattern (P a -> P b)
  P (Pattern a) = Pattern a
  P String = TextPattern
  P Double = NumberPattern
  P Bool = NumberPattern
  P a = Pattern a

instance P.Show Text where
  show (Text x) = Text.unpack x

instance P.Show Number where
  show (Num x) = P.show x

class Show a where
  _show :: Pattern a -> TextPattern

instance Show Text where
  _show = P.id

instance Show Number where
  _show = P.fmap (\x -> Text (Text.pack $$ P.show x))

infixl 0 $$

($$) :: (a -> b) -> a -> b
($$) = (P.$)
