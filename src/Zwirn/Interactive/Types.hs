{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExtendedDefaultRules #-}
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

import qualified Prelude as P
import qualified Sound.Tidal.Context as T

import qualified Data.Map as Map
import qualified Data.Text as Text

type Pattern = T.Pattern
type ValueMap = T.ValueMap
type ControlPattern = Pattern ValueMap
type Value = T.Value
type Note = T.Note
type Map = Map.Map
type Time = T.Time
type Int = P.Int
type Double = P.Double
type Char = P.Char
type String = P.String
type Bool = P.Bool
type Maybe = P.Maybe

-- this is the only number type in the system to avoid type ambiguities
newtype Number
   = Num Double
  deriving (P.Show, P.Eq, P.Num, P.Enum, P.Ord, P.Fractional)

newtype Text
   = Text Text.Text
   deriving (P.Show, P.Eq)

type TextPattern = Pattern Text
type NumberPattern = Pattern Number

default (NumberPattern)

-- this is a helper that transforms some types to types that are useful in the system
-- basically this can be thought of as a transformation given as follows
-- ToPat a == (Pattern a); for any basic type a (like Bool, String, Number etc.)
-- ToPat (a -> b) == Pattern (ToPat a) -> Pattern (ToPat a);
-- in practice this is more difficult..
type family P x where
 P (Pattern a -> b) = Pattern (Pattern a -> P b)
 P ((Pattern a -> Pattern b) -> c) = Pattern (Pattern (Pattern a -> Pattern b) -> P c)
 P (Pattern a) = Pattern a
 P ([Pattern a]) = Pattern [Pattern a]
 P a = Pattern a


class Show a where
  _show :: Pattern a -> TextPattern

instance Show Text where
  _show = P.id

instance Show Number where
  _show = P.fmap (\x -> Text (Text.pack $$ P.show x))

instance Show ValueMap where
  _show = P.fmap (\x -> Text (Text.pack $$ P.show x))

instance T.Valuable Text where
  toValue (Text t) = T.VS (Text.unpack t)

instance T.Valuable Number where
  toValue (Num n) = T.VF n

instance T.Valuable (String, Value) where
  toValue (s,v) = T.VList [T.toValue s, v]

instance T.Valuable T.ValueMap where
  toValue vm = T.VList $$ P.map T.toValue (Map.toList vm)

infixl 0 $$
($$) :: (a -> b) -> a -> b
($$) = (P.$)
