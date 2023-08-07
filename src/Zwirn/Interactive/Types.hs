{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Zwirn.Interactive.Types where

import qualified Prelude as P
import qualified Sound.Tidal.Context as T

import qualified Data.Map as Map

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
   = Text String
   deriving (P.Show, P.Eq)

type TextPattern = Pattern Text
type NumberPattern = Pattern Number

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
  showT :: Pattern a -> TextPattern

instance Show Text where
  showT = P.id

instance Show Number where
  showT = P.fmap (\x -> Text (P.show x))

instance Show ValueMap where
  showT = P.fmap (\x -> Text (P.show x))

infixl 0 $$
($$) :: (a -> b) -> a -> b
($$) = (P.$)

fmap :: P.Functor f => (a -> b) -> f a -> f b
fmap = P.fmap
