{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Zwirn.Interactive.Convert where

import qualified Prelude as P
import qualified Sound.Tidal.Context as T
import qualified Data.Text as Text

import Zwirn.Interactive.Types

-- this class will help us convert basic types.
class Convertible a where
  type Target a
  toTarget :: a -> Target a
  fromTarget :: Target a -> a

instance Convertible Bool where
  type Target Bool = Number
  toTarget P.True = Num 1
  toTarget P.False = Num 0
  fromTarget n = (n P.> 0)

instance Convertible Number where
  type Target Number = Number
  toTarget = P.id
  fromTarget = P.id

instance Convertible Double where
  type Target Double = Number
  toTarget d = Num d
  fromTarget (Num n) = n

instance Convertible Time where
  type Target Time = Number
  toTarget d = Num $$ P.fromRational d
  fromTarget (Num n) = P.toRational n

instance Convertible Int where
  type Target Int = Number
  toTarget i = Num $$ P.fromIntegral i
  fromTarget (Num n) = P.floor n

instance Convertible Note where
  type Target Note = Number
  toTarget (T.Note i) = Num i
  fromTarget (Num n) = T.Note n

instance Convertible String where
  type Target String = Text
  toTarget s = (Text (Text.pack s))
  fromTarget (Text t) = (Text.unpack t)

instance Convertible ValueMap where
  type Target ValueMap = ValueMap
  toTarget = P.id
  fromTarget = P.id

instance Convertible a => Convertible (Pattern a) where
  type Target (Pattern a) = Pattern (Target a)
  toTarget = fmap toTarget
  fromTarget = fmap fromTarget

instance (Convertible a, Convertible b) => Convertible (a -> b) where
  type Target (a -> b) = Target a -> Target b
  toTarget f x = toTarget $$ f (fromTarget x)
  fromTarget f x = fromTarget $$ f (toTarget x)
