{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Zwirn.Interactive.Convert where

import qualified Prelude as P
import qualified Sound.Tidal.Context as T
import qualified Data.Text as Text

import Zwirn.Interactive.Types

-- this class will help us convert basic types.
class Convertible a where
  type Target a
  _toTarget :: a -> Target a
  _fromTarget :: Target a -> a

instance Convertible Bool where
  type Target Bool = Number
  _toTarget P.True = Num 1
  _toTarget P.False = Num 0
  _fromTarget n = (n P.> 0)

instance Convertible Number where
  type Target Number = Number
  _toTarget = P.id
  _fromTarget = P.id

instance Convertible Double where
  type Target Double = Number
  _toTarget d = Num d
  _fromTarget (Num n) = n

instance Convertible Time where
  type Target Time = Number
  _toTarget d = Num $$ P.fromRational d
  _fromTarget (Num n) = P.toRational n

instance Convertible Int where
  type Target Int = Number
  _toTarget i = Num $$ P.fromIntegral i
  _fromTarget (Num n) = P.floor n

instance Convertible Note where
  type Target Note = Number
  _toTarget (T.Note i) = Num i
  _fromTarget (Num n) = T.Note n

instance Convertible String where
  type Target String = Text
  _toTarget s = (Text (Text.pack s))
  _fromTarget (Text t) = (Text.unpack t)

instance Convertible ValueMap where
  type Target ValueMap = ValueMap
  _toTarget = P.id
  _fromTarget = P.id

instance Convertible a => Convertible (Pattern a) where
  type Target (Pattern a) = Pattern (Target a)
  _toTarget = P.fmap _toTarget
  _fromTarget = P.fmap _fromTarget

instance (Convertible a, Convertible b) => Convertible (a -> b) where
  type Target (a -> b) = Target a -> Target b
  _toTarget f x = _toTarget $$ f (_fromTarget x)
  _fromTarget f x = _fromTarget $$ f (_toTarget x)
