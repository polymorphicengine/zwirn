{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Zwirn.Language.Evaluate.Convert where

{-
    Convert.hs - convert from and to Expressions
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
import Data.String (IsString, fromString)
import Data.Text (Text, pack)
import Zwirn.Core.Time (Time (..))
import Zwirn.Core.Types
import Zwirn.Language.Evaluate.Expression

fromZwirn :: Zwirn Expression -> Expression
fromZwirn = EZwirn

toZwirn :: Expression -> Zwirn Expression
toZwirn (EZwirn x) = x
toZwirn _ = silence

class FromExpression a where
  fromExp :: Expression -> Zwirn a

class ToExpression a where
  toExp :: a -> Expression

instance FromExpression Time where
  fromExp (EZwirn tz) = fmap (\(ENum t) -> Time (toRational t) 0) tz
  fromExp _ = silence

instance FromExpression Double where
  fromExp (EZwirn tz) = fmap (\(ENum t) -> t) tz
  fromExp _ = silence

instance FromExpression Int where
  fromExp (EZwirn tz) = fmap (\(ENum t) -> floor t) tz
  fromExp _ = silence

instance FromExpression Expression where
  fromExp (EZwirn z) = z
  fromExp _ = silence

instance FromExpression Text where
  fromExp (EZwirn z) = fmap (\(EText t) -> t) z
  fromExp _ = silence

instance FromExpression Bool where
  fromExp (EZwirn z) = fmap (\(ENum x) -> x >= 1) z
  fromExp _ = silence

instance FromExpression ExpressionMap where
  fromExp (EZwirn z) = fmap (\(EMap m) -> m) z
  fromExp _ = silence

instance (ToExpression a, FromExpression b) => FromExpression (Zwirn a -> Zwirn b) where
  fromExp (EZwirn z) = fmap (\(ELam f) -> fromExp . f . toExp) z
  fromExp _ = silence

instance (FromExpression a) => FromExpression (Zwirn a) where
  fromExp (EZwirn z) = fmap fromExp z
  fromExp _ = silence

instance ToExpression Expression where
  toExp = id

instance ToExpression Double where
  toExp = ENum

instance ToExpression Time where
  toExp (Time t _) = ENum $ fromRational t

instance ToExpression Int where
  toExp i = ENum $ fromIntegral i

instance ToExpression Bool where
  toExp True = ENum 1
  toExp False = ENum 0

instance ToExpression Text where
  toExp = EText

instance (ToExpression a) => ToExpression (Map.Map Text a) where
  toExp m = EMap $ toExp <$> m

instance (ToExpression a) => ToExpression (Zwirn a) where
  toExp a = EZwirn $ fmap toExp a

instance (FromExpression a, ToExpression b) => ToExpression (Zwirn a -> b) where
  toExp f = lambda $ \x -> toExp $ f (fromExp x)

instance Num Expression where
  (+) = pervasive2 ((+) @Double)
  (*) = pervasive2 ((*) @Double)
  abs = pervasive (abs @Double)
  signum = pervasive (signum @Double)
  fromInteger i = ENum $ fromInteger i
  negate = pervasive (negate @Double)

instance Fractional Expression where
  fromRational r = ENum $ fromRational r
  (/) = pervasive2 ((/) @Double)

instance Floating Expression where
  pi = EZwirn $ pure $ ENum pi
  exp = pervasive (exp :: Double -> Double)
  log = pervasive (log :: Double -> Double)
  sin = pervasive (sin :: Double -> Double)
  cos = pervasive (cos :: Double -> Double)
  asin = pervasive (asin :: Double -> Double)
  acos = pervasive (acos :: Double -> Double)
  atan = pervasive (atan :: Double -> Double)
  sinh = pervasive (sinh :: Double -> Double)
  cosh = pervasive (cosh :: Double -> Double)
  asinh = pervasive (asinh :: Double -> Double)
  acosh = pervasive (acosh :: Double -> Double)
  atanh = pervasive (atanh :: Double -> Double)

instance IsString Expression where
  fromString = EText . pack

class Pervasive a where
  pervasive :: (a -> a) -> Expression -> Expression
  pervasive2 :: (a -> a -> a) -> Expression -> Expression -> Expression

instance Pervasive Double where
  pervasive f (ENum d) = ENum $ f d
  pervasive f (EMap m) = EMap $ fmap (pervasive f) m
  pervasive _ e = e
  pervasive2 f (ENum d) (ENum e) = ENum $ f d e
  pervasive2 f (EMap m) (EMap n) = EMap $ Map.unionWith (pervasive2 f) m n
  pervasive2 _ e _ = e

instance Pervasive Bool where
  pervasive f (ENum d) = toExp $ f (d >= 1)
  pervasive f (EMap m) = EMap $ fmap (pervasive f) m
  pervasive _ e = e
  pervasive2 f (ENum d) (ENum e) = toExp $ f (d >= 1) (e >= 1)
  pervasive2 f (EMap m) (EMap n) = EMap $ Map.unionWith (pervasive2 f) m n
  pervasive2 _ e _ = e

instance Pervasive Text where
  pervasive f (EText d) = EText $ f d
  pervasive f (EMap m) = EMap $ fmap (pervasive f) m
  pervasive _ e = e
  pervasive2 f (EText d) (EText e) = EText $ f d e
  pervasive2 f (EMap m) (EMap n) = EMap $ Map.unionWith (pervasive2 f) m n
  pervasive2 _ e _ = e

instance Pervasive (Either Double Text) where
  pervasive f (EText d) = EText $ (\(Right t) -> t) $ f (Right d)
  pervasive f (ENum d) = ENum $ (\(Left t) -> t) $ f (Left d)
  pervasive f (EMap m) = EMap $ fmap (pervasive f) m
  pervasive _ e = e
  pervasive2 f (EText d) (EText e) = EText $ (\(Right t) -> t) $ f (Right d) (Right e)
  pervasive2 f (ENum d) (ENum e) = ENum $ (\(Left t) -> t) $ f (Left d) (Left e)
  pervasive2 f (EMap m) (EMap n) = EMap $ Map.unionWith (pervasive2 f) m n
  pervasive2 _ e _ = e
