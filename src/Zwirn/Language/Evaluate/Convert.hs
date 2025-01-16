{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Zwirn.Language.Evaluate.Convert where

import qualified Data.Map as Map
import Data.Text (Text)
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

instance ToExpression ExpressionMap where
  toExp = EMap

instance ToExpression Double where
  toExp = ENum

instance ToExpression Time where
  toExp (Time t _) = ENum $ fromRational t

instance ToExpression Int where
  toExp i = ENum $ fromIntegral i

instance ToExpression Bool where
  toExp True = ENum 1
  toExp False = ENum 0

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
