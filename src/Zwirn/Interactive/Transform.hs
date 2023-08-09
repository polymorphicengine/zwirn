{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Zwirn.Interactive.Transform where

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
