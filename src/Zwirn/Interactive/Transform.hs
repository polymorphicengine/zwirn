{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Zwirn.Interactive.Transform where

import qualified Prelude as P

import Zwirn.Interactive.Types
import Zwirn.Interactive.TidalT

class Pat a where
  toPat :: a -> P a

instance Pat a => (Pat (Pattern a)) where
  toPat = P.id

instance Pat b => Pat (Pattern a -> b) where
  toPat g = P.pure (\x -> toPat $$ g x)

instance (Pat b, Pat c) => Pat ((Pattern a -> Pattern b) -> c) where
  toPat g = P.pure (\x -> toPat (g $$ apply x))

instance Pat ([Pattern a]) where
  toPat = P.pure

instance Pat Number where
  toPat = P.pure

instance Pat ValueMap where
  toPat = P.pure

instance Pat Text where
  toPat = P.pure
