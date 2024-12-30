{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Zwirn.Language.Evaluate.Internal where

import qualified Data.Map as Map
import Data.Text (Text, pack)
import Sound.Zwirn.Core.Cord
import Sound.Zwirn.Core.Core (lift2, modTime, withState)
import Sound.Zwirn.Core.State
import Sound.Zwirn.Core.Types hiding (Zwirn)
import Sound.Zwirn.Time (Time)
import Zwirn.Language.Evaluate.Convert
import Zwirn.Language.Evaluate.Expression

-- helper

insert :: (Text, Expression) -> ExpressionMap -> ExpressionMap
insert (k, x) = Map.insert k x

getStateN :: Zwirn Text -> Zwirn Expression
getStateN xc = innerJoin $ liftA2 (\k l -> fromLookup $ Map.lookup k l) xc (get (pure ()))
  where
    fromLookup (Just (EZwirn x)) = outerJoin $ fmap fromNum x
    fromLookup _ = silence
    fromNum (ENum n) = pure $ ENum n
    fromNum _ = silence

getStateT :: Zwirn Text -> Zwirn Expression
getStateT xc = innerJoin $ liftA2 (\k l -> fromLookup $ Map.lookup k l) xc (get (pure ()))
  where
    fromLookup (Just (EZwirn x)) = outerJoin $ fmap fromText x
    fromLookup _ = silence
    fromText (EText n) = pure $ EText n
    fromText _ = silence

getStateM :: Zwirn Text -> Zwirn Expression
getStateM xc = innerJoin $ liftA2 (\k l -> fromLookup $ Map.lookup k l) xc (get (pure ()))
  where
    fromLookup (Just (EZwirn x)) = outerJoin $ fmap fromMap x
    fromLookup _ = silence
    fromMap (EMap n) = pure $ EMap n
    fromMap _ = silence

modifyStateN :: Zwirn Text -> Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression
modifyStateN = lift2 $ \key f -> withState (Map.update (Just . toExp . f . fromExp) key)

setState :: Zwirn Text -> Zwirn Expression -> Zwirn Expression -> Zwirn Expression
setState t x = setMap t (pure $ EZwirn x)

-- fromNum _ = silence

-- create a singleton map with specific key
singMap :: Zwirn Text -> Zwirn Expression -> Zwirn Expression
singMap kz vz = innerJoin $ fmap (\k -> fmap (EMap . Map.singleton k) vz) kz

-- lookup a number key in a map
lookN :: Zwirn Text -> Zwirn ExpressionMap -> Zwirn Expression
lookN tz xz = outerJoin $ liftA2Right (\t x -> fromLookup $ Map.lookup t x) tz xz
  where
    fromLookup (Just (ENum e)) = pure $ ENum e
    fromLookup _ = silence

-- lookup a text key in a map
lookT :: Zwirn Text -> Zwirn ExpressionMap -> Zwirn Expression
lookT tz xz = outerJoin $ liftA2Right (\t x -> fromLookup $ Map.lookup t x) tz xz
  where
    fromLookup (Just (EText e)) = pure $ EText e
    fromLookup _ = silence

modTimeExp :: Zwirn (Zwirn Time -> Zwirn Time) -> Zwirn Expression -> Zwirn Expression
modTimeExp fz x = innerJoin $ fmap (`modTime` x) fz
