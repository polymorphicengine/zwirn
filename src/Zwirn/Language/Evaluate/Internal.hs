{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Zwirn.Language.Evaluate.Internal where

{-
    Internal.hs - internal functions, specific to Expressions
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
import Data.Text (Text, pack)
import Zwirn.Core.Core (withState, (<$$>))
import Zwirn.Core.Map
import Zwirn.Core.State
import Zwirn.Core.Types
import Zwirn.Language.Evaluate.Convert
import Zwirn.Language.Evaluate.Expression
import Control.Applicative (liftA2)

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

modifyState :: Zwirn Text -> Zwirn (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression
modifyState kz fz xz = modifyState' <$> kz <*> fz <$$> xz
  where
    modifyState' :: Text -> (Zwirn Expression -> Zwirn Expression) -> Zwirn Expression -> Zwirn Expression
    modifyState' key f = withState (Map.update (Just . toExp . f . fromExp) key)

setState :: Zwirn Text -> Zwirn Expression -> Zwirn Expression -> Zwirn Expression
setState t x = setMap t (pure $ EZwirn x)

recv :: Zwirn Text -> Zwirn Int -> Zwirn ExpressionMap
recv t i = singleton t (fmap (toExp . (\x -> pack $ "c" ++ show x)) i)
