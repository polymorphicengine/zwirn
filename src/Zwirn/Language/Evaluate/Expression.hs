{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Zwirn.Language.Evaluate.Expression where

{-
    Expression.hs - Abstract Expressions
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

import Data.List
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Zwirn.Core.Cord
import Zwirn.Core.Query
import Zwirn.Core.Time (Time (..))
import Zwirn.Language.Syntax
import Zwirn.Language.TypeCheck.Types

type ExpressionMap = Map.Map Text Expression

type Zwirn = Cord ExpressionMap Position

data Expression
  = EVar (Maybe Position) Name
  | EApp Expression Expression
  | ELam (Expression -> Expression)
  | ENum Double
  | EText Text
  | EMap ExpressionMap
  | ESeq [Expression]
  | EStack [Expression]
  | EChoice Int [Expression]
  | ECase Expression (Maybe Expression) [(Pattern, Expression)]
  | EZwirn (Zwirn Expression)

showWithState :: ExpressionMap -> Expression -> String
showWithState st (EZwirn x) = intercalate "\n" $ (\(t, y) -> show t ++ ":" ++ showWithState st y) <$> findAllValuesWithTime (Time 0 1, Time 1 1) st x
showWithState _ (ENum x) = show $ (fromIntegral (floor (x * 10 ^ (5 :: Int)) :: Int) :: Double) / 10 ^ (5 :: Int)
showWithState _ (EText x) = unpack x
showWithState st (EMap m) = show $ Map.toList $ showWithState st <$> m
showWithState _ _ = "can't show"

instance Show Expression where
  show = showWithState Map.empty

instance Eq Expression where
  (==) (ENum n) (ENum m) = n == m
  (==) (EText n) (EText m) = n == m
  (==) (EMap n) (EMap m) = n == m
  (==) _ _ = False

instance Ord Expression where
  (<=) (ENum n) (ENum m) = n <= m
  (<=) _ _ = False

lambda :: (Expression -> Expression) -> Expression
lambda f = EZwirn $ pure $ ELam f
