{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Zwirn.Language.Evaluate.Expression where

import Data.List
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Sound.Zwirn.Core.Cord
import Sound.Zwirn.Core.Query
import Sound.Zwirn.Time (Time (..))
import Zwirn.Language.TypeCheck.Types

type ExpressionMap = Map.Map Text Expression

type Zwirn = Cord ExpressionMap

data Expression
  = EVar Name
  | EApp Expression Expression
  | ELam (Expression -> Expression)
  | ENum Double
  | EText Text
  | EMap ExpressionMap
  | ESeq [Expression]
  | EStack [Expression]
  | EZwirn (Zwirn Expression)

showWithState :: ExpressionMap -> Expression -> String
showWithState st (EZwirn x) = intercalate ", " $ (\(t, y) -> show t ++ ":" ++ showWithState st y) <$> findAllValuesWithTime (Time 0 1, Time 1 1) st x
showWithState _ (ENum x) = show x
showWithState _ (EText x) = unpack x
showWithState st (EMap m) = show $ Map.toList $ showWithState st <$> m
showWithState _ _ = "can't show"

lambda :: (Expression -> Expression) -> Expression
lambda f = EZwirn $ pure $ ELam f
