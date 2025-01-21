{-# OPTIONS_GHC -Wno-orphans #-}

module Zwirn.Language.Builtin.Internal where

import qualified Data.Map as Map
import Data.String
import Data.Text (Text, pack)
import Zwirn.Language.Environment
import Zwirn.Language.Evaluate hiding (insert)
import Zwirn.Language.Parser (parseScheme)
import Zwirn.Language.TypeCheck.Types

instance IsString Scheme where
  fromString s = fromEither $ parseScheme (pack s)
    where
      fromEither (Right r) = r
      fromEither (Left e) = error e

(===) :: Text -> Expression -> Map.Map Text Expression
(===) = Map.singleton

(<::) :: Map.Map Text Expression -> Scheme -> Map.Map Text (Expression, Scheme)
(<::) x s = fmap (\l -> (l, s)) x

(--|) :: Map.Map Text (Expression, Scheme) -> Text -> Map.Map Text AnnotatedExpression
(--|) n t = fmap (\(x, s) -> Annotated x s (Just t)) n

noDesc :: Map.Map Text (Expression, Scheme) -> Map.Map Text AnnotatedExpression
noDesc = fmap (\(x, s) -> Annotated x s Nothing)
