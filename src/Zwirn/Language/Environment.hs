module Zwirn.Language.Environment where

import qualified Data.Map as Map
import Data.Text (Text)
import Zwirn.Core.Types (silence)
import Zwirn.Language.Evaluate.Expression
import Zwirn.Language.TypeCheck.Types

data AnnotatedExpression
  = Annotated
  { aExp :: Expression,
    aType :: Scheme,
    aDesc :: Maybe Text
  }

data InterpreterEnv = IEnv
  { eExpressions :: Map.Map Text AnnotatedExpression,
    eInstances :: [Instance]
  }

withExpressions :: (Map.Map Text AnnotatedExpression -> Map.Map Text AnnotatedExpression) -> InterpreterEnv -> InterpreterEnv
withExpressions f (IEnv l i) = IEnv (f l) i

extend :: (Text, Expression, Scheme) -> InterpreterEnv -> InterpreterEnv
extend (n, x, s) = withExpressions (Map.insert n (Annotated x s Nothing))

lookupType :: Text -> InterpreterEnv -> Maybe Scheme
lookupType k (IEnv l _) = aType <$> Map.lookup k l

insertType :: Text -> Scheme -> InterpreterEnv -> InterpreterEnv
insertType t s = withExpressions (Map.alter alt t)
  where
    dummy = EZwirn silence
    alt Nothing = Just $ Annotated dummy s Nothing
    alt (Just (Annotated x _ i)) = Just $ Annotated x s i

lookupDescription :: Text -> InterpreterEnv -> Maybe Text
lookupDescription k (IEnv l _) = aDesc =<< Map.lookup k l

lookupExp :: Text -> InterpreterEnv -> Maybe Expression
lookupExp k (IEnv l _) = aExp <$> Map.lookup k l

lookupFull :: Text -> InterpreterEnv -> Maybe AnnotatedExpression
lookupFull k (IEnv l _) = Map.lookup k l
