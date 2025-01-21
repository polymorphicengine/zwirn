{-# LANGUAGE OverloadedStrings #-}

module Zwirn.Language.Rotate
  ( runRotate,
    runRotateUnsafe,
    RotationError,
  )
where

{-
    Rotate.hs - syntax tree rotation, code adapted from
    https://gist.github.com/heitor-lassarote/b20d6da0a9042d31e439befb8c236a4e
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

import Control.Monad.Except
import Control.Monad.Identity
import Zwirn.Language.Simple
import Zwirn.Language.Syntax

ops :: [Declaration]
ops =
  [ ("*", Fixity LeftA 9),
    ("/", Fixity LeftA 9),
    ("$", Fixity RightA 0),
    ("#", Fixity RightA 3),
    ("++", Fixity RightA 4),
    ("+", Fixity LeftA 6),
    ("|+", Fixity LeftA 6),
    ("+|", Fixity LeftA 6),
    ("|*", Fixity LeftA 7),
    ("*|", Fixity LeftA 7),
    ("//", Fixity LeftA 7),
    ("|/", Fixity LeftA 7),
    ("/|", Fixity LeftA 7)
  ]

defaultFixity :: Fixity
defaultFixity = Fixity LeftA 8

type RotationError = String

type Rotate a = ExceptT RotationError Identity a

-- | Describes which action the rotation algorithm should use.
data Rotation
  = -- | Fail due to the mixing of incompatible operators.
    Fail
  | -- | Keep the tree as it is.
    Keep
  | -- | Balance the tree to the left.
    Rotate

runRotate :: SimpleTerm -> Either RotationError SimpleTerm
runRotate t = runIdentity $ runExceptT $ rotate t

runRotateUnsafe :: SimpleTerm -> SimpleTerm
runRotateUnsafe t = case runRotate t of
  Left err -> error $ show err
  Right r -> r

-- | The Happy parser is written in a way so that it will always create a right-balanced AST.
-- We compare the operators and indicate how to rotate the tree.
shouldRotate :: Fixity -> Fixity -> Rotation
shouldRotate (Fixity a p) (Fixity a' p') = case compare p p' of
  LT -> Keep
  EQ -> case (a, a') of
    (LeftA, LeftA) -> Rotate
    (RightA, RightA) -> Keep
    (_, _) -> Fail
  GT -> Rotate

-- | Rebalances the tree to respect the associativity and precedence of the
-- parsed operators.

-- Not very efficient, but enough for demonstration purposes.
findOp :: OperatorSymbol -> Rotate Fixity
findOp o = case lookup o ops of
  Just d -> return d
  Nothing -> return defaultFixity

rotate :: SimpleTerm -> Rotate SimpleTerm
rotate (SInfix l op r) = do
  -- Rotating the left side is unneeded since this grammar is very simple.
  -- This is because trees are always right-balanced and the left side is
  -- always an atom.
  lRotated <- rotate l
  rRotated <- rotate r
  case rRotated of
    SInfix l' op' r' -> do
      opDec <- findOp op
      opDec' <- findOp op'
      case shouldRotate opDec opDec' of
        Fail -> throwError "can't handle precedence of operators"
        Keep -> return $ SInfix lRotated op rRotated
        Rotate -> return $ SInfix (SInfix lRotated op l') op' r'
    _ -> return $ SInfix lRotated op rRotated
rotate (SApp l r) = do
  lRotated <- rotate l
  rRotated <- rotate r
  return $ SApp lRotated rRotated
rotate e@(SVar _ _) = return e
rotate e@(SText _ _) = return e
rotate e@(SNum _ _) = return e
rotate SRest = return SRest
rotate (SSeq ts) = fmap SSeq (mapM rotate ts)
rotate (SStack ts) = fmap SStack (mapM rotate ts)
rotate (SChoice n ts) = fmap (SChoice n) (mapM rotate ts)
rotate (SLambda vs t) = SLambda vs <$> rotate t
rotate (SBracket t) = fmap SBracket (rotate t)
