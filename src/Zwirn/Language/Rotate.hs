{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.Rotate
    ( runRotate
    , runRotateUnsafe
    ) where

import Zwirn.Language.Syntax

import Control.Monad.Except
import Control.Monad.Identity

ops :: [Declaration]
ops = [ ("*", Fixity LeftA 2)
      , ("/", Fixity LeftA 2)
      , ("$", Fixity RightA 0)
      , ("+", Fixity LeftA 1)
      , ("|+", Fixity LeftA 1)
      , ("+|", Fixity LeftA 1)
      , ("|*", Fixity LeftA 2)
      , ("*|", Fixity LeftA 2)
      , ("//", Fixity LeftA 2)
      ]

defaultFixity :: Fixity
defaultFixity = Fixity LeftA 9

data RotationError = RotationError deriving (Show, Eq)

type Rotate a = ExceptT RotationError Identity a

-- | Describes which action the rotation algorithm should use.
data Rotation
  = Fail  -- ^ Fail due to the mixing of incompatible operators.
  | Keep  -- ^ Keep the tree as it is.
  | Rotate  -- ^ Balance the tree to the left.

runRotate :: Term -> Either RotationError Term
runRotate t = runIdentity $ runExceptT $ rotate t

runRotateUnsafe :: Term -> Term
runRotateUnsafe t = case runRotate t of
                        Left err -> error $ show err
                        Right r -> r

-- | The Happy parser is written in a way so that it will always create a right-balanced AST.
-- We compare the operators and indicate how to rotate the tree.

shouldRotate :: Fixity -> Fixity -> Rotation
shouldRotate (Fixity a p) (Fixity a' p') = case compare p p' of
  LT -> Keep
  EQ -> case (a, a') of
    (LeftA , LeftA)  -> Rotate
    (RightA, RightA)  -> Keep
    (_     , _     )  -> Fail
  GT -> Rotate

-- | Rebalances the tree to respect the associativity and precedence of the
-- parsed operators.

-- Not very efficient, but enough for demonstration purposes.
findOp :: OperatorSymbol -> Rotate Fixity
findOp o = case lookup o ops of
                    Just d -> return d
                    Nothing -> return defaultFixity

rotate :: Term -> Rotate Term
rotate (TInfix l op r) = do
-- Rotating the left side is unneeded since this grammar is very simple.
-- This is because trees are always right-balanced and the left side is
-- always an atom.
  lRotated <- rotate l
  rRotated <- rotate r
  case rRotated of
    TInfix l' op' r' -> do
      opDec  <- findOp op
      opDec' <- findOp op'
      case shouldRotate opDec opDec' of
        Fail   -> throwError RotationError
        Keep   -> return $ TInfix lRotated op rRotated
        Rotate -> return $ TInfix (TInfix lRotated op l') op' r'
    _ -> return $ TInfix lRotated op rRotated
rotate (TApp l r) = do
        lRotated <- rotate l
        rRotated <- rotate r
        case rRotated of
          TInfix l' op r' -> return $ TInfix (TApp lRotated l') op r'
          _ -> return $ TApp lRotated rRotated
rotate e@(TVar _ _) = return e
rotate e@(TRest) = return e
rotate (TElong t i) = rotate t >>= \rotated -> return $ TElong rotated i
rotate (TRepeat t i) = rotate t >>= \rotated -> return $ TRepeat rotated i
rotate (TSeq ts) = (sequence $ map rotate ts) >>= \rs -> return $ TSeq rs
rotate (TStack ts) = (sequence $ map rotate ts) >>= \rs -> return $ TStack rs
rotate (TAlt ts) = (sequence $ map rotate ts) >>= \rs -> return $ TAlt rs
rotate (TChoice n ts) = (sequence $ map rotate ts) >>= \rs -> return $ TChoice n rs
rotate (TEuclid t1 t2 t3 (Just t4)) = do
                        rot1 <- rotate t1
                        rot2 <- rotate t2
                        rot3 <- rotate t3
                        rot4 <- rotate t4
                        return $ TEuclid rot1 rot2 rot3 (Just rot4)
rotate (TEuclid t1 t2 t3 Nothing) = do
                        rot1 <- rotate t1
                        rot2 <- rotate t2
                        rot3 <- rotate t3
                        return $ TEuclid rot1 rot2 rot3 Nothing
rotate (TPoly t1 t2) = do
                rot1 <- rotate t1
                rot2 <- rotate t2
                return $ TPoly rot1 rot2
rotate (TLambda vs t) = rotate t >>= \rotated -> return $ TLambda vs rotated
