{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.Rotate
    ( rotate
    , rotateUnsafe
    ) where

import Data.List (find)

import Zwirn.Language.Syntax

ops :: [Declaration]
ops = [ DOperator LeftA 2 "*"
      , DOperator LeftA 2 "/"
      , DOperator RightA 0 "$"
      , DOperator LeftA 1 "+"
      , DOperator LeftA 1 "|+"
      , DOperator LeftA 1 "+|"
      , DOperator LeftA 2 "|*"
      , DOperator LeftA 2 "*|"
      , DOperator LeftA 2 "//"
      ]

-- | Describes which action the rotation algorithm should use.
data Rotation
  = Fail  -- ^ Fail due to the mixing of incompatible operators.
  | Keep  -- ^ Keep the tree as it is.
  | Rotate  -- ^ Balance the tree to the left.

-- | The Happy parser is written in a way so that it will always create a right-balanced AST.
-- We compare the operators and indicate how to rotate the tree.

shouldRotate :: Declaration -> Declaration -> Rotation
shouldRotate (DOperator a p _) (DOperator a' p' _) = case compare p p' of
  LT -> Keep
  EQ -> case (a, a') of
    (LeftA ,  LeftA)  -> Rotate
    (RightA, RightA)  -> Keep
    (_     , _     )  -> Fail
  GT -> Rotate

-- | Rebalances the tree to respect the associativity and precedence of the
-- parsed operators.

-- Not very efficient, but enough for demonstration purposes.
findOp :: OperatorSymbol -> Either String Declaration
findOp o = maybe (Left "Operator not found") Right $ find (\(DOperator _ _ o') -> o == o') ops

rotate :: Term -> Either String Term
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
        Fail   -> Left "Cannot mix operators with equal precedences and different (or null) associativities"
        Keep   -> Right $ TInfix lRotated op rRotated
        Rotate -> Right $ TInfix (TInfix lRotated op l') op' r'
    _ -> Right $ TInfix lRotated op rRotated
rotate (TApp l r) = do
        lRotated <- rotate l
        rRotated <- rotate r
        case rRotated of
          TInfix l' op r' -> Right $ TInfix (TApp lRotated l') op r'
          _ -> Right $ TApp lRotated rRotated
rotate e@(TVar _ _) = Right e
rotate e@(TRest) = Right e
rotate (TElong t i) = rotate t >>= \rotated -> return $ TElong rotated i
rotate (TRepeat t i) = rotate t >>= \rotated -> return $ TRepeat rotated i
rotate (TSeq ts) = mapEither rotate ts >>= \rs -> return $ TSeq rs
rotate (TStack ts) = mapEither rotate ts >>= \rs -> return $ TStack rs
rotate (TAlt ts) = mapEither rotate ts >>= \rs -> return $ TAlt rs
rotate (TChoice n ts) = mapEither rotate ts >>= \rs -> return $ TChoice n rs
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

mapEither :: (a -> Either b c) -> [a] -> Either b [c]
mapEither _ [] = Right []
mapEither f (a:as) = case f a of
                         Left e -> Left e
                         Right x -> case (mapEither f as) of
                                            Left e -> Left e
                                            Right xs -> Right (x:xs)

rotateUnsafe :: Term -> Term
rotateUnsafe t = case rotate t of
                        Left err -> error err
                        Right r -> r
