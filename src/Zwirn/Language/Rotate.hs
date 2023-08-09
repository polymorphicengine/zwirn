{-# LANGUAGE OverloadedStrings #-}
module Zwirn.Language.Rotate
    ( runRotate
    , runRotateUnsafe
    , RotationError
    ) where

import Zwirn.Language.Syntax
import Zwirn.Language.Simple

import Control.Monad.Except
import Control.Monad.Identity

ops :: [Declaration]
ops = [ ("*", Fixity LeftA 9)
      , ("/", Fixity LeftA 9)
      , ("$", Fixity RightA 0)
      , ("#", Fixity RightA 3)
      , ("++", Fixity RightA 4)
      , ("+", Fixity LeftA 6)
      , ("|+", Fixity LeftA 6)
      , ("+|", Fixity LeftA 6)
      , ("|*", Fixity LeftA 7)
      , ("*|", Fixity LeftA 7)
      , ("//", Fixity LeftA 7)
      , ("|/", Fixity LeftA 7)
      , ("/|", Fixity LeftA 7)
      ]

defaultFixity :: Fixity
defaultFixity = Fixity LeftA 8

type RotationError = String

type Rotate a = ExceptT RotationError Identity a

-- | Describes which action the rotation algorithm should use.
data Rotation
  = Fail  -- ^ Fail due to the mixing of incompatible operators.
  | Keep  -- ^ Keep the tree as it is.
  | Rotate  -- ^ Balance the tree to the left.

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

rotate :: SimpleTerm -> Rotate SimpleTerm
rotate (SInfix l op r) = do
-- Rotating the left side is unneeded since this grammar is very simple.
-- This is because trees are always right-balanced and the left side is
-- always an atom.
  lRotated <- rotate l
  rRotated <- rotate r
  case rRotated of
    SInfix l' op' r' -> do
      opDec  <- findOp op
      opDec' <- findOp op'
      case shouldRotate opDec opDec' of
        Fail   -> throwError "can't handle precedence of operators"
        Keep   -> return $ SInfix lRotated op rRotated
        Rotate -> return $ SInfix (SInfix lRotated op l') op' r'
    _ -> return $ SInfix lRotated op rRotated
rotate (SApp l r) = do
        lRotated <- rotate l
        rRotated <- rotate r
        return $ SApp lRotated rRotated
rotate e@(SVar _ _) = return e
rotate e@(SText _ _) = return e
rotate e@(SNum _ _) = return e
rotate e@(SRest) = return e
rotate (SElong t i) = fmap (flip SElong i) $ rotate t
rotate (SSeq ts) = fmap SSeq (sequence $ map rotate ts)
rotate (SStack ts) = fmap SStack (sequence $ map rotate ts)
rotate (SChoice n ts) = fmap (SChoice n) (sequence $ map rotate ts)
rotate (SEuclid t1 t2 t3 (Just t4)) = do
                        rot1 <- rotate t1
                        rot2 <- rotate t2
                        rot3 <- rotate t3
                        rot4 <- rotate t4
                        return $ SEuclid rot1 rot2 rot3 (Just rot4)
rotate (SEuclid t1 t2 t3 Nothing) = do
                        rot1 <- rotate t1
                        rot2 <- rotate t2
                        rot3 <- rotate t3
                        return $ SEuclid rot1 rot2 rot3 Nothing
rotate (SLambda vs t) = fmap (SLambda vs) $ rotate t
rotate (SBracket t) = fmap SBracket (rotate t)
