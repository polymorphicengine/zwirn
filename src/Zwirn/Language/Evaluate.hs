{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Zwirn.Language.Evaluate where

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Sound.Zwirn.Core.Cord
import Sound.Zwirn.Core.Core
import Sound.Zwirn.Core.Types
import Sound.Zwirn.Time (Time (..))
import Zwirn.Language.Simple
import Zwirn.Language.TypeCheck.Types
import Prelude hiding (readFile)

type ExpressionMap st = Map.Map Text (Expression st)

data Expression st
  = EVar Name
  | EApp (Expression st) (Expression st)
  | ELam (Expression st -> Expression st)
  | ENum Double
  | EText Text
  | EMap (Map.Map Text (Expression st))
  | ESeq [Expression st]
  | EStack [Expression st]
  | EZwirn (Cord st (Expression st))

instance (Num st) => Show (Expression st) where
  show (EZwirn x) = show x
  show (ENum x) = show x
  show (EMap x) = show $ Map.toList x
  show (ESeq xs) = show $ fastcat $ map toZwirn xs
  show (EStack xs) = show $ stack $ map toZwirn xs
  show (EText x) = unpack x
  show _ = "can't show"

compile :: SimpleTerm -> Expression st
compile (SVar _ n) = EVar n
compile (SApp fun arg) = EApp (compile fun) (compile arg)
compile (SLambda x body) = abstract x (compile body)
compile (SNum _ x) = EZwirn $ pure $ ENum $ read $ unpack x
compile (SText _ x) = EZwirn $ pure $ EText x
compile (SSeq xs) = ESeq $ map compile xs
compile (SStack xs) = EStack $ map compile xs
compile (SInfix s1 n s2) = EApp (EApp (EVar n) (compile s1)) (compile s2)
compile (SBracket s) = compile s
compile SRest = EZwirn silence
compile _ = error "not yet implemented"

abstract :: Name -> Expression st -> Expression st
abstract x (EVar n) | x == n = combI
abstract x (EApp fun arg) = combS (abstract x fun) (abstract x arg)
abstract x (ESeq xs) = ESeq $ map (abstract x) xs
abstract x (EStack xs) = EStack $ map (abstract x) xs
abstract _ k = combK k

combS :: Expression st -> Expression st -> Expression st
combS f = EApp (EApp (EVar "compose") f)

combK :: Expression st -> Expression st
combK = EApp (EVar "const")

combI :: Expression st
combI = EVar "id"

infixl 0 !

(!) :: Expression st -> Expression st -> Expression st
(EZwirn fp) ! (EZwirn x) = EZwirn $ squeezeApply (fmap (\(ELam f) -> toZwirn . f . fromZwirn) fp) x
_ ! _ = error "Error in (!)"

primitives :: ExpressionMap st
primitives =
  Map.fromList
    [ ("id", EZwirn $ pure $ ELam id),
      ("const", EZwirn $ pure $ ELam $ \x -> EZwirn $ pure $ ELam $ const x),
      ("compose", EZwirn $ pure $ ELam $ \f -> EZwirn $ pure $ ELam $ \g -> EZwirn $ pure $ ELam $ \x -> f ! x ! (g ! x)),
      ("rev", EZwirn $ pure $ ELam (fromZwirn . rev . toZwirn)),
      ("fast", EZwirn $ pure $ ELam (\(EZwirn np) -> EZwirn $ pure $ ELam $ \(EZwirn xp) -> EZwirn $ Sound.Zwirn.Core.Core.lift fast (fmap (\(ENum x) -> Time (toRational x) 0) np) xp)),
      ("|+", EZwirn $ pure $ ELam (\(EZwirn np) -> EZwirn $ pure $ ELam $ \(EZwirn mp) -> EZwirn $ ENum <$> liftA2Left (+) (fmap (\(ENum x) -> x) np) (fmap (\(ENum x) -> x) mp))),
      ("+|", EZwirn $ pure $ ELam (\(EZwirn np) -> EZwirn $ pure $ ELam $ \(EZwirn mp) -> EZwirn $ ENum <$> liftA2Right (+) (fmap (\(ENum x) -> x) np) (fmap (\(ENum x) -> x) mp))),
      ("|$", EZwirn $ pure $ ELam (\(EZwirn fp) -> EZwirn $ pure $ ELam $ \(EZwirn xp) -> EZwirn $ leftApply (fmap (\(ELam f) -> toZwirn . f . fromZwirn) fp) xp)),
      ("$|", EZwirn $ pure $ ELam (\(EZwirn fp) -> EZwirn $ pure $ ELam $ \(EZwirn xp) -> EZwirn $ rightApply (fmap (\(ELam f) -> toZwirn . f . fromZwirn) fp) xp)),
      ("$", EZwirn $ pure $ ELam (\(EZwirn fp) -> EZwirn $ pure $ ELam $ \(EZwirn xp) -> EZwirn $ squeezeApply (fmap (\(ELam f) -> toZwirn . f . fromZwirn) fp) xp)),
      ("map", EZwirn $ pure $ ELam (\(EZwirn fp) -> EZwirn $ pure $ ELam $ \(EZwirn xp) -> EZwirn $ cordMap (fmap (\(ELam f) -> toZwirn . f . fromZwirn) fp) xp)),
      ("layer", EZwirn $ pure $ ELam (\(EZwirn fp) -> EZwirn $ pure $ ELam $ \(EZwirn xp) -> EZwirn $ layer (fmap (\(ELam f) -> toZwirn . f . fromZwirn) fp) xp)),
      ("singleton", EZwirn $ pure $ ELam $ \(EZwirn tx) -> EZwirn $ pure $ ELam $ \(EZwirn vx) -> EZwirn $ singMap tx vx),
      ("#", EZwirn $ pure $ ELam $ \(EZwirn xx) -> EZwirn $ pure $ ELam $ \(EZwirn yx) -> EZwirn $ union xx yx)
    ]

link :: ExpressionMap st -> Expression st -> Expression st
link bs (EVar n) = fromJust (Map.lookup n bs)
link bs (EApp f x) = link bs f ! link bs x
link bs (ESeq xs) = EZwirn $ fastcat $ map (toZwirn . link bs) xs
link bs (EStack xs) = EZwirn $ stack $ map (toZwirn . link bs) xs
link _ e = e

eval :: ExpressionMap st -> SimpleTerm -> Expression st
eval bs = link bs . compile

insert :: (Text, Expression st) -> ExpressionMap st -> ExpressionMap st
insert (k, x) = Map.insert k x

toCord :: Expression st -> Cord st Double
toCord (EZwirn x) = fmap (\(ENum n) -> n) x
toCord _ = error "error when converting to cord"

-- helper

fromZwirn :: Cord st (Expression st) -> Expression st
fromZwirn = EZwirn

toZwirn :: Expression st -> Cord st (Expression st)
toZwirn (EZwirn x) = x
toZwirn _ = error "toZwirn not working here"

cordMap :: Cord st (Cord st a -> Cord st b) -> Cord st a -> Cord st b
cordMap fp xp = squeezeJoin $ fmap (squeezeApply fp . pure) xp

singMap :: Cord st (Expression st) -> Cord st (Expression st) -> Cord st (Expression st)
singMap kz vz = innerJoin $ fmap (\k -> fmap (EMap . Map.singleton k) vz) keyZ
  where
    keyZ = fmap (\(EText x) -> x) kz

union :: Cord st (Expression st) -> Cord st (Expression st) -> Cord st (Expression st)
union xz yz = EMap <$> liftA2 Map.union x y
  where
    x = fmap (\(EMap l) -> l) xz
    y = fmap (\(EMap l) -> l) yz
