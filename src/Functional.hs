{-# LANGUAGE DeriveFunctor, DeriveGeneric#-}

module Functional where

import qualified Prelude as P
import Data.List (intercalate)

import qualified Sound.Tidal.Context as T

import GHC.Generics


type Var = P.String

data Mini a = FVal a
           | FRest
           | FEmpty
           | FElong (Mini a)
           | FSeq (Mini a) (Mini a)
           | FStack (Mini a) (Mini a)
           | FMult (Mini a) (Mini Int)
           | FDiv (Mini a) (Mini Int)
           deriving (P.Functor, Generic)

type Int = P.Int
type Bool = P.Bool
type Pattern = T.Pattern

infixl 0 $
($) :: (a -> b) -> a -> b
($) = (P.$)

fmap :: P.Functor f => (a -> b) -> f a -> f b
fmap = P.fmap

displayMini :: P.Show a => Mini a -> P.String
displayMini (FVal i) = P.show i
displayMini (FRest) = "~"
displayMini FEmpty = ""
displayMini (FElong t) = displayMini t P.++ "@"
--displayMini t@(FSeq _ _) = "(" P.++ (intercalate " " P.$  P.map displayMini (getFSeq t)) P.++ ")"
displayMini (FStack t1 t2) = "(" P.++ displayMini t1 P.++ "," P.++ displayMini t2 P.++ ")"
displayMini (FMult t1 t2) = "(" P.++ displayMini t1 P.++ "*" P.++ displayMini t2 P.++ ")"
displayMini (FDiv t1 t2) = "(" P.++ displayMini t1 P.++ "/" P.++ displayMini t2 P.++ ")"

instance P.Show a => P.Show (Mini a) where
  show = displayMini


getFSeq :: Mini a -> [Mini a]
getFSeq FEmpty = []
getFSeq (FSeq t1 t2) = t1:(getFSeq t2)
getFSeq x = [x]

eventLengths :: Pattern a -> Pattern T.Time
eventLengths = T.withEvent (\e -> e {T.value = (T.wholeStop e) P.- (T.wholeStart e)})

apply :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
apply fp p = T.innerJoin $ fmap (\f -> T.outside (eventLengths fp) f $ p) fp

infixr 0 $$
($$) :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
($$) = apply

lift :: (a -> b) -> Pattern (a -> b)
lift = P.pure

liftF :: (a -> b) -> (Pattern a -> Pattern b)
liftF = fmap

lift2 :: (a -> b -> c) -> Pattern (a -> Pattern (b -> c))
lift2 f = P.pure $ \x -> P.pure $ f x

liftF2 :: (a -> b -> c) -> (Pattern a -> Pattern b -> Pattern c)
liftF2 _ = P.undefined
