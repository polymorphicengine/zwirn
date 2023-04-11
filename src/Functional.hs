{-# LANGUAGE DeriveFunctor, DeriveGeneric#-}

module Functional where

import qualified Prelude as P
import Data.List (intercalate)

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
