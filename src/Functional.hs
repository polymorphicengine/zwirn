{-# LANGUAGE DeriveFunctor, DeriveGeneric#-}

module Functional where

import qualified Prelude as P
import Data.List (intercalate)

import GHC.Generics

type Var = P.String

data Mini a = FVal a
           | FRest
           | FSeq [Mini a]
           | FAlt [Mini a]
           deriving (P.Functor, Generic)

data Mini2 a b = App {func :: Mini (Mini a -> Mini b)
                     ,arg :: Mini a
                     }


type Int = P.Int
type Bool = P.Bool


fmap :: P.Functor f => (a -> b) -> f a -> f b
fmap = P.fmap

displayMini :: P.Show a => Mini a -> P.String
displayMini (FVal i) = P.show i
displayMini (FRest) = "~"
displayMini (FSeq ps) = "(" P.++ intercalate " "  (P.map displayMini ps) P.++ ")"
displayMini (FAlt ps) = intercalate ";"  (P.map displayMini ps)

instance P.Show a => P.Show (Mini a) where
  show = displayMini


normalise :: Mini a -> Mini a
normalise (FVal x) = FVal x
normalise (FSeq (FAlt xs):ys) = FAlt (P.map (\x -> FSeq x:ys))

apply :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
apply (FVal f) a = f a
apply (FSeq fs) a = P.undefined
