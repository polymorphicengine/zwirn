{-# LANGUAGE TypeOperators #-}
module MiniPrelude where

import qualified Prelude as P
import Functional

import qualified Sound.Tidal.Context as T

t :: Pattern Bool
t = P.pure True

f :: Pattern Bool
f = P.pure False

id :: a ->> a
id = P.pure (\x -> x)

const :: a ->> b ->> a
const = P.pure (\x -> P.pure (\_ -> x))

rev :: a ->> a
rev = P.pure T.rev

fast :: Int ->> a ->> a
fast = lift2 T.fast

slow :: Int ->> a ->> a
slow = lift2 T.slow

ply :: Int ->> a ->> a
ply = lift2 T.ply

rot :: Int ->> a ->> a
rot = lift2 T.rot
