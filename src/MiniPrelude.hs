module MiniPrelude where

import qualified Prelude as P

import qualified Sound.Tidal.Context as T

import Functional (lift2)

type Pattern = T.Pattern
type Time = T.Time
type Int = P.Int
type Bool = P.Bool

t :: Pattern Bool
t = P.pure P.True

f :: Pattern Bool
f = P.pure P.False

id :: Pattern (Pattern a -> Pattern a)
id = P.pure (\x -> x)

const :: Pattern (Pattern a -> Pattern (Pattern b -> Pattern a))
const = P.pure (\x -> P.pure (\_ -> x))

rev :: Pattern (Pattern a -> Pattern a)
rev = P.pure T.rev

fast :: Pattern (Pattern Time -> Pattern (Pattern a -> Pattern a))
fast = lift2 T.fast

slow :: Pattern (Pattern Time -> Pattern (Pattern a -> Pattern a))
slow = lift2 T.slow

ply :: Pattern (Pattern Time -> Pattern (Pattern a -> Pattern a))
ply = lift2 T.ply

rot :: P.Ord a => Pattern (Pattern Int -> Pattern (Pattern a -> Pattern a))
rot = lift2 T.rot

rotL :: Pattern (Pattern Time -> Pattern (Pattern a -> Pattern a))
rotL = lift2 (\x y -> x T.<~ y)

rotR :: Pattern (Pattern Time -> Pattern (Pattern a -> Pattern a))
rotR = lift2 (\x y -> x T.~> y)

struct :: Pattern (Pattern Bool -> Pattern (Pattern a -> Pattern a))
struct = lift2 T.struct

mask :: Pattern (Pattern Bool -> Pattern (Pattern a -> Pattern a))
mask = lift2 T.mask

toInt :: Pattern (Pattern Bool -> Pattern Int)
toInt = P.pure (P.fmap (\x -> if x then 1 else 0))
