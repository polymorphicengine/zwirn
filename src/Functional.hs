module Functional where

import qualified Prelude as P
import qualified Sound.Tidal.Context as T

-- this module contains meta functions

type Int = P.Int
type Bool = P.Bool
type Pattern = T.Pattern


infixl 0 $
($) :: (a -> b) -> a -> b
($) = (P.$)

fmap :: P.Functor f => (a -> b) -> f a -> f b
fmap = P.fmap

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
