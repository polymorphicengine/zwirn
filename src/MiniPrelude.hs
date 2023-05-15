{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, OverloadedStrings, GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables, InstanceSigs, TypeOperators, AllowAmbiguousTypes#-}
{-# LANGUAGE FlexibleInstances #-}


module MiniPrelude where

import qualified Prelude as P

import qualified Sound.Tidal.Context as T

import qualified Data.Map as Map


import Functional (apply, ($))

type Pattern = T.Pattern
type ControlPattern = T.ControlPattern
type ValueMap = T.ValueMap
type Value = T.Value
type Map = Map.Map
type Time = T.Time
type Int = P.Int
type Double = P.Double
type Char = P.Char
type String = P.String
type Bool = P.Bool

-- g :: a -> b
-- fmap g :: Pattern a -> Pattern b
--
-- toPat :: a -> ToPat a
-- flat :: Pattern (ToPat a) -> ToPat a
--
-- x :: ToPat a

--impossible
-- fmap g :: Pattern (x -> y) -> Pattern z
-- fm
-- not gonna happen
try:: ((x -> y) -> z) -> Pattern (Pattern (Pattern x -> Pattern y) -> Pattern z) -- = ToPat ((x -> y) -> z)
try g = P.pure $ \x -> P.fmap g (try2 x)
        where try2 :: Pattern (Pattern x -> Pattern y) -> Pattern (x -> y)
              try2 y = P.undefined

try3 :: ((Pattern x -> Pattern y) -> Pattern z) -> Pattern (Pattern (Pattern x -> Pattern y) -> Pattern z)
try3 g = P.pure $ \l -> g (apply l)

type family P x where
  P (Pattern a -> b) = Pattern (Pattern a -> P b)
  P ((Pattern a -> Pattern b) -> c) = Pattern (Pattern (Pattern a -> Pattern b) -> P c)
  P (Pattern a) = Pattern a
  P a = Pattern a

-- type family ToPat x where
--   ToPat (a -> b) = Pattern (ToPat a -> ToPat b)
--   ToPat (Pattern a) = ToPat a
--   ToPat (m a) = Pattern (m (ToPat a))
--   ToPat a = Pattern a
--
class Pat a where
  toPat :: a -> P a

instance Pat a => (Pat (Pattern a)) where
  toPat = P.id

instance Pat b => Pat (Pattern a -> b) where
  toPat g = P.pure (\x -> toPat $ g x)

instance Pat c => Pat ((Pattern a -> Pattern b) -> c) where
  toPat g = P.pure (\x -> toPat (g $ apply x))

instance Pat Int where
  toPat = P.pure

instance Pat Double where
  toPat = P.pure

instance Pat ValueMap where
  toPat = P.pure

  --from :: ToPat a -> Pattern a
--
-- instance Pat P.Bool where
--   toPat = P.return
--   flat = T.outerJoin
--   --from = P.id
--
-- instance (Pat a, Pat b) => Pat (a -> b) where
--   toPat g = P.undefined --P.pure (\x -> toPat $ g x)
--   flat = T.outerJoin
--   --from p = P.fmap help p
--   --       where help g x = ((from @b) $ g (toPat x))
--
-- instance Pat a => Pat (T.Pattern a) where
--   toPat x = (flat @a) (P.fmap toPat x)
--   flat = (flat @a)


-- revv :: forall a. Pat a => ToPat (a -> a)
-- revv = (toPat @(Pattern a -> Pattern a)) T.rev
--
-- sometime :: forall a. Pat a => ToPat ((a -> a) -> a -> a)
-- sometime = (toPat @((Pattern a -> Pattern a) -> Pattern a -> Pattern a)) T.sometimes

infixr 0 $$
($$) :: Pat b => P ((Pattern a -> Pattern b) -> Pattern a -> Pattern b)
($$) = toPat apply

t :: Pattern Bool
t = P.pure P.True

f :: Pattern Bool
f = P.pure P.False

id :: Pattern (Pattern a -> Pattern a)
id = P.pure P.id

const :: Pattern (Pattern a -> Pattern (Pattern b -> Pattern a))
const = P.pure (\x -> P.pure (\_ -> x))

rev :: Pat a => P (Pattern a -> Pattern a)
rev = toPat T.rev

fast :: Pat a => P (Pattern Time -> Pattern a -> Pattern a)
fast = toPat T.fast

slow :: Pat a => P (Pattern Time -> Pattern a -> Pattern a)
slow = toPat T.slow

ply :: Pat a => P (Pattern Time -> Pattern a -> Pattern a)
ply = toPat T.ply

rot :: (Pat a, P.Ord a) => P (Pattern Int -> Pattern a -> Pattern a)
rot = toPat T.rot

run :: P(Pattern Int -> Pattern Int)
run = toPat T.run

irand :: P (Pattern Int -> Pattern Int)
irand = toPat T.irand

rotL :: Pat a => P (Pattern Time -> Pattern a -> Pattern a)
rotL = toPat (\x y -> x T.<~ y)

rotR :: Pat a => P (Pattern Time -> Pattern a -> Pattern a)
rotR = toPat (\x y -> x T.~> y)

struct :: Pat a => P (Pattern Bool -> Pattern a -> Pattern a)
struct = toPat T.struct

toBool :: P (Pattern a -> Pattern Bool)
toBool = P.pure (P.fmap (\_ -> P.True))

structFrom :: Pattern (Pattern b -> Pattern (Pattern a -> Pattern a))
structFrom = P.pure (\b -> P.pure (T.struct (apply toBool b)))

mask :: Pat a => P (Pattern Bool -> Pattern a -> Pattern a)
mask = toPat T.mask

every :: Pat a => P (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
every = toPat T.every

while :: Pat a => P (Pattern Bool -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
while = toPat T.while

superimpose :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
superimpose = toPat T.superimpose

jux :: P ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)
jux = toPat T.jux

iter :: Pat a => P (Pattern Int -> Pattern a -> Pattern a)
iter = toPat T.iter

sometimes :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
sometimes = toPat T.sometimes

rarely :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
rarely = toPat T.rarely


(+) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(+) = toPat (+)

(+|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(+|) = right (+)

(|+) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|+) = left (+)


-- (-) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
-- (-) = toPat (\x y -> x T.-| y)
--
-- (-|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
-- (-|) = right (-)
--
-- (|-) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
-- (|-) = left (-)
--
--
-- (|*|) :: P.Num a => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
-- (|*|) = toPat (\x y -> x T.* y)
--
-- (*|) :: P.Num a => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
-- (*|) = right (|*|)
--
-- (|*) :: P.Num a => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
-- (|*) = left (|*|)

-- (//) :: (Pat a, P.Num a, P.Fractional a) => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
-- (//) = lift2 (\x y -> x P./ y)


-- control pattern stuff

n :: P (Pattern Int -> ControlPattern)
n = P.pure (\m -> T.n $ P.fmap (\x -> T.Note $ P.fromIntegral x) m)

s :: P (Pattern String -> ControlPattern)
s = P.pure T.s

sound :: P(Pattern String -> ControlPattern)
sound = s

room :: P (Pattern Double -> ControlPattern)
room = P.pure T.room

size :: P (Pattern Double -> ControlPattern)
size = P.pure T.size

speed :: P (Pattern Double -> ControlPattern)
speed = P.pure T.speed

accelerate :: P (Pattern Double -> ControlPattern)
accelerate = P.pure T.accelerate

gain :: P (Pattern Double -> ControlPattern)
gain = P.pure T.gain

pan :: P (Pattern Double -> ControlPattern)
pan = P.pure T.pan

krush :: P (Pattern Double -> ControlPattern)
krush = P.pure T.krush

(#) :: P (ControlPattern -> ControlPattern -> ControlPattern)
(#) = toPat (\x y -> x T.# y)

-- samples

bd :: Pattern String
bd = P.pure "bd"

sn :: Pattern String
sn = P.pure "sn"

-- meta functions to get the structure

right :: Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
right op = P.pure (\x -> P.pure (\y -> apply (apply structFrom y) (apply (apply op x) y)))

left :: Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
left op = P.pure (\x -> P.pure (\y -> apply (apply structFrom x) (apply (apply op x) y)))
