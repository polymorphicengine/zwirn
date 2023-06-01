{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings, TypeApplications #-}
{-# LANGUAGE ImplicitParams, LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module MiniPrelude where

import qualified Prelude as P

import qualified Sound.Tidal.Context as T hiding (fromList)

import qualified Data.Map as Map

import Functional (apply, ($))

type Pattern = T.Pattern
type ControlPattern = T.ControlPattern
type ValueMap = T.ValueMap
type Value = T.Value
type Map = Map.Map
--type Time = T.Time
type Int = P.Int
type Double = P.Double
type Char = P.Char
type String = P.String
type Bool = P.Bool

default (Pattern Int, Pattern String)

type family P x where
  P (Pattern a -> b) = Pattern (Pattern a -> P b)
  P ((Pattern a -> Pattern b) -> c) = Pattern (Pattern (Pattern a -> Pattern b) -> P c)
  P (Pattern a) = Pattern a
  P ([Pattern a]) = Pattern [Pattern a]
  P a = Pattern a

class Pat a where
  toPat :: a -> P a

instance Pat a => (Pat (Pattern a)) where
  toPat = P.id

instance Pat b => Pat (Pattern a -> b) where
  toPat g = P.pure (\x -> toPat $ g x)

instance (Pat b, Pat c) => Pat ((Pattern a -> Pattern b) -> c) where
  toPat g = P.pure (\x -> toPat (g $ apply x))

instance Pat ([Pattern a]) where
  toPat = P.pure

instance Pat Int where
  toPat = P.pure

instance Pat Double where
  toPat = P.pure

instance Pat ValueMap where
  toPat = P.pure

instance Pat Bool where
  toPat = P.pure

instance Pat String where
  toPat = P.pure

infixr 0 $$
($$) :: Pat b => P (Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b)
($$) = toPat apply

class IsBool a where
  asBool :: a -> Bool

instance IsBool Int where
  asBool x = (x P.> 0)

instance IsBool Double where
  asBool x = (x P.> 0)

instance IsBool Bool where
  asBool = P.id

t :: Pattern Bool
t = toPat P.True

f :: Pattern Bool
f = toPat P.False

id :: Pat a => P (Pattern a -> Pattern a)
id = toPat (P.id :: Pattern a -> Pattern a)

const :: Pat a => P (Pattern a -> Pattern b -> Pattern a)
const = toPat (P.const :: Pattern a -> Pattern b -> Pattern a)

(.) :: (Pat b, Pat d) => P ((Pattern b -> Pattern d) -> (Pattern a -> Pattern b)-> Pattern a -> Pattern d)
(.) = toPat compose
    where compose = (P..) :: ((Pattern b -> Pattern d) -> (Pattern a -> Pattern b)-> Pattern a -> Pattern d)

rev :: Pat a => P (Pattern a -> Pattern a)
rev = toPat T.rev

fast :: Pat a => P (Pattern Double -> Pattern a -> Pattern a)
fast = toPat (\x -> T.fast (P.fmap P.toRational x))

slow :: Pat a => P (Pattern Double -> Pattern a -> Pattern a)
slow = toPat (\x -> T.slow (P.fmap P.toRational x))

ply :: Pat a => P (Pattern Double -> Pattern a -> Pattern a)
ply = toPat (\x -> T.ply (P.fmap P.toRational x))

plyWith :: Pat a => P (Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
plyWith = toPat (\x -> T.plyWith (P.fmap P.toRational x))

rot :: (Pat a, P.Ord a) => P (Pattern Int -> Pattern a -> Pattern a)
rot = toPat T.rot

run :: P (Pattern Int -> Pattern Int)
run = toPat T.run

irand :: (Pat a, P.Num a) => P (Pattern Int -> Pattern a)
irand = toPat T.irand

rotL :: Pat a => P (Pattern Double -> Pattern a -> Pattern a)
rotL = toPat (\x y -> (P.fmap P.toRational x) T.<~ y)

(<~) :: Pat a => P (Pattern Double -> Pattern a -> Pattern a)
(<~) = rotL

rotR :: Pat a => P (Pattern Double -> Pattern a -> Pattern a)
rotR = toPat (\x y -> (P.fmap P.toRational x) T.~> y)

(~>) :: Pat a => P (Pattern Double -> Pattern a -> Pattern a)
(~>) = rotR

struct :: Pat a => P (Pattern Bool -> Pattern a -> Pattern a)
struct = toPat T.struct

toBool :: P (Pattern a -> Pattern Bool)
toBool = P.pure (P.fmap (\_ -> P.True))

structFrom :: P (Pattern b -> Pattern a -> Pattern a)
structFrom = P.pure (\b -> P.pure (T.struct (apply toBool b)))

mask :: Pat a => P (Pattern Bool -> Pattern a -> Pattern a)
mask = toPat T.mask

every :: Pat a => P (Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
every = toPat T.every

while :: (IsBool b, Pat a) => P (Pattern b -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
while = toPat (\x -> T.while (P.fmap asBool x))

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

degrade :: Pat a =>  P (Pattern a -> Pattern a)
degrade = toPat T.degrade

degradeBy :: Pat a => P (Pattern Double -> Pattern a -> Pattern a)
degradeBy = toPat T.degradeBy

(?) :: Pat a => P (Pattern a -> Pattern Double -> Pattern a)
(?) = toPat (\x d -> T.degradeBy d x)

timeLoop :: Pat a => P (Pattern Double -> Pattern a -> Pattern a)
timeLoop = toPat (\tm -> T.timeLoop $ P.fmap P.toRational tm)

loop :: Pat a => P (Pattern Double -> Pattern a -> Pattern a)
loop = timeLoop

-- arithmetic

(+) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(+) = toPat ((T.|+|) @Pattern)

(+|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(+|) = right (+)

(|+) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|+) = left (+)


(-) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(-) = toPat ((T.|-|) @Pattern)

(-|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(-|) = right (-)

(|-) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|-) = left (-)


(|*|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|*|) = toPat ((T.|*|) @Pattern)

(*|) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(*|) = right (|*|)

(|*) :: (Pat a, P.Num a) => P (Pattern a -> Pattern a -> Pattern a)
(|*) = left (|*|)

(//) :: (Pat a, P.Num a, P.Fractional a) => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(//) = toPat ((T.|/|) @Pattern)

(/|) :: (Pat a, P.Num a, P.Fractional a) => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(/|) = right (//)

(|/) :: (Pat a, P.Num a, P.Fractional a) => Pattern (Pattern a -> Pattern (Pattern a -> Pattern a))
(|/) = left (//)

-- control pattern stuff

n :: P.Integral a => P (Pattern a -> ControlPattern)
n = P.pure (\m -> T.n $ P.fmap (\x -> T.Note $ P.fromIntegral x) m)

note :: P.Integral a => P (Pattern a -> ControlPattern)
note = P.pure (\m -> T.note $ P.fmap (\x -> T.Note $ P.fromIntegral x) m)

s :: P (Pattern String -> ControlPattern)
s = P.pure T.s

sound :: P (Pattern String -> ControlPattern)
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
(#) = toPat (T.#)

slice :: P (Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern)
slice = toPat T.slice

splice :: P (Pattern Int -> Pattern Int -> ControlPattern -> ControlPattern)
splice = toPat T.splice

striate :: P (Pattern Int -> ControlPattern -> ControlPattern)
striate = toPat T.striate

chop :: P (Pattern Int -> ControlPattern -> ControlPattern)
chop = toPat T.chop

loopAt :: P (Pattern Double -> ControlPattern -> ControlPattern)
loopAt = toPat (\tm -> T.loopAt $ P.fmap P.toRational tm)


-- samples

bd :: Pattern String
bd = P.pure "bd"

sn :: Pattern String
sn = P.pure "sn"

--

c :: P.Num a => Pattern a
c = 0

e :: P.Num a => Pattern a
e = 4

-- meta functions to get the structure

right :: Pat c => Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
right op = P.pure (\x -> P.pure (\y -> apply (apply structFrom y) (apply (apply op x) y)))

left :: Pat c => Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
left op = P.pure (\x -> P.pure (\y -> apply (apply structFrom x) (apply (apply op x) y)))


-- list stuff

maj :: P.Num a => Pattern [Pattern a]
maj = P.pure [0,4,7]

min :: P.Num a => Pattern [Pattern a]
min = P.pure [0,3,7]

stack :: Pattern (Pattern [Pattern a] -> Pattern a)
stack = P.pure (\x -> T.innerJoin $ P.fmap T.stack x)

mapT :: Pattern (Pattern a -> Pattern b) -> Pattern [Pattern a] -> Pattern [Pattern b]
mapT g pxs = P.fmap (\xs -> P.map (apply g) xs) pxs

map :: P (Pattern (Pattern a -> Pattern b) -> Pattern [Pattern a] -> Pattern [Pattern b])
map = toPat mapT

layerT :: Pattern [Pattern (Pattern a -> Pattern b)] -> Pattern a -> Pattern [Pattern b]
layerT fsp x = P.fmap (\fs -> P.map (\g -> apply g x) fs) fsp

layer :: Pattern (Pattern [Pattern (Pattern a -> Pattern b)] -> Pattern (Pattern a -> Pattern [Pattern b]))
layer = toPat layerT

-- type conversions

int :: Pattern (Pattern Int -> Pattern Int)
int = id

double :: Pattern (Pattern Double -> Pattern Double)
double = id

string :: Pattern (Pattern String -> Pattern String)
string = id

bool :: Pattern (Pattern Bool -> Pattern Bool)
bool = id


-- state

-- setD :: (?tidal :: T.Stream) => String -> Pattern Double -> P.IO ()
-- setD = T.streamSetF ?tidal

-- pattern matching compilation

-- try 0 x = x
-- try n x = [x (try (n-1) x)]

try :: Pattern (Pattern Int -> (Pattern (Pattern a -> Pattern a)))
try = P.pure (\iP -> T.innerJoin $ P.fmap (
                            \case {
                              0 -> P.pure $ (\x -> x);
                              n -> P.pure $ (\x -> T.fastcat [x, apply (apply try (P.pure (n P.- 1))) x])
                            }) iP
                            )

-- continous

sine :: Pattern Double
sine = T.sine

rand :: Pattern Double
rand = T.rand

perlin :: Pattern Double
perlin = T.perlin

saw :: Pattern Double
saw = T.saw

tri :: Pattern Double
tri = T.tri

smooth :: P (Pattern Double -> Pattern Double)
smooth = toPat T.smooth

segment :: Pat a => P (Pattern Double -> Pattern a -> Pattern a)
segment = toPat $ \x -> (T.segment $ P.fmap P.toRational x)

range :: P (Pattern Double -> Pattern Double -> Pattern Double -> Pattern Double)
range = toPat T.range


--- comparisons

geqT :: Pattern Double -> Pattern Double -> Pattern Bool
geqT = T.tParam func
     where func i jP = P.fmap (\j -> i P.>= j) jP

leqT :: Pattern Double -> Pattern Double -> Pattern Bool
leqT = T.tParam func
    where func i jP = P.fmap (\j -> i P.<= j) jP

eqT :: Pattern Double -> Pattern Double -> Pattern Bool
eqT = T.tParam func
    where func i jP = P.fmap (\j -> i P.<= j) jP

(>=) :: P (Pattern Double -> Pattern Double -> Pattern Bool)
(>=) = toPat geqT

(<=) :: P (Pattern Double -> Pattern Double -> Pattern Bool)
(<=) = toPat leqT

(==) :: P (Pattern Double -> Pattern Double -> Pattern Bool)
(==) = toPat eqT

andT :: Pattern Bool -> Pattern Bool -> Pattern Bool
andT = T.tParam func
    where func i jP = P.fmap (\j -> i P.&& j) jP

orT :: Pattern Bool -> Pattern Bool -> Pattern Bool
orT = T.tParam func
    where func i jP = P.fmap (\j -> i P.|| j) jP

(&&) :: P (Pattern Bool -> Pattern Bool -> Pattern Bool)
(&&) = toPat andT

(||) :: P (Pattern Bool -> Pattern Bool -> Pattern Bool)
(||) = toPat orT

--

-- replicate a sequence of i patterns
repT :: Pattern Int -> Pattern a -> Pattern a
repT iP x = T.innerJoin $ P.fmap (\i -> T.fastcat $ P.replicate i x) iP

rep :: Pat a => P (Pattern Int -> Pattern a -> Pattern a)
rep = toPat repT

--c:maj:i:o
data Modifier = Chord (Pattern [Pattern Int]) | Invert Int

class Modifiable a where
  modify :: a -> Pattern Modifier -> Pattern [Pattern Int]
