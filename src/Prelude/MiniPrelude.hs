{-# LANGUAGE TypeApplications #-}
module Prelude.MiniPrelude where

import qualified Prelude as P
import qualified Sound.Tidal.Context as T hiding (fromList)

import Meta

infixr 0 $
($) :: Pat b => P (Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b)
($) = toPat apply

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

(++) :: P (Pattern String -> Pattern String -> Pattern String)
(++) = toPat $$ lift2 (P.++)

rev :: Pat a => P (Pattern a -> Pattern a)
rev = toPat T.rev

fast :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
fast = toPat (\x -> T.fast (fromNum x))

slow :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
slow = toPat (\x -> T.slow (fromNum x))

ply :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
ply = toPat (\x -> T.ply (fromNum x))

plyWith :: Pat a => P (Pattern Number -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
plyWith = toPat (\x -> (T.plyWith :: Pattern Double -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a) (fromNum x))

rot :: (Pat a, P.Ord a) => P (Pattern Number -> Pattern a -> Pattern a)
rot = toPat (\x -> T.rot $$ fromNum x)

run :: P (Pattern Number -> Pattern Number)
run = toPat $$ toNum (T.run :: Pattern Double -> Pattern Double)

irand :: P (Pattern Number -> Pattern Number)
irand = toPat $$ toNum (T.irand :: Pattern Int -> Pattern Double)

rotL :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
rotL = toPat (\x y -> (fromNum x) T.<~ y)

(<~) :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
(<~) = rotL

rotR :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
rotR = toPat (\x y -> (fromNum x) T.~> y)

(~>) :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
(~>) = rotR

struct :: Pat a => P (Pattern Bool -> Pattern a -> Pattern a)
struct = toPat T.struct

toBool :: P (Pattern a -> Pattern Bool)
toBool = P.pure (P.fmap (\_ -> P.True))

structFrom :: P (Pattern b -> Pattern a -> Pattern a)
structFrom = P.pure (\b -> P.pure (T.struct (apply toBool b)))

mask :: Pat a => P (Pattern Bool -> Pattern a -> Pattern a)
mask = toPat T.mask

every :: Pat a => P (Pattern Number -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
every = toPat (\x -> T.every $$ fromNum x)

while :: (IsBool b, Pat a) => P (Pattern b -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
while = toPat (\x -> T.while (P.fmap asBool x))

superimpose :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
superimpose = toPat T.superimpose

jux :: P ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)
jux = toPat T.jux

iter :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
iter = toPat (\x -> T.iter $$ fromNum x)

sometimes :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
sometimes = toPat T.sometimes

rarely :: Pat a => P ((Pattern a -> Pattern a) -> Pattern a -> Pattern a)
rarely = toPat T.rarely

degrade :: Pat a =>  P (Pattern a -> Pattern a)
degrade = toPat T.degrade

degradeBy :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
degradeBy = toPat (\x -> T.degradeBy $$ fromNum x)

(?) :: Pat a => P (Pattern a -> Pattern Number -> Pattern a)
(?) = toPat (\x d -> T.degradeBy (fromNum d) x)

timeLoop :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
timeLoop = toPat (\tm -> T.timeLoop $$ fromNum tm)

loop :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
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

round :: P (Pattern Number -> Pattern Number)
round = toPat $$ toNum (lift (P.round :: Double -> Int))

floor :: P (Pattern Number -> Pattern Number)
floor = toPat $$ toNum (lift (P.floor :: Double -> Int))




-- samples

bd :: Pattern String
bd = P.pure "bd"

sn :: Pattern String
sn = P.pure "sn"

--

c :: Pattern Number
c = 0

e :: Pattern Number
e = 4

-- meta functions to get the structure

right :: Pat c => Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
right op = P.pure (\x -> P.pure (\y -> apply (apply structFrom y) (apply (apply op x) y)))

left :: Pat c => Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
left op = P.pure (\x -> P.pure (\y -> apply (apply structFrom x) (apply (apply op x) y)))


-- list stuff

maj :: Pattern [Pattern Number]
maj = P.pure [0,4,7]

min :: Pattern [Pattern Number]
min = P.pure [0,3,7]

stack :: Pattern (Pattern [Pattern a] -> Pattern a)
stack = P.pure (\x -> T.innerJoin $$ P.fmap T.stack x)

mapT :: Pattern (Pattern a -> Pattern b) -> Pattern [Pattern a] -> Pattern [Pattern b]
mapT g pxs = P.fmap (\xs -> P.map (apply g) xs) pxs

map :: P (Pattern (Pattern a -> Pattern b) -> Pattern [Pattern a] -> Pattern [Pattern b])
map = toPat mapT

layerT :: Pattern [Pattern (Pattern a -> Pattern b)] -> Pattern a -> Pattern [Pattern b]
layerT fsp x = P.fmap (\fs -> P.map (\g -> apply g x) fs) fsp

layer :: Pattern (Pattern [Pattern (Pattern a -> Pattern b)] -> Pattern (Pattern a -> Pattern [Pattern b]))
layer = toPat layerT

-- type conversions
-- not neccessary?
-- int :: Pattern (Pattern Int -> Pattern Int)
-- int = id
--
-- double :: Pattern (Pattern Double -> Pattern Double)
-- double = id
--
-- string :: Pattern (Pattern String -> Pattern String)
-- string = id
--
-- bool :: Pattern (Pattern Bool -> Pattern Bool)
-- bool = id


-- state

-- setD :: (?tidal :: T.Stream) => String -> Pattern Double -> P.IO ()
-- setD = T.streamSetF ?tidal

-- pattern matching compilation

-- try 0 x = x
-- try n x = [x (try (n-1) x)]

-- try :: Pattern (Pattern Int -> (Pattern (Pattern a -> Pattern a)))
-- try = P.pure (\iP -> T.innerJoin $ P.fmap (
--                             \case {
--                               0 -> P.pure $ (\x -> x);
--                               n -> P.pure $ (\x -> T.fastcat [x, apply (apply try (P.pure (n P.- 1))) x])
--                             }) iP
--                             )

-- continous

sine :: Pattern Number
sine = toNum (T.sine :: Pattern Double)

rand :: Pattern Number
rand = toNum (T.rand :: Pattern Double)

perlin :: Pattern Number
perlin = toNum (T.perlin :: Pattern Double)

saw :: Pattern Number
saw = toNum (T.saw :: Pattern Double)

tri :: Pattern Number
tri = toNum (T.tri :: Pattern Double)

smooth :: P (Pattern Number -> Pattern Number)
smooth = toNum $$ toPat (T.smooth :: Pattern Double -> Pattern Double)

segment :: Pat a => P (Pattern Number -> Pattern a -> Pattern a)
segment = toPat $$ (\x -> T.segment $$ fromNum x)

range :: P (Pattern Number -> Pattern Number -> Pattern Number -> Pattern Number)
range = toPat $$ toNum (T.range ::  Pattern Double -> Pattern Double -> Pattern Double -> Pattern Double)


--- comparisons

geqT :: Pattern Number -> Pattern Number -> Pattern Bool
geqT = T.tParam func
     where func i jP = P.fmap (\j -> i P.>= j) jP

leqT :: Pattern Number -> Pattern Number -> Pattern Bool
leqT = T.tParam func
    where func i jP = P.fmap (\j -> i P.<= j) jP

eqT :: Pattern Number -> Pattern Number -> Pattern Bool
eqT = T.tParam func
    where func i jP = P.fmap (\j -> i P.<= j) jP

(>=) :: P (Pattern Number -> Pattern Number -> Pattern Bool)
(>=) = toPat geqT

(<=) :: P (Pattern Number -> Pattern Number -> Pattern Bool)
(<=) = toPat leqT

(==) :: P (Pattern Number -> Pattern Number -> Pattern Bool)
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


--c:maj:i:o
data Modifier = Chord (Pattern [Pattern Int]) | Invert Int

class Modifiable a where
  modify :: a -> Pattern Modifier -> Pattern [Pattern Int]
