{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Zwirn.Interactive.Meta where

import qualified Prelude as P
import qualified Sound.Tidal.Context as T

import qualified Data.Map as Map
import qualified Control.Monad as M

type Pattern = T.Pattern
type ValueMap = T.ValueMap
type ControlPattern = Pattern ValueMap
type Value = T.Value
type Note = T.Note
type Map = Map.Map
type Time = T.Time
type Int = P.Int
type Double = P.Double
type Char = P.Char
type String = P.String
type Bool = P.Bool
type Maybe = P.Maybe

-- this is the only number type in the system to avoid type ambiguities
newtype Number = Num Double
               deriving (P.Show, P.Eq, P.Num, P.Enum, P.Ord, P.Fractional)


-- this is a helper that transforms some types to types that are useful in the system
-- basically this can be thought of as a transformation given as follows
-- ToPat a == (Pattern a); for any basic type a (like Bool, String, Number etc.)
-- ToPat (a -> b) == Pattern (ToPat a) -> Pattern (ToPat a);
-- in practice this is more difficult..
type family P x where
  P (Pattern a -> b) = Pattern (Pattern a -> P b)
  P ((Pattern a -> Pattern b) -> c) = Pattern (Pattern (Pattern a -> Pattern b) -> P c)
  P (Pattern a) = Pattern a
  P ([Pattern a]) = Pattern [Pattern a]
  P a = Pattern a

class Pat a where
  toPat :: a -> P a

-- this class will help us convert anything to use our new Number type replacing
-- Double, Int, Rational etc.
class Convertible a where
  type ToNum a
  toNum :: a -> ToNum a
  fromNum :: ToNum a -> a

-- for allowing Number to act as Bool
class IsBool a where
  asBool :: a -> Bool

instance Convertible Bool where
  type ToNum Bool = Number
  toNum P.True = Num 1
  toNum P.False = Num 0
  fromNum n = (n P.> 0)

instance Convertible Double where
  type ToNum Double = Number
  toNum d = Num d
  fromNum (Num n) = n

instance Convertible Time where
  type ToNum Time = Number
  toNum d = Num $$ P.fromRational d
  fromNum (Num n) = P.toRational n

instance Convertible Int where
  type ToNum Int = Number
  toNum i = Num $$ P.fromIntegral i
  fromNum (Num n) = P.floor n

instance Convertible Note where
  type ToNum Note = Number
  toNum (T.Note i) = Num i
  fromNum (Num n) = T.Note n

instance Convertible String where
  type ToNum String = String
  toNum = P.id
  fromNum = P.id

instance Convertible ValueMap where
  type ToNum ValueMap = ValueMap
  toNum = P.id
  fromNum = P.id

instance Convertible a => Convertible (Pattern a) where
  type ToNum (Pattern a) = Pattern (ToNum a)
  toNum = fmap toNum
  fromNum = fmap fromNum

instance (Convertible a, Convertible b) => Convertible (a -> b) where
  type ToNum (a -> b) = ToNum a -> ToNum b
  toNum f x = toNum $$ f (fromNum x)
  fromNum f x = fromNum $$ f (toNum x)


instance Pat a => (Pat (Pattern a)) where
  toPat = P.id

instance Pat b => Pat (Pattern a -> b) where
  toPat g = P.pure (\x -> toPat $$ g x)

instance (Pat b, Pat c) => Pat ((Pattern a -> Pattern b) -> c) where
  toPat g = P.pure (\x -> toPat (g $$ apply x))

instance Pat ([Pattern a]) where
  toPat = P.pure

instance Pat Number where
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

instance IsBool Number where
  asBool x = (x P.> 0)

instance IsBool Bool where
  asBool = P.id

infixl 0 $$
($$) :: (a -> b) -> a -> b
($$) = (P.$)

fmap :: P.Functor f => (a -> b) -> f a -> f b
fmap = P.fmap

pat :: a -> Pattern a
pat = P.pure

eventLengths :: Pattern a -> Pattern T.Time
eventLengths = T.withEvent (\e -> e {T.value = (T.wholeStop e) P.- (T.wholeStart e)})

applyLeft :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
applyLeft fp p = T.outerJoin $$ T.applyPatToPatLeft (scaleFunction fp) (fmap P.pure p)

applyRight :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
applyRight fp p = T.outerJoin $$ T.applyPatToPatRight (scaleFunction fp) (fmap P.pure p)

applyBoth :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
applyBoth fp p = T.outerJoin $$ T.applyPatToPatBoth (scaleFunction fp) (fmap P.pure p)

-- meta functions to get the structure

right :: Pat c => Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
right op = P.pure (\x -> P.pure (\y -> applyRight (apply op x) y))

left :: Pat c => Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
left op =  P.pure (\x -> P.pure (\y -> applyLeft (applyRight op x) y))

scaleFunction :: Pattern (Pattern a -> Pattern b) -> Pattern (Pattern a -> Pattern b)
scaleFunction fp = fmap (\f -> scaleWith (collect fp) f) fp
            where scaleWith st f = T.outside (deleteContext $$ eventLengths st) f

apply :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
apply fp p = T.innerJoin $$ fmap (\f -> f p) (scaleFunction fp)

squeezeApply :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
squeezeApply fp p = T.squeezeJoin $$ fmap (\f -> f p) (scaleFunction fp)

deleteContext :: Pattern a -> Pattern a
deleteContext = T.withEvent (\e -> e {T.context = T.Context []})

addContext :: ((Int,Int),(Int,Int)) -> Pattern a -> Pattern a
addContext i = T.withEvent (\e -> e {T.context = T.combineContexts [(T.context e),(T.Context [i])] })

match :: [(P.Double,a)] -> [(P.Double,b)] -> [(a,b)]
match [] _ = []
match _ [] = []
match (x@(x2,a):xs) (y@(y2,b):ys) | x2 P.> y2 = (a,b):(match (x:xs) ys)
                                  | y2 P.> x2 = (a,b):(match xs (y:ys))
                                  | P.otherwise = (a,b):(match xs ys)


choiceBy :: Int -> [Pattern a] -> Pattern a
choiceBy seed xs = T.innerJoin (T.segment 1 $$ T.chooseBy (T.rotL (0.0001 P.* P.fromIntegral seed) T.rand) xs)


-- the following will be integrated in tidal in the future

groupEventsBy :: (T.Event a -> T.Event a -> Bool) -> [T.Event a] -> [[T.Event a]]
groupEventsBy _ [] = []
groupEventsBy f (e:es) = ins (groupEventsBy f es) e
                       where ins [] y = [[y]]
                             ins ([]:xss) y = ins xss y
                             ins ((x:xs):xss) y = case f y x of
                                                    P.True -> (y:x:xs):xss
                                                    P.False -> (x:xs):(ins xss y)

sameDur :: T.Event a -> T.Event a -> Bool
sameDur e1 e2 = (T.whole e1 P.== T.whole e2) P.&& (T.part e1 P.== T.part e2)

-- lifting

lift :: (a -> r) -> Pattern a -> Pattern r
lift = M.liftM

lift2 :: (a -> b -> r) -> Pattern a -> Pattern b -> Pattern r
lift2 = M.liftM2

lift3 :: (a -> b -> c -> r) -> Pattern a -> Pattern b -> Pattern c -> Pattern r
lift3 = M.liftM3

lift4 :: (a -> b -> c -> d -> r) -> Pattern a -> Pattern b -> Pattern c -> Pattern d -> Pattern r
lift4 = M.liftM4

lift5 :: (a -> b -> c -> d -> e -> r) -> Pattern a -> Pattern b -> Pattern c -> Pattern d -> Pattern e -> Pattern r
lift5 = M.liftM5

-- assumes that all events in the list have same whole/part
collectEvent :: [T.Event a] -> Maybe (T.Event [a])
collectEvent [] = P.Nothing
collectEvent l@(e:_) = P.Just $$ e {T.context = con, T.value = vs}
                      where con = unionC $$ P.map T.context l
                            vs = P.map T.value l
                            unionC [] = T.Context []
                            unionC ((T.Context is):cs) = T.Context (is P.++ iss)
                                                       where T.Context iss = unionC cs

collectEventsBy :: (T.Event a -> T.Event a -> Bool) -> [T.Event a] -> [T.Event [a]]
collectEventsBy f es = remNo $$ P.map collectEvent (groupEventsBy f es)
                     where
                     remNo [] = []
                     remNo (P.Nothing:cs) = remNo cs
                     remNo ((P.Just c):cs) = c : (remNo cs)

collectBy :: (T.Event a -> T.Event a -> Bool) -> Pattern a -> Pattern [a]
collectBy f = T.withEvents (collectEventsBy f)

collect :: Pattern a -> Pattern [a]
collect = collectBy sameDur

--

insert :: String -> [String] -> String
insert code args = go code
    where
    at xs i = xs P.!! i
    argument i = (args `at` i)

    go []           = []
    go ('%':'%':cs) = '%' : go cs
    go ('%':c  :cs) = argument index P.++ go cs
        where index = P.fromEnum c P.- P.fromEnum '1'
    go (c:cs)       = c : go cs
