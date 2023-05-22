module Functional where

import qualified Prelude as P
import qualified Sound.Tidal.Context as T

-- this module contains meta functions

type Int = P.Int
type Bool = P.Bool
type Pattern = T.Pattern
type Maybe = P.Maybe

infixl 0 $
($) :: (a -> b) -> a -> b
($) = (P.$)

fmap :: P.Functor f => (a -> b) -> f a -> f b
fmap = P.fmap

pat :: a -> Pattern a
pat = P.pure

eventLengths :: Pattern a -> Pattern T.Time
eventLengths = T.withEvent (\e -> e {T.value = (T.wholeStop e) P.- (T.wholeStart e)})

apply :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
apply fp p = T.innerJoin $ fmap (\f -> scaleWith (collect fp) f $ p) fp
            where scaleWith st f = T.outside (eventLengths st) f

match :: [(P.Double,a)] -> [(P.Double,b)] -> [(a,b)]
match [] _ = []
match _ [] = []
match (x@(x2,a):xs) (y@(y2,b):ys) | x2 P.> y2 = (a,b):(match (x:xs) ys)
                                  | y2 P.> x2 = (a,b):(match xs (y:ys))
                                  | P.otherwise = (a,b):(match xs ys)


choiceBy :: Int -> [Pattern a] -> Pattern a
choiceBy seed xs = T.innerJoin (T.segment 1 $ T.chooseBy (T.rotL (0.0001 P.* P.fromIntegral seed) T.rand) xs)


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

-- assumes that all events in the list have same whole/part
collectEvent :: [T.Event a] -> Maybe (T.Event [a])
collectEvent [] = P.Nothing
collectEvent l@(e:_) = P.Just $ e {T.context = con, T.value = vs}
                      where con = unionC $ P.map T.context l
                            vs = P.map T.value l
                            unionC [] = T.Context []
                            unionC ((T.Context is):cs) = T.Context (is P.++ iss)
                                                       where T.Context iss = unionC cs

collectEventsBy :: (T.Event a -> T.Event a -> Bool) -> [T.Event a] -> [T.Event [a]]
collectEventsBy f es = remNo $ P.map collectEvent (groupEventsBy f es)
                     where
                     remNo [] = []
                     remNo (P.Nothing:cs) = remNo cs
                     remNo ((P.Just c):cs) = c : (remNo cs)

collectBy :: (T.Event a -> T.Event a -> Bool) -> Pattern a -> Pattern [a]
collectBy f = T.withEvents (collectEventsBy f)

collect :: Pattern a -> Pattern [a]
collect = collectBy sameDur
