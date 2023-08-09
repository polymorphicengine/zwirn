module Zwirn.Interactive.TidalT where

import qualified Prelude as P
import qualified Sound.Tidal.Context as T
import qualified Control.Monad as M
import qualified Data.Map as Map
import qualified Data.Text as Text

import Zwirn.Interactive.Types

_valToNum :: T.Value -> P.Maybe Number
_valToNum (T.VF x) = P.Just (Num x)
_valToNum _ = P.Nothing

_valToText :: T.Value -> P.Maybe Text
_valToText (T.VS x) = P.Just (Text (Text.pack x))
_valToText _ = P.Nothing

_cX' :: Pattern a -> (T.Value -> P.Maybe a) -> P.String -> Pattern a
_cX' d f s = T.Pattern P.$ \(T.State a m) -> T.queryArc (P.maybe d (T._getP_ f P.. T.valueToPattern) P.$ Map.lookup s m) a

pat :: a -> Pattern a
pat = P.pure

numPat :: Double -> Pattern Number
numPat d = P.pure (Num d)

textPat :: String -> Pattern Text
textPat s = P.pure (Text (Text.pack s))

eventLengths :: Pattern a -> Pattern T.Time
eventLengths = T.withEvent (\e -> e {T.value = (T.wholeStop e) P.- (T.wholeStart e)})

applyLeft :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
applyLeft fp p = T.outerJoin $$ T.applyPatToPatLeft (scaleFunction fp) (fmap P.pure p)

applyRight :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
applyRight fp p = T.outerJoin $$ T.applyPatToPatRight (scaleFunction fp) (fmap P.pure p)

applyBoth :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
applyBoth fp p = T.outerJoin $$ T.applyPatToPatBoth (scaleFunction fp) (fmap P.pure p)

-- meta functions to get the structure

right :: Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
right op = P.pure (\x -> P.pure (\y -> applyRight (apply op x) y))

left :: Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
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

_layer :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
_layer f x = joined
           where pfs = collect f -- :: Pattern [Pattern a -> Pattern b]
                 mapped = fmap (\fs -> P.map (\g -> g x) fs) pfs -- :: Pattern [Pattern a]
                 uncol = T.uncollect mapped -- Pattern (Pattern a)
                 joined = T.innerJoin uncol


geqT :: Pattern Number -> Pattern Number -> Pattern Bool
geqT = T.tParam func
     where func i jP = P.fmap (\j -> i P.>= j) jP

leqT :: Pattern Number -> Pattern Number -> Pattern Bool
leqT = T.tParam func
    where func i jP = P.fmap (\j -> i P.<= j) jP

eqT :: Pattern Number -> Pattern Number -> Pattern Bool
eqT = T.tParam func
    where func i jP = P.fmap (\j -> i P.== j) jP

andT :: Pattern Bool -> Pattern Bool -> Pattern Bool
andT = T.tParam func
    where func i jP = P.fmap (\j -> i P.&& j) jP

orT :: Pattern Bool -> Pattern Bool -> Pattern Bool
orT = T.tParam func
    where func i jP = P.fmap (\j -> i P.|| j) jP
