module Zwirn.Interactive.TidalT where

{-
    TidalT.hs - provides some non standard tidal functions
    Copyright (C) 2023, Martin Gius

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import qualified Prelude as P
import qualified Sound.Tidal.Context as T
import qualified Sound.Tidal.Chords as C
import qualified Control.Monad as M
import qualified Data.Map as Map
import qualified Data.Text as Text

import Zwirn.Interactive.Types
import Zwirn.Interactive.Convert

-- the following functions are needed for converting the AST to a haskell expression

-- SNum
_numPat :: Double -> Pattern Number
_numPat d = P.pure (Num d)

-- SText
_textPat :: String -> Pattern Text
_textPat s = P.pure (Text (Text.pack s))

--SLambda
_pat :: a -> Pattern a
_pat = P.pure

-- SApp
_apply :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
_apply fp p = T.innerJoin $$ P.fmap (\f -> f p) (_scaleFunction fp)

-- SChoice
_choiceBy :: Int -> [Pattern a] -> Pattern a
_choiceBy seed xs = T.innerJoin (T.segment 1 $$ T.chooseBy (T.rotL (0.0001 P.* P.fromIntegral seed) T.rand) xs)

-- SEuclid
_euclidOff :: Pattern Number -> Pattern Number -> Pattern Number -> Pattern a -> Pattern a
_euclidOff a b c x = T.euclidOff (_fromTarget a) (_fromTarget b) (_fromTarget c) x

_euclid :: Pattern Number -> Pattern Number -> Pattern a -> Pattern a
_euclid a b x = T.euclid (_fromTarget a) (_fromTarget b) x

-- for the atoms with positions
_addContext :: ((Int,Int),(Int,Int)) -> Pattern a -> Pattern a
_addContext i = T.withEvent (\e -> e {T.context = T.combineContexts [(T.context e),(T.Context [i])] })


-- these are for the streamSet action

_valToNum :: T.Value -> P.Maybe Number
_valToNum (T.VF x) = P.Just (Num x)
_valToNum _ = P.Nothing

_valToText :: T.Value -> P.Maybe Text
_valToText (T.VS x) = P.Just (Text (Text.pack x))
_valToText _ = P.Nothing

_valToString :: T.Value -> P.Maybe String
_valToString (T.VS x) = P.Just x
_valToString _ = P.Nothing

_valToVM :: T.Value -> P.Maybe ValueMap
_valToVM (T.VList xs) = P.Just (Map.fromList $$ P.concatMap toTuples xs)
                      where toTuples (T.VList [k,v]) = case _valToString k of
                                                                P.Just s -> [(s,v)]
                                                                P.Nothing -> []
                            toTuples _ = []
_valToVM _ = P.Nothing

_emptyVM :: ValueMap
_emptyVM = Map.empty

_cX' :: a -> (T.Value -> P.Maybe a) -> P.String -> Pattern a
_cX' d f s = T.Pattern P.$ \x@(T.State _ m) -> T.query (P.maybe (P.pure d) (T._getP_ f P.. T.valueToPattern) P.$ Map.lookup s m) x

_cN' :: Number -> TextPattern -> NumberPattern
_cN' n tp = T.outerJoin $$ P.fmap (\t -> _cX' n _valToNum (_fromTarget t)) tp

_cN :: NumberPattern -> TextPattern -> NumberPattern
_cN = T.tParam _cN'

-- differten kinds of function application

_scaleFunction :: Pattern (Pattern a -> Pattern b) -> Pattern (Pattern a -> Pattern b)
_scaleFunction fp = P.fmap (\f -> scaleWith (_collect fp) f) fp
            where scaleWith st f = T.outside (_deleteContext $$ _eventLengths st) f

_deleteContext :: Pattern a -> Pattern a
_deleteContext = T.withEvent (\e -> e {T.context = T.Context []})

_eventLengths :: Pattern a -> Pattern T.Time
_eventLengths = T.withEvent (\e -> e {T.value = (T.wholeStop e) P.- (T.wholeStart e)})


_squeezeApply :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
_squeezeApply fp p = T.squeezeJoin $$ P.fmap (\f -> f p) (_scaleFunction fp)

_applyLeft :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
_applyLeft fp p = T.outerJoin $$ T.applyPatToPatLeft (_scaleFunction fp) (P.fmap P.pure p)

_applyRight :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
_applyRight fp p = T.outerJoin $$ T.applyPatToPatRight (_scaleFunction fp) (P.fmap P.pure p)

_applyBoth :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
_applyBoth fp p = T.outerJoin $$ T.applyPatToPatBoth (_scaleFunction fp) (P.fmap P.pure p)

-- meta functions to get the structure

_right :: Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
_right op = P.pure (\x -> P.pure (\y -> _applyRight (_apply op x) y))

_left :: Pattern (Pattern a -> Pattern (Pattern b -> Pattern c)) -> Pattern (Pattern a -> Pattern (Pattern b -> Pattern c))
_left op =  P.pure (\x -> P.pure (\y -> _applyLeft (_applyRight op x) y))

-- lifting

_lift :: (a -> r) -> Pattern a -> Pattern r
_lift = M.liftM

_lift2 :: (a -> b -> r) -> Pattern a -> Pattern b -> Pattern r
_lift2 = M.liftM2

_lift3 :: (a -> b -> c -> r) -> Pattern a -> Pattern b -> Pattern c -> Pattern r
_lift3 = M.liftM3

_lift4 :: (a -> b -> c -> d -> r) -> Pattern a -> Pattern b -> Pattern c -> Pattern d -> Pattern r
_lift4 = M.liftM4

_lift5 :: (a -> b -> c -> d -> e -> r) -> Pattern a -> Pattern b -> Pattern c -> Pattern d -> Pattern e -> Pattern r
_lift5 = M.liftM5

-- assumes that all events in the list have same whole/part
_collectEvent :: [T.Event a] -> Maybe (T.Event [a])
_collectEvent [] = P.Nothing
_collectEvent l@(e:_) = P.Just $$ e {T.context = con, T.value = vs}
                      where con = unionC $$ P.map T.context l
                            vs = P.map T.value l
                            unionC [] = T.Context []
                            unionC ((T.Context is):cs) = T.Context (is P.++ iss)
                                                       where T.Context iss = unionC cs

_collectEventsBy :: (T.Event a -> T.Event a -> Bool) -> [T.Event a] -> [T.Event [a]]
_collectEventsBy f es = remNo $$ P.map _collectEvent (_groupEventsBy f es)
                     where
                     remNo [] = []
                     remNo (P.Nothing:cs) = remNo cs
                     remNo ((P.Just c):cs) = c : (remNo cs)

_collectBy :: (T.Event a -> T.Event a -> Bool) -> Pattern a -> Pattern [a]
_collectBy f = T.withEvents (_collectEventsBy f)

_collect :: Pattern a -> Pattern [a]
_collect = _collectBy _sameDur

_groupEventsBy :: (T.Event a -> T.Event a -> Bool) -> [T.Event a] -> [[T.Event a]]
_groupEventsBy _ [] = []
_groupEventsBy f (e:es) = ins (_groupEventsBy f es) e
                       where ins [] y = [[y]]
                             ins ([]:xss) y = ins xss y
                             ins ((x:xs):xss) y = case f y x of
                                                    P.True -> (y:x:xs):xss
                                                    P.False -> (x:xs):(ins xss y)

_sameDur :: T.Event a -> T.Event a -> Bool
_sameDur e1 e2 = (T.whole e1 P.== T.whole e2) P.&& (T.part e1 P.== T.part e2)

-- another version of layer that uses stacks instead of lists

_layer :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
_layer f x = joined
           where pfs = _collect f -- :: Pattern [Pattern a -> Pattern b]
                 mapped = P.fmap (\fs -> P.map (\g -> g x) fs) pfs -- :: Pattern [Pattern a]
                 uncol = T.uncollect mapped -- Pattern (Pattern a)
                 joined = T.innerJoin uncol

-- ord and eq pattern functions

_geq :: Pattern Number -> Pattern Number -> Pattern Bool
_geq = T.tParam func
     where func i jP = P.fmap (\j -> i P.>= j) jP

_leq :: Pattern Number -> Pattern Number -> Pattern Bool
_leq = T.tParam func
    where func i jP = P.fmap (\j -> i P.<= j) jP

_eq :: Pattern Number -> Pattern Number -> Pattern Bool
_eq = T.tParam func
    where func i jP = P.fmap (\j -> i P.== j) jP

_and :: Pattern Bool -> Pattern Bool -> Pattern Bool
_and = T.tParam func
    where func i jP = P.fmap (\j -> i P.&& j) jP

_or :: Pattern Bool -> Pattern Bool -> Pattern Bool
_or = T.tParam func
    where func i jP = P.fmap (\j -> i P.|| j) jP

-- chords

_toChord :: [Double] -> Pattern (Pattern Number -> Pattern Number)
_toChord ds = P.pure $$ _toTarget (\pd -> T.stack $$ P.map (\d -> P.fmap (P.+ d) pd) ds)

_lookupChord :: P.String -> [P.Double]
_lookupChord s = case P.lookup s C.chordTable of
                           P.Just x -> x
                           P.Nothing -> []

_chordMaker :: P.String -> Pattern (Pattern Number -> Pattern Number)
_chordMaker s = _toChord (_lookupChord s)

_transformStack :: ([a] -> b) -> Pattern a -> Pattern b
_transformStack f p = pf
           where pns = _collect p
                 pf = P.fmap f pns

_transformStackSafe :: ([a] -> a) -> Pattern a -> Pattern a
_transformStackSafe f p = T.outerJoin (P.fmap f' pns)
          where pns = _collect p
                f' xs = case empty xs of
                            P.False -> P.pure (f xs)
                            P.True -> p
                empty [] = P.True
                empty _ = P.False

_withChordFunc :: ([a] -> [a]) -> Pattern a -> Pattern a
_withChordFunc f p = uncol
           where pns = _collect p            -- Pattern [a]
                 pf = P.fmap f pns           -- Pattern [a]
                 uncol = T.uncollect pf      -- Pattern a

_invertChord :: Pattern Number -> Pattern Number
_invertChord = _withChordFunc inv
           where inv [] = []
                 inv (x:xs) = xs P.++ [x P.+ 12]

_expandChord :: Pattern Number -> Pattern (Pattern Number -> Pattern Number)
_expandChord p = P.fmap (\l -> _withChordFunc (expand P.$ _fromTarget l)) p
          where expand i ds = P.take i P.$ P.concatMap (\x -> P.map (P.+ x) ds) [0,12..]

_dropChord :: Pattern Number -> Pattern (Pattern Number -> Pattern Number)
_dropChord p = P.fmap (\l -> _withChordFunc (drop P.$ _fromTarget l)) p
          where drop i ds = case P.length ds P.< i of
                              P.True -> ds
                              P.False -> (ds P.!! s P.- 12):(xs P.++ P.drop 1 ys)
                          where (xs,ys) = P.splitAt s ds
                                s = P.length ds P.- i

_openChord :: Pattern Number -> Pattern Number
_openChord = _withChordFunc open
           where open ds = case P.length ds P.> 2 of
                              P.True -> [ (ds P.!! 0 P.- 12), (ds P.!! 2 P.- 12), (ds P.!! 1) ] P.++ P.reverse (P.take (P.length ds P.- 3) (P.reverse ds))
                              P.False -> ds

-- other list stuff

_at :: Pattern Int -> Pattern a -> Pattern a
_at = T.tParam func
    where func i p = T.uncollect P.$ do
                    xs <- _collect p
                    case P.length xs P.- 1 P.< i  P.|| i P.< 0 of
                                P.True -> P.return []
                                P.False -> P.return [xs P.!! i]

_filterAt :: Pattern Int -> Pattern a -> Pattern a
_filterAt = T.tParam func
    where func i p = T.uncollect P.$ do
                    xs <- _collect p
                    case P.length xs P.- 1 P.< i  P.|| i P.< 0 of
                                P.True -> P.return []
                                P.False -> P.return (fs P.++ P.drop 1 ls)
                                        where (fs,ls) = P.splitAt i xs

_over :: Pattern Int -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a
_over i f p = T.overlay (f (_at i p)) (_filterAt i p)

-- control busses

_recv :: (NumberPattern -> ControlPattern) -> NumberPattern -> ControlPattern
_recv cpf busid = (T.tParam T.pI) (P.fmap (\v -> '^':((Map.keys v) P.!! 0)) P.$ cpf 0) (_fromTarget busid)

_send :: NumberPattern -> NumberPattern -> ControlPattern
_send busid pat = T.pF "" (_fromTarget pat) T.# T.pI "^" (_fromTarget busid)

_lookup :: TextPattern -> ControlPattern -> NumberPattern
_lookup = T.tParam $$ \k vmp -> T.outerJoin $$ P.fmap (\vm -> case Map.lookup (_fromTarget k) vm of
                        P.Just (T.VI x) -> P.pure $$ _toTarget x
                        P.Just (T.VF x) -> P.pure $$ _toTarget x
                        P.Just (T.VN x) -> P.pure $$ _toTarget x
                        P.Just (T.VR x) -> P.pure $$ _toTarget x
                        P.Just (T.VB x) -> P.pure $$ _toTarget x
                        _ -> T.silence) vmp

_lookupT :: TextPattern -> ControlPattern -> TextPattern
_lookupT = T.tParam $$ \k vmp -> T.outerJoin $$ P.fmap (\vm -> case Map.lookup (_fromTarget k) vm of
                        P.Just (T.VS x) -> P.pure $$ _toTarget x
                        _ -> T.silence) vmp

_map :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
_map = T.tParam $$ \f xp -> T.squeezeJoin (P.fmap (\x -> f (P.pure x)) xp)
