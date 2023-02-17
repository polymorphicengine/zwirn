{-# LANGUAGE DeriveFunctor, DeriveGeneric#-}

module Functional where

import qualified Prelude as P
import Data.List (intercalate)

import GHC.Generics

type Var = P.String

data Mini a = FVal a
           | FRest
           | FEmpty
           | FElong (Mini a)
           | FSeq (Mini a) (Mini a)
           | FStack (Mini a) (Mini a)
           | FMult (Mini a) (Mini Int)
           | FDiv (Mini a) (Mini Int)
           deriving (P.Functor, Generic)

type Int = P.Int
type Bool = P.Bool


($) :: (a -> b) -> a -> b
($) = (P.$)

fmap :: P.Functor f => (a -> b) -> f a -> f b
fmap = P.fmap

displayMini :: P.Show a => Mini a -> P.String
displayMini (FVal i) = P.show i
displayMini (FRest) = "~"
displayMini FEmpty = ""
displayMini (FElong t) = displayMini t P.++ "@"
displayMini t@(FSeq _ _) = "(" P.++ (intercalate " " P.$  P.map displayMini (getFSeq t)) P.++ ")"
displayMini (FStack t1 t2) = "(" P.++ displayMini t1 P.++ "," P.++ displayMini t2 P.++ ")"
displayMini (FMult t1 t2) = "(" P.++ displayMini t1 P.++ "*" P.++ displayMini t2 P.++ ")"
displayMini (FDiv t1 t2) = "(" P.++ displayMini t1 P.++ "/" P.++ displayMini t2 P.++ ")"

instance P.Show a => P.Show (Mini a) where
  show = displayMini

-- utility functions for converting to a more haskell representation

getFSeq :: Mini a -> [Mini a]
getFSeq FEmpty = []
getFSeq (FSeq t1 t2) = t1:(getFSeq t2)
getFSeq x = [x]

toFSeq :: [Mini a] -> Mini a
toFSeq [] = FEmpty
toFSeq (t:ts) = FSeq t (toFSeq ts)

getFStack :: Mini a -> [Mini a]
getFStack (FStack t1 t2) = getFStack t1 P.++ getFStack t2
getFStack t = [t]

toFStack :: [Mini a] -> Mini a
toFStack [] = FEmpty
toFStack [t] = t
toFStack (t1:t2) = FStack t1 (toFStack t2)

lastEl :: Mini a -> Mini a
lastEl s@(FSeq _ _) = lastEl $ P.last (getFSeq s)
lastEl (FStack t1 t2) = FStack (lastEl t1) (lastEl t2)
lastEl (FElong t) = lastEl t
lastEl (FMult t _) = lastEl t
lastEl (FDiv t _) = lastEl t
lastEl x = x

-- | break a sequence into an almost equivalent sequence with exactly n elements
breakSeq :: Int -> Mini a -> [Mini a]
breakSeq n p = P.map (\hs -> toFSeq $ P.map (\(k,x) -> elongN k x) hs) hss
             where ps = getFSeq p
                   m = P.length ps
                   l = P.lcm n m
                   i = P.div l n
                   j = P.div l m
                   hss = helper2 i j P.Nothing ps

-- | i specifies how much a single slot can hold, while j specifies the length of each individual event
-- | r determinies how much of a slot is already filled
-- | the function returns a complete slot and a leftover when a split occured as well as the rest of the elements that have not been filled in yet
helper1 :: Int -> Int -> Int -> P.Maybe (Int, Mini a) -> [Mini a] -> ([(Int,Mini a)],(Int, [Mini a]))
helper1 _ _ _ _  [] = ([], (0, []))
helper1 _ _ 0 _ _ = P.error "This operation is undefined"
helper1 r i j P.Nothing (p:ps) = case leftover P.> 0 of
                                P.True -> ((newLength, p):xs, n)
                                        where newLength = P.min j (i P.- r)
                                              newRest = r P.+ newLength
                                              (xs,n) = case newLength P.< j of
                                                              P.False -> helper1 newRest i j P.Nothing ps
                                                              P.True -> ([], (j P.- newLength, ps))
                                P.False -> ([], (0, p:ps))
                                where leftover = i P.- r
helper1 _ i j (P.Just (n,x)) ps = ((n,x):xs,l)
                                where (xs,l) = helper1 n i j P.Nothing ps

-- | recursively applc helper1 to fill get all slots
helper2 :: Int -> Int -> P.Maybe (Int, Mini a) -> [Mini a] -> [[(Int, Mini a)]]
helper2 _ _ (P.Just x) [] = [[x]]
helper2 _ _ (P.Nothing) [] = []
helper2 i j may ps = case n P.== 0 of
                              P.True -> ls:(helper2 i j P.Nothing xs)
                              P.False -> [ls] P.++ rss P.++ (helper2 i j r xs) -- change FRest to P.head xs for different effect
               where (ls,(n,xs)) = helper1 0 i j may ps
                     (rss, r) = resolveRests i n

resolveRests :: Int -> Int -> ([[(Int, Mini a)]], P.Maybe (Int, Mini a))
resolveRests i n = case n P.> i of
                      P.True -> ([(i, FRest)]:pss, may)
                        where (pss, may) = resolveRests i (n P.- i)
                      P.False -> ([], P.Just (n, FRest))

elongN :: Int -> Mini a -> Mini a
elongN 1 m = m
elongN i m = FElong (elongN (i P.- 1) m)


applySeq :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
applySeq f t = toFSeq (P.map (\i -> (rss P.!! i) P.!! i) [0..n P.- 1])
             where fs = getFSeq f
                   n = P.length fs
                   rss = P.map (\g -> breakSeq n (apply g t)) fs

-- the idea is that the structure will always come from the right term and the values in the left term will be matched against them
-- applySeqR :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
-- applySeqR t1 t2 = toFSeq (P.map (\i -> (P.!!) zs i) [(P.*) x l2 | x <- [0..(P.-) n2 1]])
--                     where s1 = getFSeq t1
--                           s2 = getFSeq t2
--                           n1 = P.length s1
--                           n2 = P.length s2
--                           l = P.lcm n1 n2
--                           l1 = P.div l n1
--                           l2 = P.div l n2
--                           f1 = P.concatMap (\x -> P.take l1 (P.repeat x)) s1
--                           f2 = P.concatMap (\x -> P.take l2 (P.repeat x)) s2
--                           zs = P.zipWith (\x y -> apply x y) f1 f2

applyStack :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
applyStack t1 t2 = toFStack zs
                  where s1 = getFStack t1
                        s2 = getFStack t2
                        zs = P.zipWith (\x y -> apply x y) s1 s2


combine :: Mini (Mini a) -> Mini a
combine (FVal x) = x
combine t@(FSeq _ _) = toFSeq P.$ P.concatMap getFSeq (P.fmap combine P.$ getFSeq t)
combine t@(FStack _ _) = toFStack P.$ P.concatMap getFStack (P.fmap combine P.$ getFStack t)
combine (FDiv t n) = FDiv (combine t) n
combine (FMult t n) = FMult (combine t) n
combine (FElong t) = FElong (combine t)
combine FEmpty = FEmpty
combine FRest = FRest

apply :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
apply (FVal f) t = f t
apply fs@(FSeq _ _) t = applySeq fs t
apply FEmpty _ = FEmpty
apply FRest _ = FRest
apply _ _ = P.error "cannot apply these terms for now"

($|) :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
($|) = apply

lift2 :: (a -> b -> c) -> Mini a -> (Mini (Mini b -> Mini c))
lift2 f x = lift (fmap f x)


lift :: Mini (a -> b) -> Mini (Mini a -> Mini b)
lift (FVal f) = FVal $ fmap f
lift (FElong t) = FElong (lift t)
lift (FSeq t ts) = FSeq (lift t) (lift ts)
lift (FStack t ts) = FStack (lift t) (lift ts)
lift (FDiv t n) = FDiv (lift t) n
lift (FMult t n) = FMult (lift t) n
lift FEmpty = FEmpty
lift FRest = FRest

pure :: a -> Mini a
pure = FVal
