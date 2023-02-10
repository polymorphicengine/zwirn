{-# LANGUAGE DeriveFunctor, DeriveGeneric#-}

module Functional where

import qualified Prelude as P
import Data.List (intercalate)

import GHC.Generics

type Var = P.String

data Mini a = FVal a
           | FRest
           | FEmpty
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
displayMini t@(FSeq _ _) = "(" P.++ (intercalate " " P.$  P.map displayMini (removeEmpty P.$ getFSeq t)) P.++ ")"
displayMini (FStack t1 t2) = "(" P.++ displayMini t1 P.++ "," P.++ displayMini t2 P.++ ")"
displayMini (FMult t1 t2) = "(" P.++ displayMini t1 P.++ "*" P.++ displayMini t2 P.++ ")"
displayMini (FDiv t1 t2) = "(" P.++ displayMini t1 P.++ "/" P.++ displayMini t2 P.++ ")"

instance P.Show a => P.Show (Mini a) where
  show = displayMini

removeEmpty :: [Mini a] -> [Mini a]
removeEmpty [] = []
removeEmpty (FEmpty:xs) = xs
removeEmpty (x:xs) = x:removeEmpty xs

getFSeq :: Mini a -> [Mini a]
getFSeq (FSeq t1 t2) = t1:(getFSeq t2)
getFSeq t = [t]

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


-- the idea is that the structure will always come from the right term and the values in the left term will be matched against them
applySeqR :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
applySeqR t1 t2 = toFSeq (P.map (\i -> (P.!!) zs i) [(P.*) x l2 | x <- [0..(P.-) n2 1]])
                    where s1 = removeEmpty P.$ getFSeq t1
                          s2 = removeEmpty P.$ getFSeq t2
                          n1 = P.length s1
                          n2 = P.length s2
                          l = P.lcm n1 n2
                          l1 = P.div l n1
                          l2 = P.div l n2
                          f1 = P.concatMap (\x -> P.take l1 (P.repeat x)) s1
                          f2 = P.concatMap (\x -> P.take l2 (P.repeat x)) s2
                          zs = P.zipWith (\x y -> apply x y) f1 f2

applySeqSmartR :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
applySeqSmartR t1 t2 = case P.mod n1 n2 P.== 0 of
                              P.True -> toFSeq zs
                              P.False -> toFSeq (P.map (\i -> (P.!!) zs i) [(P.*) x l2 | x <- [0..(P.-) n2 1]])
                    where s1 = removeEmpty P.$ getFSeq t1
                          s2 = removeEmpty P.$ getFSeq t2
                          n1 = P.length s1
                          n2 = P.length s2
                          l = P.lcm n1 n2
                          l1 = P.div l n1
                          l2 = P.div l n2
                          f1 = P.concatMap (\x -> P.take l1 (P.repeat x)) s1
                          f2 = P.concatMap (\x -> P.take l2 (P.repeat x)) s2
                          zs = P.zipWith (\x y -> apply x y) f1 f2

subseqGroup :: Int -> [Mini a] -> [Mini a]
subseqGroup n xs = (toFSeq $ P.take n xs):(subseqGroup n (P.drop n xs))

applySubSeqSmartR :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
applySubSeqSmartR t1 t2 = case P.mod n2 n1 P.== 0 of
                              P.True -> toFSeq $ P.zipWith (\x y -> apply x y) s1 (subseqGroup l1 s2)
                              P.False -> case P.mod n1 n2 P.== 0 of
                                                      P.True -> toFSeq zs
                                                      P.False -> toFSeq (P.map (\i -> (P.!!) zs i) [(P.*) x l2 | x <- [0..(P.-) n2 1]])
                    where s1 = removeEmpty P.$ getFSeq t1
                          s2 = removeEmpty P.$ getFSeq t2
                          n1 = P.length s1
                          n2 = P.length s2
                          l = P.lcm n1 n2
                          l1 = P.div l n1
                          l2 = P.div l n2
                          f1 = P.concatMap (\x -> P.take l1 (P.repeat x)) s1
                          f2 = P.concatMap (\x -> P.take l2 (P.repeat x)) s2
                          zs = P.zipWith (\x y -> apply x y) f1 f2

applySubSeqSmartL :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
applySubSeqSmartL t1 t2 = case P.mod n1 n2 P.== 0 of
                              P.True -> toFSeq $ P.zipWith (\x y -> apply x y) s1 (subseqGroup l2 s2)
                              P.False -> case P.mod n2 n1 P.== 0 of
                                                      P.True -> toFSeq zs
                                                      P.False -> toFSeq (P.map (\i -> (P.!!) zs i) [(P.*) x l1 | x <- [0..(P.-) n1 1]])
                    where s1 = removeEmpty P.$ getFSeq t1
                          s2 = removeEmpty P.$ getFSeq t2
                          n1 = P.length s1
                          n2 = P.length s2
                          l = P.lcm n1 n2
                          l1 = P.div l n1
                          l2 = P.div l n2
                          f1 = P.concatMap (\x -> P.take l1 (P.repeat x)) s1
                          f2 = P.concatMap (\x -> P.take l2 (P.repeat x)) s2
                          zs = P.zipWith (\x y -> apply x y) f1 f2

applySeqL :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
applySeqL t1 t2 = toFSeq (P.map (\i -> (P.!!) zs i) [(P.*) x l1 | x <- [0..(P.-) n1 1]])
                    where s1 = removeEmpty P.$ getFSeq t1
                          s2 = removeEmpty P.$ getFSeq t2
                          n1 = P.length s1
                          n2 = P.length s2
                          l = P.lcm n1 n2
                          l1 = P.div l n1
                          l2 = P.div l n2
                          f1 = P.concatMap (\x -> P.take l1 (P.repeat x)) s1
                          f2 = P.concatMap (\x -> P.take l2 (P.repeat x)) s2
                          zs = P.zipWith (\x y -> apply x y) f1 f2

applySeqB :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
applySeqB t1 t2 = toFSeq zs
                    where s1 = removeEmpty P.$ getFSeq t1
                          s2 = removeEmpty P.$ getFSeq t2
                          n1 = P.length s1
                          n2 = P.length s2
                          l = P.lcm n1 n2
                          l1 = P.div l n1
                          l2 = P.div l n2
                          f1 = P.concatMap (\x -> P.take l1 (P.repeat x)) s1
                          f2 = P.concatMap (\x -> P.take l2 (P.repeat x)) s2
                          zs = P.zipWith (\x y -> apply x y) f1 f2

applyStack :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
applyStack t1 t2 = toFStack zs
                  where s1 = getFStack t1
                        s2 = getFStack t2
                        zs = P.zipWith (\x y -> apply x y) s1 s2

applyNew :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
applyNew f x = toFSeq xs
            where ss = removeEmpty $ getFSeq f
                  xs = P.map (\g -> apply g x) ss
                  -- n1 = P.length ss
                  -- n2s = P.map P.length xs
                  -- ls = P.map (\x -> P.lcm n1 x) n2s
                  -- l1s = P.map (\l -> P.div l) ls
                  -- l2s = P.zipWith (\l n -> P.div l n) ls n2s

combine :: Mini (Mini a) -> Mini a
combine (FVal x) = x
combine t@(FSeq _ _) = toFSeq P.$ P.concatMap getFSeq (P.fmap combine P.$ getFSeq t)
combine t@(FStack _ _) = toFStack P.$ P.concatMap getFStack (P.fmap combine P.$ getFStack t)
combine (FDiv t n) = FDiv (combine t) n
combine (FMult t n) = FMult (combine t) n
combine FEmpty = FEmpty
combine FRest = FRest

apply :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
apply (FVal f) t = f t
apply (FSeq f fs) t = FSeq (apply f t) (apply fs t)
apply FEmpty _ = FEmpty
apply FRest _ = FRest
apply _ _ = P.error "cannot apply these terms for now"

($|) :: Mini (Mini a -> Mini b) -> Mini a -> Mini b
($|) = apply

lift2 :: (a -> b -> c) -> Mini a -> (Mini (Mini b -> Mini c))
lift2 f x = lift (fmap f x)


lift :: Mini (a -> b) -> Mini (Mini a -> Mini b)
lift (FVal f) = FVal $ fmap f
lift (FSeq t ts) = FSeq (lift t) (lift ts)
lift (FStack t ts) = FStack (lift t) (lift ts)
lift (FDiv t n) = FDiv (lift t) n
lift (FMult t n) = FMult (lift t) n
lift FEmpty = FEmpty
lift FRest = FRest

pure :: a -> Mini a
pure = FVal
