module Functional where

import qualified Prelude as P
import Data.List (intercalate)

type Var = P.String

data TermF = FVar Var
           | FInt P.Int
           | FBool P.Bool
           | FRest
           | FEmpty
           | FSeq TermF TermF
           | FStack TermF TermF
           | FMult TermF TermF
           | FDiv TermF TermF
           | FLambda (TermF -> TermF)

displayTermF :: TermF -> P.String
displayTermF (FVar x) = x
displayTermF (FInt i) = P.show i
displayTermF (FBool P.True) = "t"
displayTermF (FBool P.False) = "f"
displayTermF (FRest) = "~"
displayTermF FEmpty = ""
displayTermF t@(FSeq _ _) = "(" P.++ (intercalate " " P.$  P.map displayTermF (removeEmpty P.$ getFSeq t)) P.++ ")"
displayTermF (FStack t1 t2) = displayTermF t1 P.++ "," P.++ displayTermF t2
displayTermF (FMult t1 t2) = displayTermF t1 P.++ "*" P.++ displayTermF t2
displayTermF (FDiv t1 t2) = displayTermF t1 P.++ "/" P.++ displayTermF t2
displayTermF (FLambda ps) = P.error "Can't display functions"

removeEmpty :: [TermF] -> [TermF]
removeEmpty [] = []
removeEmpty (FEmpty:xs) = xs
removeEmpty (x:xs) = x:removeEmpty xs

getFSeq :: TermF -> [TermF]
getFSeq (FSeq t1 t2) = t1:(getFSeq t2)
getFSeq t = [t]

toFSeq :: [TermF] -> TermF
toFSeq [] = FEmpty
toFSeq (t:ts) = FSeq t (toFSeq ts)

getFStack :: TermF -> [TermF]
getFStack (FStack t1 t2) = getFStack t1 P.++ getFStack t2
getFStack t = [t]

toFStack :: [TermF] -> TermF
toFStack [t] = t
toFStack (t1:t2) = FStack t1 (toFStack t2)


-- the idea is that the structure will always come from the right term and the values in the left term will be matched against them
applySeqR :: TermF -> TermF -> TermF
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

applySeqL :: TermF -> TermF -> TermF
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

applyStack :: TermF -> TermF -> TermF
applyStack t1 t2 = toFStack zs
                  where s1 = getFStack t1
                        s2 = getFStack t2
                        zs = P.zipWith (\x y -> apply x y) s1 s2

apply :: TermF -> TermF -> TermF
apply (FLambda f) t = f t
apply t1@(FSeq _ _) t2 = applySeqR t1 t2
apply t1@(FStack _ _) t2 = applyStack t1 t2
apply FEmpty t2 = t2
apply _ _ = P.error "Cannot apply these terms!"

intToTerm :: (P.Int -> P.Int) -> TermF
intToTerm f = FLambda g
            where g (FInt i) = FInt (f i)
                  g (FSeq t1 t2) = FSeq (apply (intToTerm f) t1) (apply (intToTerm f) t2)
                  g (FStack t1 t2) = FStack (apply (intToTerm f) t1) (apply (intToTerm f) t2)
                  g (FDiv t n) = FDiv (apply (intToTerm f) t) n
                  g (FMult t n) = FMult (apply (intToTerm f) t) n
                  g x = x -- or throw an error maybe ?

boolToTerm :: (P.Bool -> P.Bool) -> TermF
boolToTerm f = FLambda g
            where g (FBool i) = FBool (f i)
                  g (FSeq t1 t2) = FSeq (apply (boolToTerm f) t1) (apply (boolToTerm f) t2)
                  g (FStack t1 t2) = FStack (apply (boolToTerm f) t1) (apply (boolToTerm f) t2)
                  g (FDiv t n) = FDiv (apply (boolToTerm f) t) n
                  g (FMult t n) = FMult (apply (boolToTerm f) t) n
                  g x = x

intToTerm2 :: (P.Int -> P.Int -> P.Int) -> TermF
intToTerm2 f = FLambda g
            where g (FInt i) = (intToTerm (f i))
                  g (FSeq t1 t2) = FSeq (apply (intToTerm2 f) t1) (apply (intToTerm2 f) t2)
                  g (FStack t1 t2) = FStack (apply (intToTerm2 f) t1) (apply (intToTerm2 f) t2)
                  g (FDiv t n) = FDiv (apply (intToTerm2 f) t) n
                  g (FMult t n) = FMult (apply (intToTerm2 f) t) n
                  g x = x

boolToTerm2 :: (P.Bool -> P.Bool -> P.Bool) -> TermF
boolToTerm2 f = FLambda g
            where g (FBool b) = (boolToTerm (f b))
                  g (FSeq t1 t2) = FSeq (apply (boolToTerm2 f) t1) (apply (boolToTerm2 f) t2)
                  g (FStack t1 t2) = FStack (apply (boolToTerm2 f) t1) (apply (boolToTerm2 f) t2)
                  g (FDiv t n) = FDiv (apply (boolToTerm2 f) t) n
                  g (FMult t n) = FMult (apply (boolToTerm2 f) t) n
                  g x = x
