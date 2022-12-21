module Tidal where

import Sound.Tidal.Pattern (Pattern)
import Sound.Tidal.Core (stack,fastcat,silence,fast,slow)
import Sound.Tidal.Show()

import Interpreter (TermF (..), getFSeq)

toPatternI :: TermF -> Maybe (Pattern Int)
toPatternI (FInt i) = Just $ pure i
toPatternI FRest = Just $ silence
toPatternI FEmpty = Just $ silence
toPatternI (FStack t1 t2) = do
                      p1 <- toPatternI t1
                      p2 <- toPatternI t2
                      return $ stack [p1,p2]
toPatternI t@(FSeq _ _) = do
                      let ts = getFSeq t
                      ps <- sequence $ map toPatternI ts
                      return $ fastcat ps
toPatternI (FMult t1 t2) = do
                      p1 <- toPatternI t1
                      p2 <- toPatternI t2
                      return $ fast (fmap fromIntegral p2) $ p1
toPatternI (FDiv t1 t2) = do
                      p1 <- toPatternI t1
                      p2 <- toPatternI t2
                      return $ slow (fmap fromIntegral p2) $ p1
toPatternI  _ = Nothing

toPatternB :: TermF -> Maybe (Pattern Bool)
toPatternB (FBool b) = Just $ pure b
toPatternB FRest = Just $ silence
toPatternB FEmpty = Just $ silence
toPatternB (FStack t1 t2) = do
                      p1 <- toPatternB t1
                      p2 <- toPatternB t2
                      return $ stack [p1,p2]
toPatternB t@(FSeq _ _) = do
                      let ts = getFSeq t
                      ps <- sequence $ map toPatternB ts
                      return $ fastcat ps
toPatternB (FMult t1 t2) = do
                      p1 <- toPatternB t1
                      p2 <- toPatternI t2
                      return $ fast (fmap fromIntegral p2) $ p1
toPatternB (FDiv t1 t2) = do
                      p1 <- toPatternB t1
                      p2 <- toPatternI t2
                      return $ slow (fmap fromIntegral p2) $ p1
toPatternB  _ = Nothing
