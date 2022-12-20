module Tidal where

import Sound.Tidal.Pattern (Pattern)
import Sound.Tidal.Core (stack,fastcat,silence,fast,slow)
import Sound.Tidal.Show()

import Language

toPattern :: TermF -> Maybe (Pattern Int)
toPattern (FInt i) = Just $ pure i
toPattern FRest = Just $ silence
toPattern FEmpty = Just $ silence
toPattern (FStack t1 t2) = do
                      p1 <- toPattern t1
                      p2 <- toPattern t2
                      return $ stack [p1,p2]
toPattern t@(FSeq _ _) = do
                      let ts = getFSeq t
                      ps <- sequence $ map toPattern ts
                      return $ fastcat ps
toPattern (FMult t1 t2) = do
                      p1 <- toPattern t1
                      p2 <- toPattern t2
                      return $ fast (fmap fromIntegral p2) $ p1
toPattern (FDiv t1 t2) = do
                      p1 <- toPattern t1
                      p2 <- toPattern t2
                      return $ slow (fmap fromIntegral p2) $ p1
toPattern  _ = Nothing
