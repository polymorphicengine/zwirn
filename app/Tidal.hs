module Tidal where

import Sound.Tidal.Pattern (Pattern)
import Sound.Tidal.Core (stack,cat,fastcat,silence,fast,slow)
import Sound.Tidal.Show()

import Language

toPattern :: Term -> Maybe (Pattern Int)
toPattern (TInt i) = Just $ pure i
toPattern TRest = Just $ silence
toPattern TEmpty = Just $ silence
toPattern (TStack t1 t2) = do
                      p1 <- toPattern t1
                      p2 <- toPattern t2
                      return $ stack [p1,p2]
toPattern t@(TAlt _ _) = do
                      let ts = removeEmpty $ getTAlt t
                      ps <- sequence $ map toPattern ts
                      return $ cat ps
toPattern (TSub t) = toPattern t
toPattern t@(TSeq _ _) = do
                      let ts = getTSeq t
                      ps <- sequence $ map toPattern ts
                      return $ fastcat ps
toPattern (TMult t1 t2) = do
                      p1 <- toPattern t1
                      p2 <- toPattern t2
                      return $ fast (fmap fromIntegral p2) $ p1
toPattern (TDiv t1 t2) = do
                      p1 <- toPattern t1
                      p2 <- toPattern t2
                      return $ slow (fmap fromIntegral p2) $ p1
toPattern  _ = Nothing
