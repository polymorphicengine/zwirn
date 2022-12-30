module Tidal where

import Sound.Tidal.Pattern (Pattern)
import Sound.Tidal.Core (stack,fastcat,silence,fast,slow)
import Sound.Tidal.Show()

import Functional (Mini (..), getFSeq, removeEmpty)

toPattern :: Mini a -> Pattern a
toPattern (FVal i) =  pure i
toPattern FRest = silence
toPattern FEmpty = silence
toPattern (FStack t1 t2) = stack [toPattern t1,toPattern t2]
toPattern t@(FSeq _ _) = fastcat $ map toPattern (removeEmpty $ getFSeq t)
toPattern (FMult t1 t2) = fast (fmap fromIntegral $ toPattern t2) $ toPattern t1
toPattern (FDiv t1 t2) = slow (fmap fromIntegral $ toPattern t2) $ toPattern t1
