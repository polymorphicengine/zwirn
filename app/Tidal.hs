module Tidal where

import Sound.Tidal.Pattern (Pattern)
import Sound.Tidal.Core (stack,silence,fast,slow, timecat)
import Sound.Tidal.Show()

import Functional (Mini (..), getFSeq, removeEmpty)

toPattern :: Mini a -> Pattern a
toPattern (FVal i) =  pure i
toPattern FRest = silence
toPattern FEmpty = silence
toPattern (FElong t) =  toPattern t
toPattern (FStack t1 t2) = stack [toPattern t1,toPattern t2]
toPattern t@(FSeq _ _) = timecat $ map (\(n,m) -> (fromIntegral n,toPattern m)) (resolveSize $ removeEmpty $ getFSeq t)
toPattern (FMult t1 t2) = fast (fmap fromIntegral $ toPattern t2) $ toPattern t1
toPattern (FDiv t1 t2) = slow (fmap fromIntegral $ toPattern t2) $ toPattern t1

resolveSize :: [Mini a] -> [(Int,Mini a)]
resolveSize = map (\m -> (elongAmount m, m))

elongAmount :: Mini a -> Int
elongAmount (FElong t) = elongAmount t + 1
elongAmount _ = 1
