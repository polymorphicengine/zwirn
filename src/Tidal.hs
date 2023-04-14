module Tidal where

import qualified Sound.Tidal.Pattern as T
import Sound.Tidal.Core (stack,silence,fast,slow,timecat)
import Sound.Tidal.Show()

import qualified Prelude as P

import Functional (Mini (..), getFSeq)

class ToPat m where
 toPattern :: m a -> T.Pattern a

instance ToPat Mini where
  toPattern (FVal i) =  P.pure i
  toPattern FRest = silence
  toPattern FEmpty = silence
  toPattern (FElong t) =  toPattern t
  toPattern (FStack t1 t2) = stack [toPattern t1,toPattern t2]
  toPattern t@(FSeq _ _) = timecat P.$ P.map (\(n,m) -> (P.fromIntegral n,toPattern m)) (resolveSize P.$ getFSeq t)
  toPattern (FMult t1 t2) = fast (P.fmap P.fromIntegral P.$ toPattern t2) P.$ toPattern t1
  toPattern (FDiv t1 t2) = slow (P.fmap P.fromIntegral P.$ toPattern t2) P.$ toPattern t1

instance ToPat T.Pattern where
  toPattern = P.id


resolveSize :: [Mini a] -> [(P.Int,Mini a)]
resolveSize = P.map (\m -> (elongAmount m, m))

elongAmount :: Mini a -> P.Int
elongAmount (FElong t) = elongAmount t P.+ 1
elongAmount _ = 1
