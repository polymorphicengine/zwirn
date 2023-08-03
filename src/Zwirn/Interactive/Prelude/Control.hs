module Zwirn.Interactive.Prelude.Control where

import qualified Prelude as P ()
import qualified Sound.Tidal.Context as T hiding (fromList)

import Zwirn.Interactive.Types
import Zwirn.Interactive.Transform
import Zwirn.Interactive.Convert

-- control pattern stuff

pN :: P (Pattern Text -> Pattern Number -> ControlPattern)
pN = toPat $$ toTarget (T.tParam T.pF)

pS :: P (Pattern Text -> Pattern Text -> ControlPattern)
pS = toPat $$ toTarget (T.tParam T.pS)

pB :: P (Pattern Text -> Pattern Number -> ControlPattern)
pB = toPat $$ toTarget (T.tParam T.pB)


-- functions

(#) :: P (ControlPattern -> ControlPattern -> ControlPattern)
(#) = toPat (T.#)

spin :: P (Pattern Number -> ControlPattern -> ControlPattern)
spin = toPat $$ toTarget T.spin

gap :: P (Pattern Number -> ControlPattern -> ControlPattern)
gap = toPat $$ toTarget T.gap

slice :: P (Pattern Number -> Pattern Number -> ControlPattern -> ControlPattern)
slice = toPat $$ toTarget T.slice

randslice :: P (Pattern Number -> ControlPattern -> ControlPattern)
randslice = toPat $$ toTarget T.randslice

splice :: P (Pattern Number -> Pattern Number -> ControlPattern -> ControlPattern)
splice = toPat $$ toTarget T.splice

striate :: P (Pattern Number -> ControlPattern -> ControlPattern)
striate = toPat $$ toTarget T.striate

striateBy :: P (Pattern Number -> Pattern Number -> ControlPattern -> ControlPattern)
striateBy = toPat $$ toTarget T.striateBy

chop :: P (Pattern Number -> ControlPattern -> ControlPattern)
chop = toPat $$ toTarget T.chop

loopAt :: P (Pattern Number -> ControlPattern -> ControlPattern)
loopAt = toPat $$ toTarget T.loopAt
