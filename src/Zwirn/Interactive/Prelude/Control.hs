module Zwirn.Interactive.Prelude.Control where

import qualified Prelude as P ()
import qualified Sound.Tidal.Context as T hiding (fromList)

import Zwirn.Interactive.Types
import Zwirn.Interactive.Transform
import Zwirn.Interactive.Convert

-- control pattern stuff

pN :: P (TextPattern -> NumberPattern -> ControlPattern)
pN = toPat $$ toTarget (T.tParam T.pF)

pS :: P (TextPattern -> TextPattern -> ControlPattern)
pS = toPat $$ toTarget (T.tParam T.pS)

pB :: P (TextPattern -> NumberPattern -> ControlPattern)
pB = toPat $$ toTarget (T.tParam T.pB)


-- functions

(#) :: P (ControlPattern -> ControlPattern -> ControlPattern)
(#) = toPat (T.#)

spin :: P (NumberPattern -> ControlPattern -> ControlPattern)
spin = toPat $$ toTarget T.spin

gap :: P (NumberPattern -> ControlPattern -> ControlPattern)
gap = toPat $$ toTarget T.gap

slice :: P (NumberPattern -> NumberPattern -> ControlPattern -> ControlPattern)
slice = toPat $$ toTarget T.slice

randslice :: P (NumberPattern -> ControlPattern -> ControlPattern)
randslice = toPat $$ toTarget T.randslice

splice :: P (NumberPattern -> NumberPattern -> ControlPattern -> ControlPattern)
splice = toPat $$ toTarget T.splice

striate :: P (NumberPattern -> ControlPattern -> ControlPattern)
striate = toPat $$ toTarget T.striate

striateBy :: P (NumberPattern -> NumberPattern -> ControlPattern -> ControlPattern)
striateBy = toPat $$ toTarget T.striateBy

chop :: P (NumberPattern -> ControlPattern -> ControlPattern)
chop = toPat $$ toTarget T.chop

loopAt :: P (NumberPattern -> ControlPattern -> ControlPattern)
loopAt = toPat $$ toTarget T.loopAt
