module Zwirn.Interactive.Prelude.Control where

import qualified Prelude as P ()
import qualified Sound.Tidal.Context as T hiding (fromList)

import Zwirn.Interactive.Types
import Zwirn.Interactive.Transform
import Zwirn.Interactive.Convert

-- control pattern stuff

pN :: P (TextPattern -> NumberPattern -> ControlPattern)
pN = _toPat $$ _toTarget (T.tParam T.pF)

pS :: P (TextPattern -> TextPattern -> ControlPattern)
pS = _toPat $$ _toTarget (T.tParam T.pS)

pB :: P (TextPattern -> NumberPattern -> ControlPattern)
pB = _toPat $$ _toTarget (T.tParam T.pB)


-- functions

(#) :: P (ControlPattern -> ControlPattern -> ControlPattern)
(#) = _toPat (T.#)

spin :: P (NumberPattern -> ControlPattern -> ControlPattern)
spin = _toPat $$ _toTarget T.spin

gap :: P (NumberPattern -> ControlPattern -> ControlPattern)
gap = _toPat $$ _toTarget T.gap

slice :: P (NumberPattern -> NumberPattern -> ControlPattern -> ControlPattern)
slice = _toPat $$ _toTarget T.slice

randslice :: P (NumberPattern -> ControlPattern -> ControlPattern)
randslice = _toPat $$ _toTarget T.randslice

splice :: P (NumberPattern -> NumberPattern -> ControlPattern -> ControlPattern)
splice = _toPat $$ _toTarget T.splice

striate :: P (NumberPattern -> ControlPattern -> ControlPattern)
striate = _toPat $$ _toTarget T.striate

striateBy :: P (NumberPattern -> NumberPattern -> ControlPattern -> ControlPattern)
striateBy = _toPat $$ _toTarget T.striateBy

chop :: P (NumberPattern -> ControlPattern -> ControlPattern)
chop = _toPat $$ _toTarget T.chop

loopAt :: P (NumberPattern -> ControlPattern -> ControlPattern)
loopAt = _toPat $$ _toTarget T.loopAt
