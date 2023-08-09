module Zwirn.Interactive.Prelude.Hydra where

import qualified Prelude as P ()

import Zwirn.Interactive.Types
import Zwirn.Interactive.Transform
import Zwirn.Interactive.Convert
import Zwirn.Interactive.HydraT

-- sources

gradient :: P (NumberPattern -> TextPattern)
gradient = _toPat $$ _toTarget _gradient

noise :: P (NumberPattern -> TextPattern)
noise = _toPat $$ _toTarget _noise

osc :: P (NumberPattern -> TextPattern)
osc = _toPat $$ _toTarget _osc

out :: P (NumberPattern -> TextPattern -> TextPattern)
out = _toPat $$ _toTarget _out

hshape :: P (NumberPattern -> NumberPattern -> NumberPattern -> TextPattern)
hshape = _toPat $$ _toTarget _shape

solid :: P (NumberPattern -> NumberPattern -> NumberPattern -> TextPattern)
solid = _toPat $$ _toTarget _solid

src :: P (TextPattern -> TextPattern)
src = _toPat $$ _toTarget _src

voronoi :: P (NumberPattern -> NumberPattern -> NumberPattern -> TextPattern)
voronoi = _toPat $$ _toTarget _voronoi

-- operators

add :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
add = _toPat $$ _toTarget _add

mult :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
mult = _toPat $$ _toTarget _mult

blend :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
blend = _toPat $$ _toTarget _blend

diff :: P (TextPattern -> TextPattern -> TextPattern)
diff = _toPat $$ _toTarget _diff

hlayer :: P (TextPattern -> TextPattern -> TextPattern)
hlayer = _toPat $$ _toTarget _hlayer

hmask :: P (TextPattern -> TextPattern -> TextPattern)
hmask = _toPat $$ _toTarget _hmask

-- modulators

modulate :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulate = _toPat $$ _toTarget _modulate

modulateHue :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulateHue = _toPat $$ _toTarget _modulateHue

modulateKaleid :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulateKaleid = _toPat $$ _toTarget _modulateKaleid

-- modifiers

color :: P (NumberPattern -> NumberPattern -> NumberPattern -> TextPattern -> TextPattern)
color = _toPat $$ _toTarget _color
