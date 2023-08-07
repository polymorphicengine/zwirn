module Zwirn.Interactive.Prelude.Hydra where

import qualified Prelude as P ()

import Zwirn.Interactive.Types
import Zwirn.Interactive.Transform
import Zwirn.Interactive.Convert
import Zwirn.Interactive.HydraT

-- sources

gradient :: P (NumberPattern -> TextPattern)
gradient = toPat $$ toTarget gradientT

noise :: P (NumberPattern -> TextPattern)
noise = toPat $$ toTarget noiseT

osc :: P (NumberPattern -> TextPattern)
osc = toPat $$ toTarget oscT

out :: P (NumberPattern -> TextPattern -> TextPattern)
out = toPat $$ toTarget outT

hshape :: P (NumberPattern -> NumberPattern -> NumberPattern -> TextPattern)
hshape = toPat $$ toTarget shapeT

solid :: P (NumberPattern -> NumberPattern -> NumberPattern -> TextPattern)
solid = toPat $$ toTarget solidT

src :: P (TextPattern -> TextPattern)
src = toPat $$ toTarget srcT

voronoi :: P (NumberPattern -> NumberPattern -> NumberPattern -> TextPattern)
voronoi = toPat $$ toTarget voronoiT

-- operators

add :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
add = toPat $$ toTarget addT

mult :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
mult = toPat $$ toTarget multT

blend :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
blend = toPat $$ toTarget blendT

diff :: P (TextPattern -> TextPattern -> TextPattern)
diff = toPat $$ toTarget diffT

hlayer :: P (TextPattern -> TextPattern -> TextPattern)
hlayer = toPat $$ toTarget hlayerT

hmask :: P (TextPattern -> TextPattern -> TextPattern)
hmask = toPat $$ toTarget hmaskT

-- modulators

modulate :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulate = toPat $$ toTarget modulateT

modulateHue :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulateHue = toPat $$ toTarget modulateHueT

modulateKaleid :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulateKaleid = toPat $$ toTarget modulateKaleidT

-- modifiers

color :: P (NumberPattern -> NumberPattern -> NumberPattern -> TextPattern -> TextPattern)
color = toPat $$ toTarget colorT
