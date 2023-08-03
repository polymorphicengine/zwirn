module Zwirn.Interactive.Prelude.Hydra where

import qualified Prelude as P ()

import Zwirn.Interactive.Types
import Zwirn.Interactive.Transform
import Zwirn.Interactive.Convert
import Zwirn.Interactive.HydraT

-- sources

gradient :: P (Pattern Number -> Pattern Text)
gradient = toPat $$ toTarget gradientT

noise :: P (Pattern Number -> Pattern Text)
noise = toPat $$ toTarget noiseT

osc :: P (Pattern Number -> Pattern Text)
osc = toPat $$ toTarget oscT

out :: P (Pattern Number -> Pattern Text -> Pattern Text)
out = toPat $$ toTarget outT

hshape :: P (Pattern Number -> Pattern Number -> Pattern Number -> Pattern Text)
hshape = toPat $$ toTarget shapeT

solid :: P (Pattern Number -> Pattern Number -> Pattern Number -> Pattern Text)
solid = toPat $$ toTarget solidT

src :: P (Pattern Text -> Pattern Text)
src = toPat $$ toTarget srcT

voronoi :: P (Pattern Number -> Pattern Number -> Pattern Number -> Pattern Text)
voronoi = toPat $$ toTarget voronoiT

-- operators

add :: P (Pattern Number -> Pattern Text -> Pattern Text -> Pattern Text)
add = toPat $$ toTarget addT

mult :: P (Pattern Number -> Pattern Text -> Pattern Text -> Pattern Text)
mult = toPat $$ toTarget multT

blend :: P (Pattern Number -> Pattern Text -> Pattern Text -> Pattern Text)
blend = toPat $$ toTarget blendT

diff :: P (Pattern Text -> Pattern Text -> Pattern Text)
diff = toPat $$ toTarget diffT

hlayer :: P (Pattern Text -> Pattern Text -> Pattern Text)
hlayer = toPat $$ toTarget hlayerT

hmask :: P (Pattern Text -> Pattern Text -> Pattern Text)
hmask = toPat $$ toTarget hmaskT

-- modulators

modulate :: P (Pattern Number -> Pattern Text -> Pattern Text -> Pattern Text)
modulate = toPat $$ toTarget modulateT

modulateHue :: P (Pattern Number -> Pattern Text -> Pattern Text -> Pattern Text)
modulateHue = toPat $$ toTarget modulateHueT

modulateKaleid :: P (Pattern Number -> Pattern Text -> Pattern Text -> Pattern Text)
modulateKaleid = toPat $$ toTarget modulateKaleidT

-- modifiers

color :: P (Pattern Number -> Pattern Number -> Pattern Number -> Pattern Text -> Pattern Text)
color = toPat $$ toTarget colorT
