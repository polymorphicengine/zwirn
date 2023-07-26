module Zwirn.Interactive.Prelude.Hydra where

import qualified Prelude as P
-- import qualified Sound.Tidal.Context as T
import Zwirn.Interactive.Meta

-- sources

gradientT :: Pattern Double -> Pattern String
gradientT = lift $$ \i -> insert "gradient(%1)" [P.show i]

gradient :: P (Pattern Number -> Pattern String)
gradient = toPat $$ toNum gradientT

noiseT :: Pattern Double -> Pattern String
noiseT = lift $$ \i -> insert "noise(%1)" [P.show i]

noise :: P (Pattern Number -> Pattern String)
noise = toPat $$ toNum noiseT

oscT :: Pattern Double -> Pattern String
oscT = lift $$ \i -> insert "osc(%1)" [P.show i]

osc :: P (Pattern Number -> Pattern String)
osc = toPat $$ toNum oscT

outT :: Pattern Int -> Pattern String -> Pattern String
outT = lift2 $$ (\i o -> insert "%2.out(%1)" [P.show i, o])

out :: P (Pattern Number -> Pattern String -> Pattern String)
out = toPat $$ toNum outT

shapeT :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String
shapeT = lift3 $$ \i j k -> insert "shape(%1,%2,%3)" [P.show i, P.show j, P.show k]

hshape :: P (Pattern Number -> Pattern Number -> Pattern Number -> Pattern String)
hshape = toPat $$ toNum shapeT

solidT :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String
solidT = lift3 $$ \i j k -> insert "solid(%1,%2,%3)" [P.show i, P.show j, P.show k]

solid :: P (Pattern Number -> Pattern Number -> Pattern Number -> Pattern String)
solid = toPat $$ toNum solidT

srcT :: Pattern String -> Pattern String
srcT = lift $$ \i -> insert "src(%1)" [i]

src :: P (Pattern String -> Pattern String)
src = toPat $$ toNum srcT

voronoiT :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String
voronoiT = lift3 $$ \i j k -> insert "voronoi(%1,%2,%3)" [P.show i, P.show j, P.show k]

voronoi :: P (Pattern Number -> Pattern Number -> Pattern Number -> Pattern String)
voronoi = toPat $$ toNum voronoiT

-- operators

addT :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
addT = lift3 $$ (\i o1 o2 -> insert "%2.add(%3,%1)" [P.show i, o1, o2])

add :: P (Pattern Number -> Pattern String -> Pattern String -> Pattern String)
add = toPat $$ toNum addT

multT :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
multT = lift3 $$ (\i o1 o2 -> insert "%2.mult(%3,%1)" [P.show i, o1, o2])

mult :: P (Pattern Number -> Pattern String -> Pattern String -> Pattern String)
mult = toPat $$ toNum multT

blendT :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
blendT = lift3 $$ (\i o1 o2 -> insert "%2.blend(%3,%1)" [P.show i, o1, o2])

blend :: P (Pattern Number -> Pattern String -> Pattern String -> Pattern String)
blend = toPat $$ toNum blendT

diffT :: Pattern String -> Pattern String -> Pattern String
diffT = lift2 $$ (\o1 o2 -> insert "%1.diff(%2)" [o1, o2])

diff :: P (Pattern String -> Pattern String -> Pattern String)
diff = toPat $$ toNum diffT

hlayerT :: Pattern String -> Pattern String -> Pattern String
hlayerT = lift2 $$ (\o1 o2 -> insert "%1.layer(%2)" [o1, o2])

hlayer :: P (Pattern String -> Pattern String -> Pattern String)
hlayer = toPat $$ toNum hlayerT

hmaskT :: Pattern String -> Pattern String -> Pattern String
hmaskT = lift2 $$ (\o1 o2 -> insert "%1.mask(%2)" [o1, o2])

hmask :: P (Pattern String -> Pattern String -> Pattern String)
hmask = toPat $$ toNum hmaskT

-- modulators

modulateT :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
modulateT = lift3 $$ (\i o1 o2 -> insert "%2.modulate(%3,%1)" [P.show i, o1, o2])

modulate :: P (Pattern Number -> Pattern String -> Pattern String -> Pattern String)
modulate = toPat $$ toNum modulateT

modulateHueT :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
modulateHueT = lift3 $$ (\i o1 o2 -> insert "%2.modulateHue(%3,%1)" [P.show i, o1, o2])

modulateHue :: P (Pattern Number -> Pattern String -> Pattern String -> Pattern String)
modulateHue = toPat $$ toNum modulateHueT

modulateKaleidT :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
modulateKaleidT = lift3 $$ (\i o1 o2 -> insert "%2.modulateKaleid(%3,%1)" [P.show i, o1, o2])

modulateKaleid :: P (Pattern Number -> Pattern String -> Pattern String -> Pattern String)
modulateKaleid = toPat $$ toNum modulateKaleidT

-- modifiers

colorT :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String -> Pattern String
colorT = lift4 $$ \i j k o -> insert "%4.color(%1,%2,%3)" [P.show i, P.show j, P.show k, o]

color :: P (Pattern Number -> Pattern Number -> Pattern Number -> Pattern String -> Pattern String)
color = toPat $$ toNum colorT
