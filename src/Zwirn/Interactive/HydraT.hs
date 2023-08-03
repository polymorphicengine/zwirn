module Zwirn.Interactive.HydraT where

import qualified Prelude as P
import Zwirn.Interactive.Types
import Zwirn.Interactive.TidalT

insert :: String -> [String] -> String
insert code args = go code
    where
    at xs i = xs P.!! i
    argument i = (args `at` i)

    go []           = []
    go ('%':'%':cs) = '%' : go cs
    go ('%':c  :cs) = argument index P.++ go cs
        where index = P.fromEnum c P.- P.fromEnum '1'
    go (c:cs)       = c : go cs

-- sources

gradientT :: Pattern Double -> Pattern String
gradientT = lift $$ \i -> insert "gradient(%1)" [P.show i]

noiseT :: Pattern Double -> Pattern String
noiseT = lift $$ \i -> insert "noise(%1)" [P.show i]

oscT :: Pattern Double -> Pattern String
oscT = lift $$ \i -> insert "osc(%1)" [P.show i]

outT :: Pattern Int -> Pattern String -> Pattern String
outT = lift2 $$ (\i o -> insert "%2.out(%1)" [P.show i, o])

shapeT :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String
shapeT = lift3 $$ \i j k -> insert "shape(%1,%2,%3)" [P.show i, P.show j, P.show k]

solidT :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String
solidT = lift3 $$ \i j k -> insert "solid(%1,%2,%3)" [P.show i, P.show j, P.show k]

srcT :: Pattern String -> Pattern String
srcT = lift $$ \i -> insert "src(%1)" [i]

voronoiT :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String
voronoiT = lift3 $$ \i j k -> insert "voronoi(%1,%2,%3)" [P.show i, P.show j, P.show k]

-- operators

addT :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
addT = lift3 $$ (\i o1 o2 -> insert "%2.add(%3,%1)" [P.show i, o1, o2])

multT :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
multT = lift3 $$ (\i o1 o2 -> insert "%2.mult(%3,%1)" [P.show i, o1, o2])

blendT :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
blendT = lift3 $$ (\i o1 o2 -> insert "%2.blend(%3,%1)" [P.show i, o1, o2])

diffT :: Pattern String -> Pattern String -> Pattern String
diffT = lift2 $$ (\o1 o2 -> insert "%1.diff(%2)" [o1, o2])

hlayerT :: Pattern String -> Pattern String -> Pattern String
hlayerT = lift2 $$ (\o1 o2 -> insert "%1.layer(%2)" [o1, o2])

hmaskT :: Pattern String -> Pattern String -> Pattern String
hmaskT = lift2 $$ (\o1 o2 -> insert "%1.mask(%2)" [o1, o2])

-- modulators

modulateT :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
modulateT = lift3 $$ (\i o1 o2 -> insert "%2.modulate(%3,%1)" [P.show i, o1, o2])

modulateHueT :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
modulateHueT = lift3 $$ (\i o1 o2 -> insert "%2.modulateHue(%3,%1)" [P.show i, o1, o2])

modulateKaleidT :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
modulateKaleidT = lift3 $$ (\i o1 o2 -> insert "%2.modulateKaleid(%3,%1)" [P.show i, o1, o2])

-- modifiers

colorT :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String -> Pattern String
colorT = lift4 $$ \i j k o -> insert "%4.color(%1,%2,%3)" [P.show i, P.show j, P.show k, o]
