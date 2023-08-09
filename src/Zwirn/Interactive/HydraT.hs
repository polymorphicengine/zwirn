module Zwirn.Interactive.HydraT where

import qualified Prelude as P
import Zwirn.Interactive.Types
import Zwirn.Interactive.TidalT

_insert :: String -> [String] -> String
_insert code args = go code
    where
    at xs i = xs P.!! i
    argument i = (args `at` i)

    go []           = []
    go ('%':'%':cs) = '%' : go cs
    go ('%':c  :cs) = argument index P.++ go cs
        where index = P.fromEnum c P.- P.fromEnum '1'
    go (c:cs)       = c : go cs

-- sources

_gradient :: Pattern Double -> Pattern String
_gradient = _lift $$ \i -> _insert "gradient(%1)" [P.show i]

_noise :: Pattern Double -> Pattern String
_noise = _lift $$ \i -> _insert "noise(%1)" [P.show i]

_osc :: Pattern Double -> Pattern String
_osc = _lift $$ \i -> _insert "osc(%1)" [P.show i]

_out :: Pattern Int -> Pattern String -> Pattern String
_out = _lift2 $$ (\i o -> _insert "%2.out(%1)" [P.show i, o])

_shape :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String
_shape = _lift3 $$ \i j k -> _insert "shape(%1,%2,%3)" [P.show i, P.show j, P.show k]

_solid :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String
_solid = _lift3 $$ \i j k -> _insert "solid(%1,%2,%3)" [P.show i, P.show j, P.show k]

_src :: Pattern String -> Pattern String
_src = _lift $$ \i -> _insert "src(%1)" [i]

_voronoi :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String
_voronoi = _lift3 $$ \i j k -> _insert "voronoi(%1,%2,%3)" [P.show i, P.show j, P.show k]

-- operators

_add :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_add = _lift3 $$ (\i o1 o2 -> _insert "%2.add(%3,%1)" [P.show i, o1, o2])

_mult :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_mult = _lift3 $$ (\i o1 o2 -> _insert "%2.mult(%3,%1)" [P.show i, o1, o2])

_blend :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_blend = _lift3 $$ (\i o1 o2 -> _insert "%2.blend(%3,%1)" [P.show i, o1, o2])

_diff :: Pattern String -> Pattern String -> Pattern String
_diff = _lift2 $$ (\o1 o2 -> _insert "%1.diff(%2)" [o1, o2])

_hlayer :: Pattern String -> Pattern String -> Pattern String
_hlayer = _lift2 $$ (\o1 o2 -> _insert "%1.layer(%2)" [o1, o2])

_hmask :: Pattern String -> Pattern String -> Pattern String
_hmask = _lift2 $$ (\o1 o2 -> _insert "%1.mask(%2)" [o1, o2])

-- modulators

_modulate :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulate = _lift3 $$ (\i o1 o2 -> _insert "%2.modulate(%3,%1)" [P.show i, o1, o2])

_modulateHue :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulateHue = _lift3 $$ (\i o1 o2 -> _insert "%2.modulateHue(%3,%1)" [P.show i, o1, o2])

_modulateKaleid :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulateKaleid = _lift3 $$ (\i o1 o2 -> _insert "%2.modulateKaleid(%3,%1)" [P.show i, o1, o2])

-- modifiers

_color :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String -> Pattern String
_color = _lift4 $$ \i j k o -> _insert "%4.color(%1,%2,%3)" [P.show i, P.show j, P.show k, o]
