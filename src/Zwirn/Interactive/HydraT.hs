module Zwirn.Interactive.HydraT where

{-
    HydraT.hs - defines all hydra functions as patterns that
    contain javascript (hydra) code
    Copyright (C) 2023, Martin Gius

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

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

_sub :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_sub = _lift3 $$ (\i o1 o2 -> _insert "%2.sub(%3,%1)" [P.show i, o1, o2])

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

_modulateScale :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulateScale = _lift3 $$ (\i o1 o2 -> _insert "%2.modulateScale(%3,%1)" [P.show i, o1, o2])

_modulateRepeat :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulateRepeat = _lift3 $$ (\i o1 o2 -> _insert "%2.modulateRepeat(%3,%1)" [P.show i, o1, o2])

_modulateRepeatX :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulateRepeatX = _lift3 $$ (\i o1 o2 -> _insert "%2.modulateRepeatX(%3,%1)" [P.show i, o1, o2])

_modulateRepeatY :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulateRepeatY = _lift3 $$ (\i o1 o2 -> _insert "%2.modulateRepeatY(%3,%1)" [P.show i, o1, o2])

_modulateScrollX :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulateScrollX = _lift3 $$ (\i o1 o2 -> _insert "%2.modulateScrollX(%3,%1)" [P.show i, o1, o2])

_modulateScrollY :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulateScrollY = _lift3 $$ (\i o1 o2 -> _insert "%2.modulateScrollY(%3,%1)" [P.show i, o1, o2])

_modulatePixelate :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulatePixelate = _lift3 $$ (\i o1 o2 -> _insert "%2.modulatePixelate(%3,%1)" [P.show i, o1, o2])

_modulateRotate :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulateRotate = _lift3 $$ (\i o1 o2 -> _insert "%2.modulateRotate(%3,%1)" [P.show i, o1, o2])

_modulateHue :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulateHue = _lift3 $$ (\i o1 o2 -> _insert "%2.modulateHue(%3,%1)" [P.show i, o1, o2])

_modulateKaleid :: Pattern Double -> Pattern String -> Pattern String -> Pattern String
_modulateKaleid = _lift3 $$ (\i o1 o2 -> _insert "%2.modulateKaleid(%3,%1)" [P.show i, o1, o2])

-- modifiers

_posterize :: Pattern Double -> Pattern Double -> Pattern String -> Pattern String
_posterize = _lift3 $$ \i j k -> _insert "%3.shift(%1,%2)" [P.show i, P.show j, k]

_shift :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String -> Pattern String
_shift = _lift4 $$ \i j k o -> _insert "%4.shift(%1,%2,%3)" [P.show i, P.show j, P.show k, o]

_invert :: Pattern Double -> Pattern String -> Pattern String
_invert = _lift2 $$ \i j -> _insert "%2.invert(%1)" [P.show i, j]

_contrast :: Pattern Double -> Pattern String -> Pattern String
_contrast = _lift2 $$ \i j -> _insert "%2.contrast(%1)" [P.show i, j]

_brightness :: Pattern Double -> Pattern String -> Pattern String
_brightness = _lift2 $$ \i j -> _insert "%2.brightness(%1)" [P.show i, j]

_luma :: Pattern Double -> Pattern Double -> Pattern String -> Pattern String
_luma = _lift3 $$ \i j k -> _insert "%3.luma(%1,%2)" [P.show i, P.show j, k]

_thresh :: Pattern Double -> Pattern Double -> Pattern String -> Pattern String
_thresh = _lift3 $$ \i j k -> _insert "%3.thresh(%1,%2)" [P.show i, P.show j, k]

_color :: Pattern Double -> Pattern Double -> Pattern Double -> Pattern String -> Pattern String
_color = _lift4 $$ \i j k o -> _insert "%4.color(%1,%2,%3)" [P.show i, P.show j, P.show k, o]

_saturate :: Pattern Double -> Pattern String -> Pattern String
_saturate = _lift2 $$ \i j -> _insert "%2.saturate(%1)" [P.show i, j]

_hue :: Pattern Double -> Pattern String -> Pattern String
_hue = _lift2 $$ \i j -> _insert "%2.hue(%1)" [P.show i, j]

_colorama :: Pattern Double -> Pattern String -> Pattern String
_colorama = _lift2 $$ \i j -> _insert "%2.colorama(%1)" [P.show i, j]

-- external

_initCam :: Pattern String -> Pattern Int -> Pattern String
_initCam = _lift2 $$ (\o i -> _insert "%1.initCam(%2)" [o, P.show i])

_initImage :: Pattern String -> Pattern String -> Pattern String
_initImage = _lift2 $$ (\o i -> _insert "%1.initImage(%2)" [o, i])

_initVideo :: Pattern String -> Pattern String -> Pattern String
_initVideo = _lift2 $$ (\o i -> _insert "%1.initVideo(%2)" [o, i])

_initScreen :: Pattern String -> Pattern String
_initScreen = _lift $$ (\o -> _insert "%1.initScreen()" [o])
