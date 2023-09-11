module Zwirn.Interactive.Prelude.Hydra where

{-
    Hydra.hs - import functions hydra functions
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

sub :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
sub = _toPat $$ _toTarget _sub

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

modulateRepeat :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulateRepeat = _toPat $$ _toTarget _modulateRepeat

modulateRepeatX :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulateRepeatX = _toPat $$ _toTarget _modulateRepeatX

modulateRepeatY :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulateRepeatY = _toPat $$ _toTarget _modulateRepeatY

modulateScrollX :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulateScrollX = _toPat $$ _toTarget _modulateScrollX

modulateScale :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulateScale = _toPat $$ _toTarget _modulateScale

modulatePixelate :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulatePixelate = _toPat $$ _toTarget _modulatePixelate

modulateRotate :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulateRotate = _toPat $$ _toTarget _modulateRotate

modulateHue :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulateHue = _toPat $$ _toTarget _modulateHue

modulateKaleid :: P (NumberPattern -> TextPattern -> TextPattern -> TextPattern)
modulateKaleid = _toPat $$ _toTarget _modulateKaleid

-- modifiers

color :: P (NumberPattern -> NumberPattern -> NumberPattern -> TextPattern -> TextPattern)
color = _toPat $$ _toTarget _color

posterize :: P (NumberPattern -> NumberPattern -> TextPattern -> TextPattern)
posterize = _toPat $$ _toTarget _posterize

shift :: P (NumberPattern -> NumberPattern -> NumberPattern -> TextPattern -> TextPattern)
shift = _toPat $$ _toTarget _shift

invert :: P (NumberPattern -> TextPattern -> TextPattern)
invert = _toPat $$ _toTarget _invert

hcontrast :: P (NumberPattern -> TextPattern -> TextPattern)
hcontrast = _toPat $$ _toTarget _contrast

brightness :: P (NumberPattern -> TextPattern -> TextPattern)
brightness = _toPat $$ _toTarget _brightness

luma :: P (NumberPattern -> NumberPattern -> TextPattern -> TextPattern)
luma = _toPat $$ _toTarget _luma

thresh :: P (NumberPattern -> NumberPattern -> TextPattern -> TextPattern)
thresh = _toPat $$ _toTarget _thresh

saturate :: P (NumberPattern -> TextPattern -> TextPattern)
saturate = _toPat $$ _toTarget _saturate

hue :: P (NumberPattern -> TextPattern -> TextPattern)
hue = _toPat $$ _toTarget _hue

colorama :: P (NumberPattern -> TextPattern -> TextPattern)
colorama = _toPat $$ _toTarget _colorama

-- external

initCam :: P (TextPattern -> NumberPattern -> TextPattern)
initCam = _toPat $$ _toTarget _initCam

initImage :: P (TextPattern -> TextPattern -> TextPattern)
initImage = _toPat $$ _toTarget _initImage

initVideo :: P (TextPattern -> TextPattern -> TextPattern)
initVideo = _toPat $$ _toTarget _initVideo

initScreen :: P (TextPattern -> TextPattern)
initScreen = _toPat $$ _toTarget _initScreen
