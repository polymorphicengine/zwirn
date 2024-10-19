module Zwirn.Interactive.Prelude.Control where

{-
    Control.hs - import functions for manipulating controlpatterns
    from tidal
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

-- import Zwirn.Interactive.Convert
-- import Zwirn.Interactive.TidalT
-- import Zwirn.Interactive.Transform
-- import Zwirn.Interactive.Types
-- import qualified Prelude as P ()

-- TODO
-- weave, weaveWith, smash, smash'

-- control pattern stuff

-- pN :: P (TextPattern -> NumberPattern -> ControlPattern)
-- pN = _toPat $$ _toTarget (T.tParam T.pF)

-- pS :: P (TextPattern -> TextPattern -> ControlPattern)
-- pS = _toPat $$ _toTarget (T.tParam T.pS)

-- pB :: P (TextPattern -> NumberPattern -> ControlPattern)
-- pB = _toPat $$ _toTarget (T.tParam T.pB)

-- -- functions

-- (#) :: P (ControlPattern -> ControlPattern -> ControlPattern)
-- (#) = _toPat (T.#)

-- spin :: P (NumberPattern -> ControlPattern -> ControlPattern)
-- spin = _toPat $$ _toTarget T.spin

-- gap :: P (NumberPattern -> ControlPattern -> ControlPattern)
-- gap = _toPat $$ _toTarget T.gap

-- slice :: P (NumberPattern -> NumberPattern -> ControlPattern -> ControlPattern)
-- slice = _toPat $$ _toTarget T.slice

-- randslice :: P (NumberPattern -> ControlPattern -> ControlPattern)
-- randslice = _toPat $$ _toTarget T.randslice

-- splice :: P (NumberPattern -> NumberPattern -> ControlPattern -> ControlPattern)
-- splice = _toPat $$ _toTarget T.splice

-- striate :: P (NumberPattern -> ControlPattern -> ControlPattern)
-- striate = _toPat $$ _toTarget T.striate

-- chew :: P (NumberPattern -> NumberPattern -> ControlPattern -> ControlPattern)
-- chew = _toPat $$ _toTarget T.chew

-- striateBy :: P (NumberPattern -> NumberPattern -> ControlPattern -> ControlPattern)
-- striateBy = _toPat $$ _toTarget T.striateBy

-- chop :: P (NumberPattern -> ControlPattern -> ControlPattern)
-- chop = _toPat $$ _toTarget T.chop

-- loopAt :: P (NumberPattern -> ControlPattern -> ControlPattern)
-- loopAt = _toPat $$ _toTarget T.loopAt

-- interlace :: P (ControlPattern -> ControlPattern -> ControlPattern)
-- interlace = _toPat T.interlace

-- hurry :: P (NumberPattern -> ControlPattern -> ControlPattern)
-- hurry = _toPat $$ _toTarget T.hurry

-- echo :: P (NumberPattern -> NumberPattern -> NumberPattern -> ControlPattern -> ControlPattern)
-- echo = _toPat $$ _toTarget T.echo

-- echoWith :: Pat a => P (NumberPattern -> NumberPattern -> (Pattern a -> Pattern a) -> Pattern a -> Pattern a)
-- echoWith = _toPat $$ (\x y -> T.echoWith (_fromTarget x) (_fromTarget y))

-- splat :: P (NumberPattern -> ControlPattern -> ControlPattern -> ControlPattern)
-- splat = _toPat $$ _toTarget T.splat

-- jux :: P ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)
-- jux = _toPat T.jux

-- juxcut :: P ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)
-- juxcut = _toPat T.juxcut

-- juxBy :: P (NumberPattern -> (ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern)
-- juxBy = _toPat (\x -> T.juxBy (_fromTarget x))

-- contrast :: P ((ControlPattern -> ControlPattern) -> (ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern -> ControlPattern)
-- contrast = _toPat T.contrast

-- fix :: P ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern -> ControlPattern)
-- fix = _toPat T.fix

-- unfix :: P ((ControlPattern -> ControlPattern) -> ControlPattern -> ControlPattern -> ControlPattern)
-- unfix = _toPat T.unfix

-- grain :: P (NumberPattern -> NumberPattern -> ControlPattern)
-- grain = _toPat $$ _toTarget T.grain

-- recv :: P ((NumberPattern -> ControlPattern) -> NumberPattern -> ControlPattern)
-- recv = _toPat _recv

-- send :: P (NumberPattern -> NumberPattern -> ControlPattern)
-- send = _toPat _send
