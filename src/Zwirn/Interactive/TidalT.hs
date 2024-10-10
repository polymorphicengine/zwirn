module Zwirn.Interactive.TidalT where

{-
    TidalT.hs - provides some non standard tidal functions
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

import qualified Data.Text as Text
import qualified Sound.Zwirn.Pattern as Z
import Zwirn.Interactive.Types
import Zwirn.Interactive.Convert
import qualified Prelude as P

-- the following functions are needed for converting the AST to a haskell expression

-- SNum
_numPat :: Double -> Pattern Number
_numPat d = P.pure (Num d)

-- SText
_textPat :: String -> Pattern Text
_textPat s = P.pure (Text (Text.pack s))

-- SLambda
_pat :: a -> Pattern a
_pat = P.pure

-- SApp
_apply :: Pattern (Pattern a -> Pattern b) -> Pattern a -> Pattern b
_apply fp p = Z.innerJoin $$ P.fmap (\f -> f p) fp

_euclid :: NumberPattern -> NumberPattern -> Pattern a -> Pattern a
_euclid a b = Z.euclid (_fromTarget a) (_fromTarget b)
