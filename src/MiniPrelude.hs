module MiniPrelude where

import qualified Prelude as P
import Functional

import qualified Sound.Tidal.Context as T


id :: Pattern a -> Pattern a
id x = x

rev :: Pattern a -> Pattern a
rev = T.rev

const :: Pattern a -> Pattern (Pattern b -> Pattern a)
const x = P.pure (\_ -> x)
