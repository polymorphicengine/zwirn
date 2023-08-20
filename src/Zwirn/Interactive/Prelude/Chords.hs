{-# LANGUAGE TemplateHaskell #-}
module Zwirn.Interactive.Prelude.Chords where

import qualified Prelude as P ()
import qualified Sound.Tidal.Context as T hiding (fromList)

import Zwirn.Interactive.Types
import Zwirn.Interactive.Transform
import Zwirn.Interactive.Convert
import Zwirn.Interactive.Generic
import Zwirn.Interactive.TidalT

$(_mkChords _chords)
$(_mkNotes _notes)

scale :: P (TextPattern -> NumberPattern -> NumberPattern)
scale = _toPat (\x n -> T.scale (_fromTarget x) (_fromTarget n))

i :: P (NumberPattern -> NumberPattern)
i = _toPat _invertChord

open :: P (NumberPattern -> NumberPattern)
open = _toPat _openChord

expand :: P (NumberPattern -> NumberPattern -> NumberPattern)
expand = _toPat _expandChord

drop :: P (NumberPattern -> NumberPattern -> NumberPattern)
drop = _toPat _dropChord
