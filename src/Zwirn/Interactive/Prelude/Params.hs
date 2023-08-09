{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Zwirn.Interactive.Prelude.Params where

import qualified Prelude as P ()
import Zwirn.Interactive.Generic (_mkNumParams, _mkStringParams, _stringParams, _numParams)
import qualified Sound.Tidal.Context as T hiding (fromList)

$(_mkNumParams _numParams)

$(_mkStringParams _stringParams)
