{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Zwirn.Interactive.Prelude.Params where

import qualified Prelude as P ()
import Zwirn.Interactive.Generic (mkNumParams, mkStringParams)
import qualified Sound.Tidal.Context as T hiding (fromList)
import Zwirn.Interactive.Prelude.Control

$(mkNumParams numParams)

$(mkStringParams stringParams)