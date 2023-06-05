{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Prelude.Params where

import qualified Prelude as P ()
import Generic (mkNumParams, mkStringParams)
import qualified Sound.Tidal.Context as T hiding (fromList)
import Prelude.Control

$(mkNumParams numParams)

$(mkStringParams stringParams)
