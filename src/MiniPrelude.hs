module MiniPrelude where

import qualified Prelude as P
import Functional

id :: Mini (Mini a -> Mini a)
id = FVal (\x -> x)

const :: Mini (Mini a -> Mini (Mini b -> Mini a))
const = FVal (\x -> FVal (\_ -> x))
