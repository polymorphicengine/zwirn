module Arbitrary where

import Test.QuickCheck hiding (Testable)
import Control.Monad (liftM2)
import Functional (Mini (..))


instance Arbitrary a => Arbitrary (Mini a) where
  arbitrary = sized mini
            where mini 0 = oneof [fmap FVal arbitrary
                                  ,return FRest
                                  ]
                  mini n = oneof [fmap FVal arbitrary
                                 ,return FRest
                                 ,fmap (\x -> FSeq x FEmpty) submini
                                 ,liftM2 FStack submini submini
                                 ,liftM2 FMult submini (fmap FVal arbitrary)
                                 ,liftM2 FDiv submini (fmap FVal arbitrary)
                                 ]
                         where submini = mini (div n 2)


instance CoArbitrary a => CoArbitrary (Mini a)
