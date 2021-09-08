module Test.Rate where

import Prelude
import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Newtype (unwrap)
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqualIsh)
import WAGS.Lib.Rate (makeRate)

testRate :: Spec Unit
testRate = do
  describe "Tests rate" do
    it "Produces the correct rate" do
      let
        rate = makeRate { startsAt: 0.0, prevTime: 0.0 }

        r0 = rate { time: 1.0, rate: 1.0 }

        r1 = unwrapCofree r0 { time: 2.1, rate: 1.0 }

        r2 = unwrapCofree r1 { time: 3.1, rate: 2.0 }
      (unwrap $ extract r0) `shouldEqualIsh` 1.0
      (unwrap $ extract r1) `shouldEqualIsh` 2.1
      (unwrap $ extract r2) `shouldEqualIsh` 4.1
