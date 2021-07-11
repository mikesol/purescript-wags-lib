module Test.Rate where

import Prelude
import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Newtype (unwrap, wrap)
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqualIsh)
import WAGS.Lib.Emitter (makeEmitter)
import WAGS.Lib.Rate (makeRate)

testRate :: Spec Unit
testRate = do
  describe "Tests rate" do
    it "Produces the correct rate" do
      let
        rate = unwrap $ makeRate { startsAt: 0.0, prevTime: 0.0 }

        r0 = rate { time: 1.0, rate: 1.0 }

        r1 = unwrap (unwrapCofree r0) { time: 2.1, rate: 1.0 }

        r2 = unwrap (unwrapCofree r1) { time: 3.1, rate: 2.0 }
      extract r0 `shouldEqualIsh` 1.0
      extract r1 `shouldEqualIsh` 2.1
      extract r2 `shouldEqualIsh` 4.1
  describe "Tests emitter" do
    it "Produces the correct emissions for a simple emitter" do
      let
        rate = unwrap $ makeEmitter { startsAt: 0.0, prevTime: 0.0 }

        r0 = rate { time: 0.04, rate: 1.0, headroom: 0.02 }

        r1 = unwrap (unwrapCofree r0) { time: 0.1, rate: 1.0, headroom: 0.02 }

        r2 = unwrap (unwrapCofree r1) { time: 0.95, rate: 1.0, headroom: 0.06 }
      extract r0 `shouldEqualIsh` (wrap [ 0.0 ])
      extract r1 `shouldEqualIsh` (wrap [])
      extract r2 `shouldEqualIsh` (wrap [ 0.05 ])
    it "Produces the correct emissions for a 2x speed emitter" do
      let
        rate = unwrap $ makeEmitter { startsAt: 0.0, prevTime: 0.0 }

        r0 = rate { time: 0.04, rate: 2.0, headroom: 0.02 }

        r1 = unwrap (unwrapCofree r0) { time: 0.1, rate: 2.0, headroom: 0.02 }

        r2 = unwrap (unwrapCofree r1) { time: 0.48, rate: 2.0, headroom: 0.06 }

        r3 = unwrap (unwrapCofree r2) { time: 0.49, rate: 2.0, headroom: 0.03 }

        r4 = unwrap (unwrapCofree r3) { time: 0.78, rate: 2.0, headroom: 0.03 }

        r5 = unwrap (unwrapCofree r4) { time: 0.99, rate: 2.0, headroom: 0.03 }
      extract r0 `shouldEqualIsh` (wrap [ 0.0 ])
      extract r1 `shouldEqualIsh` (wrap [])
      extract r2 `shouldEqualIsh` (wrap [ 0.02 ])
      extract r3 `shouldEqualIsh` (wrap [])
      extract r4 `shouldEqualIsh` (wrap [ 0.01 ])
