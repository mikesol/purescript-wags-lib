module Test.Emitter where

import Prelude
import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Newtype (unwrap)
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqualIsh)
import WAGS.Lib.Emitter (makeEmitter)

testEmitter :: Spec Unit
testEmitter = do
  describe "Tests emitter" do
    it "Produces the correct emissions for a simple emitter" do
      let
        freq = unwrap $ makeEmitter { startsAt: 0.0, prevTime: 0.0 }

        r0 = freq { time: 0.04, freq: 1.0, headroom: 0.02 }

        r1 = unwrap (unwrapCofree r0) { time: 0.1, freq: 1.0, headroom: 0.02 }

        r2 = unwrap (unwrapCofree r1) { time: 0.95, freq: 1.0, headroom: 0.06 }
      extract r0 `shouldEqualIsh` ([ 0.0 ])
      extract r1 `shouldEqualIsh` ([])
      extract r2 `shouldEqualIsh` ([ 0.05 ])
    it "Produces the correct emissions for a 2x speed emitter" do
      let
        freq = unwrap $ makeEmitter { startsAt: 0.0, prevTime: 0.0 }

        r0 = freq { time: 0.04, freq: 2.0, headroom: 0.02 }

        r1 = unwrap (unwrapCofree r0) { time: 0.1, freq: 2.0, headroom: 0.02 }

        r2 = unwrap (unwrapCofree r1) { time: 0.48, freq: 2.0, headroom: 0.06 }

        r3 = unwrap (unwrapCofree r2) { time: 0.49, freq: 2.0, headroom: 0.03 }

        r4 = unwrap (unwrapCofree r3) { time: 0.78, freq: 2.0, headroom: 0.03 }

        r5 = unwrap (unwrapCofree r4) { time: 0.99, freq: 2.0, headroom: 0.03 }
      extract r0 `shouldEqualIsh` ([ 0.0 ])
      extract r1 `shouldEqualIsh` ([])
      extract r2 `shouldEqualIsh` ([ 0.02 ])
      extract r3 `shouldEqualIsh` ([])
      extract r4 `shouldEqualIsh` ([ 0.01 ])
