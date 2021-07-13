module Test.Trigger where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Newtype (unwrap)
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqualIsh)
import WAGS.Lib.Trigger (makeSnappyTrigger)

testTrigger :: Spec Unit
testTrigger = do
  describe "Tests trigger" do
    it "Produces the correct time for a snappy trigger" do
      let
        r0 = (unwrap makeSnappyTrigger) { time: 0.0, headroom: 0.02, freq: 1.0 }

        r1 = unwrap (unwrapCofree r0) { time: 0.1, freq: 1.0, headroom: 0.02 }

        r2 = unwrap (unwrapCofree r1) { time: 0.95, freq: 1.0, headroom: 0.06 }
        r3 = unwrap (unwrapCofree r2) { time: 1.4, freq: 1.0, headroom: 0.06 }
      extract r0 `shouldEqualIsh` (pure 0.0)
      extract r1 `shouldEqualIsh` (pure 0.1)
      extract r2 `shouldEqualIsh` (pure (-0.05))
      extract r3 `shouldEqualIsh` (pure (0.4))
      pure unit
    it "Produces the correct time for a 2x speed snappy trigger" do
      let
        r0 = (unwrap makeSnappyTrigger) { time: 0.0, headroom: 0.02, freq: 2.0 }

        r1 = unwrap (unwrapCofree r0) { time: 0.1, freq: 2.0, headroom: 0.02 }

        r2 = unwrap (unwrapCofree r1) { time: 0.48, freq: 2.0, headroom: 0.03 }

        r3 = unwrap (unwrapCofree r2) { time: 0.78, freq: 2.0, headroom: 0.03 }

      extract r0 `shouldEqualIsh` (pure 0.0)
      extract r1 `shouldEqualIsh` (pure 0.1)
      extract r2 `shouldEqualIsh` (pure (-0.02))
      extract r3 `shouldEqualIsh` (pure 0.28)
