module Test.Emitter where

import Prelude
import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqualIsh)
import WAGS.Lib.Emitter (makeEmitter, fEmitter)

testEmitter :: Spec Unit
testEmitter = do
  describe "Tests emitter" do
    it "Produces the correct emissions for a simple emitter" do
      let
        freq = makeEmitter { startsAt: 0.0, prevTime: 0.0 }

        r0 = freq { time: 0.04, freq: 1.0, headroom: 0.02 }

        r1 = unwrapCofree r0 { time: 0.1, freq: 1.0, headroom: 0.02 }

        r2 = unwrapCofree r1 { time: 0.95, freq: 1.0, headroom: 0.06 }
      extract r0 `shouldEqualIsh` ([ 0.0 ])
      extract r1 `shouldEqualIsh` ([])
      extract r2 `shouldEqualIsh` ([ 0.05 ])
    it "Produces the correct emissions for a 2x speed emitter" do
      let
        freq = makeEmitter { startsAt: 0.0, prevTime: 0.0 }

        r0 = freq { time: 0.04, freq: 2.0, headroom: 0.02 }

        r1 = unwrapCofree r0 { time: 0.1, freq: 2.0, headroom: 0.02 }

        r2 = unwrapCofree r1 { time: 0.48, freq: 2.0, headroom: 0.06 }

        r3 = unwrapCofree r2 { time: 0.49, freq: 2.0, headroom: 0.03 }

        r4 = unwrapCofree r3 { time: 0.78, freq: 2.0, headroom: 0.03 }

      extract r0 `shouldEqualIsh` ([ 0.0 ])
      extract r1 `shouldEqualIsh` ([])
      extract r2 `shouldEqualIsh` ([ 0.02 ])
      extract r3 `shouldEqualIsh` ([])
      extract r4 `shouldEqualIsh` ([ 0.01 ])
    it "Produces the correct emissions for a function emitter" do
      let
        r0 = fEmitter 1.0 { time: 0.0, headroom: 0.02 }

        r1 = fEmitter 1.0 { time: 0.1, headroom: 0.02 }

        r2 = fEmitter 1.0 { time: 0.95, headroom: 0.06 }
      r0 `shouldEqualIsh` (Just 0.0)
      r1 `shouldEqualIsh` (Nothing)
      r2 `shouldEqualIsh` (Just 0.05)
    it "Produces the correct emissions for a 2x speed function emitter" do
      let
        r0 = fEmitter 2.0 { time: 0.0, headroom: 0.02 }

        r1 = fEmitter 2.0 { time: 0.1, headroom: 0.02 }

        r2 = fEmitter 2.0 { time: 0.48, headroom: 0.03 }

        r3 = fEmitter 2.0 { time: 0.78, headroom: 0.03 }

      r0 `shouldEqualIsh` (Just 0.0)
      r1 `shouldEqualIsh` (Nothing)
      r2 `shouldEqualIsh` (Just 0.02)
      r3 `shouldEqualIsh` (Nothing)
