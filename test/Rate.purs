module Test.Rate where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqualIsh)
import WAGS.Lib.Rate (makeRate)
import WAGS.Lib.Emitter (makeEmitter)

testRate :: Spec Unit
testRate = do
  describe "Tests rate" do
    it "Produces the correct rate" do
      let
        rate = makeRate { startsAt: 0.0, prevTime: 0.0 }
        r0 = rate { time: 1.0, rate: 1.0 }
        r1 = tail r0 { time: 2.1, rate: 1.0 }
        r2 = tail r1 { time: 3.1, rate: 2.0 }
      head r0 `shouldEqualIsh` 1.0
      head r1 `shouldEqualIsh` 2.1
      head r2 `shouldEqualIsh` 4.1
  describe "Tests emitter" do
    it "Produces the correct emissions for a simple emitter" do
      let
        rate = makeEmitter { startsAt: 0.0, prevTime: 0.0 }
        r0 = rate { time: 0.04, rate: 1.0, headroom: 0.02 }
        r1 = tail r0 { time: 0.1, rate: 1.0, headroom: 0.02 }
        r2 = tail r1 { time: 0.95, rate: 1.0, headroom: 0.06 }
      head r0 `shouldEqualIsh` [ 0.0 ]
      head r1 `shouldEqualIsh` [ ]
      head r2 `shouldEqualIsh` [ 0.05 ]
    it "Produces the correct emissions for a 2x speed emitter" do
      let
        rate = makeEmitter { startsAt: 0.0, prevTime: 0.0 }
        r0 = rate { time: 0.04, rate: 2.0, headroom: 0.02 }
        r1 = tail r0 { time: 0.1, rate: 2.0, headroom: 0.02 }
        r2 = tail r1 { time: 0.48, rate: 2.0, headroom: 0.06 }
        r3 = tail r2 { time: 0.49, rate: 2.0, headroom: 0.03 }
        r4 = tail r3 { time: 0.78, rate: 2.0, headroom: 0.03 }
        r5 = tail r4 { time: 0.99, rate: 2.0, headroom: 0.03 }
      head r0 `shouldEqualIsh` [ 0.0 ]
      head r1 `shouldEqualIsh` [ ]
      head r2 `shouldEqualIsh` [ 0.02 ]
      head r3 `shouldEqualIsh` [  ]
      head r4 `shouldEqualIsh` [ 0.01 ]
