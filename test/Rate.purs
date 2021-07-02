module Test.Rate where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Lib.Rate (makeRate)

testRate :: Spec Unit
testRate = describe "Tests rate" do
  it "Produces the correct rate" do
    let
      rate = makeRate 0.0
      r0 = rate { time: 1.0, rate: 1.0 }
      r1 = tail r0 { time: 2.1, rate: 1.0 }
      r2 = tail r1 { time: 3.1, rate: 2.0 }
    head r0 `shouldEqual` 1.0
    head r1 `shouldEqual` 2.1
    head r2 `shouldEqual` 4.1