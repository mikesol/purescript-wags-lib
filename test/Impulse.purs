module Test.Impulse where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Newtype (unwrap)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Lib.Impulse (blip, impulse)

testImpulse :: Spec Unit
testImpulse = do
  describe "Tests impulse" do
    it "Produces the correct impulse" do
      let
        r0 = impulse
        r1 = unwrap (tail r0)
        r2 = unwrap (tail r1)
      head r0 `shouldEqual` true
      head r1 `shouldEqual` false
      head r2 `shouldEqual` false
  describe "Tests blip" do
    it "Produces a blip only when there is a change from 0 to 1" do
      let
        r0 = blip false
        r1 = tail r0 false
        r2 = tail r1 true
        r3 = tail r2 true
        r4 = tail r3 true
        r5 = tail r4 false
        r6 = tail r5 false
        r7 = tail r6 true
      head r0 `shouldEqual` false
      head r1 `shouldEqual` false
      head r2 `shouldEqual` true
      head r3 `shouldEqual` false
      head r4 `shouldEqual` false
      head r5 `shouldEqual` false
      head r6 `shouldEqual` false
      head r7 `shouldEqual` true
