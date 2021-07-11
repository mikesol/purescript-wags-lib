module Test.Impulse where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Newtype (unwrap)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Lib.Impulse (makeImpulse)

testImpulse :: Spec Unit
testImpulse = do
  describe "Tests impulse" do
    it "Produces the correct impulse" do
      let
        r0 = unwrap $ unwrap makeImpulse
        r1 = unwrap (tail r0)
        r2 = unwrap (tail r1)
      head r0 `shouldEqual` true
      head r1 `shouldEqual` false
      head r2 `shouldEqual` false
