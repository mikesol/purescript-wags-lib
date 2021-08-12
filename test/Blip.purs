module Test.Blip where

import Prelude
import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Newtype (unwrap)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Lib.Blip (makeBlip)

testBlip :: Spec Unit
testBlip = do
  describe "Tests blip" do
    it "Produces a blip only when there is a change from 0 to 1" do
      let
        r0 = unwrap makeBlip false

        r1 = unwrap (unwrapCofree r0) false

        r2 = unwrap (unwrapCofree r1) true

        r3 = unwrap (unwrapCofree r2) true

        r4 = unwrap (unwrapCofree r3) true

        r5 = unwrap (unwrapCofree r4) false

        r6 = unwrap (unwrapCofree r5) false

        r7 = unwrap (unwrapCofree r6) true
      extract r0 `shouldEqual` false
      extract r1 `shouldEqual` false
      extract r2 `shouldEqual` true
      extract r3 `shouldEqual` false
      extract r4 `shouldEqual` false
      extract r5 `shouldEqual` false
      extract r6 `shouldEqual` false
      extract r7 `shouldEqual` true
    it "Starts blip correctly" do
      let
        r0 = unwrap makeBlip true

        r1 = unwrap (unwrapCofree r0) false

        r2 = unwrap (unwrapCofree r1) true

        r3 = unwrap (unwrapCofree r2) true

        r4 = unwrap (unwrapCofree r3) true

        r5 = unwrap (unwrapCofree r4) false

        r6 = unwrap (unwrapCofree r5) false

        r7 = unwrap (unwrapCofree r6) true
      extract r0 `shouldEqual` true
      extract r1 `shouldEqual` false
      extract r2 `shouldEqual` true
      extract r3 `shouldEqual` false
      extract r4 `shouldEqual` false
      extract r5 `shouldEqual` false
      extract r6 `shouldEqual` false
      extract r7 `shouldEqual` true
