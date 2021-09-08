module Test.Blip where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Newtype (wrap)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Lib.Blip (makeBlip)

testBlip :: Spec Unit
testBlip = do
  describe "Tests blip" do
    it "Produces a blip only when there is a change from 0 to 1" do
      let
        r0 = makeBlip false

        r1 = (unwrapCofree r0) false

        r2 = (unwrapCofree r1) true

        r3 = (unwrapCofree r2) true

        r4 = (unwrapCofree r3) true

        r5 = (unwrapCofree r4) false

        r6 = (unwrapCofree r5) false

        r7 = (unwrapCofree r6) true
      extract r0 `shouldEqual` wrap false
      extract r1 `shouldEqual` wrap false
      extract r2 `shouldEqual` wrap true
      extract r3 `shouldEqual` wrap false
      extract r4 `shouldEqual` wrap false
      extract r5 `shouldEqual` wrap false
      extract r6 `shouldEqual` wrap false
      extract r7 `shouldEqual` wrap true
    it "Starts blip correctly" do
      let
        r0 = makeBlip true

        r1 = (unwrapCofree r0) false

        r2 = (unwrapCofree r1) true

        r3 = (unwrapCofree r2) true

        r4 = (unwrapCofree r3) true

        r5 = (unwrapCofree r4) false

        r6 = (unwrapCofree r5) false

        r7 = (unwrapCofree r6) true
      extract r0 `shouldEqual` wrap true
      extract r1 `shouldEqual` wrap false
      extract r2 `shouldEqual` wrap true
      extract r3 `shouldEqual` wrap false
      extract r4 `shouldEqual` wrap false
      extract r5 `shouldEqual` wrap false
      extract r6 `shouldEqual` wrap false
      extract r7 `shouldEqual` wrap true
