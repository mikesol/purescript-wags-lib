module Test.Vec where

import Prelude

import Data.Vec (empty, (+>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Lib.Vec (indexMod)

testVec :: Spec Unit
testVec = describe "Tests vector" do
  it "looks up modulo index" do
    indexMod (true +> false +> empty) 0 `shouldEqual` true
    indexMod (true +> false +> empty) 1 `shouldEqual` false
    indexMod (true +> false +> empty) 2 `shouldEqual` true
    indexMod (true +> false +> empty) 3 `shouldEqual` false
