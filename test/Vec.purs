module Test.Vec where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (toInt)
import Data.Vec (empty, (+>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Lib.Vec (mapWithTypedIndex)

testMapWithTypedIndex :: Spec Unit
testMapWithTypedIndex = describe "mapWithTypedIndex" do
  it "works" do
    mapWithTypedIndex (\i a -> Tuple (toInt i) a) (true +> true +> false +> empty) `shouldEqual` (Tuple 0 true +> Tuple 1 true +> Tuple 2 false +> empty)
