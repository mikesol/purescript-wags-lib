module Test.Stream where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.List (List(..), (:))
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Test.Spec (Spec, describe, it)
import WAGS.Lib.Stream (cycle, deadEnd)
import Test.Spec.Assertions (shouldEqual)

testStream :: Spec Unit
testStream = do
  describe "Tests deadEnd" do
    it "Produces the correct stream" do
      let
        r0 = deadEnd (1 :| 2 : 3 : Nil)
        r1 = unwrap $ tail r0 
        r2 = unwrap $ tail r1
        r3 = unwrap $ tail r2
        r4 = unwrap $ tail r3 
      head r0 `shouldEqual` 1
      head r1 `shouldEqual` 2
      head r2 `shouldEqual` 3
      head r3 `shouldEqual` 3
      head r4 `shouldEqual` 3
  describe "Tests cycle" do
    it "Produces the correct stream" do
      let
        r0 = cycle (1 :| 2 : 3 : Nil)
        r1 = unwrap $ tail r0 
        r2 = unwrap $ tail r1
        r3 = unwrap $ tail r2
        r4 = unwrap $ tail r3 
      head r0 `shouldEqual` 1
      head r1 `shouldEqual` 2
      head r2 `shouldEqual` 3
      head r3 `shouldEqual` 1
      head r4 `shouldEqual` 2
