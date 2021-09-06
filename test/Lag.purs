module Test.Lag where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Lib.Lag (makeLag)

testLag :: Spec Unit
testLag = do
  describe "Tests lag" do
    it "Produces the correct lag" do
      let
        lag = unwrap $ makeLag

        r0 = lag 0

        r1 = unwrap (unwrapCofree r0) 1

        r2 = unwrap (unwrapCofree r1) 2
      extract r0 `shouldEqual` Left 0
      extract r1 `shouldEqual` Right (Tuple 0 1)
      extract r2 `shouldEqual` Right (Tuple 1 2)