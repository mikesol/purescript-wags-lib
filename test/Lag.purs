module Test.Lag where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (deferCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Lib.Blip (makeBlip)
import WAGS.Lib.Lag (makeLag, withDeferredLag, withLag)

testLag :: Spec Unit
testLag = do
  describe "Tests lag" do
    it "Produces the correct lag" do
      let
        lag = makeLag
        r0 = lag 0
        r1 = unwrapCofree r0 1
        r2 = unwrapCofree r1 2
      extract r0 `shouldEqual` Left 0
      extract r1 `shouldEqual` Right (Tuple 0 1)
      extract r2 `shouldEqual` Right (Tuple 1 2)
    it "Produces the correct withLag" do
      let
        r0 = withLag makeBlip false
        r1 = (unwrapCofree r0) false
        r2 = (unwrapCofree r1) true
        r3 = (unwrapCofree r2) true
        r4 = (unwrapCofree r3) true
        r5 = (unwrapCofree r4) false
        r6 = (unwrapCofree r5) false
        r7 = (unwrapCofree r6) true
      extract r0 `shouldEqual` (Left $ wrap false)
      extract r1 `shouldEqual` (Right $ wrap false /\ wrap false)
      extract r2 `shouldEqual` (Right $ wrap false /\ wrap true)
      extract r3 `shouldEqual` (Right $ wrap true /\ wrap false)
      extract r4 `shouldEqual` (Right $ wrap false /\ wrap false)
      extract r5 `shouldEqual` (Right $ wrap false /\ wrap false)
      extract r6 `shouldEqual` (Right $ wrap false /\ wrap false)
      extract r7 `shouldEqual` (Right $ wrap false /\ wrap true)
    it "Produces the correct withDeferredLag" do
      let
        r0 = unwrap $ withDeferredLag (let x n = deferCofree \_ -> Tuple n $ Identity (x (n + 1)) in Identity $ x 0)
        r1 = unwrap $ unwrapCofree r0
        r2 = unwrap $ unwrapCofree r1
        r3 = unwrap $ unwrapCofree r2
        r4 = unwrap $ unwrapCofree r3
        r5 = unwrap $ unwrapCofree r4
        r6 = unwrap $ unwrapCofree r5
        r7 = unwrap $ unwrapCofree r6
      extract r0 `shouldEqual` (Left $ 0)
      extract r1 `shouldEqual` (Right $ 0 /\ 1)
      extract r2 `shouldEqual` (Right $ 1 /\ 2)
      extract r3 `shouldEqual` (Right $ 2 /\ 3)
      extract r4 `shouldEqual` (Right $ 3 /\ 4)
      extract r5 `shouldEqual` (Right $ 4 /\ 5)
      extract r6 `shouldEqual` (Right $ 5 /\ 6)
      extract r7 `shouldEqual` (Right $ 6 /\ 7)