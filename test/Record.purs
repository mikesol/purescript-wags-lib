module Test.Record where

import Prelude

import Data.Const (Const(..))
import Data.Identity (Identity(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Lib.Record (applyRecord, unwrapRecord)

testRecord :: Spec Unit
testRecord = do
  describe "Tests record" do
    it "Produces the correct record with unwrap record" do
      let unwrapped = unwrapRecord { a: Identity 1, b: Const "b", c: { a: Identity 1, b: Identity "b" } }
      unwrapped.a `shouldEqual` 1
      unwrapped.b `shouldEqual` "b"
      unwrapped.c `shouldEqual` { a: 1, b: "b" }
    it "Produces the correct record with apply record" do
      let applied = applyRecord { a: add 1, b: append "a", c: { a: add 3, b: append "q" } } { a: 1, b: "b", c: { a: 1, b: "b" } }
      applied.a `shouldEqual` 2
      applied.b `shouldEqual` "ab"
      applied.c `shouldEqual` { a: 4, b: "qb" }