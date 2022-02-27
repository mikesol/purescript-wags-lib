module Test.Piecewise where

import Prelude

import Data.List (List(..), (:))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqualIsh)
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.Piecewise (asSingleValue, makeLoopingPiecewise, makePiecewise, makeTerracedL, makeTerracedR)

testPiecewise :: Spec Unit
testPiecewise = do
  describe "Tests piecewise" do
    it "Produces the correct piecewise function" do
      let
        pw = makePiecewise ((0.0 /\ 1.0) :| (0.3 /\ 0.75) : (0.9 /\ 0.0) : Nil)
      pw { time: -10.0, headroomInSeconds: 0.0 } `shouldEqualIsh` (asSingleValue 1.0)
      pw { time: 0.0, headroomInSeconds: 0.0 } `shouldEqualIsh` (asSingleValue 1.0)
      pw { time: 0.1, headroomInSeconds: 0.1 } `shouldEqualIsh` (asSingleValue (1.0 - (0.25 / 3.0)))
      pw { time: 0.2, headroomInSeconds: 0.05 } `shouldEqualIsh` (asSingleValue (1.0 - (0.25 * 2.0 / 3.0)))
      pw { time: 0.2, headroomInSeconds: 0.1 } `shouldEqualIsh` (ff 0.1 $ asSingleValue 0.75)
      pw { time: 10.0, headroomInSeconds: 0.0 } `shouldEqualIsh` (asSingleValue 0.0)
  describe "Tests looping piecewise" do
    it "Produces the correct looping function" do
      let
        pw = makeLoopingPiecewise ((0.0 /\ 1.0) :| (0.3 /\ 0.75) : (1.0 /\ 0.0) : Nil)
      pw { time: 0.0, headroomInSeconds: 0.0 } `shouldEqualIsh` (asSingleValue 1.0)
      pw { time: 0.1, headroomInSeconds: 0.1 } `shouldEqualIsh` (asSingleValue (1.0 - (0.25 / 3.0)))
      pw { time: 0.2, headroomInSeconds: 0.05 } `shouldEqualIsh` (asSingleValue (1.0 - (0.25 * 2.0 / 3.0)))
      pw { time: 0.2, headroomInSeconds: 0.1 } `shouldEqualIsh` (ff 0.1 $ asSingleValue 0.75)
      pw { time: 10.1, headroomInSeconds: 0.1 } `shouldEqualIsh` (asSingleValue (1.0 - (0.25 / 3.0)))
  describe "Produces the correct terraced-left piecewise function" do
    it "Works" do
      let
        pw = makeTerracedL ((0.0 /\ 1.0) :| (0.3 /\ 0.75) : (0.9 /\ 0.0) : Nil)
      pw { time: -10.0, headroomInSeconds: 0.0 } `shouldEqualIsh` (asSingleValue 1.0)
      pw { time: 0.0, headroomInSeconds: 0.0 } `shouldEqualIsh` (asSingleValue 1.0)
      pw { time: 0.1, headroomInSeconds: 0.1 } `shouldEqualIsh` (asSingleValue 1.0)
      pw { time: 0.2, headroomInSeconds: 0.05 } `shouldEqualIsh` (asSingleValue 1.0)
      pw { time: 0.2, headroomInSeconds: 0.1 } `shouldEqualIsh` (ff 0.1 $ asSingleValue 1.0)
      pw { time: 0.4, headroomInSeconds: 0.1 } `shouldEqualIsh` (asSingleValue 0.75)
      pw { time: 10.0, headroomInSeconds: 0.0 } `shouldEqualIsh` (asSingleValue 0.0)
  describe "Produces the correct terraced-right piecewise function" do
    it "Works" do
      let
        pw = makeTerracedR ((0.0 /\ 1.0) :| (0.3 /\ 0.75) : (0.9 /\ 0.0) : Nil)
      pw { time: -10.0, headroomInSeconds: 0.0 } `shouldEqualIsh` (asSingleValue 1.0)
      pw { time: 0.0, headroomInSeconds: 0.0 } `shouldEqualIsh` (asSingleValue 1.0)
      pw { time: 0.1, headroomInSeconds: 0.1 } `shouldEqualIsh` (asSingleValue 1.0)
      pw { time: 0.2, headroomInSeconds: 0.05 } `shouldEqualIsh` (asSingleValue 1.0)
      pw { time: 0.2, headroomInSeconds: 0.1 } `shouldEqualIsh` (ff 0.1 $ asSingleValue 0.75)
      pw { time: 0.4, headroomInSeconds: 0.1 } `shouldEqualIsh` (asSingleValue 0.75)
      pw { time: 10.0, headroomInSeconds: 0.0 } `shouldEqualIsh` (asSingleValue 0.0)
