module Test.Latch where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqualIsh)
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.Latch (makeLatchAP)
import WAGS.Lib.Piecewise (asSingleValue, makeTerracedR)

testLatch :: Spec Unit
testLatch = do
  describe "Tests blip" do
    it "Changes the latched value only when there is a change" do
      let
        pw = makeTerracedR ((0.0 /\ 1.0) :| (1.0 /\ 0.0) : (2.0 /\ 1.0) : Nil)
        r0 = makeLatchAP (pw { time: 0.04, headroomInSeconds: 0.02 })

        r1 = unwrapCofree r0 (pw { time: 0.70, headroomInSeconds: 0.02 })

        r2 = unwrapCofree r1 (pw { time: 0.99, headroomInSeconds: 0.02 })

        r3 = unwrapCofree r2 (pw { time: 0.995, headroomInSeconds: 0.02 })

        r4 = unwrapCofree r3 (pw { time: 1.4, headroomInSeconds: 0.02 })

        r5 = unwrapCofree r4 (pw { time: 2.0, headroomInSeconds: 0.02 })

        r6 = unwrapCofree r5 (pw { time: 2.3, headroomInSeconds: 0.02 })

      extract r0 `shouldEqualIsh` (Just (asSingleValue 1.0))
      extract r1 `shouldEqualIsh` (Nothing)
      extract r2 `shouldEqualIsh` (Just (ff 0.01 (asSingleValue 0.0)))
      extract r3 `shouldEqualIsh` (Nothing)
      extract r4 `shouldEqualIsh` (Nothing)
      extract r5 `shouldEqualIsh` (Just (asSingleValue 1.0))
      extract r6 `shouldEqualIsh` (Nothing)
