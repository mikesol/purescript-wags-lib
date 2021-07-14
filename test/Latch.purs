module Test.Latch where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqualIsh)
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.Latch (makeLatchAP)
import WAGS.Lib.Piecewise (makeTerracedR)

testLatch :: Spec Unit
testLatch = do
  describe "Tests blip" do
    it "Changes the latched value only when there is a change" do
      let
        pw = makeTerracedR ((0.0 /\ 1.0) :| (1.0 /\ 0.0) : (2.0 /\ 1.0) : Nil)
        r0 = (unwrap makeLatchAP) (pw { time: 0.04, headroom: 0.02 })

        r1 = unwrap (unwrapCofree r0) (pw { time: 0.70, headroom: 0.02 })

        r2 = unwrap (unwrapCofree r1) (pw { time: 0.99, headroom: 0.02 })

        r3 = unwrap (unwrapCofree r2) (pw { time: 0.995, headroom: 0.02 })

        r4 = unwrap (unwrapCofree r3) (pw { time: 1.4, headroom: 0.02 })

        r5 = unwrap (unwrapCofree r4) (pw { time: 2.0, headroom: 0.02 })

        r6 = unwrap (unwrapCofree r5) (pw { time: 2.3, headroom: 0.02 })

      extract r0 `shouldEqualIsh` (Just (pure 1.0))
      extract r1 `shouldEqualIsh` (Nothing)
      extract r2 `shouldEqualIsh` (Just (ff 0.01 (pure 0.0)))
      extract r3 `shouldEqualIsh` (Nothing)
      extract r4 `shouldEqualIsh` (Nothing)
      extract r5 `shouldEqualIsh` (Just (pure 1.0))
      extract r6 `shouldEqualIsh` (Nothing)
