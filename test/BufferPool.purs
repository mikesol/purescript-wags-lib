module Test.BufferPool where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (D5, d0, d1)
import Data.Vec as V
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqualIsh)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Lib.BufferPool (ABufferPool, bOnOff, makeBufferPool)

testBufferPool :: Spec Unit
testBufferPool = do
  describe "Tests buffer pool" do
    it "Produces a correct buffer pool" do
      let
        buf = makeBufferPool (Just 0.4) Nothing :: ABufferPool D5 Unit

        b0 = buf { time: 0.0, headroom: 0.02, offsets: [ { offset: 0.0, rest: unit } ] }

        b1 = unwrapCofree b0 { time: 0.3, headroom: 0.02, offsets: [] }

        b2 = unwrapCofree b1 { time: 0.34, headroom: 0.02, offsets: [ { offset: 0.0, rest: unit } ] }

        b3 = unwrapCofree b2 { time: 0.41, headroom: 0.02, offsets: [] }
      --
      bOnOff 0.0 (V.index (extract b0) d0) `shouldEqualIsh` (pure OffOn)
      bOnOff 0.0 (V.index (extract b0) d1) `shouldEqualIsh` (pure Off)
      bOnOff 0.3 (V.index (extract b1) d0) `shouldEqualIsh` (pure On)
      bOnOff 0.3 (V.index (extract b1) d1) `shouldEqualIsh` (pure Off)
      bOnOff 0.34 (V.index (extract b2) d0) `shouldEqualIsh` (pure On)
      bOnOff 0.34 (V.index (extract b2) d1) `shouldEqualIsh` (pure OffOn)
      bOnOff 0.41 (V.index (extract b3) d0) `shouldEqualIsh` (pure On)
      bOnOff 0.41 (V.index (extract b3) d1) `shouldEqualIsh` (pure On)
