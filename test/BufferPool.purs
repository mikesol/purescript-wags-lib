module Test.BufferPool where

import Prelude

import Control.Comonad.Cofree (head, tail)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (D5, d0, d1)
import Data.Vec as V
import Test.Spec (Spec, describe, it)
import Test.Util (shouldEqualIsh)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Lib.BufferPool (BuffyStream, bGain, bOnOff, bufferPool)

testBufferPool :: Spec Unit
testBufferPool = do
  describe "Tests buffer pool" do
    it "Produces a correct buffer pool" do
      let
        (buf :: BuffyStream D5 Unit) = bufferPool (Just 0.4) Nothing
        b0 = buf { time: 0.0, headroom: 0.02, offsets: [ {offset: 0.0, rest: unit} ] }
        b1 = tail b0 { time: 0.3, headroom: 0.02, offsets: [ ] }
        b2 = tail b1 { time: 0.34, headroom: 0.02, offsets: [ {offset: 0.0, rest: unit} ] }
        b3 = tail b2 { time: 0.41, headroom: 0.02, offsets: [ ] }
      bGain (V.index (head b0) d0) `shouldEqualIsh` (pure 1.0)
      bGain (V.index (head b0) d1) `shouldEqualIsh` (pure 0.0)
      bGain (V.index (head b1) d0) `shouldEqualIsh` (pure 1.0)
      bGain (V.index (head b1) d1) `shouldEqualIsh` (pure 0.0)
      bGain (V.index (head b2) d0) `shouldEqualIsh` (pure 1.0)
      bGain (V.index (head b2) d1) `shouldEqualIsh` (pure 1.0)
      bGain (V.index (head b3) d0) `shouldEqualIsh` (pure 0.0)
      bGain (V.index (head b3) d1) `shouldEqualIsh` (pure 1.0)
      --
      bOnOff (V.index (head b0) d0) `shouldEqualIsh` (pure On)
      bOnOff (V.index (head b0) d1) `shouldEqualIsh` (pure Off)
      bOnOff (V.index (head b1) d0) `shouldEqualIsh` (pure On)
      bOnOff (V.index (head b1) d1) `shouldEqualIsh` (pure Off)
      bOnOff (V.index (head b2) d0) `shouldEqualIsh` (pure On)
      bOnOff (V.index (head b2) d1) `shouldEqualIsh` (pure On)
      bOnOff (V.index (head b3) d0) `shouldEqualIsh` (pure Off)
      bOnOff (V.index (head b3) d1) `shouldEqualIsh` (pure On)