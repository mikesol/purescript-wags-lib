module Test.BufferPool where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (D5)
import Data.Vec ((+>))
import Data.Vec as V
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Lib.BufferPool (ABufferPool, AHotBufferPool, Buffy(..), makeBufferPool, makeHotBufferPool)

testBufferPool :: Spec Unit
testBufferPool = do
  describe "Tests buffer pool" do
    it "Produces a correct buffer pool" do
      let
        buf = makeBufferPool :: ABufferPool D5
        b0 = buf { time: 0.0, offsets: [ { offset: 0.0} ] }
        b1 = unwrapCofree b0 { time: 0.3, offsets: [] }
        b2 = unwrapCofree b1 { time: 0.34, offsets: [ { offset: 0.03 } ] }
        b3 = unwrapCofree b2 { time: 0.41, offsets: [] }
        b4 = unwrapCofree b3 { time: 0.5, offsets: [{offset:0.02}] }
        b5 = unwrapCofree b4 { time: 0.8, offsets: [{offset:0.00}] }
        b6 = unwrapCofree b5 { time: 1.0, offsets: [{offset:0.00}] }
        b7 = unwrapCofree b6 { time: 1.1, offsets: [{offset:0.00}] }
      --
      extract b0 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: true, rest: unit }) +> Nothing +> Nothing +> Nothing +> Nothing +> V.empty)
      extract b1 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: false, rest: unit }) +> Nothing +> Nothing +> Nothing +> Nothing +> V.empty)
      extract b2 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 0.37, starting: true, rest: unit }) +> Nothing +> Nothing +> Nothing +> V.empty)
      extract b3 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 0.37, starting: false, rest: unit }) +> Nothing +> Nothing +> Nothing +> V.empty)
      extract b4 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 0.37, starting: false, rest: unit }) +> Just (Buffy { startTime: 0.52, starting: true, rest: unit }) +> Nothing +> Nothing +> V.empty)
      extract b5 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 0.37, starting: false, rest: unit }) +> Just (Buffy { startTime: 0.52, starting: false, rest: unit }) +> Just (Buffy { startTime: 0.8, starting: true, rest: unit }) +> Nothing +> V.empty)
      extract b6 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 0.37, starting: false, rest: unit }) +> Just (Buffy { startTime: 0.52, starting: false, rest: unit }) +> Just (Buffy { startTime: 0.8, starting: false, rest: unit }) +> Just (Buffy { startTime: 1.0, starting: true, rest: unit }) +> V.empty)
      extract b7 `shouldEqual` (Just (Buffy { startTime: 1.1, starting: true, rest: unit }) +> Just (Buffy { startTime: 0.37, starting: false, rest: unit }) +> Just (Buffy { startTime: 0.52, starting: false, rest: unit }) +> Just (Buffy { startTime: 0.8, starting: false, rest: unit }) +> Just (Buffy { startTime: 1.0, starting: false, rest: unit }) +> V.empty)
    it "Produces a correct hot buffer pool" do
      let
        buf = makeHotBufferPool { startsAt: 0.0 } :: AHotBufferPool D5
        b0 = buf { time: 0.0, headroomInSeconds: 0.02, freq: 1.0 }
        b1 = unwrapCofree b0 { time: 0.5, headroomInSeconds: 0.02, freq: 1.0 }
        b2 = unwrapCofree b1 { time: 0.99, headroomInSeconds: 0.02, freq: 1.0 }
        b3 = unwrapCofree b2 { time: 1.2, headroomInSeconds: 0.02, freq: 1.0 }
        b4 = unwrapCofree b3 { time: 1.99, headroomInSeconds: 0.02, freq: 1.0 }
        b5 = unwrapCofree b4 { time: 3.03, headroomInSeconds: 0.02, freq: 1.0 }
        b6 = unwrapCofree b5 { time: 4.0, headroomInSeconds: 0.02, freq: 1.0 }
        b7 = unwrapCofree b6 { time: 5.01, headroomInSeconds: 0.02, freq: 1.0 }
      --
      extract b0 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: true, rest: unit }) +> Nothing +> Nothing +> Nothing +> Nothing +> V.empty)
      extract b1 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: false, rest: unit }) +> Nothing +> Nothing +> Nothing +> Nothing +> V.empty)
      extract b2 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 1.0, starting: true, rest: unit }) +> Nothing +> Nothing +> Nothing +> V.empty)
      extract b3 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 1.0, starting: false, rest: unit }) +> Nothing +> Nothing +> Nothing +> V.empty)
      extract b4 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 1.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 2.0, starting: true, rest: unit }) +> Nothing +> Nothing +> V.empty)
      extract b5 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 1.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 2.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 3.03, starting: true, rest: unit }) +> Nothing +> V.empty)
      extract b6 `shouldEqual` (Just (Buffy { startTime: 0.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 1.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 2.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 3.03, starting: false, rest: unit }) +> Just (Buffy { startTime: 4.0, starting: true, rest: unit }) +> V.empty)
      extract b7 `shouldEqual` (Just (Buffy { startTime: 5.01, starting: true, rest: unit }) +> Just (Buffy { startTime: 1.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 2.0, starting: false, rest: unit }) +> Just (Buffy { startTime: 3.03, starting: false, rest: unit }) +> Just (Buffy { startTime: 4.0, starting: false, rest: unit }) +> V.empty)
