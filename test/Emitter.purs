module Test.Emitter where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Int (floor)
import Data.Lens (over, traversed)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (shouldEqualIsh)
import Type.Proxy (Proxy(..))
import WAGS.Lib.Emitter (makeEmitter, fEmitter)

intize :: forall r. Array { offset :: Number | r } -> Array { offset :: Int | r }
intize = over (traversed <<< prop (Proxy :: _ "offset")) (floor <<< mul 100000.0)

testEmitter :: Spec Unit
testEmitter = do
  describe "Tests emitter" do
    it "Produces the correct emissions for a simple emitter" do
      let
        freq = makeEmitter { startsAt: 0.0 }

        r0 = freq { time: 0.04, freq: 1.0, headroomInSeconds: 0.02 }

        r1 = unwrapCofree r0 { time: 0.1, freq: 1.0, headroomInSeconds: 0.02 }

        r2 = unwrapCofree r1 { time: 0.95, freq: 1.0, headroomInSeconds: 0.06 }
      (intize $ extract r0) `shouldEqual` intize ([ { offset: -0.04, rest: unit } ])
      (intize $ extract r1) `shouldEqual` intize ([])
      (intize $ extract r2) `shouldEqual` intize ([ { offset: 0.05, rest: unit } ])
    it "Produces the correct emissions for a simple emitter starting at 0" do
      let
        freq = makeEmitter { startsAt: 0.0 }

        r0 = freq { time: 0.00, freq: 1.0, headroomInSeconds: 0.02 }

        r1 = unwrapCofree r0 { time: 0.1, freq: 1.0, headroomInSeconds: 0.02 }

        r2 = unwrapCofree r1 { time: 0.95, freq: 1.0, headroomInSeconds: 0.06 }
      (intize $ extract r0) `shouldEqual` intize ([ { offset: 0.0, rest: unit } ])
      (intize $ extract r1) `shouldEqual` intize ([])
      (intize $ extract r2) `shouldEqual` intize ([ { offset: 0.05, rest: unit } ])
    it "Produces the correct emissions for a 2x speed emitter" do
      let
        freq = makeEmitter { startsAt: 0.0 }

        r0 = freq { time: 0.04, freq: 2.0, headroomInSeconds: 0.02 }

        r1 = unwrapCofree r0 { time: 0.1, freq: 2.0, headroomInSeconds: 0.02 }

        r2 = unwrapCofree r1 { time: 0.48, freq: 2.0, headroomInSeconds: 0.06 }

        r3 = unwrapCofree r2 { time: 0.49, freq: 2.0, headroomInSeconds: 0.03 }

        r4 = unwrapCofree r3 { time: 0.78, freq: 2.0, headroomInSeconds: 0.03 }

      (intize $ extract r0) `shouldEqual` intize ([ { offset: -0.04, rest: unit } ])
      (intize $ extract r1) `shouldEqual` intize ([])
      (intize $ extract r2) `shouldEqual` intize ([ { offset: 0.02, rest: unit } ])
      (intize $ extract r3) `shouldEqual` intize ([])
      (intize $ extract r4) `shouldEqual` intize ([ ])
    it "Produces the correct emissions for a function emitter" do
      let
        r0 = fEmitter 1.0 { time: 0.0, headroomInSeconds: 0.02 }

        r1 = fEmitter 1.0 { time: 0.1, headroomInSeconds: 0.02 }

        r2 = fEmitter 1.0 { time: 0.95, headroomInSeconds: 0.06 }
      r0 `shouldEqualIsh` (Just 0.0)
      r1 `shouldEqualIsh` (Nothing)
      r2 `shouldEqualIsh` (Just 0.05)
    it "Produces the correct emissions for a 2x speed function emitter" do
      let
        r0 = fEmitter 2.0 { time: 0.0, headroomInSeconds: 0.02 }

        r1 = fEmitter 2.0 { time: 0.1, headroomInSeconds: 0.02 }

        r2 = fEmitter 2.0 { time: 0.48, headroomInSeconds: 0.03 }

        r3 = fEmitter 2.0 { time: 0.78, headroomInSeconds: 0.03 }

      r0 `shouldEqualIsh` (Just 0.0)
      r1 `shouldEqualIsh` (Nothing)
      r2 `shouldEqualIsh` (Just 0.02)
      r3 `shouldEqualIsh` (Nothing)
