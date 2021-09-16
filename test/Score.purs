module Test.Score where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (deferCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Int (floor)
import Data.Lens (over, traversed)
import Data.Lens.Record (prop)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import WAGS.Lib.Score (makeScore)

intize :: forall r. Array { offset :: Number | r } -> Array { offset :: Int | r }
intize = over (traversed <<< prop (Proxy :: _ "offset")) (floor <<< mul 100000.0)

testScore :: Spec Unit
testScore = do
  describe "Tests score" do
    it "Produces the correct score" do
      let
        freq = makeScore { startsAt: 0.0, rest: let f x b = deferCofree \_ -> Tuple { duration: if x == 0 then 0.0 else if b then 1.0 else 0.5, rest: x } $ wrap (f (x + 1) (not b)) in f 0 false }
        r0 = freq { time: 0.04, headroomInSeconds: 0.02 }
        r1 = unwrapCofree r0 { time: 0.1, headroomInSeconds: 0.02 }
        r2 = unwrapCofree r1 { time: 0.98, headroomInSeconds: 0.06 }
        r3 = unwrapCofree r2 { time: 0.49, headroomInSeconds: 0.03 }
        r4 = unwrapCofree r3 { time: 1.49, headroomInSeconds: 0.03 }
        r5 = unwrapCofree r4 { time: 2.5, headroomInSeconds: 0.03 }

      (intize $ extract r0) `shouldEqual` intize ([ { offset: -0.04, rest: 0 } ])
      (intize $ extract r1) `shouldEqual` intize ([])
      (intize $ extract r2) `shouldEqual` intize ([ { offset: 0.02, rest: 1 } ])
      (intize $ extract r3) `shouldEqual` intize ([])
      (intize $ extract r4) `shouldEqual` intize ([ { offset: 0.01, rest: 2 } ])
      (intize $ extract r5) `shouldEqual` intize ([ { offset: 0.0, rest: 3 } ])
