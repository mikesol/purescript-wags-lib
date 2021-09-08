-- | A rate is the playhead changing over time.
module WAGS.Lib.Rate where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Data.Monoid.Additive (Additive)
import Data.Newtype (wrap)
import WAGS.Run (SceneI(..))

type MakeRate a
  = { time :: Number, rate :: Number } -> a

type Rate
  = Additive Number

type CfRate
  = Cofree ((->) { time :: Number, rate :: Number }) Rate

type ARate
  = MakeRate CfRate

makeRate :: { startsAt :: Number, prevTime :: Number } -> ARate
makeRate { startsAt, prevTime } = go startsAt prevTime
  where
  go n i { time, rate } =
    let
      tnow = (time - i) * rate + n
    in
      wrap tnow :< go tnow time

timeIs :: forall trigger world analyserCallbacks. Number -> SceneI trigger world analyserCallbacks -> SceneI trigger world analyserCallbacks
timeIs time (SceneI x) = SceneI (x { time = time })