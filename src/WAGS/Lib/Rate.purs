-- | A rate is the playhead changing over time.
module WAGS.Lib.Rate where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Data.Lens (set)
import Data.Lens.Record (prop)
import Data.Monoid.Additive (Additive)
import Data.Newtype (class Newtype, unwrap, wrap)
import Type.Proxy (Proxy(..))

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

timeIs :: forall ntype r oldTime time.
  Newtype
    { time :: time
    | r
    }
    ntype
   => time -> { time :: oldTime  | r  } -> ntype
timeIs time = unwrap <<< set (prop (Proxy :: _ "time")) time