-- | A rate is the playhead changing over time.

module WAGS.Lib.Rate where

import Prelude
import Control.Comonad.Cofree (Cofree, (:<))

type TimeRate
  = { time :: Number, rate :: Number }

type Rate
  = TimeRate -> Cofree ((->) TimeRate) Number

makeRate :: Number -> Rate
makeRate = go 0.0
  where
  go n i { time, rate } = let tnow = (time - i) * rate + n in tnow :< \t -> go tnow time t