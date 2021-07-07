-- | A rate is the playhead changing over time.
module WAGS.Lib.Rate where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Data.Newtype (class Newtype, wrap)
import WAGS.Lib.Cofree (class Actualize)
import WAGS.Run (SceneI(..))


type TimeRate
  = { time :: Number, rate :: Number }

type Rate
  = TimeRate -> Cofree ((->) TimeRate) Number

newtype ARate
  = ARate Rate

derive instance newtypeARate :: Newtype ARate _

makeRate :: { startsAt :: Number, prevTime :: Number } -> ARate
makeRate { startsAt, prevTime } = wrap (go startsAt prevTime)
  where
  go n i { time, rate } = let tnow = (time - i) * rate + n in tnow :< \t -> go tnow time t

instance actualizeRate :: Actualize ARate (SceneI a b) Number (Cofree ((->) TimeRate) Number) where
  actualize (ARate r) (SceneI { time }) rate = r { time, rate }
