-- | A rate is the playhead changing over time.
module WAGS.Lib.Rate where

import Prelude
import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Data.Newtype (class Newtype, unwrap, wrap)
import WAGS.Lib.Cofree (class Actualize)
import WAGS.Lib.FofT (FofTimeRate(..))
import WAGS.Run (SceneI(..))

newtype Rate
  = Rate Number

derive instance newtypeRate :: Newtype Rate _

newtype ARate
  = ARate (FofTimeRate (Cofree FofTimeRate Rate))

derive instance newtypeARate :: Newtype ARate _

makeRate :: { startsAt :: Number, prevTime :: Number } -> ARate
makeRate { startsAt, prevTime } = wrap (wrap (go startsAt prevTime))
  where
  go n i { time, rate } = let tnow = (time - i) * rate + n in (Rate tnow) :< wrap (\t -> go tnow time t)

instance semigroupARate :: Semigroup ARate where
  append (ARate (FofTimeRate f0)) (ARate (FofTimeRate f1)) =
    ARate
      ( FofTimeRate \tr ->
          let
            f0i = f0 tr

            f1i = f1 tr
          in
            (wrap ((unwrap (head f0i)) + (unwrap (head f0i)) / 2.0))
              :< unwrap ((wrap (tail f0i) :: ARate) <> (wrap (tail f1i)))
      )

instance monoidARate :: Monoid ARate where
  mempty = makeRate { startsAt: 0.0, prevTime: 0.0 }

instance actualizeRate :: Actualize ARate (SceneI a b) Number (Cofree FofTimeRate Rate) where
  actualize (ARate (FofTimeRate r)) (SceneI { time }) rate = r { time, rate }
