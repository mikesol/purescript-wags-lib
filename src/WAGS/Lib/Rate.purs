-- | A rate is the playhead changing over time.
module WAGS.Lib.Rate where

import Prelude

import Control.Comonad (class Comonad, class Extend, extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Data.Newtype (class Newtype, unwrap, wrap)
import WAGS.Lib.Cofree (class Actualize, class ActualizeE, actualizeE)
import WAGS.Run (SceneI(..))

newtype MakeRate a
  = MakeRate ({ time :: Number, rate :: Number } -> a)

derive instance newtypeMakeRate :: Newtype (MakeRate a) _

derive instance functorMakeRate :: Functor MakeRate

derive newtype instance semigroupMakeRate :: Semigroup a => Semigroup (MakeRate a)

type Rate
  = Number

newtype CfRate f a
  = CfRate (Cofree f a)

derive instance newtypeCfRate :: Newtype (CfRate MakeRate Rate) _

derive newtype instance functorCfRate :: Functor (CfRate MakeRate)

derive newtype instance extendCfRate :: Extend (CfRate MakeRate)

derive newtype instance comonadCfRate :: Comonad (CfRate MakeRate)

derive newtype instance comonadCofreeCfRate :: ComonadCofree MakeRate (CfRate MakeRate)

type ARate
  = MakeRate (CfRate MakeRate Rate)

makeRate :: { startsAt :: Number, prevTime :: Number } -> ARate
makeRate { startsAt, prevTime } = wrap (go startsAt prevTime)
  where
  go n i { time, rate } =
    let
      tnow = (time - i) * rate + n
    in
      wrap (tnow :< map unwrap (wrap (go tnow time)))

instance semigroupCfRate :: Semigroup (CfRate MakeRate Rate) where
  append f0i f1i =
    let
      hd = ((extract f0i) + (extract f1i)) / 2.0

      tl = unwrapCofree f0i <> unwrapCofree f1i
    in
      wrap (hd :< map unwrap tl)

instance monoidARate :: Monoid ARate where
  mempty = makeRate { startsAt: 0.0, prevTime: 0.0 }

instance actualizeRate :: Actualize (MakeRate out) (SceneI a b c) Number out where
  actualize = actualizeE

instance actualizeRateE :: ActualizeE (MakeRate) (SceneI a b c) Number where
  actualizeE (MakeRate r) (SceneI { time }) rate = r { time, rate }

timeIs :: forall trigger world analyserCallbacks. Number -> SceneI trigger world analyserCallbacks -> SceneI trigger world analyserCallbacks
timeIs time (SceneI x) = SceneI (x { time = time })