-- | A rate is the playhead changing over time.
module WAGS.Lib.Rate where

import Prelude
import Control.Comonad (class Comonad, class Extend, extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Data.Newtype (class Newtype, unwrap, wrap)
import WAGS.Lib.Cofree (class Actualize)
import WAGS.Run (SceneI(..))

newtype MakeRate a
  = MakeRate ({ time :: Number, rate :: Number } -> a)

derive instance newtypeFofTimeRate :: Newtype (MakeRate a) _

derive instance functorFofTimeRate :: Functor MakeRate

newtype Rate
  = Rate Number

derive instance newtypeRate :: Newtype Rate _

newtype CfRate f a
  = CfRate (Cofree f a)

derive instance newtypeCfRate :: Newtype (CfRate MakeRate Rate) _

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
      wrap ((wrap tnow) :< map unwrap (wrap (go tnow time)))

instance semigroupCfRate :: Semigroup (CfRate MakeRate Rate) where
  append f0i f1i =
    let
      hd = wrap ((unwrap (extract f0i) + unwrap (extract f0i)) / 2.0)

      tl = unwrapCofree f0i <> unwrapCofree f1i
    in
      wrap (hd :< map unwrap tl)

instance semigroupARate :: Semigroup ARate where
  append (MakeRate f0) (MakeRate f1) = wrap ((<>) <$> f0 <*> f1)

instance monoidARate :: Monoid ARate where
  mempty = makeRate { startsAt: 0.0, prevTime: 0.0 }

instance actualizeRate :: Actualize ARate (SceneI a b) Number (CfRate MakeRate Rate) where
  actualize (MakeRate r) (SceneI { time }) rate = r { time, rate }
