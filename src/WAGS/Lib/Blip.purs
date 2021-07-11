module WAGS.Lib.Blip where

import Prelude
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Control.Extend (class Extend)
import Data.Newtype (class Newtype, unwrap, wrap)
import WAGS.Lib.Cofree (class Actualize)

newtype MakeBlip a
  = MakeBlip (Boolean -> a)

derive instance newtypeMakeBlip :: Newtype (MakeBlip a) _

derive instance functorMakeBlip :: Functor MakeBlip

derive newtype instance semigroupMakeBlip :: Semigroup a => Semigroup (MakeBlip a)

type Blip
  = Boolean

newtype CfBlip f a
  = CfBlip (Cofree f a)

derive instance newtypeCfBlip :: Newtype (CfBlip MakeBlip Blip) _

derive instance functorCfBlip :: Functor (CfBlip MakeBlip)

derive newtype instance extendCfBlip :: Extend (CfBlip MakeBlip)

derive newtype instance comonadCfBlip :: Comonad (CfBlip MakeBlip)

derive newtype instance comonadCofreeCfBlip :: ComonadCofree MakeBlip (CfBlip MakeBlip)

type ABlip
  = MakeBlip (CfBlip MakeBlip Blip)

makeBlip :: ABlip
makeBlip = wrap (go false)
  where
  go prev cur = wrap ((not prev && cur) :< map unwrap (wrap (go cur)))

instance semigroupCfBlip :: Semigroup (CfBlip MakeBlip Blip) where
  append f0i f1i =
    let
      hd = extract f0i || extract f0i

      tl = unwrapCofree f0i <> unwrapCofree f1i
    in
      wrap (hd :< map unwrap tl)

instance monoidBlip :: Monoid ABlip where
  mempty = makeBlip

instance actualizeBlip :: Actualize ABlip e Boolean (CfBlip MakeBlip Blip) where
  actualize (MakeBlip r) _ b = r b
