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

derive instance newtypeFofTimeBlip :: Newtype (MakeBlip a) _

derive instance functorFofTimeBlip :: Functor MakeBlip

newtype Blip
  = Blip Boolean

derive instance newtypeBlip :: Newtype Blip _

derive newtype instance heytingAlgebraBlip :: HeytingAlgebra Blip

newtype CfBlip f a
  = CfBlip (Cofree f a)

derive instance newtypeCfBlip :: Newtype (CfBlip MakeBlip Blip) _

derive newtype instance extendCfBlip :: Extend (CfBlip MakeBlip)

derive newtype instance comonadCfBlip :: Comonad (CfBlip MakeBlip)

derive newtype instance comonadCofreeCfBlip :: ComonadCofree MakeBlip (CfBlip MakeBlip)

type ABlip
  = MakeBlip (CfBlip MakeBlip Blip)

makeBlip :: ABlip
makeBlip = wrap (go false)
  where
  go prev cur = wrap (wrap (not prev && cur) :< map unwrap (wrap (go cur)))

instance semigroupCfBlip :: Semigroup (CfBlip MakeBlip Blip) where
  append f0i f1i =
    let
      hd = wrap ((unwrap (extract f0i) || unwrap (extract f0i)))

      tl = unwrapCofree f0i <> unwrapCofree f1i
    in
      wrap (hd :< map unwrap tl)

instance semigroupABlip :: Semigroup ABlip where
  append (MakeBlip i0) (MakeBlip i1) = wrap (i0 <> i1)

instance monoidBlip :: Monoid ABlip where
  mempty = makeBlip

instance actualizeBlip :: Actualize ABlip e Boolean (CfBlip MakeBlip Blip) where
  actualize (MakeBlip r) _ b = r b
