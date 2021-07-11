module WAGS.Lib.Impulse where

import Prelude
import Control.Comonad (class Comonad)
import Control.Comonad.Cofree (Cofree, deferCofree, head)
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Control.Extend (class Extend)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple.Nested ((/\))
import WAGS.Lib.Cofree (class Actualize)

newtype MakeImpulse a
  = MakeImpulse a

derive instance newtypeMakeImpulse :: Newtype (MakeImpulse a) _

derive instance functorMakeImpulse :: Functor MakeImpulse

derive newtype instance semigroupMakeImpulse :: Semigroup a => Semigroup (MakeImpulse a)

type Impulse
  = Boolean

newtype CfImpulse f a
  = CfImpulse (Cofree f a)

derive instance newtypeCfImpulse :: Newtype (CfImpulse MakeImpulse Impulse) _

derive instance functorCfImpulse :: Functor (CfImpulse MakeImpulse)

derive newtype instance extendCfImpulse :: Extend (CfImpulse MakeImpulse)

derive newtype instance comonadCfImpulse :: Comonad (CfImpulse MakeImpulse)

derive newtype instance comonadCofreeCfImpulse :: ComonadCofree MakeImpulse (CfImpulse MakeImpulse)

type AnImpulse
  = MakeImpulse (CfImpulse MakeImpulse Impulse)

-- | A single impulse
makeImpulse :: AnImpulse
makeImpulse = wrap $ wrap $ go true
  where
  go tf = deferCofree \_ -> tf /\ wrap (go false)

instance semigroupImpulse :: Semigroup (CfImpulse MakeImpulse Impulse) where
  append i0r@(CfImpulse i0) i1r@(CfImpulse i1) =
    wrap
      ( deferCofree \_ ->
          let
            hd = head i0 || head i1

            tl = unwrapCofree i0r <> unwrapCofree i1r
          in
            hd /\ (map unwrap tl)
      )

instance monoidImpulse :: Monoid AnImpulse where
  mempty = makeImpulse

instance actualizeImpulse :: Actualize AnImpulse e r (CfImpulse MakeImpulse Impulse) where
  actualize (MakeImpulse c) _ _ = c
