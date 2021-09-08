module WAGS.Lib.Impulse where

import Prelude

import Control.Comonad.Cofree (Cofree, deferCofree)
import Data.Identity (Identity)
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))

type Impulse = Boolean

type CfImpulse
  = Cofree Identity Impulse

type AnImpulse
  = Identity CfImpulse

-- | A single impulse
makeImpulse :: AnImpulse
makeImpulse = wrap $ go true
  where
  go tf = deferCofree \_ -> tf /\ wrap (go false)
