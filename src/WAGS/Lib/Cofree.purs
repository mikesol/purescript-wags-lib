module WAGS.Lib.Cofree where

import Prelude

import Control.Comonad.Cofree as Cf
import Data.Identity (Identity(..))
import Data.Newtype (class Newtype, wrap)
import Heterogeneous.Mapping (class HMap, hmap)


tails :: forall ii oo i o. HMap (i -> o) { | ii } { | oo } => Tailable i o => { | ii } -> { | oo }
tails = (hmap :: (i -> o) -> { | ii } -> { | oo }) tail

class Tailable i o where
  tail :: i -> o
instance tailableCofree :: Tailable (Cf.Cofree f a) (f (Cf.Cofree f a)) where
  tail = Cf.tail

else instance tailableRate :: Newtype n (f (Cf.Cofree f a))  => Tailable (Cf.Cofree f a) n where
  tail = wrap <<< Cf.tail

class Actualize n e r o | n e r -> o where
  actualize :: n -> e -> r -> o

instance actualizeTrivial :: Actualize (Identity (Cf.Cofree Identity Boolean)) e r (Cf.Cofree Identity Boolean) where
  actualize (Identity c) _ _ = c
