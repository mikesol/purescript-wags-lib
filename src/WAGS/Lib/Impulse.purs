module WAGS.Lib.Impulse where

import Prelude

import Control.Comonad.Cofree (Cofree, deferCofree)
import Data.Identity (Identity)
import Data.Tuple.Nested ((/\))

impulse :: Cofree Identity Boolean
impulse = go true
  where
  go tf = deferCofree \_ -> tf /\ pure (go false)