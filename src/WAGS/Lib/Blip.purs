module WAGS.Lib.Blip where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Data.Monoid.Disj (Disj(..))
import Safe.Coerce (coerce)

type Blip
  = Disj Boolean

type CfBlip = Cofree ((->) Boolean) Blip

type ABlip
  = Boolean -> CfBlip

makeBlip :: ABlip
makeBlip = go false
  where
  go prev cur = coerce (not prev && cur) :< go cur
