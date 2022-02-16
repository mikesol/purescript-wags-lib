module WAGS.Lib.Tidal.Compat where

-- Everyone's favorite tidal functions, imported into wagggsssss.

import Prelude

import Data.Unfoldable (replicate)
import WAGS.Lib.Tidal.Cycle (Cycle)
import WAGS.Lib.Tidal.Tidal (b)

every
  :: forall a
   . Int
  -> (Cycle a -> Cycle a)
  -> Cycle a
  -> Cycle a
every i
  | i <= 0 = const identity
  | i == 1 = ($)
  | otherwise = flip (<*>) (replicate (i - 1)) <<< map b