module WAGS.Lib.Tidal.Compat where

-- Everyone's favorite tidal functions, imported into wagggsssss.

import Prelude

import Data.Unfoldable (replicate)
import Data.Variant.Maybe (Maybe)
import WAGS.Lib.Tidal.Cycle (Cycle, r)
import WAGS.Lib.Tidal.Tidal (b, i)
import WAGS.Lib.Tidal.Types (Note)

every
  :: forall a
   . Int
  -> (Cycle a -> Cycle a)
  -> Cycle a
  -> Cycle a
every ix
  | ix <= 0 = const identity
  | ix == 1 = ($)
  | otherwise = flip (<*>) (replicate (ix - 1)) <<< map b

fast
  :: Int
  -> Cycle
  ~> Cycle
fast ix
  | ix <= 1 = identity
  | otherwise = i <*> replicate (ix - 1)

fastGap
  :: forall event. Int
  -> Cycle (Maybe (Note event))
  -> Cycle (Maybe (Note event))
fastGap ix
  | ix <= 1 = identity
  | otherwise = flip i (replicate (ix - 1) r)