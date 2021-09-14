-- | An emitter is an impulse that is emitted once in the case of comonads or as few times as possible in the case of functions.
module WAGS.Lib.Emitter where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Data.Lens (_1, over)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Math as Math

-------
type MakeEmitter a
  = { time :: Number, headroom :: Number, freq :: Number } -> a

type Emission
  = Array Number

type CfEmitter = Cofree ((->) { time :: Number, headroom :: Number, freq :: Number }) Emission

type AnEmitter = MakeEmitter CfEmitter

makeOffsets :: { time :: Number, headroom :: Number, freq :: Number, playhead :: Number } -> Array Number /\ Number
makeOffsets { time, headroom, freq, playhead }
  | time + (headroom / freq) >= playhead = over _1 (append [ playhead - time ]) (makeOffsets { time, headroom, freq, playhead: playhead + 1.0 / freq })
  | otherwise = [] /\ playhead

makeEmitter :: { startsAt :: Number } -> AnEmitter
makeEmitter { startsAt } =
  go startsAt

  where
  go playhead { time, headroom, freq } =
    let
      offsets /\ newPlayhead = makeOffsets { time, headroom, freq, playhead }
    in
      offsets :< go newPlayhead

fEmitter' :: { sensitivity :: Number } -> Number -> { time :: Number, headroom :: Number } -> Maybe Number
fEmitter' { sensitivity } freq { time, headroom } = if dist < sensitivity then Just (if tGap < (gap / 2.0) then 0.0 else (gap - tGap)) else Nothing
  where
  gap = 1.0 / freq

  dist = Math.abs ((time + headroom) `Math.remainder` gap)

  tGap = time `Math.remainder` gap

fEmitter :: Number -> { time :: Number, headroom :: Number } -> Maybe Number
fEmitter = fEmitter' { sensitivity: 0.04 }
