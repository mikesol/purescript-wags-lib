-- | A rate is the playhead changing over time.
module WAGS.Lib.Rate where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Data.Array as A
import Data.Int as DInt
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Math (floor)
import Math as Math

type TimeRate
  = { time :: Number, rate :: Number }

type Rate
  = TimeRate -> Cofree ((->) TimeRate) Number

makeRate :: { startsAt :: Number, prevTime :: Number } -> Rate
makeRate { startsAt, prevTime } = go startsAt prevTime
  where
  go n i { time, rate } = let tnow = (time - i) * rate + n in tnow :< \t -> go tnow time t

type TimeHeadroomRate
  = { time :: Number, headroom :: Number, rate :: Number }

type Emitter
  = TimeHeadroomRate -> Cofree ((->) TimeHeadroomRate) (Array Number)

consumeLookahead :: { tnow :: Number, lookahead :: Number, rate :: Number } -> Array Number
consumeLookahead { lookahead, tnow, rate }
  | tnow <= lookahead =
    consumeLookahead { lookahead: lookahead - 1.0, tnow, rate }
      <> [ (lookahead - tnow) / rate ]
  | otherwise = []

makeOffsets :: { tnow :: Number, headroom :: Number, clearedSoFar :: Number, rate :: Number } -> Array Number /\ Number
makeOffsets { tnow, headroom, clearedSoFar, rate } =
  ( A.replicate urgent 0.0
      <> consumeLookahead { lookahead, tnow, rate }
  )
    /\ lookahead
  where
  urgent = DInt.floor (tnow - clearedSoFar)

  lookahead = floor (tnow + headroom * rate)

makeEmitter :: { startsAt :: Number, prevTime :: Number } -> Emitter
makeEmitter { startsAt, prevTime } = go (if floorStartsAt == startsAt then startsAt - 1.0 else startsAt) startsAt prevTime
  where
  floorStartsAt = floor startsAt
  go clearedSoFar n i { time, headroom, rate } =
    let
      tnow = (time - i) * rate + n

      offsets /\ newCleared = makeOffsets { tnow, headroom, clearedSoFar, rate }
    in
      offsets :< \t -> go newCleared tnow time t

fEmitter' :: { sensitivity :: Number } -> Number -> { time :: Number, headroom :: Number } -> Maybe Number
fEmitter' { sensitivity } gapInSeconds { time, headroom } = if dist < sensitivity then Just (if tGapInSeconds < (gapInSeconds / 2.0) then 0.0 else (gapInSeconds - tGapInSeconds)) else Nothing
  where

  dist = Math.abs ((time + headroom) `Math.remainder` gapInSeconds)

  tGapInSeconds = time `Math.remainder` gapInSeconds

fEmitter :: Number -> { time :: Number, headroom :: Number } -> Maybe Number
fEmitter = fEmitter' { sensitivity: 0.04 }