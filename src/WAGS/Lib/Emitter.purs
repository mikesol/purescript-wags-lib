-- | An emitter is an impulse that is emitted once in the case of comonads or as few times as possible in the case of functions.
module WAGS.Lib.Emitter where

import Prelude
import Control.Comonad.Cofree (Cofree, (:<))
import Data.Array as A
import Data.Int as DInt
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Math (floor)
import Math as Math

-------
type MakeEmitter a
  = { time :: Number, headroom :: Number, freq :: Number } -> a

type Emission
  = Array Number

type CfEmitter = Cofree ((->) { time :: Number, headroom :: Number, freq :: Number }) Emission

type AnEmitter = MakeEmitter CfEmitter

consumeLookahead :: { tnow :: Number, lookahead :: Number, freq :: Number } -> Array Number
consumeLookahead { lookahead, tnow, freq }
  | tnow <= lookahead =
      consumeLookahead { lookahead: lookahead - 1.0, tnow, freq }
        <> [ (lookahead - tnow) / freq ]
  | otherwise = []

makeOffsets
  :: { tnow :: Number
     , headroom :: Number
     , clearedSoFar :: Number
     , freq :: Number
     }
  -> Array Number /\ Number
makeOffsets { tnow, headroom, clearedSoFar, freq } =
  ( A.replicate urgent 0.0
      <> if lookahead <= clearedSoFar then [] else consumeLookahead { lookahead, tnow, freq }
  )
    /\ lookahead
  where
  urgent = DInt.floor (tnow - clearedSoFar)

  lookahead = floor (tnow + (headroom * freq))

makeEmitter :: { startsAt :: Number, prevTime :: Number } -> AnEmitter
makeEmitter { startsAt, prevTime } =
  go
    (if floorStartsAt == startsAt then startsAt - 1.0 else startsAt)
    startsAt
    prevTime

  where
  floorStartsAt = floor startsAt

  go clearedSoFar n i { time, headroom, freq } =
    let
      tnow = (time - i) * freq + n

      offsets /\ newCleared = makeOffsets { tnow, headroom, clearedSoFar, freq }
    in
      offsets :< go newCleared tnow time

fEmitter' :: { sensitivity :: Number } -> Number -> { time :: Number, headroom :: Number } -> Maybe Number
fEmitter' { sensitivity } freq { time, headroom } = if dist < sensitivity then Just (if tGap < (gap / 2.0) then 0.0 else (gap - tGap)) else Nothing
  where
  gap = 1.0 / freq

  dist = Math.abs ((time + headroom) `Math.remainder` gap)

  tGap = time `Math.remainder` gap

fEmitter :: Number -> { time :: Number, headroom :: Number } -> Maybe Number
fEmitter = fEmitter' { sensitivity: 0.04 }
