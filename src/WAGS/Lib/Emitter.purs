-- | An emitter is an impulse that is emitted once in the case of comonads or as few times as possible in the case of functions.
module WAGS.Lib.Emitter where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Data.Array as A
import Data.Int (toNumber)
import Data.Int as DInt
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Tuple.Nested ((/\), type (/\))
import Math (floor)
import Math as Math
import WAGS.Lib.Cofree (class Actualize)
import WAGS.Run (SceneI(..))

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
      <> if lookahead <= clearedSoFar then [] else consumeLookahead { lookahead, tnow, rate }
  )
    /\ lookahead
  where
  urgent = DInt.floor (tnow - clearedSoFar)

  lookahead = floor (tnow + headroom * rate)

newtype AnEmitter
  = AnEmitter Emitter

derive instance newtypeAnEmitter :: Newtype AnEmitter _

instance semigroupEmitter :: Semigroup AnEmitter where
  append (AnEmitter e0) (AnEmitter e1) = AnEmitter \i -> go e0 e1 i
    where
    go a' b' x = ((head a) <> (head b)) :< go (tail a) (tail b)
      where
      a = a' x

      b = b' x

instance monoidEmitter :: Monoid AnEmitter where
  mempty = makeEmitter { prevTime: 0.0, startsAt: 0.0 }

makeEmitter :: { startsAt :: Number, prevTime :: Number } -> AnEmitter
makeEmitter { startsAt, prevTime } = wrap (go (if floorStartsAt == startsAt then startsAt - 1.0 else startsAt) startsAt prevTime)
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

instance actualizeEmitter :: Actualize AnEmitter (SceneI a b) Number (Cofree ((->) TimeHeadroomRate) (Array Number)) where
  actualize (AnEmitter r) (SceneI { time, headroom }) rate = r { time, headroom: toNumber headroom / 1000.0, rate }
