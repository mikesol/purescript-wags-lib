-- | An emitter is an impulse that is emitted once in the case of comonads or as few times as possible in the case of functions.
module WAGS.Lib.Emitter where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Identity (Identity)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Math as Math
import Type.Proxy (Proxy(..))

-------
type MakeEmitter a
  = { time :: Number, headroomInSeconds :: Number, freq :: Number } -> a

type Emission' rest
  = Array { offset :: Number, rest :: rest }

type Emission = Emission' Unit

type CfEmitter' rest = Cofree ((->) { time :: Number, headroomInSeconds :: Number, freq :: Number }) (Emission' rest)
type CfEmitter = CfEmitter' Unit

type AnEmitter' rest = MakeEmitter (CfEmitter' rest)
type AnEmitter = MakeEmitter (CfEmitter' Unit)

makeOffsets :: forall rest. { time :: Number, headroomInSeconds :: Number, freq :: Number, playhead :: Number, rest :: Cofree Identity rest } -> { offsets :: Array { offset :: Number, rest :: rest }, playhead :: Number, rest :: Cofree Identity rest }
makeOffsets { time, headroomInSeconds, freq, playhead, rest }
  | time + (headroomInSeconds / freq) >= playhead = over (prop (Proxy :: Proxy "offsets"))
      (append [ { offset: playhead - time, rest: extract rest } ])
      (makeOffsets { time, headroomInSeconds, freq, playhead: playhead + 1.0 / freq, rest: unwrap $ unwrapCofree rest })
  | otherwise = { offsets: [], playhead, rest }

makeEmitter' :: forall rest. { startsAt :: Number, rest :: Cofree Identity rest } -> AnEmitter' rest
makeEmitter' { startsAt, rest: r } = go r startsAt

  where
  go rest playhead { time, headroomInSeconds, freq } =
    let
      o = makeOffsets { time, headroomInSeconds, freq, playhead, rest }
    in
      o.offsets :< go o.rest o.playhead

makeEmitter :: { startsAt :: Number } -> AnEmitter
makeEmitter { startsAt } = makeEmitter' { startsAt, rest: mempty }

fEmitter' :: { sensitivity :: Number } -> Number -> { time :: Number, headroomInSeconds :: Number } -> Maybe Number
fEmitter' { sensitivity } freq { time, headroomInSeconds } = if dist < sensitivity then Just (if tGap < (gap / 2.0) then 0.0 else (gap - tGap)) else Nothing
  where
  gap = 1.0 / freq

  dist = Math.abs ((time + headroomInSeconds) `Math.remainder` gap)

  tGap = time `Math.remainder` gap

fEmitter :: Number -> { time :: Number, headroomInSeconds :: Number } -> Maybe Number
fEmitter = fEmitter' { sensitivity: 0.04 }
