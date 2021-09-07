-- | An emitter is an impulse that is emitted once in the case of comonads or as few times as possible in the case of functions.
module WAGS.Lib.Emitter where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Control.Extend (class Extend)
import Data.Array as A
import Data.Int as DInt
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple.Nested ((/\), type (/\))
import Math (floor)
import Math as Math
import WAGS.Lib.Cofree (class Actualize, class ActualizeE, actualizeE)
import WAGS.Run (SceneI(..))

-------
newtype MakeEmitter a
  = MakeEmitter ({ time :: Number, headroom :: Number, freq :: Number } -> a)

derive instance newtypeFofTimeEmitter :: Newtype (MakeEmitter a) _

derive instance functorFofTimeEmitter :: Functor MakeEmitter

type Emission
  = Array Number

newtype CfEmitter f a
  = CfEmitter (Cofree f a)

derive instance newtypeCfEmitter :: Newtype (CfEmitter MakeEmitter Emission) _

derive newtype instance extendCfEmitter :: Extend (CfEmitter MakeEmitter)

derive newtype instance comonadCfEmitter :: Comonad (CfEmitter MakeEmitter)

derive newtype instance comonadCofreeCfEmitter :: ComonadCofree MakeEmitter (CfEmitter MakeEmitter)

type AnEmitter
  = MakeEmitter (CfEmitter MakeEmitter Emission)

consumeLookahead :: { tnow :: Number, lookahead :: Number, freq :: Number } -> Array Number
consumeLookahead { lookahead, tnow, freq }
  | tnow <= lookahead =
    consumeLookahead { lookahead: lookahead - 1.0, tnow, freq }
      <> [ (lookahead - tnow) / freq ]
  | otherwise = []

makeOffsets ::
  { tnow :: Number
  , headroom :: Number
  , clearedSoFar :: Number
  , freq :: Number
  } ->
  Array Number /\ Number
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
  wrap
    ( go
        (if floorStartsAt == startsAt then startsAt - 1.0 else startsAt)
        startsAt
        prevTime
    )
  where
  floorStartsAt = floor startsAt

  go clearedSoFar n i { time, headroom, freq } =
    let
      tnow = (time - i) * freq + n

      offsets /\ newCleared = makeOffsets { tnow, headroom, clearedSoFar, freq }
    in
      wrap (offsets :< map unwrap (wrap (go newCleared tnow time)))

instance semigroupCfEmitter :: Semigroup (CfEmitter MakeEmitter Emission) where
  append f0i f1i =
    let
      hd = extract f0i <> extract f1i

      tl = unwrapCofree f0i <> unwrapCofree f1i
    in
      wrap (hd :< map unwrap tl)

instance semigroupAEmitter :: Semigroup AnEmitter where
  append (MakeEmitter f0) (MakeEmitter f1) = MakeEmitter \tr -> f0 tr <> f1 tr

instance monoidAEmitter :: Monoid AnEmitter where
  mempty = makeEmitter { startsAt: 0.0, prevTime: 0.0 }

fEmitter' :: { sensitivity :: Number } -> Number -> { time :: Number, headroom :: Number } -> Maybe Number
fEmitter' { sensitivity } freq { time, headroom } = if dist < sensitivity then Just (if tGap < (gap / 2.0) then 0.0 else (gap - tGap)) else Nothing
  where
  gap = 1.0 / freq

  dist = Math.abs ((time + headroom) `Math.remainder` gap)

  tGap = time `Math.remainder` gap

fEmitter :: Number -> { time :: Number, headroom :: Number } -> Maybe Number
fEmitter = fEmitter' { sensitivity: 0.04 }

instance actualizeEmitter :: Actualize (MakeEmitter out) (SceneI a b c) Number out where
  actualize = actualizeE

instance actualizeEmitterE :: ActualizeE (MakeEmitter) (SceneI a b c) Number where
  actualizeE (MakeEmitter r) (SceneI { time, headroomInSeconds: headroom }) freq = r { time, headroom, freq }
