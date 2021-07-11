-- | An emitter is an impulse that is emitted once in the case of comonads or as few times as possible in the case of functions.
module WAGS.Lib.Emitter where

import Prelude
import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (class ComonadCofree, unwrapCofree)
import Control.Extend (class Extend)
import Data.Array as A
import Data.Int (toNumber)
import Data.Int as DInt
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple.Nested ((/\), type (/\))
import Math (floor)
import Math as Math
import WAGS.Lib.Cofree (class Actualize)
import WAGS.Run (SceneI(..))

-------
newtype MakeEmitter a
  = MakeEmitter ({ time :: Number, headroom :: Number, rate :: Number } -> a)

derive instance newtypeFofTimeEmitter :: Newtype (MakeEmitter a) _

derive instance functorFofTimeEmitter :: Functor MakeEmitter

newtype Emission
  = Emission (Array Number)

derive instance newtypeEmission :: Newtype Emission _

newtype CfEmitter f a
  = CfEmitter (Cofree f a)

derive instance newtypeCfEmitter :: Newtype (CfEmitter MakeEmitter Emission) _

derive newtype instance extendCfEmitter :: Extend (CfEmitter MakeEmitter)

derive newtype instance comonadCfEmitter :: Comonad (CfEmitter MakeEmitter)

derive newtype instance comonadCofreeCfEmitter :: ComonadCofree MakeEmitter (CfEmitter MakeEmitter)

type AnEmitter
  = MakeEmitter (CfEmitter MakeEmitter Emission)

consumeLookahead :: { tnow :: Number, lookahead :: Number, rate :: Number } -> Array Number
consumeLookahead { lookahead, tnow, rate }
  | tnow <= lookahead =
    consumeLookahead { lookahead: lookahead - 1.0, tnow, rate }
      <> [ (lookahead - tnow) / rate ]
  | otherwise = []

makeOffsets ::
  { tnow :: Number
  , headroom :: Number
  , clearedSoFar :: Number
  , rate :: Number
  } ->
  Array Number /\ Number
makeOffsets { tnow, headroom, clearedSoFar, rate } =
  ( A.replicate urgent 0.0
      <> if lookahead <= clearedSoFar then [] else consumeLookahead { lookahead, tnow, rate }
  )
    /\ lookahead
  where
  urgent = DInt.floor (tnow - clearedSoFar)

  lookahead = floor (tnow + headroom * rate)

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

  go clearedSoFar n i { time, headroom, rate } =
    let
      tnow = (time - i) * rate + n

      offsets /\ newCleared = makeOffsets { tnow, headroom, clearedSoFar, rate }
    in
      wrap ((wrap offsets) :< map unwrap (wrap (go newCleared tnow time)))

instance semigroupCfEmitter :: Semigroup (CfEmitter MakeEmitter Emission) where
  append f0i f1i =
    let
      hd = wrap ((unwrap (extract f0i) <> unwrap (extract f0i)))

      tl = unwrapCofree f0i <> unwrapCofree f1i
    in
      wrap (hd :< map unwrap tl)

instance semigroupAEmitter :: Semigroup AnEmitter where
  append (MakeEmitter f0) (MakeEmitter f1) = MakeEmitter \tr -> f0 tr <> f1 tr

instance monoidAEmitter :: Monoid AnEmitter where
  mempty = makeEmitter { startsAt: 0.0, prevTime: 0.0 }

fEmitter' :: { sensitivity :: Number } -> Number -> { time :: Number, headroom :: Number } -> Maybe Number
fEmitter' { sensitivity } gapInSeconds { time, headroom } = if dist < sensitivity then Just (if tGapInSeconds < (gapInSeconds / 2.0) then 0.0 else (gapInSeconds - tGapInSeconds)) else Nothing
  where
  dist = Math.abs ((time + headroom) `Math.remainder` gapInSeconds)

  tGapInSeconds = time `Math.remainder` gapInSeconds

fEmitter :: Number -> { time :: Number, headroom :: Number } -> Maybe Number
fEmitter = fEmitter' { sensitivity: 0.04 }

instance actualizeEmitter :: Actualize AnEmitter (SceneI a b) Number (CfEmitter MakeEmitter Emission) where
  actualize (MakeEmitter r) (SceneI { time, headroom }) rate = r { time, headroom: toNumber headroom / 1000.0, rate }
