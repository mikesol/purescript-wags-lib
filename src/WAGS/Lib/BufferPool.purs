module WAGS.Lib.BufferPool where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Data.Array as A
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (_1, over)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Pos, toInt')
import Data.Vec as V
import Type.Proxy (Proxy(..))
import WAGS.Graph.AudioUnit (OnOff(..), APOnOff)
import WAGS.Graph.Parameter (AudioParameter, ff)
import WAGS.Lib.SFofT (SAPFofT, makePiecewise)

-- use array as it is faster
type TimeHeadroomOffsets (rest :: Type)
  = { time :: Number, headroom :: Number, offsets :: Array { offset :: Number, rest :: rest } }

type TimeHeadroomRate
  = { time :: Number, headroom :: Number, rate :: Number }

type BufferInternal (rest :: Type)
  = { startedAt :: Number, env :: SAPFofT, rest :: rest }

type Buffy (rest :: Type)
  = { gain :: AudioParameter, onOff :: APOnOff, rest :: rest }

unchangingPiecewise :: NonEmpty List (Number /\ Number)
unchangingPiecewise = (0.0 /\ 1.0) :| Nil

type InternalState (rest :: Type)
  = { bufferInternal :: Maybe (BufferInternal rest), buffy :: Maybe (Buffy rest) }

notPlaying :: forall (rest :: Type). InternalState rest
notPlaying = { bufferInternal: Nothing, buffy: Nothing }

type BuffyVec (n :: Type) (rest :: Type) = V.Vec n (Maybe (Buffy rest))

type BuffyStream (n :: Type) (rest :: Type) = TimeHeadroomOffsets rest ->
  Cofree ((->) (TimeHeadroomOffsets rest)) (BuffyVec n rest)

bufferPool ::
  forall n r.
  Pos n =>
  Maybe Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  TimeHeadroomOffsets r ->
  Cofree ((->) (TimeHeadroomOffsets r)) (BuffyVec n r)
bufferPool dur pwf = go 0 (V.fill (const (Nothing :: Maybe (BufferInternal r))))
  where
  len :: Int
  len = toInt' (Proxy :: _ n)

  realPwf :: NonEmpty List (Number /\ Number)
  realPwf = fromMaybe unchangingPiecewise pwf

  refresh :: { time :: Number, headroom :: Number, offset :: Number, rest :: r } -> OnOff -> InternalState r
  refresh { time, headroom, offset, rest } onOff =
    let
      sapfot = makePiecewise (map (over _1 (add (time + offset))) realPwf)

      now = sapfot { time, headroom }
    in
      { bufferInternal: Just { startedAt: time + offset, env: tail now, rest }, buffy: Just { gain: head now, onOff: ff offset (pure onOff), rest } }

  -- cPos myPos
  maybeBufferToGainOnOff ::
    Int ->
    TimeHeadroomOffsets r ->
    Int ->
    Maybe (BufferInternal r) ->
    InternalState r
  maybeBufferToGainOnOff cPos { time, headroom, offsets } myPos Nothing
    | Just {offset, rest} <- A.index offsets (myPos - cPos `mod` len) = refresh { time, headroom, offset, rest } On
    | otherwise = notPlaying

  maybeBufferToGainOnOff cPos { time, headroom, offsets } myPos (Just { startedAt, env, rest: prevRest })
    | Just {offset, rest} <- A.index offsets (myPos - cPos `mod` len) = refresh { time, headroom, offset, rest } OffOn
    | Just dur' <- dur
    , time - startedAt > dur' = notPlaying
    | otherwise =
      let
        now = env { time, headroom }
      in
        { bufferInternal: Just { startedAt, env: tail now, rest: prevRest }, buffy: Just { gain: head now, onOff: pure On, rest: prevRest } }

  go ::
    Int ->
    V.Vec n (Maybe (BufferInternal r)) ->
    TimeHeadroomOffsets r ->
    Cofree ((->) (TimeHeadroomOffsets r)) (V.Vec n (Maybe (Buffy r)))
  go cIdx v tht@{ offsets } = map _.buffy internalStates :< go ((cIdx + A.length offsets) `mod` len) (map _.bufferInternal internalStates)
    where
    internalStates = mapWithIndex (maybeBufferToGainOnOff cIdx tht) v

bGain :: forall r. Maybe (Buffy r) -> AudioParameter
bGain = maybe (pure 0.0) _.gain

bOnOff :: forall r. Maybe (Buffy r) -> APOnOff
bOnOff = maybe (pure Off) _.onOff