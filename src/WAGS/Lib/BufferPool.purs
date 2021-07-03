module WAGS.Lib.BufferPool where

import Prelude
import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array as A
import Data.Lens (_1, over)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Pos, toInt')
import Data.Vec as V
import Type.Proxy (Proxy(..))
import WAGS.Graph.AudioUnit (APOnOff, OnOff(..))
import WAGS.Graph.Parameter (AudioParameter, ff)
import WAGS.Lib.SFofT (SAPFofT, makePiecewise)

-- use array as it is faster
type TimeHeadroomOffsets
  = { time :: Number, headroom :: Number, offsets :: Array Number }

type TimeHeadroomRate
  = { time :: Number, headroom :: Number, rate :: Number }

type BufferInternal
  = { startedAt :: Number, env :: SAPFofT }

type Buffy
  = { gain :: AudioParameter, onOff :: APOnOff }

unchangingPiecewise :: NonEmpty List (Number /\ Number)
unchangingPiecewise = (0.0 /\ 1.0) :| Nil

type InternalState
  = { bufferInternal :: Maybe BufferInternal, buffy :: Buffy }

notPlaying :: { bufferInternal :: Maybe BufferInternal, buffy :: Buffy }
notPlaying = { bufferInternal: Nothing, buffy: { gain: pure 0.0, onOff: pure Off } }

type BuffyVec :: forall k. k -> Type
type BuffyVec n = V.Vec n Buffy

type BuffyStream :: forall k. k -> Type
type BuffyStream n = TimeHeadroomOffsets ->
  Cofree ((->) TimeHeadroomOffsets) (BuffyVec n)

bufferPool ::
  forall n.
  Pos n =>
  Maybe Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  TimeHeadroomOffsets ->
  Cofree ((->) TimeHeadroomOffsets) (BuffyVec n)
bufferPool dur pwf = go 0 (V.fill (const (Nothing :: Maybe BufferInternal)))
  where
  len :: Int
  len = toInt' (Proxy :: _ n)

  realPwf :: NonEmpty List (Number /\ Number)
  realPwf = fromMaybe unchangingPiecewise pwf

  refresh :: { time :: Number, headroom :: Number, offset :: Number } -> OnOff -> InternalState
  refresh { time, headroom, offset } onOff =
    let
      sapfot = makePiecewise (map (over _1 (add (time + offset))) realPwf)

      now = sapfot { time, headroom }
    in
      { bufferInternal: Just { startedAt: time + offset, env: tail now }, buffy: { gain: head now, onOff: ff offset (pure onOff) } }

  -- cPos myPos
  maybeBufferToGainOnOff ::
    Int ->
    TimeHeadroomOffsets ->
    Int ->
    Maybe BufferInternal ->
    InternalState
  maybeBufferToGainOnOff cPos { time, headroom, offsets } myPos Nothing
    | Just offset <- A.index offsets (myPos - cPos `mod` len) = refresh { time, headroom, offset } On
    | otherwise = notPlaying

  maybeBufferToGainOnOff cPos { time, headroom, offsets } myPos (Just { startedAt, env })
    | Just offset <- A.index offsets (myPos - cPos `mod` len) = refresh { time, headroom, offset } OffOn
    | Just dur' <- dur
    , time - startedAt > dur' = notPlaying
    | otherwise =
      let
        now = env { time, headroom }
      in
        { bufferInternal: Just { startedAt, env: tail now }, buffy: { gain: head now, onOff: pure On } }

  go ::
    Int ->
    V.Vec n (Maybe BufferInternal) ->
    TimeHeadroomOffsets ->
    Cofree ((->) TimeHeadroomOffsets) (V.Vec n Buffy)
  go cIdx v tht@{ offsets } = map _.buffy internalStates :< go ((cIdx + A.length offsets) `mod` len) (map _.bufferInternal internalStates)
    where
    internalStates = mapWithIndex (maybeBufferToGainOnOff cIdx tht) v
