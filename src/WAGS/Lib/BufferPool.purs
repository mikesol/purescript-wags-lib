module BufferPool where

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


type Buffy = { gain :: AudioParameter, onOff :: APOnOff }

unchangingPiecewise :: NonEmpty List (Number /\ Number)
unchangingPiecewise = (0.0 /\ 1.0) :| Nil
{-
-- | durAtUnitRate: at a rate of 1.0, how long until we turn on the next unit?
-- | pwf: piecewise function to apply to gain
-- | initialTime: when does this loop start in time?
makeLoopingPool ::
  forall n.
  Pos n =>
  Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  Number ->
  TimeHeadroomRate ->
  Cofree ((->) TimeHeadroomRate) (V.Vec n Buffy)
makeLoopingPool durAtUnitRate pwf initialTime = ?hole
-}
-- | pwf: piecewise function to apply to gain

type InternalState = { bufferInternal :: Maybe BufferInternal, buffy :: Buffy }

makeOffsetsPool ::
  forall n.
  Pos n =>
  Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  TimeHeadroomOffsets ->
  Cofree ((->) TimeHeadroomOffsets) (V.Vec n Buffy)
makeOffsetsPool dur pwf = go 0 (V.fill (const (Nothing :: Maybe BufferInternal)))
  where
  len :: Int
  len = toInt' (Proxy :: _ n)
  notPlaying :: { bufferInternal :: Maybe BufferInternal, buffy :: Buffy }
  notPlaying =  { bufferInternal: Nothing, buffy: { gain: pure 0.0, onOff: pure Off } }
  realPwf = fromMaybe unchangingPiecewise pwf
  refresh :: { time :: Number, headroom :: Number, offset :: Number } -> OnOff -> InternalState
  refresh { time, headroom, offset } onOff =
      let
        sapfot =  makePiecewise (map (over _1 (add (time + offset))) realPwf)
        now = sapfot { time, headroom }
      in { bufferInternal: Just { startedAt: time + offset, env: tail now }, buffy: { gain: head now, onOff: ff offset (pure onOff) } }
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
    | time - startedAt > dur = notPlaying
    | otherwise =
      let
        now = env { time, headroom }
      in { bufferInternal: Just { startedAt, env: tail now }, buffy: { gain: head now, onOff: pure On } }
  go ::
    Int ->
    V.Vec n (Maybe BufferInternal) ->
    TimeHeadroomOffsets ->
    Cofree ((->) TimeHeadroomOffsets) (V.Vec n Buffy)
  go cIdx v tht@{ offsets } = map _.buffy internalStates :< go ((cIdx + A.length offsets) `mod` len) (map _.bufferInternal internalStates)
    where
    internalStates = mapWithIndex (maybeBufferToGainOnOff cIdx tht) v