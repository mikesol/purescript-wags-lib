module BufferPool where

import Prelude

import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (_1, over)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Pos)
import Data.Vec as V
import WAGS.Graph.AudioUnit (APOnOff, OnOff(..))
import WAGS.Graph.Parameter (AudioParameter)
import WAGS.Lib.SFofT (SAPFofT, makePiecewise)

type TimeHeadroomTriggered
  = { time :: Number, headroom :: Number, triggered :: Boolean }

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

makeTriggeredPool ::
  forall n.
  Pos n =>
  Number ->
  Maybe (NonEmpty List (Number /\ Number)) ->
  TimeHeadroomTriggered ->
  Cofree ((->) TimeHeadroomTriggered) (V.Vec n Buffy)
makeTriggeredPool dur pwf = go 0 (V.fill (const (Nothing :: Maybe BufferInternal)))
  where
  notPlaying :: { bufferInternal :: Maybe BufferInternal, buffy :: Buffy }
  notPlaying =  { bufferInternal: Nothing, buffy: { gain: pure 0.0, onOff: pure Off } }
  realPwf = fromMaybe unchangingPiecewise pwf
  refresh :: Number -> Number -> OnOff -> InternalState
  refresh time headroom onOff =
      let
        sapfot =  makePiecewise (map (over _1 (add time)) realPwf)
        now = sapfot { time, headroom }
      in { bufferInternal: Just { startedAt: time, env: tail now }, buffy: { gain: head now, onOff: pure onOff } }
  -- cPos myPos
  maybeBufferToGainOnOff ::
    Int ->
    TimeHeadroomTriggered ->
    Int ->
    Maybe BufferInternal ->
    InternalState
  maybeBufferToGainOnOff cPos { time, headroom, triggered } myPos Nothing
    | cPos /= myPos = notPlaying
    | not triggered = notPlaying
    | otherwise = refresh time headroom On
  maybeBufferToGainOnOff cPos { time, headroom, triggered } myPos (Just { startedAt, env })
    | triggered && cPos == myPos = refresh time headroom OffOn
    | time - startedAt > dur = notPlaying
    | otherwise =
      let
        now = env { time, headroom }
      in { bufferInternal: Just { startedAt, env: tail now }, buffy: { gain: head now, onOff: pure On } }
  go ::
    Int ->
    V.Vec n (Maybe BufferInternal) ->
    TimeHeadroomTriggered ->
    Cofree ((->) TimeHeadroomTriggered) (V.Vec n Buffy)
  go cIdx v tht@{ triggered } = map _.buffy internalStates :< go (if triggered then cIdx + 1 else cIdx) (map _.bufferInternal internalStates)
    where
    internalStates = mapWithIndex (maybeBufferToGainOnOff cIdx tht) v