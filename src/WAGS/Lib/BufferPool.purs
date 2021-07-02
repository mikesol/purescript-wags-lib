module BufferPool where

import Control.Comonad.Cofree (Cofree)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (class Pos)
import Data.Vec as V
import WAGS.Graph.AudioUnit (APOnOff)
import WAGS.Graph.Parameter (AudioParameter)

type TimeHeadroomTriggered
  = { time :: Number, headroom :: Number, triggered :: Boolean }

type TimeHeadroomRate
  = { time :: Number, headroom :: Number, rate :: Number }

type Buffy = { gain :: AudioParameter, onOff :: APOnOff }

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

-- | pwf: piecewise function to apply to gain
makeTriggeredPool ::
  forall n.
  Pos n =>
  Maybe (NonEmpty List (Number /\ Number)) ->
  TimeHeadroomTriggered ->
  Cofree ((->) TimeHeadroomTriggered) (V.Vec n Buffy)
makeTriggeredPool pwf = ?hole