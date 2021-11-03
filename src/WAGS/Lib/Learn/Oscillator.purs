module WAGS.Lib.Learn.Oscillator where

import Prelude

import Data.Newtype (over)
import Math (sin, pi, (%))
import WAGS.Lib.Learn.Pitch (Pitch(..))
import WAGS.Lib.Learn.Volume (Volume(..))
import WAGS.Math (calcSlope)

type Time = Number

lfo' :: (Number -> Number) -> { freq :: Number, amp :: Number, phase :: Number } -> Number -> Number
lfo' f { freq, amp, phase } time = amp * f (freq * (pi * time - phase))

-- | freq: frequency of the oscillator
-- | amp: amplitude of the oscillator
-- | phase: phase of the oscillator. 1 full phase would be 2.0 * pi
lfo :: { freq :: Number, amp :: Number, phase :: Number } -> Number -> Number
lfo = lfo' sin

halfPi = 0.5 * pi :: Number
threeHalvesPi = 1.5 * pi :: Number
twoPi = 2.0 * pi :: Number

saw' :: Number -> Number
saw' t' = o
  where
  t = t' % twoPi
  o
    | t < halfPi = calcSlope 0.0 0.0 halfPi 1.0 t
    | t < threeHalvesPi = calcSlope halfPi 1.0 threeHalvesPi (-1.0) t
    | otherwise = calcSlope threeHalvesPi (-1.0) twoPi 0.0 t

saw :: { freq :: Number, amp :: Number, phase :: Number } -> Number -> Number
saw = lfo' saw'

tremolo' :: forall f. Functor f => { freq :: Number, amp :: Number, phase :: Number } -> Volume f -> Volume f
tremolo' = over Volume <<< map <<< lfo

vibrato' :: forall f. Functor f => { freq :: Number, amp :: Number, phase :: Number } -> Pitch f -> Pitch f
vibrato' = over Pitch <<< map <<< lfo
