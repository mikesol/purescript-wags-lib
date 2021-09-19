module Wags.Learn.Oscillator where

import Prelude

import Data.Newtype (over)
import Math (sin, pi)
import WAGS.Lib.Learn.Pitch (Pitch(..))
import WAGS.Lib.Learn.Volume (Volume(..))

type Time = Number

-- | freq: frequency of the oscillator
-- | amp: amplitude of the oscillator
-- | phase: phase of the oscillator. 1 full phase would be 2.0 * pi
lfo :: { freq :: Number, amp :: Number, phase :: Number } -> Number -> Number
lfo { freq, amp, phase } time = amp * sin (freq * (pi * time - phase))

tremolo' :: { freq :: Number, amp :: Number, phase :: Number } -> Volume -> Volume
tremolo' = over Volume <<< lfo

vibrato' :: { freq :: Number, amp :: Number, phase :: Number } -> Pitch -> Pitch
vibrato' = over Pitch <<< lfo