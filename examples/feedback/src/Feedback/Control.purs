module Feedback.Control where

import Prelude

import Data.Int (toNumber)
import Data.Lens (over)
import Data.Lens.Grate (grate)
import Data.Newtype (wrap)
import Data.Profunctor.Closed (class Closed)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen.HTML as HH
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Feedback.Types (Instructions)


data T2 = T2_0 | T2_1
data T3 = T3_0 | T3_1 | T3_2
data T4 = T4_0 | T4_1 | T4_2 | T4_3
data T5 = T5_0 | T5_1 | T5_2 | T5_3 | T5_4
data T6 = T6_0 | T6_1 | T6_2 | T6_3 | T6_4

data Rect = Rect Int Int Int Int

data Control
  = Slider Rect Color Number
  | DiscreteChooser Rect Color Int Int
  | T2 Rect Color T2
  | T3 Rect Color T3
  | T4 Rect Color T4
  | T5 Rect Color T5
  | T6 Rect Color T6
  | Pad Rect Color Number
  | Source Rect Color

plainc = RGB 100 100 100 :: Color
focusc = RGB 10 10 10 :: Color

elts :: { | Instructions Control }
elts =
  { -- press to grow or shrink a pad
    triggerPad: Pad (Rect 60 60 240 240) focusc 0.0
  -- periodic wave in mix
  , togglePadOsc0: T2 (Rect 400 60 60 60) focusc T2_0
  -- triangle wave in mix
  , togglePadOsc1: T2 (Rect 540 940 60 60) focusc T2_0
  -- sawtooth wave in mix
  , togglePadOsc2: T2 (Rect 270 690 60 60) focusc T2_0
  -- square in mix
  , togglePadOsc3: T2 (Rect 0 60 60 60) focusc T2_0
  -- high sine in mix
  , togglePadOsc4: T2 (Rect 420 940 60 60) focusc T2_0
  -- three detuning factors + "normal" harmonic series
  , detunePad: T4 (Rect 800 690 130 100) focusc T4_0
  -- 0th lfo on the pad gain
  , gainLFO0Pad: Slider (Rect 90 810 450 60) focusc 0.0
  -- 1st lfo on the pad gain, fatter & more sluggish
  , gainLFO1Pad: Slider (Rect 630 690 90 180) focusc 0.0
  -- pad filter bank chooser
  , filterBankChooserPad: T5 (Rect 400 180 120 120) focusc T5_0 --
  -- waveshaper on the pad
  , waveshaperPad: Slider (Rect 800 0 200 70) focusc 0.0
  -- trigger synth
  , triggerLead: Source (Rect 720 790 210 210) focusc
  -- the synth we use for the lead
  , synthForLead: T6 (Rect 590 460 340 130) focusc T6_0
  -- choose which pitch out of 14 to start at
  , pitchLead: DiscreteChooser (Rect 930 70 70 520) focusc 14 0
  -- 0th delay line for the lead synth
  , leadDelayLine0: T2 (Rect 0 240 60 60) focusc T2_0
  -- 1st delay line for the lead synth
  , leadDelayLine1: T2 (Rect 460 120 60 60) focusc T2_0
  -- 2nd delay line for the lead synth
  , leadDelayLine2: T2 (Rect 90 690 60 60) focusc T2_0
  -- combined delay line for the lead synth
  , leadCombinedDelay0: T2 (Rect 300 0 60 60) focusc T2_0
  -- shifts intensities of delays
  , leadDelayGainCarousel: Slider (Rect 590 590 410 100) focusc 0.0
  -- n pitches played when pressed (fixed sequence always based on start)
  , nPitchesPlayedLead: DiscreteChooser (Rect 90 300 90 390) focusc 7 0
  -- one of three envelopes
  , envelopeLead: T3 (Rect 660 0 140 140) focusc T3_0
  -- octave shift
  , octaveLead: T3 (Rect 800 70 60 70) focusc T3_0
  -- pad for a buffer
  , drone: Pad (Rect 270 390 320 300) focusc 0.0
  -- choose drone
  , droneChooser: T5 (Rect 390 690 150 120) focusc T5_0
  -- lowpass q for drone
  , droneLowpass0Q: Slider (Rect 540 690 90 180) focusc 0.0
  -- q of bandpass filter
  , droneBandpass0Q: Slider (Rect 0 940 300 60) focusc 0.0
  -- lfo controlling freq of bandpass
  , droneBandpass0LFO: Slider (Rect 520 60 70 240) focusc 0.0
  -- q of bandpass filter
  , droneBandpass1Q: Slider (Rect 930 690 70 310) focusc 0.0
  -- lfo of bandpass filter
  , droneBandpass1LFO: Slider (Rect 180 300 90 260) focusc 0.0
  -- q of highpass filter
  , droneHighpass0Q: Slider (Rect 860 70 70 200) focusc 0.0
  -- lfo of highpass filter
  , droneHighpass0LFO: Slider (Rect 360 0 300 60) focusc 0.0
  -- how long does it take for the drone to ramp up?
  , droneActivationEnergyThreshold: Slider (Rect 90 750 300 60) focusc 0.0
  -- looping pwf
  , droneRhythmicLoopingPiecewiseFunction: T5 (Rect 660 340 270 120) focusc T5_0
  -- how long does the drone linger?
  , droneDecay: Slider (Rect 590 60 70 400) focusc 0.0
  -- flange on the drone
  , droneFlange: T2 (Rect 60 0 60 60) focusc T2_0
  -- a single sample
  , sampleOneShot: Source (Rect 660 140 200 200) focusc
  -- reverse the samples?
  , sampleReverse: T2 (Rect 400 120 60 60) focusc T2_0
  -- choose which sample to play
  , sampleChooser: DiscreteChooser (Rect 0 300 90 640) focusc 24 0
  , sampleChorusEffect: T2 (Rect 120 0 60 60) focusc T2_0
  , sampleRateChange: T3 (Rect 720 690 80 100) focusc T3_0
  -- 0th delay line for the single sample
  , sampleDelayLine0: T2 (Rect 0 0 60 60) focusc T2_0
  -- 1st delay line for the single sample
  , sampleDelayLine1: T2 (Rect 150 690 60 60) focusc T2_0
  -- 2nd delay line for the single sample
  , sampleDelayLine2: T2 (Rect 860 270 70 70) focusc T2_0
  -- 0th sample combined delay line
  , sampleCombinedDelayLine0: T2 (Rect 480 940 60 60) focusc T2_0
  -- changes intensities of various delay lines
  , sampleDelayGainCarousel: Slider (Rect 270 300 320 90) focusc 0.0
  -- 0th delay line for combined lead sample
  , leadSampleDelayLine0: T2 (Rect 660 940 60 60) focusc T2_0
  -- 1st delay line for combined lead sample
  , leadSampleDelayLine1: T2 (Rect 330 690 60 60) focusc T2_0
  -- 2nd delay line for combined lead sample
  , leadSampleDelayLine2: T2 (Rect 0 180 60 60) focusc T2_0
  -- alternates between any looping buffers that are currently playing
  , loopingBufferGainDJ: T2 (Rect 240 0 60 60) focusc T2_0
  -- how close the start/end times are
  , loopingBufferStartEndConstriction: Slider (Rect 300 160 100 140) focusc 0.0
  -- 0th looping buffer
  , loopingBuffer0: T2 (Rect 180 0 60 60) focusc T2_0
  -- 1st looping buffer
  , loopingBuffer1: T2 (Rect 210 690 60 60) focusc T2_0
  -- 2nd looping buffer
  , loopingBuffer2: T2 (Rect 360 940 60 60) focusc T2_0
  -- 3rd looping buffer
  , loopingBuffer3: T2 (Rect 300 940 60 60) focusc T2_0
  -- 4th looping buffer
  , loopingBuffer4: T2 (Rect 0 120 60 60) focusc T2_0
  -- substitutes entirely different sets of base parameters
  , radicalFlip: T2 (Rect 460 60 60 60) focusc T2_0
  -- global pan extravaganza
  , greatAndMightyPan: Slider (Rect 90 870 630 70) focusc 0.0
  -- global delay
  , globalDelay: T2 (Rect 600 940 60 60) focusc T2_0
  -- echoing uncontrollable singleton
  , echoingUncontrollableSingleton: Source (Rect 300 60 100 100) focusc
  , distantBellsFader: Slider (Rect 180 560 90 130) focusc 0.0
  }

great :: forall i o p. Closed p => p i o -> p (i /\ i) (o /\ o)
great = grate ((/\) <$> ((#) fst) <*> ((#) snd))

tnt :: Int /\ Int -> Number /\ Number
tnt = over great toNumber

c2s :: forall i w. Control -> Array (HH.HTML i w)
c2s (Slider (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s (DiscreteChooser (Rect x y w h) color _ _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s (T2 (Rect x y w h) color t) =
  [ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
  ] <> case t of
    T2_0 -> []
    T2_1 -> [ SE.circle [ SA.cx $ (toNumber x + (toNumber w / 2.0)), SA.cy $ (toNumber y + (toNumber h / 2.0)), SA.r $ toNumber ((min w h) - 20) / 2.0, SA.fill (RGB 10 10 10), SA.stroke (RGB 4 4 4) ] ]
c2s (T3 (Rect x y w h) color t) = [ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ] ] <> case t of
  T3_0 -> []
  T3_1 -> [ SE.polygon [ HH.attr (wrap "points") (show x <> "," <> show y <> " " <> show (x + w) <> "," <> show y <> " " <> show (x + w) <> "," <> show (y + h) <> " ") ] ]
  T3_2 -> [ SE.polygon [ HH.attr (wrap "points") (show x <> "," <> show y <> " " <> show x <> "," <> show (y + h) <> " " <> show (x + w) <> "," <> show (y + h) <> " ") ] ]
c2s (T4 (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s (T5 (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s (T6 (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s (Pad (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s (Source (Rect x y w h) color) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
