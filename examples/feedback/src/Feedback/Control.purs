module Feedback.Control where

import Prelude

import Data.Array ((..))
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Data.Profunctor (lcmap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Feedback.Types (Instructions)
import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.HalogenM (SubscriptionId)
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Math (floor)
import WAGS.WebAPI (AudioContext)
import Web.UIEvent.MouseEvent (MouseEvent)

data T2 = T2_0 | T2_1
data T3 = T3_0 | T3_1 | T3_2
data T4 = T4_0 | T4_1 | T4_2 | T4_3
data T5 = T5_0 | T5_1 | T5_2 | T5_3 | T5_4
data T6 = T6_0 | T6_1 | T6_2 | T6_3 | T6_4

data Rect = Rect Int Int Int Int

data SliderAction
  = SliderDown Number
  | SliderMove Number
  | SliderRemoteMove Number
  | SliderUp

data Control
  = Slider (SliderAction -> Action) (State -> Number) Rect Color Color
  | DiscreteChooser (SliderAction -> Action) (State -> Number) Rect Color Color Int
  | T2 (T2 -> Action) (State -> T2) Rect Color
  | T3 Rect Color T3
  | T4 Rect Color T4
  | T5 Rect Color T5
  | T6 Rect Color T6
  | Pad Rect Color Number
  | Source Action Rect Color

data Action
  = StartAudio
  | StopAudio
  | StubDeleteMe
  --
  | GainLFO0Pad SliderAction
  | GainLFO1Pad SliderAction
  | WaveshaperPad SliderAction
  | PitchLead SliderAction
  | LeadDelayGainCarousel SliderAction
  | NPitchesPlayedLead SliderAction
  | DroneLowpass0Q SliderAction
  | DroneBandpass0Q SliderAction
  | DroneBandpass0LFO SliderAction
  | DroneBandpass1Q SliderAction
  | DroneBandpass1LFO SliderAction
  | DroneHighpass0Q SliderAction
  | DroneHighpass0LFO SliderAction
  | DroneActivationEnergyThreshold SliderAction
  | DroneDecay SliderAction
  | SampleChooser SliderAction
  | SampleDelayGainCarousel SliderAction
  | LoopingBufferStartEndConstriction SliderAction
  | GreatAndMightyPan SliderAction
  | DistantBellsFader SliderAction
  --
  | TogglePadOsc0 T2
  | TogglePadOsc1 T2
  | TogglePadOsc2 T2
  | TogglePadOsc3 T2
  | TogglePadOsc4 T2
  | LeadDelayLine0 T2
  | LeadDelayLine1 T2
  | LeadDelayLine2 T2
  | LeadCombinedDelay0 T2
  | DroneFlange T2
  | SampleReverse T2
  | SampleChorusEffect T2
  | SampleDelayLine0 T2
  | SampleDelayLine1 T2
  | SampleDelayLine2 T2
  | SampleCombinedDelayLine0 T2
  | LeadSampleDelayLine0 T2
  | LeadSampleDelayLine1 T2
  | LeadSampleDelayLine2 T2
  | LoopingBufferGainDJ T2
  | LoopingBuffer0 T2
  | LoopingBuffer1 T2
  | LoopingBuffer2 T2
  | LoopingBuffer3 T2
  | LoopingBuffer4 T2
  | RadicalFlip T2
  | GlobalDelay T2
  | TriggerLead
  | SampleOneShot
  | UncontrollableSingleton

type State =
  { unsubscribe :: Effect Unit
  , unsubscribeHalogen :: Maybe SubscriptionId
  , audioCtx :: Maybe AudioContext
  , interactions ::
      { gainLFO0Pad :: Number
      , gainLFO0PadDown :: Boolean
      , gainLFO1Pad :: Number
      , gainLFO1PadDown :: Boolean
      , waveshaperPad :: Number
      , waveshaperPadDown :: Boolean
      , pitchLead :: Number
      , pitchLeadDown :: Boolean
      , leadDelayGainCarousel :: Number
      , leadDelayGainCarouselDown :: Boolean
      , nPitchesPlayedLead :: Number
      , nPitchesPlayedLeadDown :: Boolean
      , droneLowpass0Q :: Number
      , droneLowpass0QDown :: Boolean
      , droneBandpass0Q :: Number
      , droneBandpass0QDown :: Boolean
      , droneBandpass0LFO :: Number
      , droneBandpass0LFODown :: Boolean
      , droneBandpass1Q :: Number
      , droneBandpass1QDown :: Boolean
      , droneBandpass1LFO :: Number
      , droneBandpass1LFODown :: Boolean
      , droneHighpass0Q :: Number
      , droneHighpass0QDown :: Boolean
      , droneHighpass0LFO :: Number
      , droneHighpass0LFODown :: Boolean
      , droneActivationEnergyThreshold :: Number
      , droneActivationEnergyThresholdDown :: Boolean
      , droneDecay :: Number
      , droneDecayDown :: Boolean
      , sampleChooser :: Number
      , sampleChooserDown :: Boolean
      , sampleDelayGainCarousel :: Number
      , sampleDelayGainCarouselDown :: Boolean
      , loopingBufferStartEndConstriction :: Number
      , loopingBufferStartEndConstrictionDown :: Boolean
      , greatAndMightyPan :: Number
      , greatAndMightyPanDown :: Boolean
      , distantBellsFader :: Number
      , distantBellsFaderDown :: Boolean
      --
      , togglePadOsc0 :: T2
      , togglePadOsc1 :: T2
      , togglePadOsc2 :: T2
      , togglePadOsc3 :: T2
      , togglePadOsc4 :: T2
      , leadDelayLine0 :: T2
      , leadDelayLine1 :: T2
      , leadDelayLine2 :: T2
      , leadCombinedDelay0 :: T2
      , droneFlange :: T2
      , sampleReverse :: T2
      , sampleChorusEffect :: T2
      , sampleDelayLine0 :: T2
      , sampleDelayLine1 :: T2
      , sampleDelayLine2 :: T2
      , sampleCombinedDelayLine0 :: T2
      , leadSampleDelayLine0 :: T2
      , leadSampleDelayLine1 :: T2
      , leadSampleDelayLine2 :: T2
      , loopingBufferGainDJ :: T2
      , loopingBuffer0 :: T2
      , loopingBuffer1 :: T2
      , loopingBuffer2 :: T2
      , loopingBuffer3 :: T2
      , loopingBuffer4 :: T2
      , radicalFlip :: T2
      , globalDelay :: T2
      }
  }

plainc = RGB 100 100 100 :: Color
focusc = RGB 10 10 10 :: Color
backgroundc = RGB 100 100 100 :: Color
foregroundc = RGB 10 10 10 :: Color

elts :: { | Instructions Control }
elts =
  { -- press to grow or shrink a pad
    triggerPad: Pad (Rect 60 60 240 240) focusc 0.0
  -- periodic wave in mix
  , togglePadOsc0: T2 TogglePadOsc0 _.interactions.togglePadOsc0 (Rect 400 60 60 60) focusc
  -- triangle wave in mix
  , togglePadOsc1: T2 TogglePadOsc1 _.interactions.togglePadOsc1 (Rect 540 940 60 60) focusc
  -- sawtooth wave in mix
  , togglePadOsc2: T2 TogglePadOsc2 _.interactions.togglePadOsc2 (Rect 270 690 60 60) focusc
  -- square in mix
  , togglePadOsc3: T2 TogglePadOsc3 _.interactions.togglePadOsc3 (Rect 0 60 60 60) focusc
  -- high sine in mix
  , togglePadOsc4: T2 TogglePadOsc4 _.interactions.togglePadOsc4 (Rect 420 940 60 60) focusc
  -- three detuning factors + "normal" harmonic series
  , detunePad: T4 (Rect 800 690 130 100) focusc T4_0
  -- 0th lfo on the pad gain
  , gainLFO0Pad: Slider GainLFO0Pad _.interactions.gainLFO0Pad (Rect 90 810 450 60) backgroundc foregroundc
  -- 1st lfo on the pad gain, fatter & more sluggish
  , gainLFO1Pad: Slider GainLFO1Pad _.interactions.gainLFO1Pad (Rect 630 690 90 180) backgroundc foregroundc
  -- pad filter bank chooser
  , filterBankChooserPad: T5 (Rect 400 180 120 120) focusc T5_0 --
  -- waveshaper on the pad
  , waveshaperPad: Slider WaveshaperPad _.interactions.waveshaperPad (Rect 800 0 200 70) backgroundc foregroundc
  -- trigger synth
  , triggerLead: Source TriggerLead (Rect 720 790 210 210) focusc
  -- the synth we use for the lead
  , synthForLead: T6 (Rect 590 460 340 130) focusc T6_0
  -- choose which pitch out of 14 to start at
  , pitchLead: DiscreteChooser PitchLead _.interactions.pitchLead (Rect 930 70 70 520) backgroundc foregroundc 14
  -- 0th delay line for the lead synth
  , leadDelayLine0: T2 LeadDelayLine0 _.interactions.leadDelayLine0 (Rect 0 240 60 60) focusc
  -- 1st delay line for the lead synth
  , leadDelayLine1: T2 LeadDelayLine1 _.interactions.leadDelayLine1 (Rect 460 120 60 60) focusc
  -- 2nd delay line for the lead synth
  , leadDelayLine2: T2 LeadDelayLine2 _.interactions.leadDelayLine2 (Rect 90 690 60 60) focusc
  -- combined delay line for the lead synth
  , leadCombinedDelay0: T2 LeadCombinedDelay0 _.interactions.leadCombinedDelay0 (Rect 300 0 60 60) focusc
  -- shifts intensities of delays
  , leadDelayGainCarousel: Slider LeadDelayGainCarousel _.interactions.leadDelayGainCarousel (Rect 590 590 410 100) backgroundc foregroundc
  -- n pitches played when pressed (fixed sequence always based on start)
  , nPitchesPlayedLead: DiscreteChooser NPitchesPlayedLead _.interactions.nPitchesPlayedLead (Rect 90 300 90 390) backgroundc foregroundc 7
  -- one of three envelopes
  , envelopeLead: T3 (Rect 660 0 140 140) focusc T3_0
  -- octave shift
  , octaveLead: T3 (Rect 800 70 60 70) focusc T3_0
  -- pad for a buffer
  , drone: Pad (Rect 270 390 320 300) focusc 0.0
  -- choose drone
  , droneChooser: T5 (Rect 390 690 150 120) focusc T5_0
  -- lowpass q for drone
  , droneLowpass0Q: Slider DroneLowpass0Q _.interactions.droneLowpass0Q (Rect 540 690 90 180) backgroundc foregroundc
  -- q of bandpass filter
  , droneBandpass0Q: Slider DroneBandpass0Q _.interactions.droneBandpass0Q (Rect 0 940 300 60) backgroundc foregroundc
  -- lfo controlling freq of bandpass
  , droneBandpass0LFO: Slider DroneBandpass0LFO _.interactions.droneBandpass0LFO (Rect 520 60 70 240) backgroundc foregroundc
  -- q of bandpass filter
  , droneBandpass1Q: Slider DroneBandpass1Q _.interactions.droneBandpass1Q (Rect 930 690 70 310) backgroundc foregroundc
  -- lfo of bandpass filter
  , droneBandpass1LFO: Slider DroneBandpass1LFO _.interactions.droneBandpass1LFO (Rect 180 300 90 260) backgroundc foregroundc
  -- q of highpass filter
  , droneHighpass0Q: Slider DroneHighpass0Q _.interactions.droneHighpass0Q (Rect 860 70 70 200) backgroundc foregroundc
  -- lfo of highpass filter
  , droneHighpass0LFO: Slider DroneHighpass0LFO _.interactions.droneHighpass0LFO (Rect 360 0 300 60) backgroundc foregroundc
  -- how long does it take for the drone to ramp up?
  , droneActivationEnergyThreshold: Slider DroneActivationEnergyThreshold _.interactions.droneActivationEnergyThreshold (Rect 90 750 300 60) backgroundc foregroundc
  -- looping pwf
  , droneRhythmicLoopingPiecewiseFunction: T5 (Rect 660 340 270 120) focusc T5_0
  -- how long does the drone linger?
  , droneDecay: Slider DroneDecay _.interactions.droneDecay (Rect 590 60 70 400) backgroundc foregroundc
  -- flange on the drone
  , droneFlange: T2 DroneFlange _.interactions.droneFlange (Rect 60 0 60 60) focusc
  -- a single sample
  , sampleOneShot: Source SampleOneShot (Rect 660 140 200 200) focusc
  -- reverse the samples?
  , sampleReverse: T2 SampleReverse _.interactions.sampleReverse (Rect 400 120 60 60) focusc
  -- choose which sample to play
  , sampleChooser: DiscreteChooser SampleChooser _.interactions.sampleChooser (Rect 0 300 90 640) backgroundc foregroundc 24
  , sampleChorusEffect: T2 SampleChorusEffect _.interactions.sampleChorusEffect (Rect 120 0 60 60) focusc
  , sampleRateChange: T3 (Rect 720 690 80 100) focusc T3_0
  -- 0th delay line for the single sample
  , sampleDelayLine0: T2 SampleDelayLine0 _.interactions.sampleDelayLine0 (Rect 0 0 60 60) focusc
  -- 1st delay line for the single sample
  , sampleDelayLine1: T2 SampleDelayLine1 _.interactions.sampleDelayLine1 (Rect 150 690 60 60) focusc
  -- 2nd delay line for the single sample
  , sampleDelayLine2: T2 SampleDelayLine2 _.interactions.sampleDelayLine2 (Rect 860 270 70 70) focusc
  -- 0th sample combined delay line
  , sampleCombinedDelayLine0: T2 SampleCombinedDelayLine0 _.interactions.sampleCombinedDelayLine0 (Rect 480 940 60 60) focusc
  -- changes intensities of various delay lines
  , sampleDelayGainCarousel: Slider SampleDelayGainCarousel _.interactions.sampleDelayGainCarousel (Rect 270 300 320 90) backgroundc foregroundc
  -- 0th delay line for combined lead sample
  , leadSampleDelayLine0: T2 LeadSampleDelayLine0 _.interactions.leadSampleDelayLine0 (Rect 660 940 60 60) focusc
  -- 1st delay line for combined lead sample
  , leadSampleDelayLine1: T2 LeadSampleDelayLine1 _.interactions.leadSampleDelayLine1 (Rect 330 690 60 60) focusc
  -- 2nd delay line for combined lead sample
  , leadSampleDelayLine2: T2 LeadSampleDelayLine2 _.interactions.leadSampleDelayLine2 (Rect 0 180 60 60) focusc
  -- alternates between any looping buffers that are currently playing
  , loopingBufferGainDJ: T2 LoopingBufferGainDJ _.interactions.loopingBufferGainDJ (Rect 240 0 60 60) focusc
  -- how close the start/end times are
  , loopingBufferStartEndConstriction: Slider LoopingBufferStartEndConstriction _.interactions.loopingBufferStartEndConstriction (Rect 300 160 100 140) backgroundc foregroundc
  -- 0th looping buffer
  , loopingBuffer0: T2 LoopingBuffer0 _.interactions.loopingBuffer0 (Rect 180 0 60 60) focusc
  -- 1st looping buffer
  , loopingBuffer1: T2 LoopingBuffer1 _.interactions.loopingBuffer1 (Rect 210 690 60 60) focusc
  -- 2nd looping buffer
  , loopingBuffer2: T2 LoopingBuffer2 _.interactions.loopingBuffer2 (Rect 360 940 60 60) focusc
  -- 3rd looping buffer
  , loopingBuffer3: T2 LoopingBuffer3 _.interactions.loopingBuffer3 (Rect 300 940 60 60) focusc
  -- 4th looping buffer
  , loopingBuffer4: T2 LoopingBuffer4 _.interactions.loopingBuffer4 (Rect 0 120 60 60) focusc
  -- substitutes entirely different sets of base parameters
  , radicalFlip: T2 RadicalFlip _.interactions.radicalFlip (Rect 460 60 60 60) focusc
  -- global pan extravaganza
  , greatAndMightyPan: Slider GreatAndMightyPan _.interactions.greatAndMightyPan (Rect 90 870 630 70) backgroundc foregroundc
  -- global delay
  , globalDelay: T2 GlobalDelay _.interactions.globalDelay (Rect 600 940 60 60) focusc
  -- echoing uncontrollable singleton
  , echoingUncontrollableSingleton: Source UncontrollableSingleton (Rect 300 60 100 100) focusc
  , distantBellsFader: Slider DistantBellsFader _.interactions.distantBellsFader (Rect 180 560 90 130) backgroundc foregroundc
  }

foreign import normalizedWidthAndHeight_ :: (Number -> Number -> Number /\ Number) -> MouseEvent -> Number /\ Number

normalizedWidthAndHeight :: MouseEvent -> Number /\ Number
normalizedWidthAndHeight = normalizedWidthAndHeight_ (/\)

dT2 :: T2 -> T2
dT2 T2_0 = T2_1
dT2 T2_1 = T2_0

snap :: Int -> Number -> Number -> Number
snap mx' v pct = let mx = toNumber mx' in (floor (pct * mx)) * v / mx

c2s :: forall m. State -> Control -> Array (H.ComponentHTML Action () m)
c2s st (Slider actionConstructor valueReader (Rect x y w h) background foreground) =
  let
    useX = w > h
  in
    [ SE.rect
        [ SA.x $ toNumber x
        , SA.y $ toNumber y
        , SA.width $ toNumber w
        , SA.height $ toNumber h
        , SA.fill background
        ]
    , SE.rect
        [ SA.x $ toNumber x
        , SA.y $ toNumber y
        , SA.width $ if useX then toNumber w * valueReader st else toNumber w
        , SA.height $ if useX then toNumber h else toNumber h * valueReader st
        , SA.fill foreground
        ]
    , SE.rect
        [ SA.x $ toNumber x
        , SA.y $ toNumber y
        , SA.width $ toNumber w
        , SA.height $ toNumber h
        , SA.fill (RGBA 0 0 0 0.0)
        , SA.stroke (RGB 4 4 4)
        , HE.onMouseDown $ lcmap normalizedWidthAndHeight
            ( actionConstructor
                <<< SliderDown
                <<< if useX then fst else snd
            )
        , HE.onMouseMove $ lcmap normalizedWidthAndHeight
            ( actionConstructor
                <<< SliderMove
                <<< if useX then fst else snd
            )
        , HE.onMouseUp $ lcmap normalizedWidthAndHeight
            (const $ actionConstructor SliderUp)
        ]
    ]
c2s st (DiscreteChooser actionConstructor valueReader (Rect x y w h) background foreground mx) =
  let
    useX = w > h
    mxx = toNumber mx
  in
    [ SE.rect
        [ SA.x $ toNumber x
        , SA.y $ toNumber y
        , SA.width $ toNumber w
        , SA.height $ toNumber h
        , SA.fill background
        ]
    ]
      <> map
        ( \n -> SE.line
            [ SA.x1 $ if useX then (((toNumber n + 0.5) / mxx) * toNumber w) else toNumber (x + 10)
            , SA.y1 $ if useX then toNumber (y + 10) else (((toNumber n + 0.5) / mxx) * toNumber h)
            , SA.x2 $ if useX then (((toNumber n + 0.5) / mxx) * toNumber w) else toNumber (x + w - 10)
            , SA.y2 $ if useX then toNumber (y + h - 10) else (((toNumber n + 0.5) / mxx) * toNumber h)
            ]
        )
        (0 .. mx)
      <>
        [ SE.rect
            [ SA.x $ if useX then snap mx (toNumber w) (valueReader st) - 10.0 else toNumber w
            , SA.y $ if useX then toNumber h else snap mx (toNumber h) (valueReader st) - 10.0
            , SA.width $ if useX then 20.0 else toNumber w
            , SA.height $ if useX then toNumber h else 20.0
            , SA.fill foreground
            ]
        , SE.rect
            [ SA.x $ toNumber x
            , SA.y $ toNumber y
            , SA.width $ toNumber w
            , SA.height $ toNumber h
            , SA.fill (RGBA 0 0 0 0.0)
            , SA.stroke (RGB 4 4 4)
            , HE.onMouseDown $ lcmap normalizedWidthAndHeight
                ( actionConstructor
                    <<< SliderDown
                    <<< if useX then fst else snd
                )
            , HE.onMouseMove $ lcmap normalizedWidthAndHeight
                ( actionConstructor
                    <<< SliderMove
                    <<< if useX then fst else snd
                )
            , HE.onMouseUp $ lcmap normalizedWidthAndHeight
                (const $ actionConstructor SliderUp)
            ]
        ]
c2s st (T2 actionConstructor valueReader (Rect x y w h) color) =
  let
    cur = valueReader st
  in
    [ SE.rect
        [ SA.x $ toNumber x
        , SA.y $ toNumber y
        , SA.width $ toNumber w
        , SA.height $ toNumber h
        , SA.fill color
        , SA.stroke (RGB 4 4 4)
        , HE.onClick (const $ actionConstructor $ dT2 cur)
        ]
    ] <> case cur of
      T2_0 -> []
      T2_1 ->
        [ SE.circle
            [ SA.cx $ (toNumber x + (toNumber w / 2.0))
            , SA.cy $ (toNumber y + (toNumber h / 2.0))
            , SA.r $ toNumber ((min w h) - 20) / 2.0
            , SA.fill (RGB 10 10 10)
            , SA.stroke (RGB 4 4 4)
            , HE.onClick (const $ actionConstructor $ dT2 cur)
            ]
        ]
c2s st (T3 (Rect x y w h) color t) = [ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ] ] <> case t of
  T3_0 -> []
  T3_1 -> [ SE.polygon [ HH.attr (wrap "points") (show x <> "," <> show y <> " " <> show (x + w) <> "," <> show y <> " " <> show (x + w) <> "," <> show (y + h) <> " ") ] ]
  T3_2 -> [ SE.polygon [ HH.attr (wrap "points") (show x <> "," <> show y <> " " <> show x <> "," <> show (y + h) <> " " <> show (x + w) <> "," <> show (y + h) <> " ") ] ]
c2s st (T4 (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s st (T5 (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s st (T6 (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s st (Pad (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s st (Source action (Rect x y w h) color) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
