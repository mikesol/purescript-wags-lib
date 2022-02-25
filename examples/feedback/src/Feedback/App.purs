module Feedback.App where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Monad.Indexed ((:*>))
import Control.Plus (empty)
import Data.Int (toNumber)
import Data.Lens (over)
import Data.Lens.Grate (grate)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Profunctor.Closed (class Closed)
import Data.Traversable (for_)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (D60)
import Data.Vec (Vec, (+>))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Log
import FRP.Event (subscribe)
import Foreign.Object (fromHomogeneous, values)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import WAGS.Change (ichange)
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit (OversampleTwoX, TBandpass, TDelay, TGain, THighpass, TLoopBuf, TLowpass, TPlayBuf, TSinOsc, TSpeaker, TStereoPanner, TWaveShaper)
import WAGS.Interpret (close, context, makeFFIAudioSnapshot)
import WAGS.Patch (ipatch)
import WAGS.Run (Run, RunAudio, RunEngine, SceneI(..), run)
import WAGS.WebAPI (AudioContext)

type World = {}

type Res = Unit

type FullGraph =
  ( speaker :: TSpeaker /\ { mainFader :: Unit }
  , mainFader ::
      TGain /\
        { subMainDelay :: Unit
        , subMainFader :: Unit
        }
  , subMainDelay :: TDelay /\ { subMainGain :: Unit }
  , subMainGain :: TGain /\ { mainFader :: Unit }
  , subMainFader ::
      TGain /\
        { loopingBuffer0 :: Unit
        , loopingBuffer1 :: Unit
        , loopingBuffer2 :: Unit
        , loopingBuffer3 :: Unit
        , loopingBuffer4 :: Unit
        , distantBells :: Unit
        , uSingleton :: Unit
        , pad :: Unit
        , drone :: Unit
        , smplr :: Unit
        , lead :: Unit
        }
  -- looping buffers
  , loopingBuffer0 :: TGain /\ { loopingBuffer0Pan :: Unit }
  , loopingBuffer0Pan :: TStereoPanner /\ { loopingBuffer0PlayBuf :: Unit }
  , loopingBuffer0PlayBuf :: TLoopBuf /\ {}
  , loopingBuffer1 :: TGain /\ { loopingBuffer1Pan :: Unit }
  , loopingBuffer1Pan :: TStereoPanner /\ { loopingBuffer1PlayBuf :: Unit }
  , loopingBuffer1PlayBuf :: TLoopBuf /\ {}
  , loopingBuffer2 :: TGain /\ { loopingBuffer2Pan :: Unit }
  , loopingBuffer2Pan :: TStereoPanner /\ { loopingBuffer2PlayBuf :: Unit }
  , loopingBuffer2PlayBuf :: TLoopBuf /\ {}
  , loopingBuffer3 :: TGain /\ { loopingBuffer3Pan :: Unit }
  , loopingBuffer3Pan :: TStereoPanner /\ { loopingBuffer3PlayBuf :: Unit }
  , loopingBuffer3PlayBuf :: TLoopBuf /\ {}
  , loopingBuffer4 :: TGain /\ { loopingBuffer4Pan :: Unit }
  , loopingBuffer4Pan :: TStereoPanner /\ { loopingBuffer4PlayBuf :: Unit }
  , loopingBuffer4PlayBuf :: TLoopBuf /\ {}
  -- bells
  , distantBells :: TGain /\ { distantBellsPan :: Unit }
  , distantBellsPan :: TStereoPanner /\ { distantBellsPlayBuf :: Unit }
  , distantBellsPlayBuf :: TLoopBuf /\ {}
  -- uncontrollable singleton
  , uSingleton :: TStereoPanner /\ { uSingletonGain :: Unit }
  , uSingletonGain ::
      TGain /\
        { uSingletonSource :: Unit
        , uSingletonHGain :: Unit
        }
  , uSingletonSource ::
      TGain /\
        { uSingletonPlayBuf0 :: Unit
        , uSingletonPlayBuf1 :: Unit
        , uSingletonPlayBuf2 :: Unit
        , uSingletonPlayBuf3 :: Unit
        , uSingletonPlayBuf4 :: Unit
        }
  , uSingletonPlayBuf0 :: TPlayBuf /\ {}
  , uSingletonPlayBuf1 :: TPlayBuf /\ {}
  , uSingletonPlayBuf2 :: TPlayBuf /\ {}
  , uSingletonPlayBuf3 :: TPlayBuf /\ {}
  , uSingletonPlayBuf4 :: TPlayBuf /\ {}
  , uSingletonHGain :: TGain /\ { uSingletonHHPF :: Unit }
  , uSingletonHHPF :: THighpass /\ { uSingletonHDelay :: Unit }
  , uSingletonHDelay :: TDelay /\ { uSingletonGain :: Unit }
  -- pad
  , pad ::
      TGain /\
        { padDry :: Unit
        , padWaveshaped :: Unit
        }
  , padDry :: TGain /\ { padMix :: Unit }
  , padWaveshaped :: TGain /\ { padWaveshaper :: Unit }
  , padWaveshaper :: TWaveShaper OversampleTwoX /\ { padMix :: Unit }
  , padMix ::
      TGain /\
        { padFilter0 :: Unit
        , padFilter1 :: Unit
        , padFilter2 :: Unit
        , padFilter3 :: Unit
        }
  , padFilter0 :: TBandpass /\ { padSources :: Unit }
  , padFilter1 :: TBandpass /\ { padSources :: Unit }
  , padFilter2 :: TBandpass /\ { padSources :: Unit }
  , padFilter3 :: TBandpass /\ { padSources :: Unit }
  , padSources ::
      TGain /\
        { padSource0 :: Unit
        , padSource1 :: Unit
        , padSource2 :: Unit
        , padSource3 :: Unit
        , padSource4 :: Unit
        }
  , padSource0 :: TGain /\ { padSource0Pan :: Unit }
  , padSource0Pan :: TStereoPanner /\ { padSource0Osc :: Unit }
  , padSource0Osc :: TSinOsc /\ {}
  , padSource1 :: TGain /\ { padSource1Pan :: Unit }
  , padSource1Pan :: TStereoPanner /\ { padSource1Osc :: Unit }
  , padSource1Osc :: TSinOsc /\ {}
  , padSource2 :: TGain /\ { padSource2Pan :: Unit }
  , padSource2Pan :: TStereoPanner /\ { padSource2Osc :: Unit }
  , padSource2Osc :: TSinOsc /\ {}
  , padSource3 :: TGain /\ { padSource3Pan :: Unit }
  , padSource3Pan :: TStereoPanner /\ { padSource3Osc :: Unit }
  , padSource3Osc :: TSinOsc /\ {}
  , padSource4 :: TGain /\ { padSource4Pan :: Unit }
  , padSource4Pan :: TStereoPanner /\ { padSource4Osc :: Unit }
  , padSource4Osc :: TSinOsc /\ {}
  -- drone
  , drone :: TGain /\
        { droneFilter0 :: Unit
        , droneFilter1 :: Unit
        , droneFilter2 :: Unit
        , droneFilter3 :: Unit
        }
  , droneFilter0 :: TLowpass /\ { droneSources :: Unit }
  , droneFilter1 :: TBandpass /\ { droneSources :: Unit }
  , droneFilter2 :: TBandpass /\ { droneSources :: Unit }
  , droneFilter3 :: THighpass /\ { droneSources :: Unit }
  , droneSources ::
      TGain /\
        { droneSource0 :: Unit
        , droneSource1 :: Unit
        , droneSource2 :: Unit
        , droneSource3 :: Unit
        , droneSource4 :: Unit
        }
  , droneSource0 :: TGain /\ { droneSource0Buf :: Unit }
  , droneSource0Buf :: TLoopBuf /\ {}
  , droneSource1 :: TGain /\ { droneSource1Buf :: Unit }
  , droneSource1Buf :: TLoopBuf /\ {}
  , droneSource2 :: TGain /\ { droneSource2Buf :: Unit }
  , droneSource2Buf :: TLoopBuf /\ {}
  , droneSource3 :: TGain /\ { droneSource3Buf :: Unit }
  , droneSource3Buf :: TLoopBuf /\ {}
  , droneSource4 :: TGain /\ { droneSource4Buf :: Unit }
  , droneSource4Buf :: TLoopBuf /\ {}
  -- smplr
  , smplr :: TStereoPanner /\ { smplrMain :: Unit }
  , smplrMain :: TGain /\
        { smplrDelay0 :: Unit
        , smplrDelay1 :: Unit
        , smplrDelay2 :: Unit
        }
  ----
  , smplrDelay0 :: TGain /\ { smplrSourcesGain0 :: Unit, smplrDelay0Gain :: Unit }
  , smplrSourcesGain0 :: TGain /\ { smplrSources :: Unit }
  , smplrDelay0Gain :: TGain /\ { smplrHighpass0 :: Unit }
  , smplrHighpass0 :: THighpass /\ { smplrDelayD0 :: Unit }
  , smplrDelayD0 :: TDelay /\ { smplrDelay0 :: Unit }
  ----
  , smplrDelay1 :: TGain /\ { smplrSourcesGain1 :: Unit, smplrDelay1Gain :: Unit }
  , smplrSourcesGain1 :: TGain /\ { smplrSources :: Unit }
  , smplrDelay1Gain :: TGain /\ { smplrHighpass1 :: Unit }
  , smplrHighpass1 :: THighpass /\ { smplrDelayD1 :: Unit }
  , smplrDelayD1 :: TDelay /\ { smplrDelay1 :: Unit }
  ----
  , smplrDelay2 :: TGain /\ { smplrSourcesGain2 :: Unit, smplrDelay2Gain :: Unit }
  , smplrSourcesGain2 :: TGain /\ { smplrSources :: Unit }
  , smplrDelay2Gain :: TGain /\ { smplrHighpass2 :: Unit }
  , smplrHighpass2 :: THighpass /\ { smplrDelayD2 :: Unit }
  , smplrDelayD2 :: TDelay /\ { smplrDelay2 :: Unit }
  ----
  , smplrDelayCbnd :: TGain /\ { smplrSourcesGainCbnd :: Unit
    , smplrDelayCbndGain :: Unit }
  , smplrSourcesGainCbnd :: TGain /\ { smplrDelay0 :: Unit, smplrDelay1 :: Unit }
  , smplrDelayCbndGain :: TGain /\ { smplrHighpassCbnd :: Unit }
  , smplrHighpassCbnd :: THighpass /\ { smplrDelayDCbnd :: Unit }
  , smplrDelayDCbnd :: TDelay /\ { smplrDelayCbnd :: Unit }
  ----
  , smplrSources ::
      TGain /\
        { smplrSource0 :: Unit
        , smplrSource1 :: Unit
        , smplrSource2 :: Unit
        , smplrSource3 :: Unit
        , smplrSource4 :: Unit
        }
  , smplrSource0 :: TGain /\ { smplrSource0Buf :: Unit }
  , smplrSource0Buf :: TPlayBuf /\ {}
  , smplrSource1 :: TGain /\ { smplrSource1Buf :: Unit }
  , smplrSource1Buf :: TPlayBuf /\ {}
  , smplrSource2 :: TGain /\ { smplrSource2Buf :: Unit }
  , smplrSource2Buf :: TPlayBuf /\ {}
  , smplrSource3 :: TGain /\ { smplrSource3Buf :: Unit }
  , smplrSource3Buf :: TPlayBuf /\ {}
  , smplrSource4 :: TGain /\ { smplrSource4Buf :: Unit }
  , smplrSource4Buf :: TPlayBuf /\ {}
  -- lead
  , lead :: TStereoPanner /\ { leadMain :: Unit }
  , leadMain :: TGain /\
        { leadDelay0 :: Unit
        , leadDelay1 :: Unit
        , leadDelay2 :: Unit
        }
  ----
  , leadDelay0 :: TGain /\ { leadSourcesGain0 :: Unit, leadDelay0Gain :: Unit }
  , leadSourcesGain0 :: TGain /\ { leadSources :: Unit }
  , leadDelay0Gain :: TGain /\ { leadHighpass0 :: Unit }
  , leadHighpass0 :: THighpass /\ { leadDelayD0 :: Unit }
  , leadDelayD0 :: TDelay /\ { leadDelay0 :: Unit }
  ----
  , leadDelay1 :: TGain /\ { leadSourcesGain1 :: Unit, leadDelay1Gain :: Unit }
  , leadSourcesGain1 :: TGain /\ { leadSources :: Unit }
  , leadDelay1Gain :: TGain /\ { leadHighpass1 :: Unit }
  , leadHighpass1 :: THighpass /\ { leadDelayD1 :: Unit }
  , leadDelayD1 :: TDelay /\ { leadDelay1 :: Unit }
  ----
  , leadDelay2 :: TGain /\ { leadSourcesGain2 :: Unit, leadDelay2Gain :: Unit }
  , leadSourcesGain2 :: TGain /\ { leadSources :: Unit }
  , leadDelay2Gain :: TGain /\ { leadHighpass2 :: Unit }
  , leadHighpass2 :: THighpass /\ { leadDelayD2 :: Unit }
  , leadDelayD2 :: TDelay /\ { leadDelay2 :: Unit }
  ----
  , leadDelayCbnd :: TGain /\ { leadSourcesGainCbnd :: Unit
    , leadDelayCbndGain :: Unit }
  , leadSourcesGainCbnd :: TGain /\ { leadDelay0 :: Unit, leadDelay1 :: Unit }
  , leadDelayCbndGain :: TGain /\ { leadHighpassCbnd :: Unit }
  , leadHighpassCbnd :: THighpass /\ { leadDelayDCbnd :: Unit }
  , leadDelayDCbnd :: TDelay /\ { leadDelayCbnd :: Unit }
  ----
  , leadSources ::
      TGain /\
        { leadSource0 :: Unit
        , leadSource1 :: Unit
        , leadSource2 :: Unit
        , leadSource3 :: Unit
        , leadSource4 :: Unit
        }
  , leadSource0 :: TGain /\ { leadSource0Buf :: Unit }
  , leadSource0Buf :: TPlayBuf /\ {}
  , leadSource1 :: TGain /\ { leadSource1Buf :: Unit }
  , leadSource1Buf :: TPlayBuf /\ {}
  , leadSource2 :: TGain /\ { leadSource2Buf :: Unit }
  , leadSource2Buf :: TPlayBuf /\ {}
  , leadSource3 :: TGain /\ { leadSource3Buf :: Unit }
  , leadSource3Buf :: TPlayBuf /\ {}
  , leadSource4 :: TGain /\ { leadSource4Buf :: Unit }
  , leadSource4Buf :: TPlayBuf /\ {}
  )

type Acc = Unit

createFrame :: SceneI Unit World () -> IxWAG RunAudio RunEngine Frame0 Unit () FullGraph Acc
createFrame = \(SceneI {}) ->
  ( ipatch { microphone: empty, mediaElement: empty }
      :*> (ichange {} $> unit)
  )

piece :: Scene (SceneI Unit World ()) RunAudio RunEngine Frame0 Unit
piece =
  createFrame
    @!> iloop \(SceneI { time, headroomInSeconds }) a ->
      ichange {} $> unit

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

type State
  =
  { unsubscribe :: Effect Unit
  , audioCtx :: Maybe AudioContext
  }

data Action
  = StartAudio
  | StopAudio

component :: forall query input output m. MonadEffect m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  }

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

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

-- SYNTH 1
-- 3 config of oscillators
-- 1 config of pitch (multiswitch)
type Instructions (a :: Type) =
  ( triggerPad :: a
  , togglePadOsc0 :: a
  , togglePadOsc1 :: a
  , togglePadOsc2 :: a
  , togglePadOsc3 :: a
  , togglePadOsc4 :: a
  , detunePad :: a
  , gainLFO0Pad :: a
  , gainLFO1Pad :: a
  , filterBankChooserPad :: a
  , waveshaperPad :: a
  , triggerLead :: a
  , synthForLead :: a
  , pitchLead :: a
  , nPitchesPlayedLead :: a
  , envelopeLead :: a
  , octaveLead :: a
  , leadDelayLine0 :: a
  , leadDelayLine1 :: a
  , leadDelayLine2 :: a
  , leadCombinedDelay0 :: a
  , leadDelayGainCarousel :: a
  , drone :: a
  , droneChooser :: a
  , droneLowpass0Q :: a
  , droneBandpass0Q :: a
  , droneBandpass0LFO :: a
  , droneBandpass1Q :: a
  , droneBandpass1LFO :: a
  , droneHighpass0Q :: a
  , droneHighpass0LFO :: a
  , droneActivationEnergyThreshold :: a
  , droneRhythmicLoopingPiecewiseFunction :: a
  , droneDecay :: a
  , droneFlange :: a
  , sampleOneShot :: a
  , sampleReverse :: a
  , sampleChooser :: a
  , sampleRateChange :: a
  , sampleChorusEffect :: a
  , sampleDelayLine0 :: a
  , sampleDelayLine1 :: a
  , sampleDelayLine2 :: a
  , sampleCombinedDelayLine0 :: a
  , sampleDelayGainCarousel :: a
  , leadSampleDelayLine0 :: a
  , leadSampleDelayLine1 :: a
  , leadSampleDelayLine2 :: a
  , loopingBufferStartEndConstriction :: a
  , loopingBufferGainDJ :: a
  , loopingBuffer0 :: a
  , loopingBuffer1 :: a
  , loopingBuffer2 :: a
  , loopingBuffer3 :: a
  , loopingBuffer4 :: a
  , radicalFlip :: a
  , greatAndMightyPan :: a
  , globalDelay :: a
  , echoingUncontrollableSingleton :: a
  , distantBellsFader :: a
  )

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
  -- high sine in mix
  , togglePadOsc3: T2 (Rect 0 60 60 60) focusc T2_0
  -- square in mix
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
  , leadDelayGainCarousel: T2 (Rect 590 590 410 100) focusc T2_0
  -- n pitches played when pressed (fixed sequence always based on start)
  , nPitchesPlayedLead: DiscreteChooser (Rect 90 300 90 390) focusc 6 0
  -- one of three envelopes
  , envelopeLead: T3 (Rect 660 0 140 140) focusc T3_0
  -- octave shift
  , octaveLead: T3 (Rect 800 70 60 70) focusc T3_0
  -- pad for a buffer
  , drone: Pad (Rect 270 390 320 300) focusc 0.0
  -- choose drone
  , droneChooser: T4 (Rect 390 690 150 120) focusc T4_0
  -- lowpass q for drone
  , droneLowpass0Q: Slider (Rect 540 690 90 180) focusc 0.0
  -- q of bandpass filter
  , droneBandpass0Q: Slider (Rect 0 940 300 60) focusc 0.0
  -- lfo controlling freq of bandpass
  , droneBandpass0LFO: T2 (Rect 520 60 70 240) focusc T2_0
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

type Quads a = Vec D60 a

controls :: Quads Control
controls =
  T2 (Rect 0 0 60 60) focusc T2_0 --

    +> T2 (Rect 590 590 410 100) focusc T2_0 --
    +> T2 (Rect 660 340 270 120) focusc T2_0 --
    +> T2 (Rect 590 460 340 130) focusc T2_0 --
    +> T2 (Rect 0 940 300 60) focusc T2_0 --
    +> T2 (Rect 90 750 300 60) focusc T2_0 --
    +> T2 (Rect 90 810 450 60) focusc T2_0 --
    +> T2 (Rect 90 870 630 70) focusc T2_0 --
    +> T2 (Rect 0 60 60 60) focusc T2_0 --
    +> T2 (Rect 60 0 60 60) focusc T2_0 --
    +> T2 (Rect 400 60 60 60) focusc T2_0 --
    +> T2 (Rect 400 120 60 60) focusc T2_0 --
    +> T2 (Rect 90 690 60 60) focusc T2_0 --
    +> T2 (Rect 150 690 60 60) focusc T2_0 --
    +> T2 (Rect 300 940 60 60) focusc T2_0 --
    +> T2 (Rect 360 940 60 60) focusc T2_0 --
    +> T2 (Rect 420 940 60 60) focusc T2_0 --
    +> T2 (Rect 480 940 60 60) focusc T2_0 --
    +> T2 (Rect 540 940 60 60) focusc T2_0 --
    +> T2 (Rect 600 940 60 60) focusc T2_0 --
    +> T2 (Rect 660 940 60 60) focusc T2_0 --
    +> T2 (Rect 210 690 60 60) focusc T2_0 --
    +> T2 (Rect 270 690 60 60) focusc T2_0 --
    +> T2 (Rect 330 690 60 60) focusc T2_0 --
    +> T2 (Rect 460 60 60 60) focusc T2_0 --
    +> T2 (Rect 460 120 60 60) focusc T2_0 --
    +> T2 (Rect 800 70 60 70) focusc T2_0 --
    +> T2 (Rect 860 270 70 70) focusc T2_0 --
    +> T2 (Rect 0 120 60 60) focusc T2_0 --
    +> T2 (Rect 120 0 60 60) focusc T2_0 --
    +> T2 (Rect 0 180 60 60) focusc T2_0 --
    +> T2 (Rect 180 0 60 60) focusc T2_0 --
    +> T2 (Rect 0 240 60 60) focusc T2_0 --
    +> T2 (Rect 0 300 90 640) focusc T2_0 --
    +> T2 (Rect 90 300 90 390) focusc T2_0 --
    +> T2 (Rect 180 300 90 260) focusc T2_0 --
    +> T2 (Rect 270 300 320 90) focusc T2_0 --
    +> T2 (Rect 240 0 60 60) focusc T2_0 --
    +> T2 (Rect 300 0 60 60) focusc T2_0 --
    +> T2 (Rect 300 60 100 100) focusc T2_0 --
    +> T2 (Rect 360 0 300 60) focusc T2_0 --
    +> T2 (Rect 300 160 100 140) focusc T2_0 --
    +> T2 (Rect 400 180 120 120) focusc T2_0 --
    +> T2 (Rect 60 60 240 240) focusc T2_0 --
    +> T2 (Rect 270 390 320 300) focusc T2_0 --
    +> T2 (Rect 660 140 200 200) focusc T2_0 --
    +> T2 (Rect 660 0 140 140) focusc T2_0 --
    +> T2 (Rect 800 0 200 70) focusc T2_0 --
    +> T2 (Rect 590 60 70 400) focusc T2_0 --
    +> T2 (Rect 860 70 70 200) focusc T2_0 --
    +> T2 (Rect 930 70 70 520) focusc T2_0 --
    +> T2 (Rect 930 690 70 310) focusc T2_0 --
    +> T2 (Rect 520 60 70 240) focusc T2_0 --
    +> T2 (Rect 180 560 90 130) focusc T2_0 --
    +> T2 (Rect 720 690 80 100) focusc T2_0 --
    +> T2 (Rect 630 690 90 180) focusc T2_0 --
    +> T2 (Rect 540 690 90 180) focusc T2_0 --
    +> T2 (Rect 390 690 150 120) focusc T2_0 --
    +> T2 (Rect 800 690 130 100) focusc T2_0 --
    +> T2 (Rect 720 790 210 210) focusc T2_0 --
    +> V.empty

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

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ classes [ "w-screen", "h-screen" ] ]
    [ SE.svg
        [ SA.classes $ map ClassName [ "w-full", "h-full" ]
        , SA.viewBox 0.0 0.0 1000.0 1000.0
        , SA.preserveAspectRatio Nothing SA.Slice
        ]
        (join $ map c2s $ values $ fromHomogeneous elts)
    ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    handleAction StopAudio
    audioCtx <- H.liftEffect context
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( run (pure unit)
                (pure {})
                { easingAlgorithm }
                ffiAudio
                piece
            )
            (\({ res } :: Run Res ()) -> Log.info $ show res)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
