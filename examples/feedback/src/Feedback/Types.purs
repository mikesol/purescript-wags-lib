module Feedback.Types where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Data.Identity (Identity)
import Data.Int (floor, toNumber)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Typelevel.Num (class Pos, D14, D2, D24, D3, D4, D5, D6, D7, toInt')
import Data.Variant (Variant)
import Data.Vec (Vec)
import Feedback.FullGraph (FullGraph)
import Foreign (ForeignError(..), fail)
import Simple.JSON (readImpl, undefined, writeImpl)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine)
import WAGS.WebAPI (BrowserAudioBuffer, BrowserFloatArray, BrowserPeriodicWave)

type AllEvents :: forall t. t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> t -> Row t
type AllEvents triggerPad togglePadOsc0 togglePadOsc1 togglePadOsc2 togglePadOsc3 togglePadOsc4 detunePad gainLFO0Pad gainLFO1Pad filterBankChooserPad waveshaperPad triggerLead synthForLead pitchLead nPitchesPlayedLead envelopeLead octaveLead leadDelayLine0 leadDelayLine1 leadDelayLine2 leadCombinedDelay0 leadDelayGainCarousel drone droneChooser droneLowpass0Q droneBandpass0Q droneBandpass0LFO droneBandpass1Q droneBandpass1LFO droneHighpass0Q droneHighpass0LFO droneActivationEnergyThreshold droneRhythmicLoopingPiecewiseFunction droneDecay droneFlange sampleOneShot sampleReverse sampleChooser sampleRateChange sampleChorusEffect sampleDelayLine0 sampleDelayLine1 sampleDelayLine2 sampleCombinedDelayLine0 sampleDelayGainCarousel leadSampleDelayLine0 leadSampleDelayLine1 leadSampleDelayLine2 loopingBufferStartEndConstriction loopingBufferGainDJ loopingBuffer0 loopingBuffer1 loopingBuffer2 loopingBuffer3 loopingBuffer4 radicalFlip greatAndMightyPan globalDelay echoingUncontrollableSingleton distantBellsFader =
  ( triggerPad :: triggerPad
  , togglePadOsc0 :: togglePadOsc0
  , togglePadOsc1 :: togglePadOsc1
  , togglePadOsc2 :: togglePadOsc2
  , togglePadOsc3 :: togglePadOsc3
  , togglePadOsc4 :: togglePadOsc4
  , detunePad :: detunePad
  , gainLFO0Pad :: gainLFO0Pad
  , gainLFO1Pad :: gainLFO1Pad
  , filterBankChooserPad :: filterBankChooserPad
  , waveshaperPad :: waveshaperPad
  , triggerLead :: triggerLead
  , synthForLead :: synthForLead
  , pitchLead :: pitchLead
  , nPitchesPlayedLead :: nPitchesPlayedLead
  , envelopeLead :: envelopeLead
  , octaveLead :: octaveLead
  , leadDelayLine0 :: leadDelayLine0
  , leadDelayLine1 :: leadDelayLine1
  , leadDelayLine2 :: leadDelayLine2
  , leadCombinedDelay0 :: leadCombinedDelay0
  , leadDelayGainCarousel :: leadDelayGainCarousel
  , drone :: drone
  , droneChooser :: droneChooser
  , droneLowpass0Q :: droneLowpass0Q
  , droneBandpass0Q :: droneBandpass0Q
  , droneBandpass0LFO :: droneBandpass0LFO
  , droneBandpass1Q :: droneBandpass1Q
  , droneBandpass1LFO :: droneBandpass1LFO
  , droneHighpass0Q :: droneHighpass0Q
  , droneHighpass0LFO :: droneHighpass0LFO
  , droneActivationEnergyThreshold :: droneActivationEnergyThreshold
  , droneRhythmicLoopingPiecewiseFunction :: droneRhythmicLoopingPiecewiseFunction
  , droneDecay :: droneDecay
  , droneFlange :: droneFlange
  , sampleOneShot :: sampleOneShot
  , sampleReverse :: sampleReverse
  , sampleChooser :: sampleChooser
  , sampleRateChange :: sampleRateChange
  , sampleChorusEffect :: sampleChorusEffect
  , sampleDelayLine0 :: sampleDelayLine0
  , sampleDelayLine1 :: sampleDelayLine1
  , sampleDelayLine2 :: sampleDelayLine2
  , sampleCombinedDelayLine0 :: sampleCombinedDelayLine0
  , sampleDelayGainCarousel :: sampleDelayGainCarousel
  , leadSampleDelayLine0 :: leadSampleDelayLine0
  , leadSampleDelayLine1 :: leadSampleDelayLine1
  , leadSampleDelayLine2 :: leadSampleDelayLine2
  , loopingBufferStartEndConstriction :: loopingBufferStartEndConstriction
  , loopingBufferGainDJ :: loopingBufferGainDJ
  , loopingBuffer0 :: loopingBuffer0
  , loopingBuffer1 :: loopingBuffer1
  , loopingBuffer2 :: loopingBuffer2
  , loopingBuffer3 :: loopingBuffer3
  , loopingBuffer4 :: loopingBuffer4
  , radicalFlip :: radicalFlip
  , greatAndMightyPan :: greatAndMightyPan
  , globalDelay :: globalDelay
  , echoingUncontrollableSingleton :: echoingUncontrollableSingleton
  , distantBellsFader :: distantBellsFader
  )

type Instructions (a :: Type) = AllEvents a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a a

newtype ZeroToOne = ZeroToOne Number

derive instance newtypeZeroToOne :: Newtype ZeroToOne _
instance toJsonZeroToOne :: JSON.WriteForeign ZeroToOne where
  writeImpl = unwrap >>> writeImpl

instance fromJsonZeroToOne :: JSON.ReadForeign ZeroToOne where
  readImpl = readImpl >=>
    (if _ then _ else _)
      <$> ((_ <= 1.0) && (_ >= 0.0))
      <*> (pure <<< wrap)
      <*> (fail <<< ForeignError <<< (<>) "Not between zero and one: " <<< show)

foreign import onElts :: forall n a. Vec n a -> Elts n -> a
foreign import updateAtElt :: forall n a. (a -> a) -> Vec n a -> Elts n -> Vec n a

nFromZeroOne :: forall n. Pos n => ZeroToOne -> Elts n
nFromZeroOne = let ii = toInt' (Proxy :: _ n) in \(ZeroToOne n) -> Elts $ min ii $ max 0 $ floor (n * toNumber ii)

ezto :: forall n. Pos n => Elts n -> ZeroToOne
ezto = let ii = toInt' (Proxy :: _ n) in \(Elts n) -> wrap (toNumber n / toNumber ii)

newtype Elts (t :: Type) = Elts Int

derive instance newtypeElts :: Newtype (Elts n) _
instance toJSONElts :: JSON.WriteForeign (Elts n) where
  writeImpl = unwrap >>> writeImpl

instance fromJsonElts :: Pos n => JSON.ReadForeign (Elts n) where
  readImpl = readImpl >=>
    let
      ti = toInt' (Proxy :: _ n)
    in
      (if _ then _ else _)
        <$> ((_ < ti) && (_ >= 0))
        <*> (pure <<< wrap)
        <*>
          ( fail <<< ForeignError
              <<< (<>)
                ( "The expected value "
                    <> show ti
                    <> " is not equal to : "
                )
              <<< show
          )

data Bang = Bang

instance toJsonBang :: JSON.WriteForeign Bang where
  writeImpl = const $ writeImpl "BANG"

instance fromJsonBang :: JSON.ReadForeign Bang where
  readImpl = readImpl >=>
    (if _ then _ else _)
      <$> (eq "BANG")
      <*> (const $ pure Bang)
      <*> (fail <<< ForeignError <<< (<>) "Not BANG: ")

type PadT = { v :: ZeroToOne, ud :: Boolean }
type SliderT = ZeroToOne
--
type TriggerPad = PadT
type TogglePadOsc0 = Elts D2
type TogglePadOsc1 = Elts D2
type TogglePadOsc2 = Elts D2
type TogglePadOsc3 = Elts D2
type TogglePadOsc4 = Elts D2
type DetunePad = Elts D4
type GainLFO0Pad = SliderT
type GainLFO1Pad = SliderT
type FilterBankChooserPad = Elts D5
type WaveshaperPad = SliderT
type TriggerLead = Bang
type SynthForLead = Elts D5
type PitchLead = Elts D14
type NPitchesPlayedLead = Elts D7
type EnvelopeLead = Elts D3
type OctaveLead = Elts D3
type LeadDelayLine0 = Elts D2
type LeadDelayLine1 = Elts D2
type LeadDelayLine2 = Elts D2
type LeadCombinedDelay0 = Elts D2
type LeadDelayGainCarousel = SliderT
type Drone = PadT
type DroneChooser = Elts D5
type DroneLowpass0Q = SliderT
type DroneBandpass0Q = SliderT
type DroneBandpass0LFO = SliderT
type DroneBandpass1Q = SliderT
type DroneBandpass1LFO = SliderT
type DroneHighpass0Q = SliderT
type DroneHighpass0LFO = SliderT
type DroneActivationEnergyThreshold = SliderT
type DroneRhythmicLoopingPiecewiseFunction = Elts D5
type DroneDecay = SliderT
type DroneFlange = Elts D2
type SampleOneShot = Bang
type SampleReverse = Elts D2
type SampleChooser = Elts D24
type SampleRateChange = Elts D3
type SampleChorusEffect = Elts D2
type SampleDelayLine0 = Elts D2
type SampleDelayLine1 = Elts D2
type SampleDelayLine2 = Elts D2
type SampleCombinedDelayLine0 = Elts D2
type SampleDelayGainCarousel = SliderT
type LeadSampleDelayLine0 = Elts D2
type LeadSampleDelayLine1 = Elts D2
type LeadSampleDelayLine2 = Elts D2
type LoopingBufferStartEndConstriction = SliderT
type LoopingBufferGainDJ = Elts D2
type LoopingBuffer0 = Elts D2
type LoopingBuffer1 = Elts D2
type LoopingBuffer2 = Elts D2
type LoopingBuffer3 = Elts D2
type LoopingBuffer4 = Elts D2
type RadicalFlip = Elts D2
type GreatAndMightyPan = SliderT
type GlobalDelay = Elts D2
type EchoingUncontrollableSingleton = Bang
type DistantBellsFader = SliderT

type IncomingEvent' = Variant (AllEvents TriggerPad TogglePadOsc0 TogglePadOsc1 TogglePadOsc2 TogglePadOsc3 TogglePadOsc4 DetunePad GainLFO0Pad GainLFO1Pad FilterBankChooserPad WaveshaperPad TriggerLead SynthForLead PitchLead NPitchesPlayedLead EnvelopeLead OctaveLead LeadDelayLine0 LeadDelayLine1 LeadDelayLine2 LeadCombinedDelay0 LeadDelayGainCarousel Drone DroneChooser DroneLowpass0Q DroneBandpass0Q DroneBandpass0LFO DroneBandpass1Q DroneBandpass1LFO DroneHighpass0Q DroneHighpass0LFO DroneActivationEnergyThreshold DroneRhythmicLoopingPiecewiseFunction DroneDecay DroneFlange SampleOneShot SampleReverse SampleChooser SampleRateChange SampleChorusEffect SampleDelayLine0 SampleDelayLine1 SampleDelayLine2 SampleCombinedDelayLine0 SampleDelayGainCarousel LeadSampleDelayLine0 LeadSampleDelayLine1 LeadSampleDelayLine2 LoopingBufferStartEndConstriction LoopingBufferGainDJ LoopingBuffer0 LoopingBuffer1 LoopingBuffer2 LoopingBuffer3 LoopingBuffer4 RadicalFlip GreatAndMightyPan GlobalDelay EchoingUncontrollableSingleton DistantBellsFader)

newtype IncomingEvent = IncomingEvent IncomingEvent'

newtype Trigger = Trigger
  ( Variant
      ( thunk :: Unit
      , event :: IncomingEvent
      )
  )

type Synths =
  { p0 :: BrowserAudioBuffer
  , p1 :: BrowserAudioBuffer
  , p2 :: BrowserAudioBuffer
  , p3 :: BrowserAudioBuffer
  , p4 :: BrowserAudioBuffer
  , p5 :: BrowserAudioBuffer
  , p6 :: BrowserAudioBuffer
  , p7 :: BrowserAudioBuffer
  , p8 :: BrowserAudioBuffer
  , p9 :: BrowserAudioBuffer
  , p10 :: BrowserAudioBuffer
  , p11 :: BrowserAudioBuffer
  , p12 :: BrowserAudioBuffer
  , p13 :: BrowserAudioBuffer
  }

type Drones =
  { d0 :: BrowserAudioBuffer
  , d1 :: BrowserAudioBuffer
  , d2 :: BrowserAudioBuffer
  , d3 :: BrowserAudioBuffer
  , d4 :: BrowserAudioBuffer
  }

type OneShots =
  { o0 :: BrowserAudioBuffer
  , o1 :: BrowserAudioBuffer
  , o2 :: BrowserAudioBuffer
  , o3 :: BrowserAudioBuffer
  , o4 :: BrowserAudioBuffer
  , o5 :: BrowserAudioBuffer
  , o6 :: BrowserAudioBuffer
  , o7 :: BrowserAudioBuffer
  , o8 :: BrowserAudioBuffer
  , o9 :: BrowserAudioBuffer
  , o10 :: BrowserAudioBuffer
  , o11 :: BrowserAudioBuffer
  , o12 :: BrowserAudioBuffer
  , o13 :: BrowserAudioBuffer
  , o14 :: BrowserAudioBuffer
  , o15 :: BrowserAudioBuffer
  , o16 :: BrowserAudioBuffer
  , o17 :: BrowserAudioBuffer
  , o18 :: BrowserAudioBuffer
  , o19 :: BrowserAudioBuffer
  , o20 :: BrowserAudioBuffer
  , o21 :: BrowserAudioBuffer
  , o22 :: BrowserAudioBuffer
  , o23 :: BrowserAudioBuffer
  }

type Loops =
  { b0 :: BrowserAudioBuffer
  , b1 :: BrowserAudioBuffer
  , b2 :: BrowserAudioBuffer
  , b3 :: BrowserAudioBuffer
  , b4 :: BrowserAudioBuffer
  }

type Buffers =
  { synths ::
      { synth0 :: Synths
      , synth1 :: Synths
      , synth2 :: Synths
      , synth3 :: Synths
      , synth4 :: Synths
      }
  , drones :: Drones
  , oneShots :: OneShots
  , oneShotsBackwards :: OneShots
  , loops :: Loops
  , bells :: BrowserAudioBuffer
  , uncontrollable :: BrowserAudioBuffer
  }

type World =
  { buffers :: Buffers
  , waveshaperArray :: BrowserFloatArray
  , periodic :: BrowserPeriodicWave
  }

type Res = {}

derive instance newtypeTrigger :: Newtype IncomingEvent _
derive newtype instance toJSONTrigger :: JSON.ReadForeign IncomingEvent
derive newtype instance fromJSONTrigger :: JSON.WriteForeign IncomingEvent

data LeadSynth = Synth0 | Synth1 | Synth2 | Synth3 | Synth4
data PitchSynth = P0 | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9 | P10 | P11 | P12 | P13
data WhichSample = Sm0 | Sm1 | Sm2 | Sm3 | Sm4 | Sm5 | Sm6 | Sm7 | Sm8 | Sm9 | Sm10 | Sm11 | Sm12 | Sm13 | Sm14 | Sm15 | Sm16 | Sm17 | Sm18 | Sm19 | Sm20 | Sm21 | Sm22 | Sm23
data SampleRate = Sr0 | Sr1 | Sr2
data EnvelopeType = Env0 | Env1 | Env2
data OctaveType = Oct0 | Oct1 | Oct2

type TriggerLeadInfo =
  { n :: Int
  , nPitches :: Int
  , synthPitchProfile :: PitchSynth
  , synthPrefix :: LeadSynth
  , buffers :: Buffers
  , envType :: EnvelopeType
  , octaveLead :: OctaveType
  }

type TriggerOneShotInfo =
  { sampleReverse :: Boolean
  , sampleChooser :: WhichSample
  , sampleChorusEffect :: Boolean
  , sampleRateChange :: SampleRate
  , buffers :: Buffers
  }

type UncontrollableInfo = {}

newtype TriggerLeadNT = TriggerLeadNT
  ( forall proof
     . TriggerLeadInfo
    -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
  )

unTriggerLeadNT
  :: TriggerLeadNT
  -> forall proof
   . TriggerLeadInfo
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
unTriggerLeadNT (TriggerLeadNT f) = f

newtype TriggerOneShotNT = TriggerOneShotNT
  ( forall proof
     . TriggerOneShotInfo
    -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
  )

unTriggerOneShotNT
  :: TriggerOneShotNT
  -> forall proof
   . TriggerOneShotInfo
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
unTriggerOneShotNT (TriggerOneShotNT f) = f

newtype UncontrollableNT = UncontrollableNT
  ( forall proof
     . UncontrollableInfo
    -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
  )

unUncontrollableNT
  :: UncontrollableNT
  -> forall proof
   . UncontrollableInfo
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
unUncontrollableNT (UncontrollableNT f) = f

type Acc =
  { triggerLead :: Cofree Identity TriggerLeadNT
  , triggerOneShot :: Cofree Identity TriggerOneShotNT
  , uncontrollable :: Cofree Identity UncontrollableNT
  , synthPrefix :: LeadSynth
  , synthPitchProfile :: PitchSynth
  , nPitches :: Int
  , envType :: EnvelopeType
  , octaveLead :: OctaveType
  , droneActivationEnergyThreshold :: ZeroToOne
  , droneDecay :: ZeroToOne
  , leadDelayInfo ::
      { leadDelayLine0 :: Boolean
      , leadDelayLine1 :: Boolean
      , leadDelayLine2 :: Boolean
      , leadCombinedDelay0 :: Boolean
      , leadDelayGainCarousel :: ZeroToOne
      }
  , sampleReverse :: Boolean
  , sampleChooser :: WhichSample
  , sampleChorusEffect :: Boolean
  , sampleRateChange :: SampleRate
  , sampleDelayInfo ::
      { sampleDelayLine0 :: Boolean
      , sampleDelayLine1 :: Boolean
      , sampleDelayLine2 :: Boolean
      , sampleCombinedDelay0 :: Boolean
      , sampleDelayGainCarousel :: ZeroToOne
      }
  , loopingBufferInfo ::
      { loopingBuffer0 :: Boolean
      , loopingBuffer1 :: Boolean
      , loopingBuffer2 :: Boolean
      , loopingBuffer3 :: Boolean
      , loopingBuffer4 :: Boolean
      , loopingBufferGainDJ :: Boolean
      }
  }