module Feedback.Types where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Typelevel.Num (class Pos, D14, D2, D24, D3, D4, D5, D6, toInt')
import Data.Variant (Variant)
import Data.Vec (Vec)
import Foreign (ForeignError(..), fail)
import Simple.JSON (readImpl, undefined, writeImpl)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

data Bang

instance toJsonBang :: JSON.WriteForeign Bang where
  writeImpl = const undefined

instance fromJsonBang :: JSON.ReadForeign Bang where
  readImpl = const $ unsafeCoerce undefined


type PadT = ZeroToOne
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
type SynthForLead = Elts D6
type PitchLead = Elts D14
type NPitchesPlayedLead = Int
type EnvelopeLead = Elts D3
type OctaveLead = Elts D3
type LeadDelayLine0 = Elts D2
type LeadDelayLine1 = Elts D2
type LeadDelayLine2 = Elts D2
type LeadCombinedDelay0 = Elts D2
type LeadDelayGainCarousel = SliderT
type Drone = PadT
type DroneChooser = Elts D4
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
type EchoingUncontrollableSingleton = Elts D2
type DistantBellsFader = SliderT

newtype Trigger = Trigger (Variant (AllEvents TriggerPad TogglePadOsc0 TogglePadOsc1 TogglePadOsc2 TogglePadOsc3 TogglePadOsc4 DetunePad GainLFO0Pad GainLFO1Pad FilterBankChooserPad WaveshaperPad TriggerLead SynthForLead PitchLead NPitchesPlayedLead EnvelopeLead OctaveLead LeadDelayLine0 LeadDelayLine1 LeadDelayLine2 LeadCombinedDelay0 LeadDelayGainCarousel Drone DroneChooser DroneLowpass0Q DroneBandpass0Q DroneBandpass0LFO DroneBandpass1Q DroneBandpass1LFO DroneHighpass0Q DroneHighpass0LFO DroneActivationEnergyThreshold DroneRhythmicLoopingPiecewiseFunction DroneDecay DroneFlange SampleOneShot SampleReverse SampleChooser SampleRateChange SampleChorusEffect SampleDelayLine0 SampleDelayLine1 SampleDelayLine2 SampleCombinedDelayLine0 SampleDelayGainCarousel LeadSampleDelayLine0 LeadSampleDelayLine1 LeadSampleDelayLine2 LoopingBufferStartEndConstriction LoopingBufferGainDJ LoopingBuffer0 LoopingBuffer1 LoopingBuffer2 LoopingBuffer3 LoopingBuffer4 RadicalFlip GreatAndMightyPan GlobalDelay EchoingUncontrollableSingleton DistantBellsFader))

type World = {}
type Res = {}
type Acc = {}

derive instance newtypeTrigger :: Newtype Trigger _
derive newtype instance toJSONTrigger :: JSON.ReadForeign Trigger
derive newtype instance fromJSONTrigger :: JSON.WriteForeign Trigger