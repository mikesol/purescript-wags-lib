module Feedback.Oracle where

import Prelude

import CallByName.Applicative as CBNA
import Control.Applicative.Indexed (ipure)
import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Array ((..))
import Data.Foldable (foldl)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Newtype (unwrap)
import Data.Typelevel.Num (class Pos, D14, D2, D3, D4, D5, D6, D7, D24)
import Data.Variant (default, match, onMatch)
import Data.Vec (Vec, empty, replicate', zipWithE, (+>))
import Feedback.Constants as C
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Acc, Bang, Elts(..), EnvelopeType(..), LeadSynth(..), OctaveType(..), PadT, PitchSynth(..), Res, SampleRate(..), Trigger(..), TriggerLeadInfo, WhichSample(..), World, ZeroToOne(..), ezto, onElts, unTriggerLeadNT, unTriggerOneShotNT, unUncontrollableNT, updateAtElt)
import Math (sin, pi, pow, (%))
import Math as M
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange, ichange')
import WAGS.Control.Indexed (IxWAG)
import WAGS.Graph.Parameter (AudioEnvelope(..), AudioSingleNumber(..), _linearRamp, singleNumber)
import WAGS.Math (calcSlope)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

cyclingTransitions :: forall n. Pos n => Vec n Number -> Number -> Elts n -> AudioSingleNumber
cyclingTransitions v duration = AudioSingleNumber
  <<< { timeOffset: duration, transition: _linearRamp, param: _ }
  <<< onElts v

fadeIO :: Number -> Number -> Elts D2 -> AudioSingleNumber
fadeIO mxm = cyclingTransitions (0.0 +> mxm +> empty)

newtype ChangeWrapper = ChangeWrapper (forall proof. IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc)

unChangeWrapper :: ChangeWrapper -> forall proof. IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
unChangeWrapper (ChangeWrapper x) = x

type ChangeSig arg = arg -> TriggeredScene Trigger World () -> Acc -> ChangeWrapper

triggerPad :: ChangeSig PadT
triggerPad { v } _ a = ChangeWrapper (ichange' (Proxy :: _ "pad") (unwrap v) $> a)

togglePadOsc0 :: ChangeSig (Elts D2)
togglePadOsc0 e _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "padSource0") (fadeIO C.padOsc0Vol C.padOsc0Duration e) $> a
  )

togglePadOsc1 :: ChangeSig (Elts D2)
togglePadOsc1 e _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "padSource1")
      (fadeIO C.padOsc1Vol C.padOsc1Duration e) $> a
  )

togglePadOsc2 :: ChangeSig (Elts D2)
togglePadOsc2 e _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "padSource2")
      (fadeIO C.padOsc2Vol C.padOsc2Duration e) $> a
  )

togglePadOsc3 :: ChangeSig (Elts D2)
togglePadOsc3 e _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "padSource3")
      (fadeIO C.padOsc3Vol C.padOsc3Duration e) $> a
  )

togglePadOsc4 :: ChangeSig (Elts D2)
togglePadOsc4 e _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "padSource4")
      (fadeIO C.padOsc4Vol C.padOsc4Duration e) $> a
  )

detunePad :: ChangeSig (Elts D4)
detunePad e _ a = ChangeWrapper
  ( ichange
      ( fromHomogeneous
          $ flap
              ( map (flip cyclingTransitions C.detuneDuration) $ homogeneous
                  { padSource0Osc: C.padOsc0Freq +> C.padOsc0Freq +> C.padOsc0Freq - 10.0 +> C.padOsc0Freq +> empty
                  , padSource1Osc: C.padOsc0Freq +> C.padOsc0Freq + 10.0 +> C.padOsc0Freq + 30.0 +> C.padOsc0Freq - 20.0 +> empty
                  , padSource2Osc: C.padOsc0Freq +> C.padOsc0Freq + 35.0 +> C.padOsc0Freq + 70.0 +> C.padOsc0Freq + 70.0 +> empty
                  , padSource3Osc: C.padOsc3Freq +> C.padOsc3Freq + 60.0 +> C.padOsc3Freq + 140.0 +> C.padOsc3Freq - 80.0 +> empty
                  , padSource4Osc: C.padOsc4Freq +> C.padOsc4Freq + 210.0 +> C.padOsc4Freq + 430.0 +> C.padOsc4Freq - 150.0 +> empty
                  }
              )
              e
      ) $> a
  )

gainLFO0Pad :: ChangeSig ZeroToOne
gainLFO0Pad (ZeroToOne n) _ a = ChangeWrapper
  ( ichange
      { padSource2LFO: AudioEnvelope
          { duration: 10000.0 + (1.0 - n) * 15000.0
          , timeOffset: C.lfoTimeOffset
          , values: if n < 0.5 then C.longLFOU0 else if n < 0.75 then C.longLFOU1 else if n < 0.9 then C.longLFOU2 else C.longLFOU3
          }
      , padSource3LFO: AudioEnvelope
          { duration: 6600.0 + (1.0 - n) * 20000.0
          , timeOffset: C.lfoTimeOffset
          , values: if n < 0.4 then C.longLFOU0 else if n < 0.6 then C.longLFOU1 else if n < 0.8 then C.longLFOU2 else C.longLFOU3
          }
      , padSource4LFO: AudioEnvelope
          { duration: 2500.0 + (1.0 - n) * 20000.0
          , timeOffset: C.lfoTimeOffset
          , values: if n < 0.25 then C.longLFOU0 else if n < 0.5 then C.longLFOU1 else if n < 0.75 then C.longLFOU2 else C.longLFOU3
          }
      } $> a
  )

gainLFO1Pad :: ChangeSig ZeroToOne
gainLFO1Pad (ZeroToOne n) _ a = ChangeWrapper
  ( ichange
      { padSource0LFO: AudioEnvelope
          { duration: 20000.0 + (1.0 - n) * 15000.0
          , timeOffset: C.lfoTimeOffset
          , values: if n < 0.8 then C.longLFOU1 else if n < 0.94 then C.longLFOU2 else C.longLFOU3
          }
      , padSource1LFO: AudioEnvelope
          { duration: 15000.0 + (1.0 - n) * 20000.0
          , timeOffset: C.lfoTimeOffset
          , values: if n < 0.7 then C.longLFOU0 else if n < 0.8 then C.longLFOU1 else if n < 0.9 then C.longLFOU2 else C.longLFOU3
          }
      } $> a
  )

-- todo: ugh, singleNumber is a hack
-- change should be overloaded to handle that natively
filterBankChooserPad :: ChangeSig (Elts D5)
filterBankChooserPad e _ a = ChangeWrapper
  ( ichange
      ( fromHomogeneous
          $ map fromHomogeneous
          $ (map <<< map) singleNumber
          $ flip (map <<< flip flap)
              ( (map <<< map) (flip cyclingTransitions C.detuneDuration) $ homogeneous
                  { padFilter0: homogeneous
                      { freq: C.padFilter0Freq +> C.padFilter0Freq +> C.padFilter0Freq +> C.padFilter0Freq +> C.padFilter0Freq +> empty
                      , q: C.padFilter0Q +> C.padFilter0Q +> C.padFilter0Q +> C.padFilter0Q +> C.padFilter0Q +> empty
                      }
                  , padFilter1: homogeneous
                      { freq: C.padFilter1Freq +> C.padFilter1Freq +> C.padFilter1Freq +> C.padFilter1Freq +> C.padFilter1Freq +> empty
                      , q: C.padFilter1Q +> C.padFilter1Q +> C.padFilter1Q +> C.padFilter1Q +> C.padFilter1Q +> empty
                      }
                  , padFilter2: homogeneous
                      { freq: C.padFilter2Freq +> C.padFilter2Freq +> C.padFilter2Freq +> C.padFilter2Freq +> C.padFilter2Freq +> empty
                      , q: C.padFilter2Q +> C.padFilter2Q +> C.padFilter2Q +> C.padFilter2Q +> C.padFilter2Q +> empty
                      }
                  , padFilter3: homogeneous
                      { freq: C.padFilter3Freq +> C.padFilter3Freq +> C.padFilter3Freq +> C.padFilter3Freq +> C.padFilter3Freq +> empty
                      , q: C.padFilter3Q +> C.padFilter3Q +> C.padFilter3Q +> C.padFilter3Q +> C.padFilter3Q +> empty
                      }
                  }
              )
              e
      ) $> a
  )

waveshaperPad :: ChangeSig ZeroToOne
waveshaperPad (ZeroToOne n) _ a = ChangeWrapper
  ( ichange
      { padDry: AudioSingleNumber
          { param: 1.0 - (n * 0.5)
          , timeOffset: C.waveshaperTimeOffset
          , transition: _linearRamp
          }
      , padWaveshaped: AudioSingleNumber
          { param: n * 0.5
          , timeOffset: C.waveshaperTimeOffset
          , transition: _linearRamp
          }
      } $> a
  )

triggerLead :: ChangeSig Bang
triggerLead _ (TriggeredScene { world: { buffers } }) acc =
  ChangeWrapper o
  where
  o :: forall proof. IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
  o =
    ( foldl (>=>) (pure :: Acc -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc)
        ( map
            ( \n a ->
                ((unTriggerLeadNT $ extract a.triggerLead) :: TriggerLeadInfo -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit)
                  { n
                  , nPitches: a.nPitches
                  , synthPitchProfile: a.synthPitchProfile
                  , synthPrefix: a.synthPrefix
                  , envType: a.envType
                  , octaveLead: a.octaveLead
                  , buffers
                  } $> a { triggerLead = unwrap $ unwrapCofree a.triggerLead }
            )
            (1 .. acc.nPitches)
        )
        acc
    )

synthForLead :: ChangeSig (Elts D5)
synthForLead e _ a = ChangeWrapper
  ( ipure unit $> a
      { synthPrefix = onElts
          ( Synth0
              +> Synth1
              +> Synth2
              +> Synth3
              +> Synth4
              +> empty
          )
          e
      }
  )

pitchLead :: ChangeSig (Elts D14)
pitchLead e _ a = ChangeWrapper
  ( ipure unit $> a
      { synthPitchProfile = onElts
          ( P0
              +> P1
              +> P2
              +> P3
              +> P4
              +> P5
              +> P6
              +> P7
              +> P8
              +> P9
              +> P10
              +> P11
              +> P12
              +> P13
              +> empty
          )
          e
      }
  )

data LeadDelay
  = LeadDelay0
  | LeadDelay1
  | LeadDelay2
  | LeadDelayCombined

data LoopingBuffer
  = LoopingBuffer0
  | LoopingBuffer1
  | LoopingBuffer2
  | LoopingBuffer3
  | LoopingBuffer4

loopingBufferSliderToVal :: LoopingBuffer -> Boolean -> Number
loopingBufferSliderToVal LoopingBuffer0 false = 0.6
loopingBufferSliderToVal LoopingBuffer0 true = 0.05
loopingBufferSliderToVal LoopingBuffer1 false = 0.5
loopingBufferSliderToVal LoopingBuffer1 true = 0.1
loopingBufferSliderToVal LoopingBuffer2 false = 0.4
loopingBufferSliderToVal LoopingBuffer2 true = 0.2
loopingBufferSliderToVal LoopingBuffer3 false = 0.1
loopingBufferSliderToVal LoopingBuffer3 true = 0.5
loopingBufferSliderToVal LoopingBuffer4 false = 0.05
loopingBufferSliderToVal LoopingBuffer4 true = 0.5

leadDelaySliderToVal :: LeadDelay -> ZeroToOne -> Number
leadDelaySliderToVal LeadDelay0 (ZeroToOne n) = (sin (2.0 * pi * n) * 0.5 + 0.5) * 0.4
leadDelaySliderToVal LeadDelay1 (ZeroToOne n) = (sin (2.0 * pi * (n + 0.5)) * 0.5 + 0.5) * 0.3
leadDelaySliderToVal LeadDelay2 (ZeroToOne n) = (sin (2.0 * pi * (n + 1.0)) * 0.5 + 0.5) * 0.2
leadDelaySliderToVal LeadDelayCombined (ZeroToOne n) = (sin (2.0 * pi * (n + 1.5)) * 0.5 + 0.5) * 0.1

fauxBool :: Elts D2 -> Boolean
fauxBool = onElts (false +> true +> empty)

leadDelayLine0 :: ChangeSig (Elts D2)
leadDelayLine0 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "leadDelay0")
        $ fadeIO (leadDelaySliderToVal LeadDelay0 a.leadDelayInfo.leadDelayGainCarousel) C.leadDelay0Duration e
    ) $> a { leadDelayInfo { leadDelayLine0 = fauxBool e } }
  )

leadDelayLine1 :: ChangeSig (Elts D2)
leadDelayLine1 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "leadDelay1")
        $ fadeIO (leadDelaySliderToVal LeadDelay1 a.leadDelayInfo.leadDelayGainCarousel) C.leadDelay1Duration e
    ) $> a { leadDelayInfo { leadDelayLine1 = fauxBool e } }
  )

leadDelayLine2 :: ChangeSig (Elts D2)
leadDelayLine2 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "leadDelay2")
        $ fadeIO (leadDelaySliderToVal LeadDelay2 a.leadDelayInfo.leadDelayGainCarousel) C.leadDelay2Duration e
    ) $> a { leadDelayInfo { leadDelayLine2 = fauxBool e } }
  )

leadCombinedDelay0 :: ChangeSig (Elts D2)
leadCombinedDelay0 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "leadDelayCbnd")
        $ fadeIO (leadDelaySliderToVal LeadDelayCombined a.leadDelayInfo.leadDelayGainCarousel) C.leadDelayCombinedDuration e
    ) $> a { leadDelayInfo { leadCombinedDelay0 = fauxBool e } }
  )

leadDelayGainCarousel :: ChangeSig ZeroToOne
leadDelayGainCarousel z _ a = ChangeWrapper do
  CBNA.when (a.leadDelayInfo.leadDelayLine0)
    ( \_ -> ichange' (Proxy :: _ "leadDelay0")
        $ AudioSingleNumber { param: leadDelaySliderToVal LeadDelay0 z, timeOffset: C.leadDelaySliderDuration, transition: _linearRamp }
    )
  CBNA.when (a.leadDelayInfo.leadDelayLine1)
    ( \_ -> ichange' (Proxy :: _ "leadDelay1")
        $ AudioSingleNumber { param: leadDelaySliderToVal LeadDelay1 z, timeOffset: C.leadDelaySliderDuration, transition: _linearRamp }
    )
  CBNA.when (a.leadDelayInfo.leadDelayLine2)
    ( \_ -> ichange' (Proxy :: _ "leadDelay2")
        $ AudioSingleNumber { param: leadDelaySliderToVal LeadDelay2 z, timeOffset: C.leadDelaySliderDuration, transition: _linearRamp }
    )
  CBNA.when (a.leadDelayInfo.leadCombinedDelay0)
    ( \_ -> ichange' (Proxy :: _ "leadDelayCbnd")
        $ AudioSingleNumber { param: leadDelaySliderToVal LeadDelayCombined z, timeOffset: C.leadDelaySliderDuration, transition: _linearRamp }
    )
  pure (a { leadDelayInfo { leadDelayGainCarousel = z } })

nPitchesPlayedLead :: ChangeSig (Elts D7)
nPitchesPlayedLead (Elts n) _ a = ChangeWrapper (ipure unit $> (a { nPitches = n }))

envelopeLead :: ChangeSig (Elts D3)
envelopeLead e _ a = ChangeWrapper
  ( ipure unit $> a
      { envType = onElts
          ( Env0
              +> Env1
              +> Env2
              +> empty
          )
          e
      }
  )

octaveLead :: ChangeSig (Elts D3)
octaveLead e _ a = ChangeWrapper
  ( ipure unit $> a
      { octaveLead = onElts
          ( Oct0
              +> Oct1
              +> Oct2
              +> empty
          )
          e
      }
  )

drone :: ChangeSig PadT
drone { v, ud } _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "drone")
      ( if ud then (unwrap v * (1.0 - 0.99 * unwrap a.droneActivationEnergyThreshold))
        else ((unwrap v) `pow` (1.0 - unwrap a.droneDecay))
      ) $> a
  )

droneSources :: forall proof. Vec D5 (Number -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit)
droneSources = (\p -> ichange' (Proxy :: _ "droneSource0") (AudioSingleNumber { param: p, timeOffset: C.droneFade, transition: _linearRamp }))
  +> (\p -> ichange' (Proxy :: _ "droneSource1") (AudioSingleNumber { param: p, timeOffset: C.droneFade, transition: _linearRamp }))
  +> (\p -> ichange' (Proxy :: _ "droneSource2") (AudioSingleNumber { param: p, timeOffset: C.droneFade, transition: _linearRamp }))
  +> (\p -> ichange' (Proxy :: _ "droneSource3") (AudioSingleNumber { param: p, timeOffset: C.droneFade, transition: _linearRamp }))
  +> (\p -> ichange' (Proxy :: _ "droneSource4") (AudioSingleNumber { param: p, timeOffset: C.droneFade, transition: _linearRamp }))
  +> empty

defaultDroneVolumes :: Vec D5 Number
defaultDroneVolumes = replicate' 0.0

droneChooser :: ChangeSig (Elts D5)
droneChooser e _ a = ChangeWrapper (foldl (*>) (pure unit) (zipWithE ($) droneSources (updateAtElt (const 1.0) defaultDroneVolumes e)) $> a)

droneLowpass0Q :: ChangeSig ZeroToOne
droneLowpass0Q e _ a = ChangeWrapper (ichange' (Proxy :: _ "droneFilter0") { q: M.e `pow` (unwrap e * 3.5 - 0.5) } $> a)

droneBandpass0Q :: ChangeSig ZeroToOne
droneBandpass0Q e _ a = ChangeWrapper (ichange' (Proxy :: _ "droneFilter1") { q: M.e `pow` (unwrap e * 3.5 - 0.5) } $> a)

droneBandpass0LFO :: ChangeSig ZeroToOne
droneBandpass0LFO (ZeroToOne n) _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "droneFilter1")
      ( AudioEnvelope
          { duration: 10000.0 + (1.0 - n) * 15000.0
          , timeOffset: C.lfoTimeOffset
          , values: C.droneBandpass0LFO
          }
      ) $> a
  )

droneBandpass1Q :: ChangeSig ZeroToOne
droneBandpass1Q e _ a = ChangeWrapper (ichange' (Proxy :: _ "droneFilter2") { q: M.e `pow` (unwrap e * 3.5 - 0.5) } $> a)

droneBandpass1LFO :: ChangeSig ZeroToOne
droneBandpass1LFO (ZeroToOne n) _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "droneFilter2")
      ( AudioEnvelope
          { duration: 10000.0 + (1.0 - n) * 15000.0
          , timeOffset: C.lfoTimeOffset
          , values: C.droneBandpass1LFO
          }
      ) $> a
  )

droneHighpass0Q :: ChangeSig ZeroToOne
droneHighpass0Q e _ a = ChangeWrapper (ichange' (Proxy :: _ "droneFilter3") { q: M.e `pow` (unwrap e * 3.5 - 0.5) } $> a)

droneHighpass0LFO :: ChangeSig ZeroToOne
droneHighpass0LFO (ZeroToOne n) _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "droneFilter3")
      ( AudioEnvelope
          { duration: 10000.0 + (1.0 - n) * 15000.0
          , timeOffset: C.lfoTimeOffset
          , values: C.droneBandpass1LFO
          }
      ) $> a
  )

droneActivationEnergyThreshold :: ChangeSig ZeroToOne
droneActivationEnergyThreshold n _ a = ChangeWrapper (ipure unit $> (a { droneActivationEnergyThreshold = n }))

droneRhythmicLoopingPiecewiseFunction :: ChangeSig (Elts D5)
droneRhythmicLoopingPiecewiseFunction n _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "dronePiecewiseGain")
      ( AudioEnvelope
          { duration: 10000.0 + (onElts (0.0 +> 1.0 +> 0.5 +> 0.25 +> 0.1 +> empty) n) * 15000.0
          , timeOffset: C.lfoTimeOffset
          , values: onElts (C.alwaysOpen +> C.longLFOU1 +> C.longLFOU2 +> C.longLFOU3 +> C.longLFOU3 +> empty) n
          }
      ) $> a
  )

droneDecay :: ChangeSig ZeroToOne
droneDecay n _ a = ChangeWrapper (ipure unit $> (a { droneDecay = n }))

droneFlange :: ChangeSig (Elts D2)
droneFlange e _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "droneDelayed") (fadeIO C.droneFlangeVol C.droneFlangeFadeDuration e) $> a
  )

sampleOneShot :: ChangeSig Bang
sampleOneShot _ (TriggeredScene { world: { buffers } }) acc =
  ChangeWrapper o
  where
  o :: forall proof. IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
  o =
    unTriggerOneShotNT (extract acc.triggerOneShot)
      { sampleReverse: acc.sampleReverse
      , sampleChooser: acc.sampleChooser
      , sampleChorusEffect: acc.sampleChorusEffect
      , sampleRateChange: acc.sampleRateChange
      , buffers
      } $> acc { triggerOneShot = unwrap $ unwrapCofree acc.triggerOneShot }

sampleReverse :: ChangeSig (Elts D2)
sampleReverse e _ a = ChangeWrapper
  ( ipure unit $> a
      { sampleReverse = onElts
          (false +> true +> empty)
          e
      }
  )

sampleChooser :: ChangeSig (Elts D24)
sampleChooser e _ a = ChangeWrapper
  ( ipure unit $> a
      { sampleChooser = onElts
          ( Sm0
              +> Sm1
              +> Sm2
              +> Sm3
              +> Sm4
              +> Sm5
              +> Sm6
              +> Sm7
              +> Sm8
              +> Sm9
              +> Sm10
              +> Sm11
              +> Sm12
              +> Sm13
              +> Sm14
              +> Sm15
              +> Sm16
              +> Sm17
              +> Sm18
              +> Sm19
              +> Sm20
              +> Sm21
              +> Sm22
              +> Sm23
              +> empty
          )
          e
      }
  )

sampleChorusEffect :: ChangeSig (Elts D2)
sampleChorusEffect e _ a = ChangeWrapper
  ( ipure unit $> a
      { sampleChorusEffect = onElts
          (false +> true +> empty)
          e
      }
  )

sampleRateChange :: ChangeSig (Elts D3)
sampleRateChange e _ a = ChangeWrapper
  ( ipure unit $> a
      { sampleRateChange = onElts
          (Sr0 +> Sr1 +> Sr2 +> empty)
          e
      }
  )

sampleDelayLine0 :: ChangeSig (Elts D2)
sampleDelayLine0 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "smplrDelay0")
        $ fadeIO (leadDelaySliderToVal LeadDelay0 a.sampleDelayInfo.sampleDelayGainCarousel) C.sampleDelay0Duration e
    ) $> a { sampleDelayInfo { sampleDelayLine0 = fauxBool e } }
  )

sampleDelayLine1 :: ChangeSig (Elts D2)
sampleDelayLine1 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "smplrDelay1")
        $ fadeIO (leadDelaySliderToVal LeadDelay1 a.sampleDelayInfo.sampleDelayGainCarousel) C.sampleDelay1Duration e
    ) $> a { sampleDelayInfo { sampleDelayLine1 = fauxBool e } }
  )

sampleDelayLine2 :: ChangeSig (Elts D2)
sampleDelayLine2 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "smplrDelay2")
        $ fadeIO (leadDelaySliderToVal LeadDelay2 a.sampleDelayInfo.sampleDelayGainCarousel) C.sampleDelay2Duration e
    ) $> a { sampleDelayInfo { sampleDelayLine2 = fauxBool e } }
  )

sampleCombinedDelayLine0 :: ChangeSig (Elts D2)
sampleCombinedDelayLine0 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "smplrDelayCbnd")
        $ fadeIO (leadDelaySliderToVal LeadDelayCombined a.sampleDelayInfo.sampleDelayGainCarousel) C.sampleDelayCombinedDuration e
    ) $> a { sampleDelayInfo { sampleCombinedDelay0 = fauxBool e } }
  )

sampleDelayGainCarousel :: ChangeSig ZeroToOne
sampleDelayGainCarousel z _ a = ChangeWrapper do
  CBNA.when (a.sampleDelayInfo.sampleDelayLine0)
    ( \_ -> ichange' (Proxy :: _ "smplrDelay0")
        $ AudioSingleNumber { param: leadDelaySliderToVal LeadDelay0 z, timeOffset: C.sampleDelaySliderDuration, transition: _linearRamp }
    )
  CBNA.when (a.sampleDelayInfo.sampleDelayLine1)
    ( \_ -> ichange' (Proxy :: _ "smplrDelay1")
        $ AudioSingleNumber { param: leadDelaySliderToVal LeadDelay1 z, timeOffset: C.sampleDelaySliderDuration, transition: _linearRamp }
    )
  CBNA.when (a.sampleDelayInfo.sampleDelayLine2)
    ( \_ -> ichange' (Proxy :: _ "smplrDelay2")
        $ AudioSingleNumber { param: leadDelaySliderToVal LeadDelay2 z, timeOffset: C.sampleDelaySliderDuration, transition: _linearRamp }
    )
  CBNA.when (a.sampleDelayInfo.sampleCombinedDelay0)
    ( \_ -> ichange' (Proxy :: _ "smplrDelayCbnd")
        $ AudioSingleNumber { param: leadDelaySliderToVal LeadDelayCombined z, timeOffset: C.sampleDelaySliderDuration, transition: _linearRamp }
    )
  pure (a { sampleDelayInfo { sampleDelayGainCarousel = z } })

-- todo: un-hardcode values
leadSampleDelayLine0 :: ChangeSig (Elts D2)
leadSampleDelayLine0 e _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "slCombo0") (fadeIO 1.0 1.0 e) $> a
  )

leadSampleDelayLine1 :: ChangeSig (Elts D2)
leadSampleDelayLine1 e _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "slCombo1") (fadeIO 1.0 1.0 e) $> a
  )

leadSampleDelayLine2 :: ChangeSig (Elts D2)
leadSampleDelayLine2 e _ a = ChangeWrapper
  ( ichange' (Proxy :: _ "slCombo2") (fadeIO 1.0 1.0 e) $> a
  )

--- looping
loopingBuffer0 :: ChangeSig (Elts D2)
loopingBuffer0 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "loopingBuffer0")
        $ fadeIO (loopingBufferSliderToVal LoopingBuffer0 a.loopingBufferInfo.loopingBufferGainDJ) C.loopingBuffer0Duration e
    ) $> a { loopingBufferInfo { loopingBuffer0 = fauxBool e } }
  )

loopingBuffer1 :: ChangeSig (Elts D2)
loopingBuffer1 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "loopingBuffer1")
        $ fadeIO (loopingBufferSliderToVal LoopingBuffer1 a.loopingBufferInfo.loopingBufferGainDJ) C.loopingBuffer1Duration e
    ) $> a { loopingBufferInfo { loopingBuffer1 = fauxBool e } }
  )

loopingBuffer2 :: ChangeSig (Elts D2)
loopingBuffer2 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "loopingBuffer2")
        $ fadeIO (loopingBufferSliderToVal LoopingBuffer2 a.loopingBufferInfo.loopingBufferGainDJ) C.loopingBuffer2Duration e
    ) $> a { loopingBufferInfo { loopingBuffer2 = fauxBool e } }
  )

loopingBuffer3 :: ChangeSig (Elts D2)
loopingBuffer3 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "loopingBuffer3")
        $ fadeIO (loopingBufferSliderToVal LoopingBuffer3 a.loopingBufferInfo.loopingBufferGainDJ) C.loopingBuffer2Duration e
    ) $> a { loopingBufferInfo { loopingBuffer2 = fauxBool e } }
  )

loopingBuffer4 :: ChangeSig (Elts D2)
loopingBuffer4 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "loopingBuffer4")
        $ fadeIO (loopingBufferSliderToVal LoopingBuffer4 a.loopingBufferInfo.loopingBufferGainDJ) C.loopingBuffer2Duration e
    ) $> a { loopingBufferInfo { loopingBuffer2 = fauxBool e } }
  )

-- ugh, forgot the gain dj is a switch
-- use ezto as a hack, but maybe try to make it a slider?
loopingBufferGainDJ :: ChangeSig (Elts D2)
loopingBufferGainDJ z' _ a =
  let
    z = onElts (false +> true +> empty) z'
  in
    ChangeWrapper do
      CBNA.when (a.loopingBufferInfo.loopingBuffer0)
        ( \_ -> ichange' (Proxy :: _ "loopingBuffer0")
            $ AudioSingleNumber { param: loopingBufferSliderToVal LoopingBuffer0 z, timeOffset: C.loopingBufferSliderDuration, transition: _linearRamp }
        )
      CBNA.when (a.loopingBufferInfo.loopingBuffer1)
        ( \_ -> ichange' (Proxy :: _ "loopingBuffer1")
            $ AudioSingleNumber { param: loopingBufferSliderToVal LoopingBuffer1 z, timeOffset: C.loopingBufferSliderDuration, transition: _linearRamp }
        )
      CBNA.when (a.loopingBufferInfo.loopingBuffer2)
        ( \_ -> ichange' (Proxy :: _ "loopingBuffer2")
            $ AudioSingleNumber { param: loopingBufferSliderToVal LoopingBuffer2 z, timeOffset: C.loopingBufferSliderDuration, transition: _linearRamp }
        )
      CBNA.when (a.loopingBufferInfo.loopingBuffer3)
        ( \_ -> ichange' (Proxy :: _ "loopingBuffer3")
            $ AudioSingleNumber { param: loopingBufferSliderToVal LoopingBuffer3 z, timeOffset: C.loopingBufferSliderDuration, transition: _linearRamp }
        )
      CBNA.when (a.loopingBufferInfo.loopingBuffer4)
        ( \_ -> ichange' (Proxy :: _ "loopingBuffer4")
            $ AudioSingleNumber { param: loopingBufferSliderToVal LoopingBuffer4 z, timeOffset: C.loopingBufferSliderDuration, transition: _linearRamp }
        )
      pure (a { loopingBufferInfo { loopingBufferGainDJ = z } })

---

-- easter egg... save for later as it is too complex for now
radicalFlip :: ChangeSig (Elts D2)
radicalFlip _ _ a = ChangeWrapper (ipure a)

globalDelay :: ChangeSig (Elts D2)
globalDelay e _ a = ChangeWrapper do
  ichange' (Proxy :: _ "subMainGain") (fadeIO 0.06 1.0 e)
  pure a

-- 6.0 magic number for end of loop
lse :: Number -> Number -> Number -> { loopStart :: Number, loopEnd :: Number }
lse x y z =
  { loopStart: calcSlope 0.0 0.0 1.0 (x - y) z
  , loopEnd: calcSlope 0.0 6.0 1.0 (x + y) z
  }

loopingBufferStartEndConstriction :: ChangeSig ZeroToOne
loopingBufferStartEndConstriction (ZeroToOne z) _ a = ChangeWrapper do
  ichange' (Proxy :: _ "loopingBuffer0PlayBuf") (lse 1.0 0.3 z)
  ichange' (Proxy :: _ "loopingBuffer1PlayBuf") (lse 2.0 0.3 z)
  ichange' (Proxy :: _ "loopingBuffer2PlayBuf") (lse 3.0 0.3 z)
  ichange' (Proxy :: _ "loopingBuffer3PlayBuf") (lse 4.0 0.3 z)
  ichange' (Proxy :: _ "loopingBuffer4PlayBuf") (lse 5.0 0.3 z)
  pure a

-- global pan extravaganza
greatAndMightyPan :: ChangeSig ZeroToOne
greatAndMightyPan (ZeroToOne z) _ a = ChangeWrapper do
  ichange' (Proxy :: Proxy "loopingBuffer0Pan") ((z * 15.0 + 1.0) % 15.0)
  ichange' (Proxy :: Proxy "loopingBuffer1Pan") ((z * 15.0 + 2.0) % 15.0)
  ichange' (Proxy :: Proxy "loopingBuffer2Pan") ((z * 15.0 + 3.0) % 15.0)
  ichange' (Proxy :: Proxy "loopingBuffer3Pan") ((z * 15.0 + 4.0) % 15.0)
  ichange' (Proxy :: Proxy "loopingBuffer4Pan") ((z * 15.0 + 5.0) % 15.0)
  ichange' (Proxy :: Proxy "distantBellsPan") ((z * 15.0 + 6.0) % 15.0)
  ichange' (Proxy :: Proxy "uSingleton") ((z * 15.0 + 7.0) % 15.0)
  ichange' (Proxy :: Proxy "padSource0Pan") ((z * 15.0 + 8.0) % 15.0)
  ichange' (Proxy :: Proxy "padSource1Pan") ((z * 15.0 + 9.0) % 15.0)
  ichange' (Proxy :: Proxy "padSource2Pan") ((z * 15.0 + 10.0) % 15.0)
  ichange' (Proxy :: Proxy "padSource3Pan") ((z * 15.0 + 11.0) % 15.0)
  ichange' (Proxy :: Proxy "padSource4Pan") ((z * 15.0 + 12.0) % 15.0)
  ichange' (Proxy :: Proxy "smplr") ((z * 15.0 + 13.0) % 15.0)
  ichange' (Proxy :: Proxy "lead") ((z * 15.0 + 14.0) % 15.0)
  pure a

echoingUncontrollableSingleton :: ChangeSig Bang
echoingUncontrollableSingleton _ _ acc =
  ChangeWrapper o
  where
  o :: forall proof. IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
  o =
    unUncontrollableNT (extract acc.uncontrollable)
      {} $> acc { uncontrollable = unwrap $ unwrapCofree acc.uncontrollable }

distantBellsFader :: ChangeSig ZeroToOne
distantBellsFader (ZeroToOne z) _ a = ChangeWrapper do
  ichange' (Proxy :: Proxy "distantBells")
    ( AudioSingleNumber
        { param: z
        , timeOffset: 2.0
        , transition: _linearRamp
        }
    )
  pure a

defaultChangeWrapper :: TriggeredScene Trigger World () -> Acc -> ChangeWrapper
defaultChangeWrapper _ a = ChangeWrapper (ipure a)

oracle
  :: forall proof
   . TriggeredScene Trigger World ()
  -> Acc
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
oracle env@(TriggeredScene { trigger: (Trigger trigger) }) a =
  match
    { thunk: const $ ipure unit $> a
    , event: unwrap
        >>> match
          { triggerPad
          , togglePadOsc0
          , togglePadOsc1
          , togglePadOsc2
          , togglePadOsc3
          , togglePadOsc4
          , detunePad
          , gainLFO0Pad
          , gainLFO1Pad
          , filterBankChooserPad
          , waveshaperPad
          , triggerLead
          , synthForLead
          , pitchLead
          , leadDelayLine0
          , leadDelayLine1
          , leadDelayLine2
          , leadCombinedDelay0
          , leadDelayGainCarousel
          , nPitchesPlayedLead
          , envelopeLead
          , octaveLead
          , drone
          , droneChooser
          , droneLowpass0Q
          , droneBandpass0Q
          , droneBandpass0LFO
          , droneBandpass1Q
          , droneBandpass1LFO
          , droneHighpass0Q
          , droneHighpass0LFO
          , droneActivationEnergyThreshold
          , droneRhythmicLoopingPiecewiseFunction
          , droneDecay
          , droneFlange
          , sampleOneShot
          , sampleReverse
          , sampleChooser
          , sampleChorusEffect
          , sampleRateChange
          , sampleDelayLine0
          , sampleDelayLine1
          , sampleDelayLine2
          , sampleCombinedDelayLine0
          , sampleDelayGainCarousel
          , leadSampleDelayLine0
          , leadSampleDelayLine1
          , leadSampleDelayLine2
          , loopingBufferGainDJ
          , loopingBuffer0
          , loopingBuffer1
          , loopingBuffer2
          , loopingBuffer3
          , loopingBuffer4
          , radicalFlip
          , greatAndMightyPan
          , echoingUncontrollableSingleton
          , loopingBufferStartEndConstriction
          , globalDelay
          , distantBellsFader
          }
        --(default defaultChangeWrapper)
        >>> (#) env
        >>> (#) a
        >>> unChangeWrapper
    }
    trigger