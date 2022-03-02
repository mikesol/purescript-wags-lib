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
import Data.Typelevel.Num (class Pos, D14, D2, D3, D4, D5, D6, D7)
import Data.Variant (default, match, onMatch)
import Data.Vec (Vec, empty, replicate', zipWithE, (+>))
import Feedback.Constants as C
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Acc, Bang, Elts(..), EnvelopeType(..), LeadSynth(..), OctaveType(..), PitchSynth(..), Res, Trigger(..), TriggerLeadInfo, World, ZeroToOne(..), onElts, unTriggerLeadNT, updateAtElt)
import Math (sin, pi, pow)
import Math as M
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange, ichange')
import WAGS.Control.Indexed (IxWAG)
import WAGS.Graph.Parameter (AudioEnvelope(..), AudioSingleNumber(..), _linearRamp, singleNumber)
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

triggerPad :: ChangeSig ZeroToOne
triggerPad e _ a = ChangeWrapper (ichange' (Proxy :: _ "pad") (unwrap e) $> a)

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

-- 7.0 magic number
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

synthForLead :: ChangeSig (Elts D6)
synthForLead e _ a = ChangeWrapper
  ( ipure unit $> a
      { synthPrefix = onElts
          ( Synth0
              +> Synth1
              +> Synth2
              +> Synth3
              +> Synth4
              +> Synth5
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

data LeadDelay = LeadDelay0 | LeadDelay1 | LeadDelay2 | LeadDelayCombined

sliderToVal :: LeadDelay -> ZeroToOne -> Number
sliderToVal LeadDelay0 (ZeroToOne n) = (sin (2.0 * pi * n) * 0.5 + 0.5) * 0.4
sliderToVal LeadDelay1 (ZeroToOne n) = (sin (2.0 * pi * (n + 0.5)) * 0.5 + 0.5) * 0.3
sliderToVal LeadDelay2 (ZeroToOne n) = (sin (2.0 * pi * (n + 1.0)) * 0.5 + 0.5) * 0.2
sliderToVal LeadDelayCombined (ZeroToOne n) = (sin (2.0 * pi * (n + 1.5)) * 0.5 + 0.5) * 0.1

fauxBool :: Elts D2 -> Boolean
fauxBool = onElts (false +> true +> empty)

leadDelayLine0 :: ChangeSig (Elts D2)
leadDelayLine0 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "leadDelay0")
        $ fadeIO (sliderToVal LeadDelay0 a.leadDelayInfo.leadDelayGainCarousel) C.leadDelay0Duration e
    ) $> a { leadDelayInfo { leadDelayLine0 = fauxBool e } }
  )

leadDelayLine1 :: ChangeSig (Elts D2)
leadDelayLine1 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "leadDelay1")
        $ fadeIO (sliderToVal LeadDelay1 a.leadDelayInfo.leadDelayGainCarousel) C.leadDelay1Duration e
    ) $> a { leadDelayInfo { leadDelayLine1 = fauxBool e } }
  )

leadDelayLine2 :: ChangeSig (Elts D2)
leadDelayLine2 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "leadDelay2")
        $ fadeIO (sliderToVal LeadDelay2 a.leadDelayInfo.leadDelayGainCarousel) C.leadDelay2Duration e
    ) $> a { leadDelayInfo { leadDelayLine2 = fauxBool e } }
  )

leadCombinedDelay0 :: ChangeSig (Elts D2)
leadCombinedDelay0 e _ a = ChangeWrapper
  ( ( ichange' (Proxy :: _ "leadDelayCbnd")
        $ fadeIO (sliderToVal LeadDelayCombined a.leadDelayInfo.leadDelayGainCarousel) C.leadDelayCombinedDuration e
    ) $> a { leadDelayInfo { leadCombinedDelay0 = fauxBool e } }
  )

leadDelayGainCarousel :: ChangeSig ZeroToOne
leadDelayGainCarousel z _ a = ChangeWrapper do
  CBNA.when (a.leadDelayInfo.leadDelayLine0)
    ( \_ -> ichange' (Proxy :: _ "leadDelay0")
        $ AudioSingleNumber { param: sliderToVal LeadDelay0 z, timeOffset: C.leadDelaySliderDuration, transition: _linearRamp }
    )
  CBNA.when (a.leadDelayInfo.leadDelayLine1)
    ( \_ -> ichange' (Proxy :: _ "leadDelay1")
        $ AudioSingleNumber { param: sliderToVal LeadDelay1 z, timeOffset: C.leadDelaySliderDuration, transition: _linearRamp }
    )
  CBNA.when (a.leadDelayInfo.leadDelayLine2)
    ( \_ -> ichange' (Proxy :: _ "leadDelay2")
        $ AudioSingleNumber { param: sliderToVal LeadDelay2 z, timeOffset: C.leadDelaySliderDuration, transition: _linearRamp }
    )
  CBNA.when (a.leadDelayInfo.leadCombinedDelay0)
    ( \_ -> ichange' (Proxy :: _ "leadDelayCbnd")
        $ AudioSingleNumber { param: sliderToVal LeadDelayCombined z, timeOffset: C.leadDelaySliderDuration, transition: _linearRamp }
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

drone :: ChangeSig ZeroToOne
drone e _ a = ChangeWrapper (ichange' (Proxy :: _ "drone") (unwrap e) $> a)

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
        >>> onMatch
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
          }
          (default defaultChangeWrapper)
        >>> (#) env
        >>> (#) a
        >>> unChangeWrapper
    }
    trigger