module Feedback.Oracle where

import Prelude

import Control.Applicative.Indexed (ipure)
import Data.Array.NonEmpty (fromNonEmpty, mapWithIndex)
import Data.Function (on)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Lens (_1, over, traversed)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Typelevel.Num (class Pos, D2, D4, D5, toInt')
import Data.Variant (default, match, onMatch)
import Data.Vec (Vec, empty, sortBy, zipWithE, (+>))
import Feedback.Constants as C
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Acc, Elts, Res, Trigger(..), World, ZeroToOne(..), onElts)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WAGS.Change (ichange, ichange')
import WAGS.Control.Functions.Graph (iloop)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Graph.Parameter (AudioEnvelope(..), AudioSingleNumber(..), _linearRamp, singleNumber)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

cyclingTransitions :: forall n. Pos n => Vec n Number -> Number -> Elts n -> AudioSingleNumber
cyclingTransitions v duration = AudioSingleNumber
  <<< { timeOffset: duration, transition: _linearRamp, param: _ }
  <<< onElts v

fadeIO :: Number -> Number -> Elts D2 -> AudioSingleNumber
fadeIO mxm = cyclingTransitions (0.0 +> mxm +> empty)

type ChangeSigP proof arg = arg -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
type ChangeSig arg = forall proof. arg -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit

triggerPad :: ChangeSig ZeroToOne
triggerPad = unwrap >>> ichange' (Proxy :: _ "pad")

togglePadOsc0 :: ChangeSig (Elts D2)
togglePadOsc0 = ichange' (Proxy :: _ "padSource0") <<< fadeIO C.padOsc0Vol C.padOsc0Duration

togglePadOsc1 :: ChangeSig (Elts D2)
togglePadOsc1 = ichange' (Proxy :: _ "padSource1")
  <<< fadeIO C.padOsc1Vol C.padOsc1Duration

togglePadOsc2 :: ChangeSig (Elts D2)
togglePadOsc2 = ichange' (Proxy :: _ "padSource2")
  <<< fadeIO C.padOsc2Vol C.padOsc2Duration

togglePadOsc3 :: ChangeSig (Elts D2)
togglePadOsc3 = ichange' (Proxy :: _ "padSource3")
  <<< fadeIO C.padOsc3Vol C.padOsc3Duration

togglePadOsc4 :: ChangeSig (Elts D2)
togglePadOsc4 = ichange' (Proxy :: _ "padSource4")
  <<< fadeIO C.padOsc4Vol C.padOsc4Duration

detunePad :: ChangeSig (Elts D4)
detunePad = ichange
  <<< fromHomogeneous
  <<< flap
    ( map (flip cyclingTransitions C.detuneDuration) $ homogeneous
        { padSource0Osc: C.padOsc0Freq +> C.padOsc0Freq +> C.padOsc0Freq - 10.0 +> C.padOsc0Freq +> empty
        , padSource1Osc: C.padOsc0Freq +> C.padOsc0Freq + 10.0 +> C.padOsc0Freq + 30.0 +> C.padOsc0Freq - 20.0 +> empty
        , padSource2Osc: C.padOsc0Freq +> C.padOsc0Freq + 35.0 +> C.padOsc0Freq + 70.0 +> C.padOsc0Freq + 70.0 +> empty
        , padSource3Osc: C.padOsc3Freq +> C.padOsc3Freq + 60.0 +> C.padOsc3Freq + 140.0 +> C.padOsc3Freq - 80.0 +> empty
        , padSource4Osc: C.padOsc4Freq +> C.padOsc4Freq + 210.0 +> C.padOsc4Freq + 430.0 +> C.padOsc4Freq - 150.0 +> empty
        }
    )

gainLFO0Pad :: ChangeSig ZeroToOne
gainLFO0Pad (ZeroToOne n) = ichange
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
  }

gainLFO1Pad :: ChangeSig ZeroToOne
gainLFO1Pad (ZeroToOne n) = ichange
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
  }

-- todo: ugh, singleNumber is a hack
-- change should be overloaded to handle that natively
filterBankChooserPad :: ChangeSig (Elts D5)
filterBankChooserPad = ichange
  <<< fromHomogeneous
  <<< map fromHomogeneous
  <<< (map <<< map) singleNumber
  <<< flip (map <<< flip flap)
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

waveshaperPad :: ChangeSig ZeroToOne
waveshaperPad (ZeroToOne n) = ichange
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
  }

oracle
  :: forall proof
   . TriggeredScene Trigger World ()
  -> Acc
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
oracle (TriggeredScene { trigger: (Trigger trigger) }) a =
  match
    { thunk: const $ ipure unit
    , event: unwrap >>> onMatch
        -- ugh, is there a way to thread proof to these functions so that we
        -- do not need the annotations?
        { triggerPad: triggerPad :: ChangeSigP proof ZeroToOne
        , togglePadOsc0: togglePadOsc0 :: ChangeSigP proof (Elts D2)
        , togglePadOsc1: togglePadOsc1 :: ChangeSigP proof (Elts D2)
        , togglePadOsc2: togglePadOsc2 :: ChangeSigP proof (Elts D2)
        , togglePadOsc3: togglePadOsc3 :: ChangeSigP proof (Elts D2)
        , togglePadOsc4: togglePadOsc4 :: ChangeSigP proof (Elts D2)
        , detunePad: detunePad :: ChangeSigP proof (Elts D4)
        , gainLFO0Pad: gainLFO0Pad :: ChangeSigP proof ZeroToOne
        , gainLFO1Pad: gainLFO1Pad :: ChangeSigP proof ZeroToOne
        , filterBankChooserPad: filterBankChooserPad :: ChangeSigP proof (Elts D5)
        , waveshaperPad: waveshaperPad :: ChangeSigP proof ZeroToOne
        }
        (default (ipure unit))
    }
    trigger $> a