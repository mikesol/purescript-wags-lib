module Feedback.Setup where

import Prelude

import Control.Applicative.Indexed (ipure)
import Feedback.Constants as C
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Res, Acc, Trigger, World)
import WAGS.Change (ichange)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine)

setup :: forall proof. IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Unit
setup = ichange
  { padSource0Osc: C.padOsc0Freq
  , padSource1Osc: C.padOsc1Freq
  , padSource2Osc: C.padOsc2Freq
  , padSource3Osc: C.padOsc3Freq
  , padSource4Osc: C.padOsc4Freq
  , padSource0LFO: 1.0
  , padSource1LFO: 1.0
  , padSource2LFO: 1.0
  , padSource3LFO: 1.0
  , padSource4LFO: 1.0
  }