module Feedback.Oracle where

import Prelude

import Control.Applicative.Indexed (ipure)
import Data.Array.NonEmpty (fromNonEmpty)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Typelevel.Num (D2)
import Data.Variant (onMatch, default)
import Data.Vec (empty, (+>))
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Acc, Elts, Res, Trigger, World, onElts)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange, ichange')
import WAGS.Control.Functions.Graph (iloop)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Graph.Parameter (AudioEnvelope(..))
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

padOsc0Vol = 0.05 :: Number
padOsc0Duration = 4.0 :: Number
padOsc1Vol = 0.05 :: Number
padOsc1Duration = 4.0 :: Number
padOsc2Vol = 0.05 :: Number
padOsc2Duration = 4.0 :: Number
padOsc3Vol = 0.05 :: Number
padOsc3Duration = 4.0 :: Number
padOsc4Vol = 0.05 :: Number
padOsc4Duration = 4.0 :: Number

fadeIO :: Number -> Number -> Elts D2 -> AudioEnvelope
fadeIO mxm duration = AudioEnvelope
  <<< { duration, timeOffset: 0.0, values: _ }
  <<< fromNonEmpty
  <<< onElts ((mxm :| [ 0.0 ]) +> (0.0 :| [ mxm ]) +> empty)

oracle
  :: forall proof
   . TriggeredScene Trigger World ()
  -> Acc
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
oracle (TriggeredScene { trigger }) a =
  onMatch
    { triggerPad: unwrap >>> ichange' (Proxy :: _ "pad")
    , togglePadOsc0: ichange' (Proxy :: _ "padSource0")
        <<< fadeIO padOsc0Vol padOsc0Duration
    , togglePadOsc1: ichange' (Proxy :: _ "padSource1")
        <<< fadeIO padOsc1Vol padOsc1Duration
    , togglePadOsc2: ichange' (Proxy :: _ "padSource2")
        <<< fadeIO padOsc2Vol padOsc2Duration
    , togglePadOsc3: ichange' (Proxy :: _ "padSource3")
        <<< fadeIO padOsc3Vol padOsc3Duration
    , togglePadOsc4: ichange' (Proxy :: _ "padSource4")
        <<< fadeIO padOsc4Vol padOsc4Duration
    }
    (default (ipure unit))
    (unwrap trigger) $> a