module Feedback.Oracle where

import Prelude

import Control.Applicative.Indexed (ipure)
import Data.Newtype (unwrap)
import Data.Variant (onMatch, default)
import Feedback.FullGraph (FullGraph)
import Feedback.Types (Res, Acc, Trigger, World)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange, ichange')
import WAGS.Control.Functions.Graph (iloop)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene(..))

oracle
  :: forall proof
   . TriggeredScene Trigger World ()
  -> Acc
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
oracle (TriggeredScene { trigger }) a =
  onMatch
    { triggerPad: unwrap >>> ichange' (Proxy :: _ "pad")
    }
    (default (ipure unit))
    (unwrap trigger) $> a