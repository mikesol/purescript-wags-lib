module Feedback.Oracle where

import Prelude

import Feedback.FullGraph (FullGraph)
import Feedback.Types (Res, Acc, Trigger, World)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Graph (iloop)
import WAGS.Control.Indexed (IxWAG)
import WAGS.Run (RunAudio, RunEngine, TriggeredScene)

oracle
  :: forall proof
   . TriggeredScene Trigger World ()
  -> Acc
  -> IxWAG RunAudio RunEngine proof Res FullGraph FullGraph Acc
oracle _  a = ichange {} $> a