module Feedback.Engine where

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
import Feedback.FullGraph (FullGraph)
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
import WAGS.Run (RunAudio, RunEngine, TriggeredScene)
import WAGS.WebAPI (AudioContext)

createFrame :: forall res. IxWAG RunAudio RunEngine Frame0 res () FullGraph Unit
createFrame = ipatch { microphone: empty, mediaElement: empty }

-- we inject oracle and initial acc to avoid rebuilding the engine whenever we change these
piece
  :: forall acc env res
  . Monoid res
  => acc
  -> (forall proof. env -> IxWAG RunAudio RunEngine proof res FullGraph FullGraph Unit)
  -> ( forall proof
        . env
       -> acc
       -> IxWAG RunAudio RunEngine proof res FullGraph FullGraph acc
     )
  -> Scene env RunAudio RunEngine Frame0 res
piece initialAcc setup oracle = (\env -> (createFrame :*> (setup env $> initialAcc))) @!> iloop oracle
