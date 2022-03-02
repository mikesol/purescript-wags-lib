module Feedback.App where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Monad.Indexed ((:*>))
import Control.Parallel (parallel, sequential)
import Control.Plus (empty)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Int (toNumber)
import Data.Lens (over)
import Data.Lens.Grate (grate)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Profunctor.Closed (class Closed)
import Data.Traversable (for_, sequence, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (D60)
import Data.Variant (inj)
import Data.Vec (Vec, (+>))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Log
import FRP.Event (Event, subscribe)
import Feedback.Acc (initialAcc)
import Feedback.Control (c2s, elts)
import Feedback.Engine (piece)
import Feedback.InnerComponent as InnerComponent
import Feedback.Oracle (oracle)
import Feedback.PubNub (PubNub, pubnubEvent)
import Feedback.Setup (setup)
import Feedback.Types (Acc, IncomingEvent(..), Res, Trigger(..), World, Buffers)
import Foreign.Object (values)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Control.Functions.Graph (iloop, (@!>))
import WAGS.Control.Indexed (IxWAG)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit (OversampleTwoX, TBandpass, TDelay, TGain, THighpass, TLoopBuf, TLowpass, TPlayBuf, TSinOsc, TSpeaker, TStereoPanner, TWaveShaper)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, makeFFIAudioSnapshot)
import WAGS.Patch (ipatch)
import WAGS.Run (BehavingRun, BehavingScene(..), RunAudio, RunEngine, TriggeredRun, run, runNoLoop)
import WAGS.WebAPI (AudioContext)

type State =
  { event :: Event IncomingEvent
  , pubnub :: PubNub
  , buffers :: Maybe Buffers
  }

data Action
  = Initialize

component :: forall query input output m. MonadEffect m => MonadAff m => Event IncomingEvent -> PubNub -> H.Component query input output m
component event pubnub =
  H.mkComponent
    { initialState: initialState event pubnub
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

type Slots = (svg :: forall query. H.Slot query Void Unit)
_svg = Proxy :: Proxy "svg"

initialState :: forall input. Event IncomingEvent -> PubNub -> input -> State
initialState event pubnub _ = { event, pubnub, buffers: Nothing }

render :: forall m. MonadEffect m => MonadAff m => State -> H.ComponentHTML Action Slots m
render { pubnub, event, buffers } =
  HH.div [ HP.classes $ map ClassName [ "w-screen", "h-screen" ] ] $
    maybe
    [ HH.text "Loading" ]
    ( append []
        <<< pure
        <<< flip (HH.slot_ _svg unit) unit
        <<< InnerComponent.component event pubnub
    )
    buffers

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  Initialize -> do
    audioCtx <- H.liftEffect context
    buffers <-
      H.liftAff
        $ map fromHomogeneous
        $ (map <<< map) fromHomogeneous
        $ sequential
        $ sequence
        $ (map <<< traverse) (parallel <<< decodeAudioDataFromUri audioCtx)
        $ map homogeneous
        $ homogeneous
            { synth0:
                { p0: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                , p1: "https://media.graphcms.com/TlkqOrhXQnmT2Fz2be14"
                , p2: "https://media.graphcms.com/jTyRKlYwQMOt1j1Oebn3"
                , p3: "https://media.graphcms.com/YPrWVetQHasqukdUAsIA"
                , p4: "https://media.graphcms.com/RirJEGSKTrm7VJflaSSJ"
                , p5: "https://media.graphcms.com/SBUMaygRpC69JAbm2CjA"
                , p6: "https://media.graphcms.com/YsWMuBjBSxKcpmT8paLq"
                , p7: "https://media.graphcms.com/MndOh6ZySdCJ7e4nTKhF"
                , p8: "https://media.graphcms.com/Ce2hOVVyTQSyvrAPyw5A"
                , p9: "https://media.graphcms.com/kogoJbrYSOeFh0geQGlx"
                , p10: "https://media.graphcms.com/ZOGZ3xTTba853tXg2KQg"
                , p11: "https://media.graphcms.com/afZV0u2SlVhkyYswbugO"
                , p12: "https://media.graphcms.com/7re9RgoQSVSJ81po5BDA"
                , p13: "https://media.graphcms.com/pOdVqoIARB67q9Fn7WCD"
                }
            , synth1:
                { p0: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                , p1: "https://media.graphcms.com/TlkqOrhXQnmT2Fz2be14"
                , p2: "https://media.graphcms.com/jTyRKlYwQMOt1j1Oebn3"
                , p3: "https://media.graphcms.com/YPrWVetQHasqukdUAsIA"
                , p4: "https://media.graphcms.com/RirJEGSKTrm7VJflaSSJ"
                , p5: "https://media.graphcms.com/SBUMaygRpC69JAbm2CjA"
                , p6: "https://media.graphcms.com/YsWMuBjBSxKcpmT8paLq"
                , p7: "https://media.graphcms.com/MndOh6ZySdCJ7e4nTKhF"
                , p8: "https://media.graphcms.com/Ce2hOVVyTQSyvrAPyw5A"
                , p9: "https://media.graphcms.com/kogoJbrYSOeFh0geQGlx"
                , p10: "https://media.graphcms.com/ZOGZ3xTTba853tXg2KQg"
                , p11: "https://media.graphcms.com/afZV0u2SlVhkyYswbugO"
                , p12: "https://media.graphcms.com/7re9RgoQSVSJ81po5BDA"
                , p13: "https://media.graphcms.com/pOdVqoIARB67q9Fn7WCD"
                }
            , synth2:
                { p0: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                , p1: "https://media.graphcms.com/TlkqOrhXQnmT2Fz2be14"
                , p2: "https://media.graphcms.com/jTyRKlYwQMOt1j1Oebn3"
                , p3: "https://media.graphcms.com/YPrWVetQHasqukdUAsIA"
                , p4: "https://media.graphcms.com/RirJEGSKTrm7VJflaSSJ"
                , p5: "https://media.graphcms.com/SBUMaygRpC69JAbm2CjA"
                , p6: "https://media.graphcms.com/YsWMuBjBSxKcpmT8paLq"
                , p7: "https://media.graphcms.com/MndOh6ZySdCJ7e4nTKhF"
                , p8: "https://media.graphcms.com/Ce2hOVVyTQSyvrAPyw5A"
                , p9: "https://media.graphcms.com/kogoJbrYSOeFh0geQGlx"
                , p10: "https://media.graphcms.com/ZOGZ3xTTba853tXg2KQg"
                , p11: "https://media.graphcms.com/afZV0u2SlVhkyYswbugO"
                , p12: "https://media.graphcms.com/7re9RgoQSVSJ81po5BDA"
                , p13: "https://media.graphcms.com/pOdVqoIARB67q9Fn7WCD"
                }
            , synth3:
                { p0: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                , p1: "https://media.graphcms.com/TlkqOrhXQnmT2Fz2be14"
                , p2: "https://media.graphcms.com/jTyRKlYwQMOt1j1Oebn3"
                , p3: "https://media.graphcms.com/YPrWVetQHasqukdUAsIA"
                , p4: "https://media.graphcms.com/RirJEGSKTrm7VJflaSSJ"
                , p5: "https://media.graphcms.com/SBUMaygRpC69JAbm2CjA"
                , p6: "https://media.graphcms.com/YsWMuBjBSxKcpmT8paLq"
                , p7: "https://media.graphcms.com/MndOh6ZySdCJ7e4nTKhF"
                , p8: "https://media.graphcms.com/Ce2hOVVyTQSyvrAPyw5A"
                , p9: "https://media.graphcms.com/kogoJbrYSOeFh0geQGlx"
                , p10: "https://media.graphcms.com/ZOGZ3xTTba853tXg2KQg"
                , p11: "https://media.graphcms.com/afZV0u2SlVhkyYswbugO"
                , p12: "https://media.graphcms.com/7re9RgoQSVSJ81po5BDA"
                , p13: "https://media.graphcms.com/pOdVqoIARB67q9Fn7WCD"
                }
            , synth4:
                { p0: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                , p1: "https://media.graphcms.com/TlkqOrhXQnmT2Fz2be14"
                , p2: "https://media.graphcms.com/jTyRKlYwQMOt1j1Oebn3"
                , p3: "https://media.graphcms.com/YPrWVetQHasqukdUAsIA"
                , p4: "https://media.graphcms.com/RirJEGSKTrm7VJflaSSJ"
                , p5: "https://media.graphcms.com/SBUMaygRpC69JAbm2CjA"
                , p6: "https://media.graphcms.com/YsWMuBjBSxKcpmT8paLq"
                , p7: "https://media.graphcms.com/MndOh6ZySdCJ7e4nTKhF"
                , p8: "https://media.graphcms.com/Ce2hOVVyTQSyvrAPyw5A"
                , p9: "https://media.graphcms.com/kogoJbrYSOeFh0geQGlx"
                , p10: "https://media.graphcms.com/ZOGZ3xTTba853tXg2KQg"
                , p11: "https://media.graphcms.com/afZV0u2SlVhkyYswbugO"
                , p12: "https://media.graphcms.com/7re9RgoQSVSJ81po5BDA"
                , p13: "https://media.graphcms.com/pOdVqoIARB67q9Fn7WCD"
                }
            , synth5:
                { p0: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                , p1: "https://media.graphcms.com/TlkqOrhXQnmT2Fz2be14"
                , p2: "https://media.graphcms.com/jTyRKlYwQMOt1j1Oebn3"
                , p3: "https://media.graphcms.com/YPrWVetQHasqukdUAsIA"
                , p4: "https://media.graphcms.com/RirJEGSKTrm7VJflaSSJ"
                , p5: "https://media.graphcms.com/SBUMaygRpC69JAbm2CjA"
                , p6: "https://media.graphcms.com/YsWMuBjBSxKcpmT8paLq"
                , p7: "https://media.graphcms.com/MndOh6ZySdCJ7e4nTKhF"
                , p8: "https://media.graphcms.com/Ce2hOVVyTQSyvrAPyw5A"
                , p9: "https://media.graphcms.com/kogoJbrYSOeFh0geQGlx"
                , p10: "https://media.graphcms.com/ZOGZ3xTTba853tXg2KQg"
                , p11: "https://media.graphcms.com/afZV0u2SlVhkyYswbugO"
                , p12: "https://media.graphcms.com/7re9RgoQSVSJ81po5BDA"
                , p13: "https://media.graphcms.com/pOdVqoIARB67q9Fn7WCD"
                }
            }
    H.modify_ (_ { buffers = Just buffers })
