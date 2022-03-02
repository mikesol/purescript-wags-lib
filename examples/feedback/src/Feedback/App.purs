module Feedback.App where

import Prelude

import Control.Parallel (parallel, sequential)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (Event)
import Feedback.InnerComponent as InnerComponent
import Feedback.PubNub (PubNub)
import Feedback.Types (Buffers, IncomingEvent)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import WAGS.Interpret (context, decodeAudioDataFromUri)

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
