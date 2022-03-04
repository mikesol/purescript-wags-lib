module Feedback.App where

import Prelude

import Control.Parallel (parallel, sequential)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (Event, EventIO)
import Feedback.InnerComponent as InnerComponent
import Feedback.PubNub (PubNub)
import Feedback.Types (Buffers, IncomingEvent)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Record (union)
import Type.Proxy (Proxy(..))
import WAGS.Interpret (context, close, decodeAudioDataFromUri)
import WAGS.Lib.Tidal.Download (reverseAudioBuffer)

type State = { buffers :: Maybe Buffers }

data Action = Initialize

component :: forall query input output m. MonadEffect m => MonadAff m => Event IncomingEvent -> EventIO IncomingEvent -> PubNub -> H.Component query input output m
component localEvent remoteEvent pubnub =
  H.mkComponent
    { initialState
    , render: render localEvent remoteEvent pubnub
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

type Slots = (svg :: forall query. H.Slot query Void Unit)
_svg = Proxy :: Proxy "svg"

initialState :: forall input. input -> State
initialState _ = { buffers: Nothing }

klz :: forall r a. Array String -> IProp (class :: String | r) a
klz = HP.classes <<< map ClassName

render :: forall m. MonadEffect m => MonadAff m => Event IncomingEvent -> EventIO IncomingEvent -> PubNub -> State -> H.ComponentHTML Action Slots m
render remoteEvent localEvent pubnub { buffers } =
  HH.div [ klz [ "w-screen", "h-screen" ] ] $
    maybe
      [ HH.div [ klz [ "flex", "flex-col", "w-full", "h-full" ] ]
          [ HH.div [ klz [ "flex-grow" ] ] [ HH.div_ [] ]
          , HH.div [ klz [ "flex-grow-0", "flex", "flex-row" ] ]
              [ HH.div [ klz [ "flex-grow" ] ]
                  []
              , HH.div [ klz [ "flex", "flex-col" ] ]
                  [ HH.h1 [ klz [ "text-center", "text-3xl", "font-bold" ] ]
                      [ HH.text "Loading..." ]
                  ]
              , HH.div [ klz [ "flex-grow" ] ] []
              ]
          , HH.div [ klz [ "flex-grow" ] ] []
          ]
      ]
      ( append []
          <<< pure
          <<< flip (HH.slot_ _svg unit) unit
          <<< InnerComponent.component remoteEvent localEvent pubnub
      )
      buffers

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  Initialize -> do
    audioCtx <- H.liftEffect context
    buffers <-
      H.liftAff
        $ sequential
        $
          ( { synths: _
            , drones: _
            , oneShots: _
            , loops: _
            , bells: _
            , uncontrollable: _
            }
              <$>
                ( map fromHomogeneous
                    $ (map <<< map) fromHomogeneous
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
                )
              <*>
                ( map fromHomogeneous
                    $ traverse (parallel <<< decodeAudioDataFromUri audioCtx)
                    $ homogeneous
                        { d0: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , d1: "https://media.graphcms.com/TlkqOrhXQnmT2Fz2be14"
                        , d2: "https://media.graphcms.com/jTyRKlYwQMOt1j1Oebn3"
                        , d3: "https://media.graphcms.com/YPrWVetQHasqukdUAsIA"
                        , d4: "https://media.graphcms.com/RirJEGSKTrm7VJflaSSJ"
                        }
                )
              <*>
                ( map fromHomogeneous
                    $ traverse (parallel <<< decodeAudioDataFromUri audioCtx)
                    $ homogeneous
                        { o0: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o1: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o2: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o3: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o4: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o5: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o6: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o7: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o8: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o9: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o10: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o11: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o12: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o13: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o14: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o15: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o16: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o17: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o18: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o19: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o20: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o21: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o22: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , o23: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        }
                )
              <*>
                ( map fromHomogeneous
                    $ traverse (parallel <<< decodeAudioDataFromUri audioCtx)
                    $ homogeneous
                        { b0: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , b1: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , b2: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , b3: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        , b4: "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92"
                        }
                )
              <*> (parallel $ decodeAudioDataFromUri audioCtx "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92")
              <*> (parallel $ decodeAudioDataFromUri audioCtx "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92")
          )
    oneShotsBackwards <- H.liftEffect
      $ map fromHomogeneous
      $ traverse (reverseAudioBuffer audioCtx)
      $ homogeneous buffers.oneShots
    H.liftEffect $ close audioCtx
    H.modify_
      ( _
          { buffers = Just (union buffers { oneShotsBackwards })
          }
      )
