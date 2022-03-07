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
import Feedback.PubNub (PubNub, PubNubEvent(..))
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

component :: forall query input output m. MonadEffect m => MonadAff m => Event PubNubEvent -> EventIO IncomingEvent -> PubNub -> H.Component query input output m
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

render :: forall m. MonadEffect m => MonadAff m => Event PubNubEvent -> EventIO IncomingEvent -> PubNub -> State -> H.ComponentHTML Action Slots m
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
                        { "synth0":
                            { "p7": "https://media.graphcms.com/jksK8BNSQ26EdFxHg7be"
                            , "p11": "https://media.graphcms.com/Z7zOxA8oTOWK4jm6gOpz"
                            , "p5": "https://media.graphcms.com/wdgJ3nvTqqWy42TfyG69"
                            , "p9": "https://media.graphcms.com/56bcksDjRZiaQbx4tyIE"
                            , "p8": "https://media.graphcms.com/9lCBrQKkT5VShGI0Ewdw"
                            , "p1": "https://media.graphcms.com/QytrJ6MvQfm2M2992NHE"
                            , "p2": "https://media.graphcms.com/6hHgoLtER5m61loRMYm4"
                            , "p13": "https://media.graphcms.com/EUIAMElQyq8TIhy0qmEY"
                            , "p0": "https://media.graphcms.com/dOJhpqUSAasq3ura1n1f"
                            , "p6": "https://media.graphcms.com/DhMBQ2LQmWJCEvOz3b6Q"
                            , "p4": "https://media.graphcms.com/bgQFoabhRvybGXMOdv77"
                            , "p10": "https://media.graphcms.com/tnII9TQ9SyiRrMXPtPBo"
                            , "p3": "https://media.graphcms.com/ZnGlnBo2QRmCa1KmDmrV"
                            , "p12": "https://media.graphcms.com/Lkj1kCNKRka6CE1XQGoQ"
                            }
                        , "synth1":
                            { "p9": "https://media.graphcms.com/yUsNnMsRFSJwKYXW0wW3"
                            , "p3": "https://media.graphcms.com/h2kidgn7TteXj5HiuHjD"
                            , "p11": "https://media.graphcms.com/W8xPrmDR3bJplQdv8CMQ"
                            , "p12": "https://media.graphcms.com/SgBqN0coRUONvXzDtHdV"
                            , "p0": "https://media.graphcms.com/CjHFSoXQ1SSK7tYMJsRG"
                            , "p2": "https://media.graphcms.com/nkI65G6tRCqhV93g3cH6"
                            , "p10": "https://media.graphcms.com/H8IsPTLyqfiwTnzq88QG"
                            , "p13": "https://media.graphcms.com/pLym6tUxTLWtizmMYRUx"
                            , "p5": "https://media.graphcms.com/BAcdeLn8Sp2p8wDr6OJx"
                            , "p4": "https://media.graphcms.com/bZDqgCjRScK0VT5JEWig"
                            , "p1": "https://media.graphcms.com/nipYeuomQyy79eGvRev4"
                            , "p6": "https://media.graphcms.com/JylJSTVpRnKomEPqtnkV"
                            , "p7": "https://media.graphcms.com/aQnHOpIRqCdbEFVQetog"
                            , "p8": "https://media.graphcms.com/ILlPHujORruOk45huALz"
                            }
                        , "synth2":
                            { "p8": "https://media.graphcms.com/PpUwWD0WRTy6DKMnuhDA"
                            , "p10": "https://media.graphcms.com/b43YO2bnSNef6Oi4bQEa"
                            , "p6": "https://media.graphcms.com/gTp0rOQwQuua8Y768PmH"
                            , "p12": "https://media.graphcms.com/R6zm2kktRe65tQKop8Zp"
                            , "p7": "https://media.graphcms.com/RStmAtKvQYKVqBFrIBiG"
                            , "p13": "https://media.graphcms.com/leOF49ppScvswM8QbTwR"
                            , "p0": "https://media.graphcms.com/27GrZzVFTf6GQ6KfHgA6"
                            , "p1": "https://media.graphcms.com/ijMM4vQURVuoEMyJSCPS"
                            , "p3": "https://media.graphcms.com/KGR2dUqrRJSZm1MHea13"
                            , "p11": "https://media.graphcms.com/RocTCen8QYSuMR9LrBgw"
                            , "p4": "https://media.graphcms.com/XLfC0r6FTwCRLkUJbzFh"
                            , "p9": "https://media.graphcms.com/D10pLDiQ1mgvm15V5qjF"
                            , "p5": "https://media.graphcms.com/Uwhw9edzSPSgkUpxfb12"
                            , "p2": "https://media.graphcms.com/ENy72a74R1GGJzrxDcoF"
                            }
                        , "synth3":
                            { "p4": "https://media.graphcms.com/fClM8PP0RRaIRawgiSeF"
                            , "p13": "https://media.graphcms.com/cRyyT5ShQHyXi9o6hydR"
                            , "p12": "https://media.graphcms.com/WelNxMHKRdasvAeRGtbX"
                            , "p8": "https://media.graphcms.com/2DKKr5FxQYi185MhGS9a"
                            , "p9": "https://media.graphcms.com/Yd2GZ6huQyi0mRakPPFH"
                            , "p11": "https://media.graphcms.com/Od37oYLeRUOsGymUkKX3"
                            , "p0": "https://media.graphcms.com/eLWR6315TwiCjwsojik6"
                            , "p10": "https://media.graphcms.com/S69FvbP2QfSPxkuhRqbt"
                            , "p6": "https://media.graphcms.com/zyArFH2RQxmBe1kJByVw"
                            , "p1": "https://media.graphcms.com/gaBRJ7fXSjWY8UxgAJby"
                            , "p5": "https://media.graphcms.com/FAFreqOmS8SvNQ4NkQ9J"
                            , "p3": "https://media.graphcms.com/plO0y65QVeL9Hl37ypqp"
                            , "p2": "https://media.graphcms.com/adYtWURlRMWi3zwQP2NJ"
                            , "p7": "https://media.graphcms.com/Bb3hEJ1uSqaLFVkCTSxS"
                            }
                        , "synth4":
                            { "p13": "https://media.graphcms.com/XUyCo7RJCKgdlicn8PQR"
                            , "p12": "https://media.graphcms.com/85luyLmoRnW0ApKgQqha"
                            , "p11": "https://media.graphcms.com/jIw1LHgjRGa9z6gUf2Mq"
                            , "p9": "https://media.graphcms.com/FAzdmjzORuiEJ873jxMx"
                            , "p6": "https://media.graphcms.com/VqZiqRU4QLyB7HyiyzKZ"
                            , "p5": "https://media.graphcms.com/lMf6eCeRR6HoYpN7Xl67"
                            , "p10": "https://media.graphcms.com/PICNMUAXRaObVOLfV3o6"
                            , "p2": "https://media.graphcms.com/Qdwyk0GTUCNhGXHmlHLc"
                            , "p4": "https://media.graphcms.com/WSzbgC7RLa5mPdDxeUvN"
                            , "p8": "https://media.graphcms.com/8SuhTfIgQxmZ3HG8wb60"
                            , "p3": "https://media.graphcms.com/HEL7VCuETBK622mPn495"
                            , "p0": "https://media.graphcms.com/4ezgcpu8SMy42nvUlFiS"
                            , "p7": "https://media.graphcms.com/Cgh7BktxTVv07rolgZYS"
                            , "p1": "https://media.graphcms.com/TH9yuCnRouSgpBONvqKM"
                            }
                        }
                )
              <*>
                ( map fromHomogeneous
                    $ traverse (parallel <<< decodeAudioDataFromUri audioCtx)
                    $ homogeneous
                        { d0: "https://media.graphcms.com/iURIigm8Tw6h6dK6ttPh"
                        , d1: "https://media.graphcms.com/XjuQQdDZTeyRQeLW5jv1"
                        , d2: "https://media.graphcms.com/BpypeBvWTcychjFAaYxe"
                        , d3: "https://media.graphcms.com/PXHmh8t9SY1PX5pkswRC"
                        , d4: "https://media.graphcms.com/b3vV3nhjTGertiRokSmz"
                        }
                )
              <*>

                ( map fromHomogeneous
                    $ traverse (parallel <<< decodeAudioDataFromUri audioCtx)
                    $ homogeneous
                        { "o18": "https://media.graphcms.com/JwxemnFSKaRGlqsk1GaQ"
                        , "o21": "https://media.graphcms.com/dMraVvsjR8myfTi5Myw8"
                        , "o20": "https://media.graphcms.com/k5vCLP0TgK48wuoPkL8A"
                        , "o23": "https://media.graphcms.com/XfMvHSIxTGenIE1GSl6r"
                        , "o14": "https://media.graphcms.com/XNQMaf9EQvmrIwbvjAmy"
                        , "o22": "https://media.graphcms.com/UjYFQu8xSw6ud6TW92WS"
                        , "o8": "https://media.graphcms.com/Ezy67CUQ2K2n1yZBloQW"
                        , "o17": "https://media.graphcms.com/o5SzcolWSdK4YNQ7lZkg"
                        , "o15": "https://media.graphcms.com/WQ1tD1x1RnaNdPeBOOgL"
                        , "o16": "https://media.graphcms.com/Pqf3mVeRSeGVXVET0Uv5"
                        , "o13": "https://media.graphcms.com/ksJ73SzsS4aC0JcQiA0s"
                        , "o10": "https://media.graphcms.com/a5Zjv7fQx2Bvbxcbl8QA"
                        , "o11": "https://media.graphcms.com/ZjuW1QzRfC5CUHGRA7NG"
                        , "o1": "https://media.graphcms.com/TbXlCanZR0qN09NivHIw"
                        , "o4": "https://media.graphcms.com/3qbM58JtSZiybw9AsPzM"
                        , "o12": "https://media.graphcms.com/ZOqpZPlETKyqGJLLItLz"
                        , "o3": "https://media.graphcms.com/xh59gMm1SjO13nIDpNmo"
                        , "o7": "https://media.graphcms.com/iEfAcWmQTgmlE2z2iwtB"
                        , "o0": "https://media.graphcms.com/fvFuVeXgROef0UoveCWK"
                        , "o6": "https://media.graphcms.com/mOksMhMwRDOiV0WKUy5T"
                        , "o19": "https://media.graphcms.com/LlXm4uSSQKqfSy1x8Xq6"
                        , "o2": "https://media.graphcms.com/DG60R3dhQcih9pMo5KO7"
                        , "o9": "https://media.graphcms.com/pXZOZT8vSaeeCOqSiHq2"
                        , "o5": "https://media.graphcms.com/QaSwKG9USNSJ4fvsVmAp"
                        }
                )
              <*>
                ( map fromHomogeneous
                    $ traverse (parallel <<< decodeAudioDataFromUri audioCtx)
                    $ homogeneous
                        {
                          -- https://freesound.org/people/esistnichtsoernst/sounds/432822/
                          b0: "https://freesound.org/data/previews/432/432822_4438873-hq.mp3"
                        -- https://freesound.org/people/djfroyd/sounds/331679/
                        , b1: "https://freesound.org/data/previews/331/331679_4766646-hq.mp3"
                        -- https://freesound.org/people/fran_ky/sounds/53658/
                        , b2: "https://freesound.org/data/previews/53/53658_110398-hq.mp3"
                        -- https://freesound.org/people/matiasromero/sounds/21710/
                        , b3: "https://freesound.org/data/previews/21/21710_117392-hq.mp3"
                        -- https://freesound.org/people/frankum/sounds/171285/
                        , b4: "https://freesound.org/data/previews/171/171285_2305278-hq.mp3"
                        }
                )
              -- a snippet of https://freesound.org/people/Zabuhailo/sounds/178648/
              <*> (parallel $ decodeAudioDataFromUri audioCtx "https://media.graphcms.com/R7hkyD9DTOK6D2UsKb92")
              -- https://freesound.org/people/ihatetoregister/sounds/31822/
              <*> (parallel $ decodeAudioDataFromUri audioCtx "https://freesound.org/data/previews/31/31822_266486-hq.mp3")
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
