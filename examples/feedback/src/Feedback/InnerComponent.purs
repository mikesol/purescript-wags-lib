module Feedback.InnerComponent where

import Prelude

import Control.Alt ((<|>))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Traversable (for_)
import Data.Typelevel.Num (class Pos, toInt')
import Data.Variant (default, inj, onMatch)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Log
import FRP.Event (Event, EventIO, subscribe)
import Feedback.Acc (initialAcc)
import Feedback.Control (Action(..), State, SliderAction(..), c2s, elts)
import Feedback.Engine (piece)
import Feedback.Oracle (oracle)
import Feedback.PubNub (PubNub)
import Feedback.Setup (setup)
import Feedback.Types (Buffers, Elts(..), IncomingEvent, IncomingEvent', Res, Trigger(..), ZeroToOne(..))
import Foreign.Object (fromHomogeneous, values)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Type.Proxy (Proxy(..))
import WAGS.Interpret (close, context, makeFFIAudioSnapshot)
import WAGS.Run (TriggeredRun, runNoLoop)

component :: forall query input output m. MonadEffect m => MonadAff m => Event IncomingEvent -> EventIO IncomingEvent -> PubNub -> Buffers -> H.Component query input output m
component remoteEvent localEvent pubnub buffers =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction remoteEvent localEvent pubnub buffers
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  , unsubscribeHalogen: Nothing
  , interactions:
      { gainLFO0Pad: 0.0
      , gainLFO0PadDown: false
      , gainLFO1Pad: 0.0
      , gainLFO1PadDown: false
      , waveshaperPad: 0.0
      , waveshaperPadDown: false
      , pitchLead: 0.0
      , pitchLeadDown: false
      , leadDelayGainCarousel: 0.0
      , leadDelayGainCarouselDown: false
      , nPitchesPlayedLead: 0.0
      , nPitchesPlayedLeadDown: false
      , droneLowpass0Q: 0.0
      , droneLowpass0QDown: false
      , droneBandpass0Q: 0.0
      , droneBandpass0QDown: false
      , droneBandpass0LFO: 0.0
      , droneBandpass0LFODown: false
      , droneBandpass1Q: 0.0
      , droneBandpass1QDown: false
      , droneBandpass1LFO: 0.0
      , droneBandpass1LFODown: false
      , droneHighpass0Q: 0.0
      , droneHighpass0QDown: false
      , droneHighpass0LFO: 0.0
      , droneHighpass0LFODown: false
      , droneActivationEnergyThreshold: 0.0
      , droneActivationEnergyThresholdDown: false
      , droneDecay: 0.0
      , droneDecayDown: false
      , sampleChooser: 0.0
      , sampleChooserDown: false
      , sampleDelayGainCarousel: 0.0
      , sampleDelayGainCarouselDown: false
      , loopingBufferStartEndConstriction: 0.0
      , loopingBufferStartEndConstrictionDown: false
      , greatAndMightyPan: 0.0
      , greatAndMightyPanDown: false
      , distantBellsFader: 0.0
      , distantBellsFaderDown: false
      }
  }

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

render :: forall m. State -> H.ComponentHTML Action () m
render st = case st.audioCtx of
  Nothing -> HH.div [ HP.classes $ map ClassName [ "w-full", "h-full" ] ]
    [ HH.div [ classes [ "flex", "flex-col", "w-full", "h-full" ] ]
        [ HH.div [ classes [ "flex-grow" ] ] [ HH.div_ [] ]
        , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
            [ HH.div [ classes [ "flex-grow" ] ]
                []
            , HH.div [ classes [ "flex", "flex-col" ] ]

                [ HH.h1 [ classes [ "text-center", "text-3xl", "font-bold" ] ]
                    [ HH.text "Loaded! Ready when you are..."
                    ]
                , HH.button
                    [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> StartAudio ]
                    [ HH.text "Start audio" ]
                ]
            , HH.div [ classes [ "flex-grow" ] ] []
            ]
        , HH.div [ classes [ "flex-grow" ] ] []
        ]
    ]
  Just _ -> SE.svg
    [ SA.classes $ map ClassName [ "w-full", "h-full" ]
    , SA.viewBox 0.0 0.0 1000.0 1000.0
    , SA.preserveAspectRatio Nothing SA.Slice
    ]
    (join $ map (c2s st) $ values $ fromHomogeneous elts)

handleSlider
  :: forall output m
   . MonadEffect m
  => MonadAff m
  => (IncomingEvent -> Effect Unit)
  -> (State -> Number -> Boolean -> State)
  -> (State -> Boolean)
  -> (State -> Number)
  -> (Number -> IncomingEvent')
  -> SliderAction
  -> H.HalogenM State Action () output m Unit
handleSlider push f _ _ mv (SliderDown n) = do
  H.modify_ (\s -> f s n true)
  H.liftEffect $ push (wrap $ mv n)
handleSlider push f fb _ mv (SliderMove n) = do
  H.modify_ (\s -> if fb s then f s n true else s)
  H.liftEffect $ push (wrap $ mv n)
handleSlider _ f _ fn _ SliderUp = do
  H.modify_ (\s -> f s (fn s) false)
handleSlider push f fb _ mv (SliderRemoteMove n) = do
  H.modify_ (\s -> f s n (fb s))
  H.liftEffect $ push (wrap $ mv n)

clamp01 :: Number -> ZeroToOne
clamp01 n =
  ZeroToOne do
    if n < 0.0 then 0.0 else if n > 1.0 then 1.0 else n

nFromZeroOne :: forall n. Pos n => ZeroToOne -> Elts n
nFromZeroOne = let ii = toInt' (Proxy :: _ n) in \(ZeroToOne n) -> Elts $ min ii $ max 0 $ floor (n * toNumber ii)

uzto :: ZeroToOne -> Number
uzto (ZeroToOne n) = n

ezto :: forall n. Pos n => Elts n -> Number
ezto = let ii = toInt' (Proxy :: _ n) in \(Elts n) -> toNumber n / toNumber ii

handleAction :: forall output m. MonadEffect m => MonadAff m => Event IncomingEvent -> EventIO IncomingEvent -> PubNub -> Buffers -> Action -> H.HalogenM State Action () output m Unit
handleAction remoteEvent localEvent pubnub buffers = case _ of
  GainLFO0Pad sliderAction -> handleSlider
    localEvent.push
    _ { interactions { gainLFO0Pad = _, gainLFO0PadDown = _ } }
    _.interactions.gainLFO0PadDown
    _.interactions.gainLFO0Pad
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "gainLFO0Pad"
        )
    )
    sliderAction
  GainLFO1Pad sliderAction -> handleSlider
    localEvent.push
    _ { interactions { gainLFO1Pad = _, gainLFO1PadDown = _ } }
    _.interactions.gainLFO1PadDown
    _.interactions.gainLFO1Pad
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "gainLFO1Pad"
        )
    )
    sliderAction
  WaveshaperPad sliderAction -> handleSlider
    localEvent.push
    _ { interactions { waveshaperPad = _, waveshaperPadDown = _ } }
    _.interactions.waveshaperPadDown
    _.interactions.waveshaperPad
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "waveshaperPad"
        )
    )
    sliderAction
  PitchLead sliderAction -> handleSlider
    localEvent.push
    _ { interactions { pitchLead = _, pitchLeadDown = _ } }
    _.interactions.pitchLeadDown
    _.interactions.pitchLead
    ( clamp01 >>> nFromZeroOne >>> inj
        ( Proxy
            :: Proxy
                 "pitchLead"
        )
    )
    sliderAction
  LeadDelayGainCarousel sliderAction -> handleSlider
    localEvent.push
    _ { interactions { leadDelayGainCarousel = _, leadDelayGainCarouselDown = _ } }
    _.interactions.leadDelayGainCarouselDown
    _.interactions.leadDelayGainCarousel
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "leadDelayGainCarousel"
        )
    )
    sliderAction
  NPitchesPlayedLead sliderAction -> handleSlider
    localEvent.push
    _ { interactions { nPitchesPlayedLead = _, nPitchesPlayedLeadDown = _ } }
    _.interactions.nPitchesPlayedLeadDown
    _.interactions.nPitchesPlayedLead
    ( clamp01 >>> nFromZeroOne >>> inj
        ( Proxy
            :: Proxy
                 "nPitchesPlayedLead"
        )
    )
    sliderAction
  DroneLowpass0Q sliderAction -> handleSlider
    localEvent.push
    _ { interactions { droneLowpass0Q = _, droneLowpass0QDown = _ } }
    _.interactions.droneLowpass0QDown
    _.interactions.droneLowpass0Q
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "droneLowpass0Q"
        )
    )
    sliderAction
  DroneBandpass0Q sliderAction -> handleSlider
    localEvent.push
    _ { interactions { droneBandpass0Q = _, droneBandpass0QDown = _ } }
    _.interactions.droneBandpass0QDown
    _.interactions.droneBandpass0Q
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "droneBandpass0Q"
        )
    )
    sliderAction
  DroneBandpass0LFO sliderAction -> handleSlider
    localEvent.push
    _ { interactions { droneBandpass0LFO = _, droneBandpass0LFODown = _ } }
    _.interactions.droneBandpass0LFODown
    _.interactions.droneBandpass0LFO
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "droneBandpass0LFO"
        )
    )
    sliderAction
  DroneBandpass1Q sliderAction -> handleSlider
    localEvent.push
    _ { interactions { droneBandpass1Q = _, droneBandpass1QDown = _ } }
    _.interactions.droneBandpass1QDown
    _.interactions.droneBandpass1Q
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "droneBandpass1Q"
        )
    )
    sliderAction
  DroneBandpass1LFO sliderAction -> handleSlider
    localEvent.push
    _ { interactions { droneBandpass1LFO = _, droneBandpass1LFODown = _ } }
    _.interactions.droneBandpass1LFODown
    _.interactions.droneBandpass1LFO
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "droneBandpass1LFO"
        )
    )
    sliderAction
  DroneHighpass0Q sliderAction -> handleSlider
    localEvent.push
    _ { interactions { droneHighpass0Q = _, droneHighpass0QDown = _ } }
    _.interactions.droneHighpass0QDown
    _.interactions.droneHighpass0Q
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "droneHighpass0Q"
        )
    )
    sliderAction
  DroneHighpass0LFO sliderAction -> handleSlider
    localEvent.push
    _ { interactions { droneHighpass0LFO = _, droneHighpass0LFODown = _ } }
    _.interactions.droneHighpass0LFODown
    _.interactions.droneHighpass0LFO
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "droneHighpass0LFO"
        )
    )
    sliderAction
  DroneActivationEnergyThreshold sliderAction -> handleSlider
    localEvent.push
    _ { interactions { droneActivationEnergyThreshold = _, droneActivationEnergyThresholdDown = _ } }
    _.interactions.droneActivationEnergyThresholdDown
    _.interactions.droneActivationEnergyThreshold
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "droneActivationEnergyThreshold"
        )
    )
    sliderAction
  DroneDecay sliderAction -> handleSlider
    localEvent.push
    _ { interactions { droneDecay = _, droneDecayDown = _ } }
    _.interactions.droneDecayDown
    _.interactions.droneDecay
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "droneDecay"
        )
    )
    sliderAction
  SampleChooser sliderAction -> handleSlider
    localEvent.push
    _ { interactions { sampleChooser = _, sampleChooserDown = _ } }
    _.interactions.sampleChooserDown
    _.interactions.sampleChooser
    ( clamp01 >>> nFromZeroOne >>> inj
        ( Proxy
            :: Proxy
                 "sampleChooser"
        )
    )
    sliderAction
  SampleDelayGainCarousel sliderAction -> handleSlider
    localEvent.push
    _ { interactions { sampleDelayGainCarousel = _, sampleDelayGainCarouselDown = _ } }
    _.interactions.sampleDelayGainCarouselDown
    _.interactions.sampleDelayGainCarousel
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "sampleDelayGainCarousel"
        )
    )
    sliderAction
  LoopingBufferStartEndConstriction sliderAction -> handleSlider
    localEvent.push
    _ { interactions { loopingBufferStartEndConstriction = _, loopingBufferStartEndConstrictionDown = _ } }
    _.interactions.loopingBufferStartEndConstrictionDown
    _.interactions.loopingBufferStartEndConstriction
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "loopingBufferStartEndConstriction"
        )
    )
    sliderAction
  GreatAndMightyPan sliderAction -> handleSlider
    localEvent.push
    _ { interactions { greatAndMightyPan = _, greatAndMightyPanDown = _ } }
    _.interactions.greatAndMightyPanDown
    _.interactions.greatAndMightyPan
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "greatAndMightyPan"
        )
    )
    sliderAction
  DistantBellsFader sliderAction -> handleSlider
    localEvent.push
    _ { interactions { distantBellsFader = _, distantBellsFaderDown = _ } }
    _.interactions.distantBellsFaderDown
    _.interactions.distantBellsFader
    ( clamp01 >>> inj
        ( Proxy
            :: Proxy
                 "distantBellsFader"
        )
    )
    sliderAction
  StubDeleteMe -> mempty
  StartAudio -> do
    handleAction remoteEvent localEvent pubnub buffers StopAudio
    audioCtx <- H.liftEffect context
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    unsubscribe0 <-
      H.liftEffect
        $ subscribe
            ( runNoLoop
                ( Trigger <$>
                    ( ( inj
                          ( Proxy
                              :: Proxy
                                   "event"
                          ) <$> remoteEvent
                      )
                        <|>
                          ( inj
                              ( Proxy
                                  :: Proxy
                                       "event"
                              ) <$> localEvent.event
                          )
                        <|>
                          ( pure $ inj
                              ( Proxy
                                  :: Proxy
                                       "thunk"
                              )
                              unit
                          )
                    )
                )
                (pure { buffers })
                {}
                ffiAudio
                (piece initialAcc setup oracle)
            )
            (\({ res } :: TriggeredRun Res ()) -> Log.info $ show res)
    { emitter, listener } <- H.liftEffect HS.create
    unsubscribe1 <- H.liftEffect $
      subscribe remoteEvent
        ( unwrap >>> onMatch
            { gainLFO0Pad: uzto
                >>> SliderRemoteMove
                >>> GainLFO0Pad
                >>> HS.notify listener
            , gainLFO1Pad: uzto
                >>> SliderRemoteMove
                >>> GainLFO1Pad
                >>> HS.notify listener
            , waveshaperPad: uzto
                >>> SliderRemoteMove
                >>> WaveshaperPad
                >>> HS.notify listener
            , pitchLead: ezto
                >>> SliderRemoteMove
                >>> PitchLead
                >>> HS.notify listener
            , leadDelayGainCarousel: uzto
                >>> SliderRemoteMove
                >>> LeadDelayGainCarousel
                >>> HS.notify listener
            , nPitchesPlayedLead: ezto
                >>> SliderRemoteMove
                >>> NPitchesPlayedLead
                >>> HS.notify listener
            , droneLowpass0Q: uzto
                >>> SliderRemoteMove
                >>> DroneLowpass0Q
                >>> HS.notify listener
            , droneBandpass0Q: uzto
                >>> SliderRemoteMove
                >>> DroneBandpass0Q
                >>> HS.notify listener
            , droneBandpass0LFO: uzto
                >>> SliderRemoteMove
                >>> DroneBandpass0LFO
                >>> HS.notify listener
            , droneBandpass1Q: uzto
                >>> SliderRemoteMove
                >>> DroneBandpass1Q
                >>> HS.notify listener
            , droneBandpass1LFO: uzto
                >>> SliderRemoteMove
                >>> DroneBandpass1LFO
                >>> HS.notify listener
            , droneHighpass0Q: uzto
                >>> SliderRemoteMove
                >>> DroneHighpass0Q
                >>> HS.notify listener
            , droneHighpass0LFO: uzto
                >>> SliderRemoteMove
                >>> DroneHighpass0LFO
                >>> HS.notify listener
            , droneActivationEnergyThreshold: uzto
                >>> SliderRemoteMove
                >>> DroneActivationEnergyThreshold
                >>> HS.notify listener
            , droneDecay: uzto
                >>> SliderRemoteMove
                >>> DroneDecay
                >>> HS.notify listener
            , sampleChooser: ezto
                >>> SliderRemoteMove
                >>> SampleChooser
                >>> HS.notify listener
            , sampleDelayGainCarousel: uzto
                >>> SliderRemoteMove
                >>> SampleDelayGainCarousel
                >>> HS.notify listener
            , loopingBufferStartEndConstriction: uzto
                >>> SliderRemoteMove
                >>> LoopingBufferStartEndConstriction
                >>> HS.notify listener
            , greatAndMightyPan: uzto
                >>> SliderRemoteMove
                >>> GreatAndMightyPan
                >>> HS.notify listener
            , distantBellsFader: uzto
                >>> SliderRemoteMove
                >>> DistantBellsFader
                >>> HS.notify listener
            }
            (default (pure unit))
        )
    subscription <- H.subscribe emitter
    H.modify_ _
      { unsubscribe = unsubscribe0 *> unsubscribe1
      , unsubscribeHalogen = Just subscription
      , audioCtx = Just audioCtx
      }
  StopAudio -> do
    { unsubscribe, unsubscribeHalogen, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ unsubscribeHalogen H.unsubscribe
    H.liftEffect do
      for_ audioCtx close
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
