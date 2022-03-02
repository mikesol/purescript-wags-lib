module Feedback.InnerComponent where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Variant (inj)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Log
import FRP.Event (Event, subscribe)
import Feedback.Acc (initialAcc)
import Feedback.Control (Action(..), SliderAction(..), c2s, elts)
import Feedback.Engine (piece)
import Feedback.Oracle (oracle)
import Feedback.PubNub (PubNub)
import Feedback.Setup (setup)
import Feedback.Types (Buffers, IncomingEvent, Res, Trigger(..))
import Foreign.Object (fromHomogeneous, values)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Type.Proxy (Proxy(..))
import WAGS.Interpret (close, context, makeFFIAudioSnapshot)
import WAGS.Run (TriggeredRun, runNoLoop)
import WAGS.WebAPI (AudioContext)

type State =
  { unsubscribe :: Effect Unit
  , audioCtx :: Maybe AudioContext
  , interactions ::
      { gainLFO0Pad :: Number
      , gainLFO0PadDown :: Boolean
      , gainLFO1Pad :: Number
      , gainLFO1PadDown :: Boolean
      , waveshaperPad :: Number
      , waveshaperPadDown :: Boolean
      , pitchLead :: Number
      , pitchLeadDown :: Boolean
      , leadDelayGainCarousel :: Number
      , leadDelayGainCarouselDown :: Boolean
      , nPitchesPlayedLead :: Number
      , nPitchesPlayedLeadDown :: Boolean
      , droneLowpass0Q :: Number
      , droneLowpass0QDown :: Boolean
      , droneBandpass0Q :: Number
      , droneBandpass0QDown :: Boolean
      , droneBandpass0LFO :: Number
      , droneBandpass0LFODown :: Boolean
      , droneBandpass1Q :: Number
      , droneBandpass1QDown :: Boolean
      , droneBandpass1LFO :: Number
      , droneBandpass1LFODown :: Boolean
      , droneHighpass0Q :: Number
      , droneHighpass0QDown :: Boolean
      , droneHighpass0LFO :: Number
      , droneHighpass0LFODown :: Boolean
      , droneActivationEnergyThreshold :: Number
      , droneActivationEnergyThresholdDown :: Boolean
      , droneDecay :: Number
      , droneDecayDown :: Boolean
      , sampleChooser :: Number
      , sampleChooserDown :: Boolean
      , sampleDelayGainCarousel :: Number
      , sampleDelayGainCarouselDown :: Boolean
      , loopingBufferStartEndConstriction :: Number
      , loopingBufferStartEndConstrictionDown :: Boolean
      , greatAndMightyPan :: Number
      , greatAndMightyPanDown :: Boolean
      , distantBellsFader :: Number
      , distantBellsFaderDown :: Boolean
      }
  }

component :: forall query input output m. MonadEffect m => MonadAff m => Event IncomingEvent -> PubNub -> Buffers -> H.Component query input output m
component event pubnub buffers =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction event pubnub buffers
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
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
render _ = SE.svg
  [ SA.classes $ map ClassName [ "w-full", "h-full" ]
  , SA.viewBox 0.0 0.0 1000.0 1000.0
  , SA.preserveAspectRatio Nothing SA.Slice
  ]
  (join $ map c2s $ values $ fromHomogeneous elts)

handleSlider :: forall output m. MonadEffect m => MonadAff m => (State -> Number -> Boolean -> State) -> (State -> Boolean) -> (State -> Number) -> SliderAction -> H.HalogenM State Action () output m Unit
handleSlider f _ _ (SliderDown n) =
  H.modify_ (\s -> f s n true)
handleSlider f fb _ (SliderMove n) =
  H.modify_ (\s -> if fb s then f s n true else s)
handleSlider f _ fn SliderUp =
  H.modify_ (\s -> f s (fn s) false)
handleSlider f fb _ (SliderRemoteMove n) =
  H.modify_ (\s -> f s n (fb s))

handleAction :: forall output m. MonadEffect m => MonadAff m => Event IncomingEvent -> PubNub -> Buffers -> Action -> H.HalogenM State Action () output m Unit
handleAction event pubnub buffers = case _ of
  GainLFO0Pad sliderAction -> handleSlider
    _ { interactions { gainLFO0Pad = _, gainLFO0PadDown = _ } }
    _.interactions.gainLFO0PadDown
    _.interactions.gainLFO0Pad
    sliderAction
  GainLFO1Pad sliderAction -> handleSlider
    _ { interactions { gainLFO1Pad = _, gainLFO1PadDown = _ } }
    _.interactions.gainLFO1PadDown
    _.interactions.gainLFO1Pad
    sliderAction
  WaveshaperPad sliderAction -> handleSlider
    _ { interactions { waveshaperPad = _, waveshaperPadDown = _ } }
    _.interactions.waveshaperPadDown
    _.interactions.waveshaperPad
    sliderAction
  PitchLead sliderAction -> handleSlider
    _ { interactions { pitchLead = _, pitchLeadDown = _ } }
    _.interactions.pitchLeadDown
    _.interactions.pitchLead
    sliderAction
  LeadDelayGainCarousel sliderAction -> handleSlider
    _ { interactions { leadDelayGainCarousel = _, leadDelayGainCarouselDown = _ } }
    _.interactions.leadDelayGainCarouselDown
    _.interactions.leadDelayGainCarousel
    sliderAction
  NPitchesPlayedLead sliderAction -> handleSlider
    _ { interactions { nPitchesPlayedLead = _, nPitchesPlayedLeadDown = _ } }
    _.interactions.nPitchesPlayedLeadDown
    _.interactions.nPitchesPlayedLead
    sliderAction
  DroneLowpass0Q sliderAction -> handleSlider
    _ { interactions { droneLowpass0Q = _, droneLowpass0QDown = _ } }
    _.interactions.droneLowpass0QDown
    _.interactions.droneLowpass0Q
    sliderAction
  DroneBandpass0Q sliderAction -> handleSlider
    _ { interactions { droneBandpass0Q = _, droneBandpass0QDown = _ } }
    _.interactions.droneBandpass0QDown
    _.interactions.droneBandpass0Q
    sliderAction
  DroneBandpass0LFO sliderAction -> handleSlider
    _ { interactions { droneBandpass0LFO = _, droneBandpass0LFODown = _ } }
    _.interactions.droneBandpass0LFODown
    _.interactions.droneBandpass0LFO
    sliderAction
  DroneBandpass1Q sliderAction -> handleSlider
    _ { interactions { droneBandpass1Q = _, droneBandpass1QDown = _ } }
    _.interactions.droneBandpass1QDown
    _.interactions.droneBandpass1Q
    sliderAction
  DroneBandpass1LFO sliderAction -> handleSlider
    _ { interactions { droneBandpass1LFO = _, droneBandpass1LFODown = _ } }
    _.interactions.droneBandpass1LFODown
    _.interactions.droneBandpass1LFO
    sliderAction
  DroneHighpass0Q sliderAction -> handleSlider
    _ { interactions { droneHighpass0Q = _, droneHighpass0QDown = _ } }
    _.interactions.droneHighpass0QDown
    _.interactions.droneHighpass0Q
    sliderAction
  DroneHighpass0LFO sliderAction -> handleSlider
    _ { interactions { droneHighpass0LFO = _, droneHighpass0LFODown = _ } }
    _.interactions.droneHighpass0LFODown
    _.interactions.droneHighpass0LFO
    sliderAction
  DroneActivationEnergyThreshold sliderAction -> handleSlider
    _ { interactions { droneActivationEnergyThreshold = _, droneActivationEnergyThresholdDown = _ } }
    _.interactions.droneActivationEnergyThresholdDown
    _.interactions.droneActivationEnergyThreshold
    sliderAction
  DroneDecay sliderAction -> handleSlider
    _ { interactions { droneDecay = _, droneDecayDown = _ } }
    _.interactions.droneDecayDown
    _.interactions.droneDecay
    sliderAction
  SampleChooser sliderAction -> handleSlider
    _ { interactions { sampleChooser = _, sampleChooserDown = _ } }
    _.interactions.sampleChooserDown
    _.interactions.sampleChooser
    sliderAction
  SampleDelayGainCarousel sliderAction -> handleSlider
    _ { interactions { sampleDelayGainCarousel = _, sampleDelayGainCarouselDown = _ } }
    _.interactions.sampleDelayGainCarouselDown
    _.interactions.sampleDelayGainCarousel
    sliderAction
  LoopingBufferStartEndConstriction sliderAction -> handleSlider
    _ { interactions { loopingBufferStartEndConstriction = _, loopingBufferStartEndConstrictionDown = _ } }
    _.interactions.loopingBufferStartEndConstrictionDown
    _.interactions.loopingBufferStartEndConstriction
    sliderAction
  GreatAndMightyPan sliderAction -> handleSlider
    _ { interactions { greatAndMightyPan = _, greatAndMightyPanDown = _ } }
    _.interactions.greatAndMightyPanDown
    _.interactions.greatAndMightyPan
    sliderAction
  DistantBellsFader sliderAction -> handleSlider
    _ { interactions { distantBellsFader = _, distantBellsFaderDown = _ } }
    _.interactions.distantBellsFaderDown
    _.interactions.distantBellsFader
    sliderAction
  StubDeleteMe -> mempty
  StartAudio -> do
    handleAction event pubnub buffers StopAudio
    audioCtx <- H.liftEffect context
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( runNoLoop
                ( Trigger <$>
                    ( ( inj
                          ( Proxy
                              :: Proxy
                                   "event"
                          ) <$> event
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
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
