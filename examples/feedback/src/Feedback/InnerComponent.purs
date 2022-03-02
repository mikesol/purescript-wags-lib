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
import Feedback.Control (c2s, elts)
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

type State
  =
  { unsubscribe :: Effect Unit
  , audioCtx :: Maybe AudioContext
  }

data Action
  = StartAudio
  | StopAudio

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

handleAction :: forall output m. MonadEffect m => MonadAff m => Event IncomingEvent -> PubNub -> Buffers -> Action -> H.HalogenM State Action () output m Unit
handleAction event pubnub buffers = case _ of
  StartAudio -> do
    handleAction event pubnub buffers StopAudio
    audioCtx <- H.liftEffect context
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( runNoLoop
                ( Trigger <$>
                    ( (inj (Proxy :: Proxy
 "event") <$> event)
                        <|> (pure $ inj (Proxy :: Proxy
 "thunk") unit)
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
