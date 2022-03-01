module Feedback.App where

import Prelude

import Control.Alt ((<|>))
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
import Feedback.Oracle (oracle)
import Feedback.PubNub (PubNub, pubnubEvent)
import Feedback.Setup (setup)
import Feedback.Types (Acc, IncomingEvent(..), Res, Trigger(..), World)
import Foreign.Object (fromHomogeneous, values)
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
import WAGS.Interpret (close, context, makeFFIAudioSnapshot)
import WAGS.Patch (ipatch)
import WAGS.Run (BehavingRun, BehavingScene(..), RunAudio, RunEngine, TriggeredRun, run, runNoLoop)
import WAGS.WebAPI (AudioContext)

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

type State
  =
  { unsubscribe :: Effect Unit
  , audioCtx :: Maybe AudioContext
  }

data Action
  = StartAudio
  | StopAudio

component :: forall query input output m. MonadEffect m => MonadAff m => Event IncomingEvent -> PubNub -> H.Component query input output m
component event pubnub =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction event pubnub }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  }

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ classes [ "w-screen", "h-screen" ] ]
    [ SE.svg
        [ SA.classes $ map ClassName [ "w-full", "h-full" ]
        , SA.viewBox 0.0 0.0 1000.0 1000.0
        , SA.preserveAspectRatio Nothing SA.Slice
        ]
        (join $ map c2s $ values $ fromHomogeneous elts)
    ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Event IncomingEvent -> PubNub -> Action -> H.HalogenM State Action () output m Unit
handleAction event pubnub = case _ of
  StartAudio -> do
    handleAction event pubnub StopAudio
    audioCtx <- H.liftEffect context
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( runNoLoop
                ( Trigger <$>
                    ( (inj (Proxy :: _ "event") <$> event)
                        <|> (pure $ inj (Proxy :: _ "thunk") unit)
                    )
                )
                (pure mempty)
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
