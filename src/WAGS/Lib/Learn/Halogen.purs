module WAGS.Lib.Learn.Halogen where

-- | Learn

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (Event, subscribe)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import WAGS.Interpret (close, context)
import WAGS.Run (Run)
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

component :: forall res analyserCallbacks query input output m. MonadEffect m => MonadAff m => Event (Run res analyserCallbacks) -> H.Component query input output m
component run =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction run }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render _ = do
  HH.div_
    [ HH.h1_
        [ HH.text "Hello world" ]
    , HH.button
        [ HE.onClick \_ -> StartAudio ]
        [ HH.text "Start audio" ]
    , HH.button
        [ HE.onClick \_ -> StopAudio ]
        [ HH.text "Stop audio" ]
    ]

handleAction
  :: forall res analyserCallbacks output m
   . MonadEffect m
  => MonadAff m
  => Event (Run res analyserCallbacks)
  -> Action
  -> H.HalogenM State Action () output m Unit
handleAction run = case _ of
  StartAudio -> do
    audioCtx <- H.liftEffect context
    unsubscribe <-
      H.liftEffect
        $ subscribe run (\(_ :: Run res analyserCallbacks) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }

withSimpleControlsRun
  :: forall res analyserCallbacks
   . Event (Run res analyserCallbacks)
  -> Effect Unit
withSimpleControlsRun run =
  runHalogenAff do
    body <- awaitBody
    runUI (component run) unit body
