module WAGS.Example.HelloWorld where

import Prelude
import Control.Applicative.Indexed ((:*>))
import Control.Comonad.Cofree (Cofree, head, mkCofree, tail)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Math (pi, sin)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Indexed (IxFrame)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (CGain, CSpeaker, CSinOsc, gain, sinOsc, speaker)
import WAGS.Graph.AudioUnit (TGain, TSinOsc, TSpeaker)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.Rate (Rate, makeRate)
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, SceneI, RunEngine, run)

type SceneType
  = { speaker :: TSpeaker /\ { oscGain :: Unit }
    , oscGain :: TGain /\ { osc :: Unit }
    , osc :: TSinOsc /\ {}
    }

type Acc
  = { myRate :: Rate
    }

createFrame :: IxFrame (SceneI Unit Unit) RunAudio RunEngine Frame0 Unit {} SceneType Acc
createFrame = \{ time } -> (ipatch :*> (ichange {} $> { myRate: makeRate { prevTime: 0.0, startsAt: time } }))

piece :: Scene (SceneI Unit Unit) RunAudio RunEngine Frame0 Unit
piece =
  createFrame
    @!> iloop \{ time } a ->
        let
          rate = a.myRate { time, rate: 4.0 + sin (time * pi * 0.25) * 3.5 }
        in
          ichange
            { oscGain: 0.1 + 0.09 * sin (pi * (head rate))
            }
            $> { myRate: tail rate }

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    runUI component unit body

type State
  = { unsubscribe :: Effect Unit
    , audioCtx :: Maybe AudioContext
    }

data Action
  = StartAudio
  | StopAudio

component :: forall query input output m. MonadEffect m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
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

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    let
      ffiAudio = defaultFFIAudio audioCtx unitCache
    unsubscribe <-
      H.liftEffect
        $ subscribe
            (run (pure unit) (pure unit) { easingAlgorithm } (FFIAudio ffiAudio) piece)
            (const $ pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
