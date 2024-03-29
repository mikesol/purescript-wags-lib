module Grains.App where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Data.Foldable (for_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Typelevel.Num (D20)
import Data.Vec (fill)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import FRP.Behavior (Behavior, behavior)
import FRP.Event (makeEvent, subscribe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Math (pow)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Graph (iloop, startUsingWithHint)
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create.Optionals (gain, pan, playBuf, speaker, subgraph)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, makeFFIAudioSnapshot)
import WAGS.Lib.BufferPool (AHotBufferPool, Buffy, BuffyVec, bOnOff, makeHotBufferPool)
import WAGS.Lib.Cofree (tails)
import WAGS.Run (RunAudio, RunEngine, BehavingScene(..), BehavingRun, run)
import WAGS.Subgraph (SubSceneSig)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

ntropi :: Behavior Number
ntropi =
  behavior \e ->
    makeEvent \f ->
      e `subscribe` \a2b -> (a2b <$> random) >>= f

type Acc = { hbp :: AHotBufferPool D20 }

acc :: Acc
acc = { hbp: makeHotBufferPool { startsAt: 0.0 } }

type World = { entropy :: Number, buffers :: { bells :: BrowserAudioBuffer } }

piece :: BrowserAudioBuffer -> Scene (BehavingScene Unit World ()) RunAudio RunEngine Frame0 Unit
piece bellz =
  startUsingWithHint
    scene
    { microphone: Nothing
    , mediaElement: Nothing
    , subgraphs:
        { mainBus: fst $ subgraph (fill \i -> { i, time: 0.0, bells: bellz, entropy: 0.5, gor: Nothing }) (const internal0) unit
        }
    , tumults: {}
    }
    (const acc)
    ( iloop \(BehavingScene { time, headroomInSeconds, world: { entropy, buffers: { bells } } }) (a :: Acc) ->
        let
          actualized = { hbp: a.hbp { time, headroomInSeconds, freq: 12.0 * (entropy `pow` 6.0) } }
        in
          ichange (scene time bells entropy (extract actualized.hbp)) $> tails actualized
    )
  where
  internal0
    :: SubSceneSig "singleton" ()
         { gor :: Maybe (Buffy Unit)
         , bells :: BrowserAudioBuffer
         , time :: Number
         , entropy :: Number
         , i :: Int
         }
  internal0 = (const unit) # SG.loopUsingScene \{ i, time, bells, entropy, gor } _ ->
    { control: unit
    , scene:
        { singleton: gain 0.5
            { myPlayer:
                pan (entropy * 2.0 - 1.0)
                  { panny:
                      playBuf
                        { onOff: bOnOff time gor
                        , playbackRate: (1.0 + (toNumber (i + 1) * 0.1 + entropy))
                        }
                        bells
                  }
            }
        }
    }
  scene (time' :: Number) (bells :: BrowserAudioBuffer) (entropy :: Number) (v :: BuffyVec D20) =
    speaker { mainBus: subgraph (mapWithIndex ({ i: _, time: time', bells, entropy, gor: _ }) v) (const internal0) {} }

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

type State =
  { unsubscribe :: Effect Unit
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
render _ = do
  HH.div_
    [ HH.h1_
        [ HH.text "Grains" ]
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
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    bells <-
      H.liftAff $ decodeAudioDataFromUri
        audioCtx
        "https://freesound.org/data/previews/339/339822_5121236-lq.mp3"
    unsubscribe <-
      H.liftEffect
        $ subscribe
            (run (pure unit) ({ entropy: _, buffers: { bells } } <$> ntropi) { easingAlgorithm } (ffiAudio) (piece bells))
            (\(_ :: BehavingRun Unit ()) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
