module WAGS.Lib.Example.FromTemplate where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Promise (toAffE)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (D20)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import FRP.Behavior (Behavior, behavior)
import FRP.Event (makeEvent, subscribe)
import Foreign.Object as O
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Math (pow)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Control.Functions (iloop, startUsingWithHint)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create.Optionals (gain, playBuf, speaker, pan)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.BufferPool (AHotBufferPool, BuffyVec, bGain, bOnOff)
import WAGS.Lib.Cofree (actualizes, tails)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), run)
import WAGS.Template (fromTemplate)

ntropi :: Behavior Number
ntropi =
  behavior \e ->
    makeEvent \f ->
      e `subscribe` \a2b -> (a2b <$> random) >>= f

type Acc
  = { hbp :: AHotBufferPool D20 }

acc :: Acc
acc = { hbp: mempty }

type World
  = { entropy :: Number }

piece :: Scene (SceneI Unit World) RunAudio RunEngine Frame0 Unit
piece =
  startUsingWithHint
    scene
    acc
    ( iloop \e@(SceneI { time, world: { entropy } }) (a :: Acc) ->
        let
          actualized = actualizes a e { hbp: 12.0 * (entropy `pow` 6.0) }
        in
          ichange (scene entropy (extract actualized.hbp)) $> tails actualized
    )
  where
  scene (entropy :: Number) (v :: BuffyVec D20 Unit) =
    speaker
      { mainBus:
          fromTemplate (Proxy :: _ "myPool") v \i gor ->
            gain (bGain gor * pure 0.5)
              { myPlayer:
                  pan (entropy * 2.0 - 1.0)
                    { panny:
                        playBuf
                          { onOff: bOnOff gor
                          , playbackRate: (1.0 + (toNumber (i + 1) * 0.1 + entropy))
                          }
                          "bells"
                    }
              }
      }

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
    unitCache <- H.liftEffect makeUnitCache
    bells <-
      H.liftAff $ toAffE
        $ decodeAudioDataFromUri
            audioCtx
            "https://freesound.org/data/previews/339/339822_5121236-lq.mp3"
    let
      ffiAudio = (defaultFFIAudio audioCtx unitCache) { buffers = pure $ O.singleton "bells" bells }
    unsubscribe <-
      H.liftEffect
        $ subscribe
            (run (pure unit) ({ entropy: _ } <$> ntropi) { easingAlgorithm } (FFIAudio ffiAudio) piece)
            (const $ pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
