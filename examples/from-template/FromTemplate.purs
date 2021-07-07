module WAGS.Lib.Example.FromTemplate where

import Prelude

import Control.Comonad.Cofree (Cofree, head, mkCofree)
import Control.Promise (toAffE)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Typelevel.Num (D5)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Foreign.Object as O
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Math (sin, pi)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Control.Functions (iloop, (@!>))
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (gain, playBuf, speaker)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.BufferPool (AHotBufferPool, BuffyVec, bGain, bOnOff)
import WAGS.Lib.Cofree (actualizes, tails)
import WAGS.Lib.Emitter (makeEmitter, Emitter)
import WAGS.Lib.Template (fromTemplate)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), run)

vol = 1.4 :: Number

emit :: Number -> Emitter
emit startsAt = unwrap $ makeEmitter { prevTime: 0.0, startsAt }

type Acc
  = { hbp :: AHotBufferPool D5 }

acc :: Acc
acc = { hbp: mempty }

piece :: Scene (SceneI Unit Unit) RunAudio RunEngine Frame0 Unit
piece =
  ( \e@(SceneI { time }) ->
      let
        actualized = actualizes acc e { hbp: 1.0 + sin (time * pi * 0.3) * 0.8 }
      in
        icreate (speaker { mainBus: scene (head actualized.hbp) }) $> tails actualized
  )
    @!> iloop \e@(SceneI { time }) (a :: Acc) ->
        let
          actualized = actualizes a e { hbp: 1.0 + sin (time * pi * 0.3) * 0.8 }
        in
          ichange (speaker { mainBus: scene (head actualized.hbp) }) $> tails actualized
  where
  scene (v :: BuffyVec D5 Unit) =
    fromTemplate (Proxy :: _ "myPool") v \i gor ->
      gain (bGain gor)
        { myPlayer:
            playBuf
              { onOff: bOnOff gor
              , playbackRate: (toNumber (i + 1) * 0.5)
              }
              "atar"
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
        [ HH.text "Atari speaks 2.0" ]
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
    atar <-
      H.liftAff $ toAffE
        $ decodeAudioDataFromUri
            audioCtx
            "https://freesound.org/data/previews/100/100981_1234256-lq.mp3"
    let
      ffiAudio = (defaultFFIAudio audioCtx unitCache) { buffers = pure $ O.singleton "atar" atar }
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
