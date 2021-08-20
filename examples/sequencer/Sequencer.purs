module WAGS.Lib.Example.Sequencer where

import Prelude
import WAGS.Create.Optionals
import Control.Alternative (guard)
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Parallel (parallel, sequential)
import Control.Promise (toAffE)
import Data.Array.NonEmpty as NEA
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..), isJust)
import Data.NonEmpty ((:|))
import Data.Semigroup.First (First(..))
import Data.Traversable (for_, sequence, traverse)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2, D3, D7)
import Data.Unfoldable as UF
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Foreign.Object (fromHomogeneous)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Math (pi, sin, cos, (%))
import Record.Builder as Record
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, startUsingWithHint)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.Parameter (AudioParameter_(..))
import WAGS.Graph.Parameter (ff)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.BufferPool (ABufferPool, Buffy(..), BuffyVec, CfBufferPool, MakeBufferPoolWithRest)
import WAGS.Lib.Cofree (actualize, heads, tails)
import WAGS.Lib.Emitter (fEmitter, fEmitter')
import WAGS.Lib.Latch (ALatchAP, CfLatchAP(..), MakeLatchAP, LatchAP)
import WAGS.Lib.Piecewise (makeLoopingTerracedR)
import WAGS.Lib.SimpleBuffer (SimpleBuffer, SimpleBufferCf, SimpleBufferHead, actualizeSimpleBuffer)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), run)
import WAGS.Template (fromTemplate)

globalFF = 0.03 :: Number

type NKeys
  = D7 -- 3 buffers

type NBuf
  = D2 -- 3 buffers

type RBuf
  = Unit -- no extra info needed

--- room0
type KeyBufs r
  = ( keyBufs :: SimpleBuffer NBuf
    | r
    )

keyBufsActualize ::
  forall trigger world r.
  SceneI trigger world ->
  { | KeyBufs r } ->
  { keyBufs :: SimpleBufferCf NBuf
  }
keyBufsActualize e { keyBufs } =
  { keyBufs:
      actualizeSimpleBuffer
        (NEA.fromNonEmpty (0.0 :| [ 0.832 ]))
        2.0
        e
        keyBufs
  }

keyBufGraph ::
  forall trigger world r.
  SceneI trigger world ->
  { keyBufs :: SimpleBufferHead NBuf
  | r
  } ->
  _
keyBufGraph (SceneI { time }) { keyBufs: { buffers } } =
  { room0Kick:
      fromTemplate (Proxy :: _ "keyBufs") buffers \_ -> case _ of
        Just (Buffy { starting, startTime }) ->
          gain 1.0
            ( playBuf
                { onOff:
                    ff globalFF
                      $ if starting then
                          ff (max 0.0 (startTime - time)) (pure OffOn)
                        else if time - startTime > 1.0 then
                          pure Off
                        else
                          pure On
                , playbackRate: 1.0
                }
                "kick1"
            )
        Nothing -> gain 0.0 (playBuf { onOff: Off } "kick1")
  }

---
-- change this to make sound
-- for example, you can try:
-- a /@\ speaker { unit0: gain (cos (pi * e.time) * -0.02 + 0.02) { osc0: sinOsc 440.0 } }
type Acc
  = (
    | KeyBufs + ()
    )

acc :: { | Acc }
acc = mempty

scene :: forall trigger world. SceneI trigger world -> { | Acc } -> _
scene e a =
  let
    actualizer = {}

    --------------------------------------------
    actualized =
      Record.build
        ( Record.union (keyBufsActualize e a)
        )
        actualizer

    headz = heads actualized

    scene =
      speaker
        { masterGain:
            gain 0.5
              ( Record.build
                  ( Record.union (keyBufGraph e headz)
                  )
                  {}
              )
        }
  in
    tails actualized /\ scene

piece :: forall env world. Scene (SceneI env world) RunAudio RunEngine Frame0 Unit
piece =
  startUsingWithHint
    scene
    acc
    ( iloop
        ( \e a ->
            let
              acc /\ graph = scene e a
            in
              ichange graph $> acc
        )
    )

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
render _ = do
  HH.div_
    [ HH.h1_
        [ HH.text "Loop" ]
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
    handleAction StopAudio
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    let
      sounds' =
        { kick1: "https://freesound.org/data/previews/171/171104_2394245-hq.mp3"
        , sideStick: "https://freesound.org/data/previews/209/209890_3797507-hq.mp3"
        , snare: "https://freesound.org/data/previews/495/495777_10741529-hq.mp3"
        , clap: "https://freesound.org/data/previews/183/183102_2394245-hq.mp3"
        , snareRoll: "https://freesound.org/data/previews/50/50710_179538-hq.mp3"
        , kick2: "https://freesound.org/data/previews/148/148634_2614600-hq.mp3"
        , closedHH: "https://freesound.org/data/previews/269/269720_4965320-hq.mp3"
        , shaker: "https://freesound.org/data/previews/432/432205_8738244-hq.mp3"
        , openHH: "https://freesound.org/data/previews/416/416249_8218607-hq.mp3"
        , tamb: "https://freesound.org/data/previews/207/207925_19852-hq.mp3"
        , crash: "https://freesound.org/data/previews/528/528490_3797507-hq.mp3"
        , ride: "https://freesound.org/data/previews/270/270138_1125482-hq.mp3"
        , trumpet: "https://freesound.org/data/previews/331/331146_3931578-hq.mp3"
        , pad: "https://freesound.org/data/previews/110/110212_1751865-hq.mp3"
        , impulse0: "https://freesound.org/data/previews/382/382907_2812020-hq.mp3"
        }
    sounds <- H.liftAff $ sequential $ traverse (parallel <<< toAffE <<< decodeAudioDataFromUri audioCtx) $ fromHomogeneous sounds'
    let
      ffiAudio = (defaultFFIAudio audioCtx unitCache) { buffers = pure sounds }
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( run (pure unit) (pure unit)
                { easingAlgorithm }
                (FFIAudio ffiAudio)
                piece
            )
            (const $ pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
