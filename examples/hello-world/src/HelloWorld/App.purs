module HelloWorld.App where

import Prelude

import Control.Applicative.Indexed ((:*>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Plus (empty)
import Control.Promise (toAffE)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (D4, d0, d1, d2, d3)
import Data.Vec as V
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Math (cos, pi, sin)
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, (@!>))
import WAGS.Control.Indexed (IxFrame)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Graph.AudioUnit (TGain, TPlayBuf, TSinOsc, TSpeaker)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.BufferPool (ABufferPool, bOnOff, makeBufferPool)
import WAGS.Lib.Emitter (AnEmitter, makeEmitter)
import WAGS.Lib.Rate (ARate, makeRate)
import WAGS.Patch (ipatch)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), Run, run)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type SceneType
  =
  { speaker ::
      TSpeaker
        /\
          { oscGain :: Unit
          , b0Gain :: Unit
          , b1Gain :: Unit
          , b2Gain :: Unit
          , b3Gain :: Unit
          }
  , oscGain :: TGain /\ { osc :: Unit }
  , osc :: TSinOsc /\ {}
  , b0Gain :: TGain /\ { b0 :: Unit }
  , b0 :: TPlayBuf /\ {}
  , b1Gain :: TGain /\ { b1 :: Unit }
  , b1 :: TPlayBuf /\ {}
  , b2Gain :: TGain /\ { b2 :: Unit }
  , b2 :: TPlayBuf /\ {}
  , b3Gain :: TGain /\ { b3 :: Unit }
  , b3 :: TPlayBuf /\ {}
  }

type Acc
  =
  { myRate :: ARate
  , myEmitter :: AnEmitter
  , buffy :: ABufferPool D4 Unit
  }

type World = { bell :: BrowserAudioBuffer }

createFrame :: IxFrame (SceneI Unit World ()) RunAudio RunEngine Frame0 Unit {} SceneType Acc
createFrame = \(SceneI { time, world: { bell } }) ->
  ( ipatch { microphone: empty }
      :*>
        ( ichange
            { b0: bell
            , b1: bell
            , b2: bell
            , b3: bell
            }
            $>
              { myRate: makeRate { prevTime: 0.0, startsAt: time }
              , myEmitter: makeEmitter { prevTime: 0.0, startsAt: time }
              , buffy: makeBufferPool (pure 6.0) empty
              }
        )
  )

piece :: Scene (SceneI Unit World ()) RunAudio RunEngine Frame0 Unit
piece =
  createFrame
    @!> iloop \e@(SceneI { time, headroomInSeconds: headroom }) a ->
      let
        rate = a.myRate { time, rate: 4.0 + sin (time * pi * 0.25) * 3.5 }

        emitter = a.myEmitter { time, headroom, freq: 4.0 + cos (time * pi * 0.25) * 3.5 }

        bufz = a.buffy { time, headroom, offsets: map { rest: unit, offset: _ } (extract emitter) }

        hbufz = extract bufz
      in
        ichange
          { oscGain: 0.1 + 0.09 * sin (pi * (unwrap $ extract rate))
          , b0Gain: 1.0
          , b1Gain: 1.0
          , b2Gain: 1.0
          , b3Gain: 1.0
          , b0: bOnOff time (V.index hbufz d0)
          , b1: bOnOff time (V.index hbufz d1)
          , b2: bOnOff time (V.index hbufz d2)
          , b3: bOnOff time (V.index hbufz d3)
          }
          $> { myRate: unwrapCofree rate, myEmitter: unwrapCofree emitter, buffy: unwrapCofree bufz }

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
    bell <-
      H.liftAff $ toAffE
        $ decodeAudioDataFromUri
            audioCtx
            "https://freesound.org/data/previews/339/339809_5121236-hq.mp3"
    let
      ffiAudio = defaultFFIAudio audioCtx unitCache
    unsubscribe <-
      H.liftEffect
        $ subscribe
            (run (pure unit) (pure { bell }) { easingAlgorithm } ffiAudio piece)
            (\(_ :: Run Unit ()) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
