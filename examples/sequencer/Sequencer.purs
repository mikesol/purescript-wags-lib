module WAGS.Lib.Example.Sequencer where

import Prelude
import WAGS.Create.Optionals
import Control.Alternative (guard)
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Parallel (parallel, sequential)
import Control.Promise (toAffE)
import Data.Array.NonEmpty as NEA
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (floor, toNumber)
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.NonEmpty ((:|))
import Data.Semigroup.First (First(..))
import Data.Traversable (for_, sequence, traverse)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2, D3, D7, toInt')
import Data.Unfoldable as UF
import Data.Vec as V
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Mouse (position)
import FRP.Event (makeEvent, subscribe)
import FRP.Event.Mouse (getMouse)
import Foreign.Object (fromHomogeneous)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Math (cos, pi, pow, sin, (%))
import Math as Math
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
import WAGS.Lib.Rate (ARate, CfRate, MakeRate, Rate, timeIs)
import WAGS.Lib.SimpleBuffer (SimpleBuffer, SimpleBufferCf, SimpleBufferHead, actualizeSimpleBuffer)
import WAGS.Math (calcSlope)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), run)
import WAGS.Template (fromTemplate)

ntropi :: Behavior Number
ntropi =
  behavior \e ->
    makeEvent \f ->
      e `subscribe` \a2b -> (a2b <$> random) >>= f

type World
  = { ntropi :: Number
    , mickey :: Maybe { x :: Int, y :: Int }
    , change :: Number
    }

globalFF = 0.03 :: Number

type NKeys
  = D7 -- 7 keys

type NBuf
  = D2 -- 3 buffers

type RBuf
  = Unit -- no extra info needed

type KeyBufs r
  = ( keyBufs :: V.Vec NKeys (SimpleBuffer NBuf)
    , rate :: ARate
    | r
    )

nps = 3.0 :: Number

keyBufsActualize ::
  forall trigger r.
  SceneI trigger World ->
  { | KeyBufs r } ->
  { keyBufs :: V.Vec NKeys (SimpleBufferCf NBuf)
  , rate :: CfRate MakeRate Rate
  }
keyBufsActualize e@(SceneI e') { keyBufs, rate } =
  { keyBufs:
      mapWithIndex
        ( \i' ->
            let
              i = toNumber i'
            in
              actualizeSimpleBuffer
                (NEA.singleton (i / nps))
                ((toNumber $ toInt' (Proxy :: _ NKeys)) / nps)
                (timeIs (extract rate') e)
        )
        keyBufs
  , rate: rate'
  }
  where
  freq = 1.0 + maybe 0.0 (\{ y } -> toNumber y / 1000.0) e'.world.mickey

  rate' = actualize rate e freq

keyBufGraph ::
  forall trigger r.
  SceneI trigger World ->
  { keyBufs :: V.Vec NKeys (SimpleBufferHead NBuf)
  , rate :: Rate
  | r
  } ->
  _
keyBufGraph (SceneI { world }) { keyBufs, rate } =
  { keyBufs:
      fromTemplate (Proxy :: _ "instruments") keyBufs \i { buffers } ->
        fromTemplate (Proxy :: _ "buffers") buffers \_ -> case _ of
          Just (Buffy { starting, startTime }) ->
            let
              pos = max 0.0 (startTime - time)

              nk = toNumber $ toInt' (Proxy :: _ NKeys)

              endT = startTime + (nk / nps)
            in
              gain (ff (globalFF + pos) (pure (max 0.0 $ calcSlope startTime 1.0 endT 0.0 time)))
                ( playBuf
                    { onOff:
                        ff globalFF
                          $ if starting then
                              ff pos (pure OffOn)
                            else
                              pure On
                    , playbackRate: 1.0
                    }
                    if xpos < world.ntropi then
                      ("note" <> show i)
                    else
                      ("note" <> show (floor $ world.change * nk))
                )
          Nothing -> gain 0.0 (playBuf { onOff: Off } ("note" <> show i))
  }
  where
  xpos = maybe 0.0 (\{ x } -> toNumber x / 1000.0) world.mickey

  time = rate

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

scene :: forall trigger. SceneI trigger World -> { | Acc } -> _
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
            gain 1.0
              ( Record.build
                  ( Record.union (keyBufGraph e headz)
                  )
                  {}
              )
        }
  in
    tails actualized /\ scene

piece :: forall env. Scene (SceneI env World) RunAudio RunEngine Frame0 Unit
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
        [ HH.text "ꦒꦩꦼꦭꦤ꧀" ]
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
        { note0: "https://freesound.org/data/previews/24/24620_130612-hq.mp3"
        , note1: "https://freesound.org/data/previews/24/24622_130612-hq.mp3"
        , note2: "https://freesound.org/data/previews/24/24623_130612-hq.mp3"
        , note3: "https://freesound.org/data/previews/24/24624_130612-hq.mp3"
        , note4: "https://freesound.org/data/previews/24/24625_130612-hq.mp3"
        , note5: "https://freesound.org/data/previews/24/24626_130612-hq.mp3"
        , note6: "https://freesound.org/data/previews/24/24627_130612-hq.mp3"
        }
    sounds <- H.liftAff $ sequential $ traverse (parallel <<< toAffE <<< decodeAudioDataFromUri audioCtx) $ fromHomogeneous sounds'
    let
      ffiAudio = (defaultFFIAudio audioCtx unitCache) { buffers = pure sounds }
    mouse' <- H.liftEffect getMouse
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( run (pure unit)
                ( { ntropi: _, mickey: _, change: _ }
                    <$> ntropi
                    <*> position mouse'
                    <*> ntropi
                )
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
