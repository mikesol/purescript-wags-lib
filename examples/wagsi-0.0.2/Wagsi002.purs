module WAGS.Lib.Example.Wagsi002 where

import Prelude
import WAGS.Create.Optionals
import Control.Alternative (guard)
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Parallel (parallel, sequential)
import Control.Promise (toAffE)
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..), isJust)
import Data.NonEmpty ((:|))
import Data.Semigroup.First (First(..))
import Data.Traversable (for_, sequence, traverse)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D3)
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
import WAGS.Run (RunAudio, RunEngine, SceneI(..), run)
import WAGS.Template (fromTemplate)

globalFF = 0.03 :: Number

type NBuf
  = D3 -- 3 buffers

type RBuf
  = Unit -- no extra info needed

--- room0
type Acc0 r
  = ( room0KickLatch :: ALatchAP (First (Maybe Boolean))
    , room0KickBuffers :: ABufferPool NBuf RBuf
    | r
    )

actualizer0 ::
  forall trigger world r.
  SceneI trigger world ->
  { | Acc0 r } ->
  { room0KickLatch :: CfLatchAP (MakeLatchAP (First (Maybe Boolean))) (LatchAP (First (Maybe Boolean)))
  , room0KickBuffers :: CfBufferPool (MakeBufferPoolWithRest RBuf) (BuffyVec NBuf RBuf)
  }
actualizer0 e@(SceneI e') a =
  { room0KickLatch
  , room0KickBuffers:
      actualize
        a.room0KickBuffers
        e
        $ UF.fromMaybe do
            AudioParameter { param, timeOffset } <- extract room0KickLatch
            (First param') <- param -- Maybe (First (Maybe Boolean)) -> First (Maybe Boolean)
            _ <- param'
            pure { offset: timeOffset, rest: unit }
  }
  where
  kicks =
    makeLoopingTerracedR
      $ ( (0.0 /\ First (Just true))
            :| (0.832 /\ First (Just false))
            : (2.0 /\ First (Just true))
            : Nil
        )

  fromPW = kicks { time: e'.time, headroom: e'.headroomInSeconds }

  room0KickLatch = actualize a.room0KickLatch e fromPW

graph0 :: forall trigger world r. SceneI trigger world -> { room0KickLatch :: (LatchAP (First (Maybe Boolean))), room0KickBuffers :: BuffyVec NBuf RBuf | r } -> _
graph0 (SceneI { time }) { room0KickBuffers } =
  { room0Kick:
      fromTemplate (Proxy :: _ "room0KickBuffs") room0KickBuffers \_ -> case _ of
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

--- room1
type Acc1 r
  = ( room1ClapLatch :: ALatchAP (First (Maybe Boolean))
    , room1ClapBuffers :: ABufferPool NBuf RBuf
    | r
    )

actualizer1 ::
  forall trigger world r.
  SceneI trigger world ->
  { | Acc1 r } ->
  { room1ClapLatch :: CfLatchAP (MakeLatchAP (First (Maybe Boolean))) (LatchAP (First (Maybe Boolean)))
  , room1ClapBuffers :: CfBufferPool (MakeBufferPoolWithRest RBuf) (BuffyVec NBuf RBuf)
  }
actualizer1 e@(SceneI e') a =
  { room1ClapLatch
  , room1ClapBuffers:
      actualize
        a.room1ClapBuffers
        e
        $ UF.fromMaybe do
            -- param (Maybe v)
            -- v = First (Maybe Boolean)
            -- param (Maybe (First (Maybe Boolean)))
            AudioParameter { param, timeOffset } <- extract room1ClapLatch
            (First param') <- param -- Maybe (First (Maybe Boolean)) -> First (Maybe Boolean)
            _ <- param'
            pure { offset: timeOffset, rest: unit }
  }
  where
  claps =
    makeLoopingTerracedR
      $ ( (0.0 /\ First Nothing)
            :| (1.0 /\ First (Just true))
            : (2.0 /\ First Nothing)
            : Nil
        )

  fromPW = claps { time: e'.time, headroom: e'.headroomInSeconds }

  room1ClapLatch = actualize a.room1ClapLatch e fromPW

graph1 :: forall trigger world r. SceneI trigger world -> { room1ClapLatch :: (LatchAP (First (Maybe Boolean))), room1ClapBuffers :: BuffyVec NBuf RBuf | r } -> _
graph1 (SceneI { time }) { room1ClapBuffers } =
  { room1Clap:
      fromTemplate (Proxy :: _ "room1ClapBuffs") room1ClapBuffers \_ -> case _ of
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
                , playbackRate: 1.2 + sin (pi * time * 0.7) * 0.5
                }
                "clap"
            )
        Nothing -> gain 0.0 (playBuf { onOff: Off } "clap")
  }

-- room2
type Acc2 r
  = ( room2HiHatLatch :: ALatchAP (First (Maybe Boolean))
    , room2HiHatBuffers :: ABufferPool NBuf RBuf
    | r
    )

actualizer2 ::
  forall trigger world r.
  SceneI trigger world ->
  { | Acc2 r } ->
  { room2HiHatLatch :: CfLatchAP (MakeLatchAP (First (Maybe Boolean))) (LatchAP (First (Maybe Boolean)))
  , room2HiHatBuffers :: CfBufferPool (MakeBufferPoolWithRest RBuf) (BuffyVec NBuf RBuf)
  }
actualizer2 e@(SceneI e') a =
  { room2HiHatLatch
  , room2HiHatBuffers:
      actualize
        a.room2HiHatBuffers
        e
        $ UF.fromMaybe do
            -- param (Maybe v)
            -- v = First (Maybe Boolean)
            -- param (Maybe (First (Maybe Boolean)))
            AudioParameter { param, timeOffset } <- extract room2HiHatLatch
            (First param') <- param -- Maybe (First (Maybe Boolean)) -> First (Maybe Boolean)
            _ <- param'
            pure { offset: timeOffset, rest: unit }
  }
  where
  hiHats =
    makeLoopingTerracedR
      $ ( (0.0 /\ First Nothing)
            :| (0.15 /\ First (Just true))
            : (0.2 /\ First Nothing)
            : (0.3 /\ First (Just false))
            : (0.4 /\ First Nothing)
            : Nil
        )

  fromPW = hiHats { time: e'.time, headroom: e'.headroomInSeconds }

  room2HiHatLatch = actualize a.room2HiHatLatch e fromPW

graph2 :: forall trigger world r. SceneI trigger world -> { room2HiHatLatch :: (LatchAP (First (Maybe Boolean))), room2HiHatBuffers :: BuffyVec NBuf RBuf | r } -> _
graph2 (SceneI { time }) { room2HiHatBuffers } =
  { room2HiHat:
      fromTemplate (Proxy :: _ "room2HiHatBuffs") room2HiHatBuffers \_ -> case _ of
        Just (Buffy { starting, startTime }) ->
          gain (sin (time * pi * 0.5) * 0.2 + 0.2) -- (if time - startTime < 0.3 then 0.6 else 0.1)
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
                "openHH"
            )
        Nothing -> gain 0.0 (playBuf { onOff: Off } "openHH")
  }

-- room3
actualizer3 ::
  forall trigger world r.
  SceneI trigger world ->
  { | r } ->
  {}
actualizer3 e _ = {}

pbFotTpt t
  | t < 0.3 = 1.0
  | t < 0.6 = 1.5
  | t < 1.2 = 0.9
  | otherwise = 1.2

graph3 :: forall trigger world r. SceneI trigger world -> { | r } -> _
graph3 (SceneI { time }) _ =
  { trumpet: gain (sin (time * pi * 0.15) * 0.2 + 0.4) (loopBuf { playbackRate: (pbFotTpt (time % 2.0) / 2.0) } "trumpet")
  }

-- room4
actualizer4 ::
  forall trigger world r.
  SceneI trigger world ->
  { | r } ->
  {}
actualizer4 e _ = {}

pbFotPad t
  | t < 0.3 = 0.5
  | t < 0.6 = 0.0
  | t < 1.2 = 0.3
  | otherwise = 0.0

graph4 :: forall trigger world r. SceneI trigger world -> { | r } -> _
graph4 (SceneI { time }) _ =
  { room4Pad: gain (pbFotPad (time % 4.0)) ((loopBuf { playbackRate: 1.0 } "pad"))
  }

---
-- change this to make sound
-- for example, you can try:
-- a /@\ speaker { unit0: gain (cos (pi * e.time) * -0.02 + 0.02) { osc0: sinOsc 440.0 } }
type Acc
  = (
    | Acc0 + Acc1 + Acc2 + ()
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
        ( Record.union (actualizer0 e a)
            >>> Record.union (actualizer1 e a)
            >>> Record.union (actualizer2 e a)
            >>> Record.union (actualizer3 e a)
            >>> Record.union (actualizer4 e a)
        )
        actualizer

    headz = heads actualized

    scene =
      speaker
        { masterGain:
            gain 0.5
              ( Record.build
                  ( Record.union (graph0 e headz)
                      >>> Record.union (graph1 e headz)
                      >>> Record.union (graph2 e headz)
                      >>> Record.union (graph3 e headz)
                      >>> Record.union (graph4 e headz)
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
