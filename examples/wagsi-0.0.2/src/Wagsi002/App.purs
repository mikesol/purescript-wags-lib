module Wagsi002.App where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Parallel (parallel, sequential)
import Control.Plus (empty)
import Control.Promise (toAffE)
import Data.Array.NonEmpty as NEA
import Data.List ((:), List(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Semigroup.First (First(..))
import Data.Symbol (class IsSymbol)
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D3)
import Data.Unfoldable as UF
import Effect (Effect)
import Effect.Aff (ParAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (subscribe)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Heterogeneous.Folding (class FoldingWithIndex, hfoldlWithIndex)
import Math (pi, sin, (%))
import Prim.Row (class Cons, class Lacks)
import Record as Rec
import Record.Builder as Record
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import WAGS.Change (ichange)
import WAGS.Control.Functions.Validated (iloop, startUsingWithHint)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create.Optionals (gain, loopBuf, playBuf, speaker)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.Parameter (AudioParameter_(..), ff)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.BufferPool (ABufferPool, Buffy(..), BuffyVec, CfBufferPool, MakeBufferPool)
import WAGS.Lib.Cofree (heads, tails)
import WAGS.Lib.Latch (ALatchAP, CfLatchAP, MakeLatchAP, LatchAP)
import WAGS.Lib.Piecewise (makeLoopingTerracedR)
import WAGS.Lib.SimpleBuffer (SimpleBuffer, SimpleBufferCf, SimpleBufferHead, actualizeSimpleBuffer)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), Run, run)
import WAGS.Template (fromTemplate)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

globalFF = 0.03 :: Number

type NBuf
  = D3 -- 3 buffers

type RBuf
  = Unit -- no extra info needed

type World =
  { kick1 :: BrowserAudioBuffer
  , sideStick :: BrowserAudioBuffer
  , snare :: BrowserAudioBuffer
  , clap :: BrowserAudioBuffer
  , snareRoll :: BrowserAudioBuffer
  , kick2 :: BrowserAudioBuffer
  , closedHH :: BrowserAudioBuffer
  , shaker :: BrowserAudioBuffer
  , openHH :: BrowserAudioBuffer
  , tamb :: BrowserAudioBuffer
  , crash :: BrowserAudioBuffer
  , ride :: BrowserAudioBuffer
  , trumpet :: BrowserAudioBuffer
  , pad :: BrowserAudioBuffer
  , impulse0 :: BrowserAudioBuffer
  }

--- room0
type Acc0 r
  =
  ( room0KickBuf :: SimpleBuffer NBuf
  | r
  )

actualizer0
  :: forall trigger world aCb r
   . SceneI trigger world aCb
  -> { | Acc0 r }
  -> { room0KickBuf :: SimpleBufferCf NBuf
     }
actualizer0 e { room0KickBuf } =
  { room0KickBuf:
      actualizeSimpleBuffer
        (NEA.fromNonEmpty (0.0 :| [ 0.832 ]))
        2.0
        e
        room0KickBuf
  }

graph0
  :: forall trigger aCb r
   . SceneI trigger World aCb
  -> { room0KickBuf :: SimpleBufferHead NBuf
     | r
     }
  -> _
graph0 (SceneI { time, world }) { room0KickBuf: { buffers } } =
  { room0Kick:
      fromTemplate (Proxy :: _ "room0KickBuffs") buffers \_ -> case _ of
        Just (Buffy { starting, startTime }) ->
          gain 1.0
            ( playBuf
                { onOff:
                    ff globalFF
                      $
                        if starting then
                          ff (max 0.0 (startTime - time)) (pure OffOn)
                        else if time - startTime > 1.0 then
                          pure Off
                        else
                          pure On
                , playbackRate: 1.0
                }
                world.kick1
            )
        Nothing -> gain 0.0 (playBuf { onOff: Off } world.kick1)
  }

--- room1
type Acc1 r
  =
  ( room1ClapLatch :: ALatchAP (First (Maybe Boolean))
  , room1ClapBuffers :: ABufferPool NBuf
  | r
  )

actualizer1
  :: forall trigger world aCb r
   . SceneI trigger world aCb
  -> { | Acc1 r }
  -> { room1ClapLatch :: CfLatchAP (First (Maybe Boolean))
     , room1ClapBuffers :: CfBufferPool NBuf
     }
actualizer1 e@(SceneI e'@{ time, headroomInSeconds: headroom }) a =
  { room1ClapLatch
  , room1ClapBuffers:
      a.room1ClapBuffers
        { time
        , offsets: UF.fromMaybe do
            AudioParameter { param, timeOffset } <- extract room1ClapLatch
            (First param') <- param -- Maybe (First (Maybe Boolean)) -> First (Maybe Boolean)
            _ <- param'
            pure { offset: timeOffset }
        }
  }
  where
  claps =
    makeLoopingTerracedR
      $
        ( (0.0 /\ First Nothing)
            :| (1.0 /\ First (Just true))
              : (2.0 /\ First Nothing)
              : Nil
        )

  fromPW = claps { time: e'.time, headroom: e'.headroomInSeconds }

  room1ClapLatch = a.room1ClapLatch fromPW

graph1 :: forall trigger aCb r. SceneI trigger World aCb -> { room1ClapLatch :: (LatchAP (First (Maybe Boolean))), room1ClapBuffers :: BuffyVec NBuf | r } -> _
graph1 (SceneI { time, world }) { room1ClapBuffers } =
  { room1Clap:
      fromTemplate (Proxy :: _ "room1ClapBuffs") room1ClapBuffers \_ -> case _ of
        Just (Buffy { starting, startTime }) ->
          gain 1.0
            ( playBuf
                { onOff:
                    ff globalFF
                      $
                        if starting then
                          ff (max 0.0 (startTime - time)) (pure OffOn)
                        else if time - startTime > 1.0 then
                          pure Off
                        else
                          pure On
                , playbackRate: 1.2 + sin (pi * time * 0.7) * 0.5
                }
                world.clap
            )
        Nothing -> gain 0.0 (playBuf { onOff: Off } world.clap)
  }

-- room2
type Acc2 r
  =
  ( room2HiHatLatch :: ALatchAP (First (Maybe Boolean))
  , room2HiHatBuffers :: ABufferPool NBuf
  | r
  )

actualizer2
  :: forall trigger world aCb r
   . SceneI trigger world aCb
  -> { | Acc2 r }
  -> { room2HiHatLatch :: CfLatchAP (First (Maybe Boolean))
     , room2HiHatBuffers :: CfBufferPool NBuf
     }
actualizer2 e@(SceneI e'@{ time, headroomInSeconds: headroom }) a =
  { room2HiHatLatch
  , room2HiHatBuffers:
      a.room2HiHatBuffers
        { time
        , offsets: UF.fromMaybe do
            AudioParameter { param, timeOffset } <- extract room2HiHatLatch
            (First param') <- param -- Maybe (First (Maybe Boolean)) -> First (Maybe Boolean)
            _ <- param'
            pure { offset: timeOffset }
        }

  }
  where
  hiHats =
    makeLoopingTerracedR
      $
        ( (0.0 /\ First Nothing)
            :| (0.15 /\ First (Just true))
              : (0.2 /\ First Nothing)
              : (0.3 /\ First (Just false))
              : (0.4 /\ First Nothing)
              : Nil
        )

  fromPW = hiHats { time: e'.time, headroom: e'.headroomInSeconds }

  room2HiHatLatch = a.room2HiHatLatch fromPW

graph2 :: forall trigger aCb r. SceneI trigger World aCb -> { room2HiHatLatch :: (LatchAP (First (Maybe Boolean))), room2HiHatBuffers :: BuffyVec NBuf | r } -> _
graph2 (SceneI { time, world }) { room2HiHatBuffers } =
  { room2HiHat:
      fromTemplate (Proxy :: _ "room2HiHatBuffs") room2HiHatBuffers \_ -> case _ of
        Just (Buffy { starting, startTime }) ->
          gain (sin (time * pi * 0.5) * 0.2 + 0.2) -- (if time - startTime < 0.3 then 0.6 else 0.1)
            ( playBuf
                { onOff:
                    ff globalFF
                      $
                        if starting then
                          ff (max 0.0 (startTime - time)) (pure OffOn)
                        else if time - startTime > 1.0 then
                          pure Off
                        else
                          pure On
                , playbackRate: 1.0
                }
                world.openHH
            )
        Nothing -> gain 0.0 (playBuf { onOff: Off } world.openHH)
  }

-- room3
actualizer3
  :: forall trigger world aCb r
   . SceneI trigger world aCb
  -> { | r }
  -> {}
actualizer3 e _ = {}

pbFotTpt t
  | t < 0.3 = 1.0
  | t < 0.6 = 1.5
  | t < 1.2 = 0.9
  | otherwise = 1.2

graph3 :: forall trigger aCb r. SceneI trigger World aCb -> { | r } -> _
graph3 (SceneI { time, world }) _ =
  { trumpet: gain (sin (time * pi * 0.15) * 0.2 + 0.4) (loopBuf { playbackRate: (pbFotTpt (time % 2.0) / 2.0) } world.trumpet)
  }

-- room4
actualizer4
  :: forall trigger world aCb r
   . SceneI trigger world aCb
  -> { | r }
  -> {}
actualizer4 e _ = {}

pbFotPad t
  | t < 0.3 = 0.5
  | t < 0.6 = 0.0
  | t < 1.2 = 0.3
  | otherwise = 0.0

graph4 :: forall trigger aCb r. SceneI trigger World aCb -> { | r } -> _
graph4 (SceneI { time, world }) _ =
  { room4Pad: gain (pbFotPad (time % 4.0)) ((loopBuf { playbackRate: 1.0 } world.pad))
  }

---
-- change this to make sound
-- for example, you can try:
-- a /@\ speaker { unit0: gain (cos (pi * e.time) * -0.02 + 0.02) { osc0: sinOsc 440.0 } }
type Acc
  =
  (
  | Acc0 + Acc1 + Acc2 + ()
  )

acc :: { | Acc }
acc = mempty

scene :: forall trigger aCb. SceneI trigger World aCb -> { | Acc } -> _
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

piece :: forall env aCb. Scene (SceneI env World aCb) RunAudio RunEngine Frame0 Unit
piece =
  startUsingWithHint
    scene
    { microphone: empty }
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

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

render :: forall m. State -> H.ComponentHTML Action () m
render _ = HH.div [ classes [ "w-screen", "h-screen" ] ]
  [ HH.div [ classes [ "flex", "flex-col", "w-full", "h-full" ] ]
      [ HH.div [ classes [ "flex-grow" ] ] []
      , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
          [ HH.div [ classes [ "flex-grow" ] ]
              []
          , HH.div [ classes [ "flex", "flex-col" ] ]
              [ HH.h1 [ classes [ "text-center", "text-3xl", "font-bold" ] ]
                  [ HH.text "wagsi-0.0.2" ]
              , HH.button
                  [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> StartAudio ]
                  [ HH.text "Start audio" ]
              , HH.button
                  [ classes [ "text-2xl", "m-5", "bg-pink-500", "p-3", "rounded-lg", "text-white", "hover:bg-pink-400" ], HE.onClick \_ -> StopAudio ]
                  [ HH.text "Stop audio" ]
              ]
          , HH.div [ classes [ "flex-grow" ] ] []
          ]
      , HH.div [ classes [ "flex-grow" ] ] []
      ]
  ]

data TraverseH f = TraverseH f

instance foldTraverseH ::
  ( IsSymbol sym
  , Lacks sym r1
  , Cons sym b r1 r2
  , Apply m
  , Functor m
  ) =>
  FoldingWithIndex (TraverseH (a -> m b)) (Proxy sym) (m { | r1 }) a (m { | r2 }) where
  foldingWithIndex (TraverseH f) prop rec a = Rec.insert prop <$> f a <*> rec

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
    sounds <- H.liftAff $ sequential $ hfoldlWithIndex (TraverseH (parallel <<< toAffE <<< decodeAudioDataFromUri audioCtx)) (pure {} :: ParAff {}) sounds'
    let
      ffiAudio = (defaultFFIAudio audioCtx unitCache)
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( run (pure unit) (pure sounds)
                { easingAlgorithm }
                (ffiAudio)
                piece
            )
            (\(_ :: Run Unit ()) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
