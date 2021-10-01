module Gamelan.App where

import Prelude

import Control.Applicative.Indexed ((:*>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Parallel (parallel, sequential)
import Control.Plus (empty)
import Control.Promise (toAffE)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D2, D7, toInt')
import Data.Vec ((+>))
import Data.Vec as V
import Data.Vec as Vec
import Debug (spy)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Log
import Effect.Random (random)
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Mouse (position)
import FRP.Event (makeEvent, subscribe)
import FRP.Event.Mouse (getMouse)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record.Builder as Record
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import WAGS.Change (ichange)
import WAGS.Control.Functions (imodifyRes)
import WAGS.Control.Functions.Graph (iloop, startUsingWithHint)
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create.Optionals (gain, playBuf, speaker, subgraph)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.Parameter (ff)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.BufferPool (Buffy(..), BuffyVec)
import WAGS.Lib.Cofree (heads, tails)
import WAGS.Lib.Rate (ARate, CfRate, Rate, timeIs)
import WAGS.Lib.SimpleBuffer (SimpleBuffer, SimpleBufferCf, SimpleBufferHead, actualizeSimpleBuffer)
import WAGS.Math (calcSlope)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), Run, run)
import WAGS.Subgraph (SubSceneSig)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

ntropi :: Behavior Number
ntropi =
  behavior \e ->
    makeEvent \f ->
      e `subscribe` \a2b -> (a2b <$> random) >>= f

globalFF = 0.03 :: Number

type NKeys
  = D7 -- 7 keys

type NBuf
  = D2 -- 3 buffers

type RBuf
  = Unit -- no extra info needed

type World
  =
  { ntropi :: Number
  , mickey :: Maybe { x :: Int, y :: Int }
  , change :: Number
  , buffers :: V.Vec NKeys BrowserAudioBuffer
  }

type KeyBufs r
  =
  ( keyBufs :: V.Vec NKeys (SimpleBuffer NBuf)
  , rate :: ARate
  | r
  )

nps = 3.0 :: Number

buffersActualized
  :: forall trigger world analyserCbs
   . V.Vec NKeys (SceneI trigger world analyserCbs -> SimpleBuffer NBuf -> SimpleBufferCf NBuf)
buffersActualized =
  V.fill \i' ->
    let
      i = toNumber i'
    in
      actualizeSimpleBuffer
        (NEA.singleton (i / nps))
        ((toNumber $ toInt' (Proxy :: _ NKeys)) / nps)

keyBufsActualize
  :: forall trigger analyserCbs r
   . SceneI trigger World analyserCbs
  -> { | KeyBufs r }
  -> { keyBufs :: V.Vec NKeys (SimpleBufferCf NBuf)
     , rate :: CfRate
     }
keyBufsActualize e@(SceneI e'@{ time }) { keyBufs, rate } =
  { keyBufs: V.zipWithE (\f x -> f newE x) buffersActualized keyBufs
  , rate: rate'
  }
  where
  freq = 1.0 + maybe 0.0 (\{ y } -> toNumber y / 1000.0) e'.world.mickey

  rate' = rate { time, rate: freq }

  newE = timeIs (unwrap $ extract rate') e

keyBufGraph
  :: forall trigger analyserCbs r
   . SceneI trigger World analyserCbs
  -> { keyBufs :: V.Vec NKeys (SimpleBufferHead NBuf)
     , rate :: Rate
     | r
     }
  -> _
keyBufGraph (SceneI { world }) { keyBufs, rate } =
  let
    vec = V.zipWithE (\browserBuf { buffers } -> { buffers, browserBuf }) world.buffers keyBufs

    internal0 :: SubSceneSig "singleton" () { ipt :: { buffers :: BuffyVec NBuf, browserBuf :: BrowserAudioBuffer }, time :: Number }
    internal0 = unit # SG.loopUsingScene \{ time, ipt } _ ->
      { control: unit
      , scene:
          { singleton:
              subgraph ipt.buffers (const $ const internal1) (const $ { time, browserBuf: ipt.browserBuf, buf: _ }) {}
          }
      }

    internal1 :: SubSceneSig "singleton" () { time :: Number, buf :: Maybe (Buffy Unit), browserBuf :: BrowserAudioBuffer }
    internal1 = unit # SG.loopUsingScene \{ time, buf, browserBuf } _ ->
      { control: unit
      , scene:
          { singleton: case buf of
              Just (Buffy { starting, startTime }) ->
                let
                  pos = max 0.0 (startTime - time)
                  _____________________ = spy "buffy" time

                  nk = toNumber $ toInt' (Proxy :: _ NKeys)

                  endT = startTime + (nk / nps)
                in
                  gain (ff (globalFF + pos) (pure (max 0.0 $ calcSlope startTime 1.0 endT 0.0 time)))
                    ( playBuf
                        { onOff:
                            ff globalFF
                              $
                                if starting then
                                  ff pos (pure OffOn)
                                else
                                  pure On
                        , playbackRate: 1.0
                        }
                        if xpos < world.ntropi then
                          browserBuf
                        else
                          fromMaybe browserBuf (A.index (Vec.toArray world.buffers) (floor $ world.change * nk))
                    )
              Nothing -> let ___________ = spy "bad" true in gain 0.0 (playBuf { onOff: Off } browserBuf)
          }
      }
  in
    { keyBufs: subgraph vec (const $ const internal0) (const $ { time: time', ipt: _ }) {}
    }
  where
  xpos = maybe 0.0 (\{ x } -> toNumber x / 1000.0) world.mickey

  time' = unwrap rate

---
-- change this to make sound
-- for example, you can try:
-- a /@\ speaker { unit0: gain (cos (pi * e.time) * -0.02 + 0.02) { osc0: sinOsc 440.0 } }
type Acc = (| KeyBufs + ())

acc :: { | Acc }
acc = mempty

scene :: forall trigger analyserCbs. SceneI trigger World analyserCbs -> { | Acc } -> _
scene e a =
  let
    actualizer = {}

    --------------------------------------------
    actualized =
      Record.build
        (Record.union (keyBufsActualize e a))
        actualizer

    headz = heads actualized

    scene' =
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
    tails actualized /\ scene'

type Res
  = { x :: Additive Int, y :: Additive Int }

piece :: forall env analyserCbs. Scene (SceneI env World analyserCbs) RunAudio RunEngine Frame0 Res
piece =
  startUsingWithHint
    scene
    { microphone: empty }
    acc
    ( iloop
        ( \e@(SceneI e') a ->
            let
              acc /\ graph = scene e a
            in
              imodifyRes
                ( const
                    ( maybe mempty
                        (\{ x, y } -> { x: Additive x, y: Additive y })
                        e'.world.mickey
                    )
                )
                :*> (ichange graph $> acc)
        )
    )

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

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ classes [ "w-screen", "h-screen" ] ]
    [ HH.div [ classes [ "flex", "flex-col", "w-full", "h-full" ] ]
        [ HH.div [ classes [ "flex-grow" ] ] []
        , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
            [ HH.div [ classes [ "flex-grow" ] ]
                []
            , HH.div [ classes [ "flex", "flex-col" ] ]
                [ HH.h1 [ classes [ "text-center", "text-3xl", "font-bold" ] ]
                    [ HH.text "ꦒꦩꦼꦭꦤ꧀" ]
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

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    handleAction StopAudio
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    let
      sounds' = "https://freesound.org/data/previews/24/24620_130612-hq.mp3"
        +> "https://freesound.org/data/previews/24/24622_130612-hq.mp3"
        +> "https://freesound.org/data/previews/24/24623_130612-hq.mp3"
        +> "https://freesound.org/data/previews/24/24624_130612-hq.mp3"
        +> "https://freesound.org/data/previews/24/24625_130612-hq.mp3"
        +> "https://freesound.org/data/previews/24/24626_130612-hq.mp3"
        +> "https://freesound.org/data/previews/24/24627_130612-hq.mp3"
        +> V.empty

    sounds <- H.liftAff $ sequential $ traverse (parallel <<< toAffE <<< decodeAudioDataFromUri audioCtx) sounds'
    let
      ffiAudio = defaultFFIAudio audioCtx unitCache
    mouse' <- H.liftEffect getMouse
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( run (pure unit)
                ( { ntropi: _, mickey: _, change: _, buffers: sounds }
                    <$> ntropi
                    <*> position mouse'
                    <*> ntropi
                )
                { easingAlgorithm }
                ffiAudio
                piece
            )
            (\({ res } :: Run Res ()) -> Log.info $ show res)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
