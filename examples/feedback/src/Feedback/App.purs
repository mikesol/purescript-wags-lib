module Feedback.App where

import Prelude

import Color (rgb)
import Control.Applicative.Indexed ((:*>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Parallel (parallel, sequential)
import Control.Plus (empty)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Int (floor, toNumber)
import Data.Lens (over)
import Data.Lens.Grate (grate)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Additive (Additive)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (D2, D7, D60, toInt')
import Data.Vec (Vec, (+>))
import Data.Vec as V
import Data.Vec as Vec
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Log
import Effect.Random (random)
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Mouse (position)
import FRP.Event (makeEvent, subscribe)
import FRP.Event.Mouse (getMouse)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Record.Builder as Record
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import WAGS.Change (ichange)
import WAGS.Control.Functions (imodifyRes)
import WAGS.Control.Functions.Graph (iloop, loopUsingScene)
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create.Optionals (gain, playBuf, sinOsc, speaker, subgraph)
import WAGS.Graph.AudioUnit (OnOff(..), TGain(..), TPeriodicOsc(..), TPlayBuf(..), TSpeaker(..), _off, _offOn, _on)
import WAGS.Graph.Parameter (ff)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, makeFFIAudioSnapshot)
import WAGS.Lib.BufferPool (Buffy(..), BuffyVec)
import WAGS.Lib.BufferPool (Buffy(..), makeBufferPool)
import WAGS.Lib.BufferPool (Buffy(..), makeBufferPool)
import WAGS.Lib.Cofree (heads, tails)
import WAGS.Lib.Cofree (heads, tails)
import WAGS.Lib.Cofree (heads, tails)
import WAGS.Lib.Latch (makeLatchAP)
import WAGS.Lib.Rate (ARate, CfRate, MakeRate, Rate, makeRate, timeIs)
import WAGS.Lib.Rate (ARate, CfRate, Rate, timeIs)
import WAGS.Lib.SimpleBuffer (SimpleBuffer, SimpleBufferCf, SimpleBufferHead, actualizeSimpleBuffer)
import WAGS.Math (calcSlope)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), Run, run)
import WAGS.Subgraph (SubSceneSig)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type World = {}

type Res = Unit

type Graph =
  ( speaker :: TSpeaker /\ { master :: Unit }
  , master :: TGain /\ { bufferSource0 :: Unit }
  , bufferSource0 :: TPlayBuf /\ {}
  , bufferSource1 :: TPlayBuf /\ {}
  , synthpad0 :: TPeriodicOsc /\ {}
  , synthSource1 :: TPeriodicOsc /\ {}
  )

piece :: forall env analyserCbs. Scene (SceneI env World analyserCbs) RunAudio RunEngine Frame0 Res
piece =
  loopUsingScene
    ( \_ _ ->
        { control: unit
        , scene: speaker
            { masterGain:
                gain 0.01 $ sinOsc 440.0
            }
        }
    )
    (const unit)

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

data T2 = T2_0 | T2_1
data T3 = T3_0 | T3_1 | T3_2
data T4 = T4_0 | T4_1 | T4_2 | T4_3
data T5 = T5_0 | T5_1 | T5_2 | T5_3 | T5_4
data T6 = T6_0 | T6_1 | T6_2 | T6_3 | T6_4

data Rect = Rect Int Int Int Int

data Control
  = Slider Rect Color Number
  | DiscreteChooser Rect Color Int Int
  | T2 Rect Color T2
  | T3 Rect Color T3
  | T4 Rect Color T4
  | T5 Rect Color T5
  | T6 Rect Color T6
  | Pad Rect Color Number
  | Source Rect Color

plainc = RGB 100 100 100
focusc = RGB 10 10 10

-- SYNTH 1
-- 3 config of oscillators
-- 1 config of pitch (multiswitch)
type Instructions (a :: Type) =
  ( triggerSynthPad0 :: a
  , toggleSynthPad0Osc0 :: a
  , toggleSynthPad0Osc1 :: a
  , toggleSynthPad0Osc2 :: a
  , detuneSynthPad0 :: a
  , triggerSynthSource1 :: a
  , pitchSynthSource1 :: a
  , nPitchesPlayedSource1 :: a
  , envelopeSource1 :: a
  , bufferPad0 :: a
  , activationEnergyThresholdPad0 :: a
  , decayPad0 :: a
  , sampleOneShot :: a
  )

elts :: { | Instructions Control }
elts =
  { -- press to grow or shrink a pad
    triggerSynthPad0: Pad (Rect 60 60 240 240) focusc 0.0
    -- periodic wave in mix
  , toggleSynthPad0Osc0: T2 (Rect 400 60 60 60) focusc T2_0
  -- triangle wave in mix
  , toggleSynthPad0Osc1: T2 (Rect 540 940 60 60) focusc T2_0
  -- sawtooth wave in mix
  , toggleSynthPad0Osc2: T2 (Rect 270 690 60 60) focusc T2_0
  -- three detuning factors + "normal" harmonic series
  , detuneSynthPad0: T4 (Rect 800 690 130 100) focusc T4_0
  -- trigger synth
  , triggerSynthSource1: Source (Rect 720 790 210 210) focusc
  -- choose which pitch out of 14 to start at
  , pitchSynthSource1: DiscreteChooser (Rect 930 70 70 520) focusc 14 0
  -- n pitches played when pressed (fixed sequence always based on start)
  , nPitchesPlayedSource1: DiscreteChooser (Rect 90 300 90 390) focusc 6 0
  -- one of three envelopes
  , envelopeSource1: T3 (Rect 660 0 140 140) focusc T3_0
  -- pad for a buffer
  , bufferPad0: Pad (Rect 270 390 320 300) focusc 0.0
  -- activation energy threshold pad 0
  , activationEnergyThresholdPad0: Slider (Rect 90 750 300 60) focusc 0.0
  , decayPad0: Slider (Rect 590 60 70 400) focusc 0.0
  , sampleOneShot: Source (Rect 660 140 200 200) focusc
  }

type Quads a = Vec D60 a

controls :: Quads Control
controls =
  T2 (Rect 0 0 60 60) plainc T2_0
    +> T2 (Rect 590 590 410 100) plainc T2_0
    +> T3 (Rect 660 340 270 120) plainc T3_0
    +> T2 (Rect 590 460 340 130) plainc T2_0
    +> T2 (Rect 0 940 300 60) plainc T2_0
    +> T2 (Rect 90 750 300 60) focusc T2_0 -- activation enery
    +> T2 (Rect 90 810 450 60) plainc T2_0
    +> T2 (Rect 90 870 630 70) plainc T2_0
    +> T2 (Rect 0 60 60 60) plainc T2_0
    +> T2 (Rect 60 0 60 60) plainc T2_0
    +> T2 (Rect 400 60 60 60) focusc T2_0 --
    +> T2 (Rect 400 120 60 60) plainc T2_0
    +> T2 (Rect 90 690 60 60) plainc T2_0
    +> T2 (Rect 150 690 60 60) plainc T2_0
    +> T2 (Rect 300 940 60 60) plainc T2_0
    +> T2 (Rect 360 940 60 60) plainc T2_0
    +> T2 (Rect 420 940 60 60) plainc T2_0
    +> T2 (Rect 480 940 60 60) plainc T2_0
    +> T2 (Rect 540 940 60 60) focusc T2_0 --
    +> T2 (Rect 600 940 60 60) plainc T2_0
    +> T2 (Rect 660 940 60 60) plainc T2_0
    +> T2 (Rect 210 690 60 60) plainc T2_0
    +> T2 (Rect 270 690 60 60) focusc T2_0 --
    +> T2 (Rect 330 690 60 60) plainc T2_0
    +> T2 (Rect 460 60 60 60) plainc T2_0
    +> T2 (Rect 460 120 60 60) plainc T2_0
    +> T2 (Rect 800 70 60 70) plainc T2_0
    +> T2 (Rect 860 270 70 70) plainc T2_0
    +> T2 (Rect 0 120 60 60) plainc T2_0
    +> T2 (Rect 120 0 60 60) plainc T2_0
    +> T2 (Rect 0 180 60 60) plainc T2_0
    +> T2 (Rect 180 0 60 60) plainc T2_0
    +> T2 (Rect 0 240 60 60) plainc T2_0
    +> T2 (Rect 0 300 90 640) plainc T2_0
    +> T2 (Rect 90 300 90 390) focusc T2_0 --
    +> T2 (Rect 180 300 90 260) plainc T2_0
    +> T2 (Rect 270 300 320 90) plainc T2_0
    +> T2 (Rect 240 0 60 60) plainc T2_0
    +> T2 (Rect 300 0 60 60) plainc T2_0
    +> T2 (Rect 300 60 100 100) plainc T2_0
    +> T2 (Rect 360 0 300 60) plainc T2_0
    +> T2 (Rect 300 160 100 140) plainc T2_0
    +> T2 (Rect 400 180 120 120) plainc T2_0
    +> T2 (Rect 60 60 240 240) focusc T2_0 --
    +> T2 (Rect 270 390 320 300) focusc T2_0 --
    +> T2 (Rect 660 140 200 200) focusc T2_0 --
    +> T2 (Rect 660 0 140 140) focusc T2_0 --
    +> T2 (Rect 800 0 200 70) plainc T2_0
    +> T2 (Rect 590 60 70 400) focusc T2_0 --
    +> T2 (Rect 860 70 70 200) plainc T2_0
    +> T2 (Rect 930 70 70 520) focusc T2_0 --
    +> T2 (Rect 930 690 70 310) plainc T2_0
    +> T2 (Rect 520 60 70 240) plainc T2_0
    +> T2 (Rect 180 560 90 130) plainc T2_0
    +> T2 (Rect 720 690 80 100) plainc T2_0
    +> T2 (Rect 630 690 90 180) plainc T2_0
    +> T2 (Rect 540 690 90 180) plainc T2_0
    +> T2 (Rect 390 690 150 120) plainc T2_0
    +> T2 (Rect 800 690 130 100) focusc T2_0 --
    +> T2 (Rect 720 790 210 210) focusc T2_0 --
    +> V.empty

great = grate ((/\) <$> ((#) fst) <*> ((#) snd))
tnt = over great toNumber

c2s :: forall i w. Control -> Array (HH.HTML i w)
c2s (Slider (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s (DiscreteChooser (Rect x y w h) color _ _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s (T2 (Rect x y w h) color t) =
  [ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
  ] <> case t of
    T2_0 -> []
    T2_1 -> [ SE.circle [ SA.cx $ (toNumber x + (toNumber w / 2.0)), SA.cy $ (toNumber y + (toNumber h / 2.0)), SA.r $ toNumber ((min w h) - 20) / 2.0, SA.fill (RGB 10 10 10), SA.stroke (RGB 4 4 4) ] ]
c2s (T3 (Rect x y w h) color t) = [ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ] ] <> case t of
  T3_0 -> []
  T3_1 -> [ SE.polygon [ HH.attr (wrap "points") (show x <> "," <> show y <> " " <> show (x + w) <> "," <> show y <> " " <> show (x + w) <> "," <> show (y + h) <> " ") ] ]
  T3_2 -> [ SE.polygon [ HH.attr (wrap "points") (show x <> "," <> show y <> " " <> show x <> "," <> show (y + h) <> " " <> show (x + w) <> "," <> show (y + h) <> " ") ] ]
c2s (T4 (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s (T5 (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s (T6 (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s (Pad (Rect x y w h) color _) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]
c2s (Source (Rect x y w h) color) = pure $ SE.rect [ SA.x $ toNumber x, SA.y $ toNumber y, SA.width $ toNumber w, SA.height $ toNumber h, SA.fill color, SA.stroke (RGB 4 4 4) ]

render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div [ classes [ "w-screen", "h-screen" ] ]
    [ SE.svg
        [ SA.classes $ map ClassName [ "w-full", "h-full" ]
        , SA.viewBox 0.0 0.0 1000.0 1000.0
        , SA.preserveAspectRatio Nothing SA.Slice
        ]
        (join $ map c2s $ V.toArray controls)
    ]

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  StartAudio -> do
    handleAction StopAudio
    audioCtx <- H.liftEffect context
    ffiAudio <- H.liftEffect $ makeFFIAudioSnapshot audioCtx
    let
      sounds' = "https://freesound.org/data/previews/24/24620_130612-hq.mp3"
        +> "https://freesound.org/data/previews/24/24622_130612-hq.mp3"
        +> "https://freesound.org/data/previews/24/24623_130612-hq.mp3"
        +> "https://freesound.org/data/previews/24/24624_130612-hq.mp3"
        +> "https://freesound.org/data/previews/24/24625_130612-hq.mp3"
        +> "https://freesound.org/data/previews/24/24626_130612-hq.mp3"
        +> "https://freesound.org/data/previews/24/24627_130612-hq.mp3"
        +> V.empty

    sounds <- H.liftAff $ sequential $ traverse (parallel <<< decodeAudioDataFromUri audioCtx) sounds'
    mouse' <- H.liftEffect getMouse
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( run (pure unit)
                (pure {})
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
