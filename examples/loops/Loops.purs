module WAGS.Lib.Example.Loops where

import Prelude
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Parallel (parallel, sequential)
import Control.Promise (toAffE)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Setter (set)
import Data.List (List(..), (:))
import Data.List as List
import Data.Unfoldable as UF
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (lcmap)
import Data.Symbol (class IsSymbol)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (D5)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Behavior.Mouse (position)
import FRP.Event (subscribe)
import FRP.Event.Mouse (getMouse)
import Foreign.Object (fromHomogeneous)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Heterogeneous.Mapping (hmap, hmapWithIndex, class MappingWithIndex)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Control.Functions (iloop, startUsingWithHint)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create.Optionals (gain, playBuf, speaker)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.Parameter (AudioParameter_(..), ff)
import WAGS.Interpret (AudioContext, FFIAudio(..), close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.BufferPool (ABufferPool, Buffy(Buffy), BuffyVec, CfBufferPool, MakeBufferPoolWithRest)
import WAGS.Lib.Cofree (actualize, heads, tails)
import WAGS.Lib.Latch (ALatchAP, CfLatchAP, LatchAP, MakeLatchAP)
import WAGS.Lib.Piecewise (APFofT, TimeHeadroom, makeLoopingTerracedR, makePiecewise, makeTerracedR)
import WAGS.Run (RunAudio, RunEngine, SceneI(..), run)
import WAGS.Template (fromTemplate)

type Trigger
  = Unit

type World
  = { position :: Maybe { x :: Int, y :: Int } }

type Extern
  = SceneI Trigger World

newtype ZipProps fns
  = ZipProps { | fns }

instance zipProps ::
  (IsSymbol sym, Row.Cons sym (a -> b) x fns) =>
  MappingWithIndex (ZipProps fns) (Proxy sym) a b where
  mappingWithIndex (ZipProps fns) p = Record.get p fns

type Instruments'' a r
  = ( kick1 :: a, sideStick :: a, snare :: a, clap :: a, snareRoll :: a, kick2 :: a, closedHH :: a, shaker :: a, openHH :: a, tamb :: a, crash :: a, ride :: a | r )

type Instruments' a
  = Instruments'' a ()

type Instruments a
  = { | Instruments' a }

data Note i
  = Note i i i

notes :: Instruments (NonEmpty Array (Note Int))
notes =
  { kick1:
      Note 127 0 12
        :| [ Note 127 24 36
          , Note 127 72 84
          , Note 127 144 154
          , Note 127 168 181
          , Note 127 216 227
          , Note 127 264 274
          , Note 126 384 393
          , Note 127 408 419
          , Note 127 456 467
          , Note 127 528 537
          , Note 127 552 563
          , Note 127 600 610
          , Note 127 648 658
          , Note 127 768 778
          , Note 126 792 802
          , Note 126 840 853
          , Note 126 912 921
          , Note 127 936 947
          , Note 126 984 995
          , Note 127 1032 1042
          , Note 127 1152 1162
          , Note 127 1176 1186
          , Note 127 1224 1236
          , Note 126 1296 1306
          , Note 127 1320 1332
          , Note 126 1368 1380
          , Note 127 1416 1428
          ]
  , sideStick:
      Note 108 96 120
        :| [ Note 108 288 312
          , Note 108 480 504
          , Note 108 672 696
          , Note 108 864 888
          , Note 108 1056 1080
          , Note 108 1248 1272
          , Note 108 1440 1464
          ]
  , snare:
      Note 127 288 312
        :| [ Note 127 672 698
          , Note 127 1056 1081
          , Note 127 1440 1460
          , Note 110 1488 1496
          , Note 110 1512 1520
          ]
  , clap:
      Note 127 96 120
        :| [ Note 113 288 312
          , Note 125 480 504
          , Note 113 672 696
          , Note 125 864 888
          , Note 113 1056 1080
          , Note 125 1248 1272
          , Note 113 1440 1464
          ]
  , snareRoll:
      Note 108 360 378
        :| [ Note 113 744 763
          , Note 110 888 903
          , Note 113 1128 1146
          , Note 110 1272 1288
          ]
  , kick2:
      Note 111 0 6
        :| [ Note 111 24 30
          , Note 110 144 150
          , Note 111 168 174
          , Note 110 384 389
          , Note 111 408 414
          , Note 112 528 534
          , Note 111 552 559
          , Note 111 768 776
          , Note 111 792 800
          , Note 111 912 920
          , Note 111 936 943
          , Note 111 1152 1158
          , Note 110 1176 1183
          , Note 111 1296 1302
          , Note 111 1320 1326
          ]
  , closedHH:
      Note 126 24 33
        :| [ Note 126 72 84
          , Note 127 120 132
          , Note 127 168 181
          , Note 126 216 233
          , Note 127 264 280
          , Note 126 360 378
          , Note 126 408 425
          , Note 126 456 473
          , Note 123 504 522
          , Note 126 552 569
          , Note 126 600 617
          , Note 127 648 667
          , Note 124 744 762
          , Note 124 792 810
          , Note 125 840 858
          , Note 124 888 906
          , Note 124 936 954
          , Note 125 984 1002
          , Note 127 1032 1051
          , Note 127 1128 1148
          , Note 125 1176 1194
          , Note 127 1224 1242
          , Note 127 1272 1290
          , Note 125 1320 1338
          , Note 126 1368 1386
          , Note 127 1416 1435
          , Note 126 1512 1532
          ]
  , shaker:
      Note 110 0 14
        :| [ Note 111 48 61
          , Note 111 96 112
          , Note 111 144 161
          , Note 111 192 210
          , Note 111 240 260
          , Note 110 288 308
          , Note 110 336 355
          , Note 111 384 403
          , Note 111 432 452
          , Note 107 480 500
          , Note 110 528 547
          , Note 111 576 597
          , Note 111 624 645
          , Note 111 672 694
          , Note 111 720 740
          , Note 111 768 788
          , Note 111 792 803
          , Note 111 816 837
          , Note 111 840 854
          , Note 111 864 884
          , Note 111 888 901
          , Note 111 912 933
          , Note 111 936 948
          , Note 111 960 981
          , Note 111 984 997
          , Note 111 1008 1029
          , Note 111 1032 1046
          , Note 111 1056 1077
          , Note 111 1080 1093
          , Note 111 1104 1126
          , Note 111 1128 1141
          , Note 111 1152 1173
          , Note 111 1176 1190
          , Note 111 1200 1220
          , Note 111 1224 1237
          , Note 111 1248 1270
          , Note 111 1272 1287
          , Note 111 1296 1317
          , Note 111 1320 1333
          , Note 111 1344 1366
          , Note 111 1368 1381
          , Note 111 1392 1414
          , Note 111 1416 1429
          , Note 111 1440 1462
          , Note 111 1464 1478
          , Note 111 1488 1507
          , Note 111 1512 1525
          ]
  , openHH:
      Note 127 288 312
        :| [ Note 127 672 696
          , Note 127 1056 1080
          , Note 127 1440 1464
          ]
  , tamb:
      Note 115 96 120
        :| [ Note 115 288 312
          , Note 115 480 504
          , Note 115 672 696
          , Note 115 864 888
          , Note 115 1056 1080
          , Note 115 1248 1272
          , Note 115 1440 1464
          ]
  , crash: Note 89 0 16 :| []
  , ride:
      Note 72 0 10
        :| [ Note 88 96 106
          , Note 88 192 201
          , Note 88 288 297
          , Note 88 384 393
          , Note 88 480 489
          , Note 88 576 585
          , Note 88 672 681
          , Note 88 768 776
          , Note 88 864 873
          , Note 88 960 968
          , Note 88 1056 1064
          , Note 88 1152 1161
          , Note 88 1248 1256
          , Note 88 1344 1353
          , Note 88 1440 1449
          ]
  }

ticksInBeatI = 96 :: Int

beatsInMeasure = 4 :: Int

measuresInLoop = 4 :: Int

ticksInLoop = ticksInBeatI * beatsInMeasure * measuresInLoop :: Int

timedTicksInLoop = toNumber ticksInLoop * ticksToTime :: Number

ticksInBeat = toNumber ticksInBeatI :: Number

tempo = 90.0 :: Number

tmul = 60.0 / tempo :: Number

ticksToTime = tmul / ticksInBeat :: Number

notesf :: Instruments (NonEmpty Array (Note Number))
notesf = hmap toLN notes
  where
  toLN :: NonEmpty Array (Note Int) -> NonEmpty Array (Note Number)
  toLN = map (\(Note a b c) -> Note (toNumber a / 127.0) (toNumber b * ticksToTime) (toNumber c * ticksToTime))

newtype GOO
  = GOO { "_eq" :: Number, gain :: APFofT Number, onOff :: APFofT OnOff }

-- todo: does this make sense. We may never "need" it, but is it a good idea?
instance semigroupGOO :: Semigroup GOO where
  append a b = a

derive instance newtypeGOO :: Newtype GOO _

instance eqGOO :: Eq GOO where
  eq (GOO { "_eq": e0 }) (GOO { "_eq": e1 }) = e0 == e1

bookend :: NonEmpty Array (Number /\ GOO) -> NonEmpty Array (Number /\ GOO)
bookend (a :| b) = (newA :| newB)
  where
  startsAt0 = fst a == 0.0

  newA =
    if startsAt0 then
      a
    else
      0.0
        /\ wrap
            { gain: makePiecewise ((0.0 /\ 0.0) :| Nil)
            , onOff: makeTerracedR ((0.0 /\ Off) :| Nil)
            , "_eq": 0.0
            }

  newB = (if startsAt0 then b else [ a ] <> b) <> [ timedTicksInLoop /\ set (simple _Newtype <<< prop (Proxy :: _ "_eq")) timedTicksInLoop (snd newA) ]

pwfs :: Instruments (APFofT GOO)
pwfs = hmap toAPFofT notesf
  where
  toAPFofT :: NonEmpty Array (Note Number) -> APFofT GOO
  toAPFofT =
    makeLoopingTerracedR
      <<< (\(a :| b) -> (a :| (List.fromFoldable b)))
      <<< bookend
      <<< map (\v@(Note _ b _) -> b /\ toPWF v)

  -- slope downward
  slopeDown = 20.0 * ticksToTime

  turnOff = 21.0 * ticksToTime

  toPWF :: Note Number -> GOO
  toPWF (Note a b c) =
    wrap
      { gain: makePiecewise ((0.0 /\ a) :| (lenny /\ a) : ((lenny + slopeDown) /\ 0.0) : Nil)
      , onOff: makeTerracedR ((0.0 /\ On) :| ((lenny + turnOff) /\ Off) : Nil)
      , "_eq": b
      }
    where
    lenny = c - b

type NBuf
  = D5

type RBuf
  = GOO

type Ctrl
  = { latch :: ALatchAP GOO, buffers :: ABufferPool NBuf RBuf }

type Acc
  = ( instruments :: Instruments Ctrl )

type CfCtrl
  = { latch :: CfLatchAP (MakeLatchAP GOO) (LatchAP GOO)
    , buffers :: CfBufferPool (MakeBufferPoolWithRest RBuf) (BuffyVec NBuf RBuf)
    }

actualizePWF ::
  Extern ->
  TimeHeadroom ->
  (TimeHeadroom -> AudioParameter_ GOO) ->
  Ctrl ->
  CfCtrl
actualizePWF e th f a =
  { latch
  , buffers:
      actualize a.buffers e
        ( UF.fromMaybe
            ( do
                AudioParameter { param, timeOffset } <- extract latch
                rest <- param
                pure { offset: timeOffset, rest }
            )
        )
  }
  where
  latch = actualize a.latch e (f th)

actualizer ::
  Extern ->
  { | Acc } ->
  { instruments :: Instruments CfCtrl }
actualizer e@(SceneI { time: time', headroomInSeconds: headroom }) { instruments } = { instruments: step1 `zipRecord` instruments }
  where
  time = time'

  step1 = hmap (actualizePWF e { time, headroom }) pwfs

  zipRecord = hmapWithIndex <<< ZipProps

-- to avoid jank
globalFF = 0.03 :: Number

acc :: { | Acc }
acc = mempty

normalizePosition :: Maybe { x :: Int, y :: Int } -> { x :: Number, y :: Number }
normalizePosition = (\{ x, y } -> { x: toNumber x / 1000.0, y: toNumber y / 1000.0 }) <<< fromMaybe { x: 500, y: 500 }

slideTime :: forall a. (Extern -> a) -> Extern -> a
slideTime =
  lcmap
    ( \(SceneI r@{ time, world: { position: p } }) ->
        SceneI (r { time = time - 0.25 + 0.6 * (normalizePosition p).y })
    )

piece :: Scene Extern RunAudio RunEngine Frame0 Unit
piece =
  startUsingWithHint
    scene
    acc
    ( iloop
        ( slideTime \e@(SceneI { time, world: { position: p } }) (a :: { | Acc }) ->
            let
              actualized = actualizer e a
            in
              ichange (scene e (normalizePosition p) (heads actualized)) $> tails actualized
        )
    )
  where
  scene ::
    Extern ->
    { x :: Number, y :: Number } ->
    { instruments :: Instruments { latch :: LatchAP GOO, buffers :: BuffyVec NBuf RBuf } } -> _
  scene (SceneI { time, headroomInSeconds: headroom }) p { instruments } =
    speaker
      ( gain (if time < 5.0 then time / 5.0 else 1.0)
          ( fromTemplate (Proxy :: _ "instruments") instruments \name { buffers } ->
              fromTemplate (Proxy :: _ "buffers") buffers \_ -> case _ of
                Just (Buffy { starting, startTime, rest: GOO goo }) ->
                  gain (ff globalFF $ goo.gain { time: time - startTime, headroom })
                    ( playBuf
                        { onOff:
                            ff globalFF
                              $ if starting then
                                  ff (max 0.0 (startTime - time)) (pure OffOn)
                                else
                                  goo.onOff { time: time - startTime, headroom }
                        , playbackRate: 0.8 + 0.2 * p.x
                        }
                        name
                    )
                Nothing -> gain 0.0 (playBuf { onOff: Off } name)
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
render state = do
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
    mouse <- H.liftEffect getMouse
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    let
      sounds' =
        { kick1: "http://localhost:9000/kick1.mp3"
        , sideStick: "http://localhost:9000/sideStick.mp3"
        , snare: "http://localhost:9000/snare.mp3"
        , clap: "http://localhost:9000/clap.mp3"
        , snareRoll: "http://localhost:9000/snareRoll.mp3"
        , kick2: "http://localhost:9000/kick2.mp3"
        , closedHH: "http://localhost:9000/closedHH.mp3"
        , shaker: "http://localhost:9000/shaker.mp3"
        , openHH: "http://localhost:9000/openHH.mp3"
        , tamb: "http://localhost:9000/tamb.mp3"
        , crash: "http://localhost:9000/crash.mp3"
        , ride: "http://localhost:9000/ride.mp3"
        }
    sounds <- H.liftAff $ sequential $ sequence $ map (parallel <<< toAffE <<< decodeAudioDataFromUri audioCtx) $ fromHomogeneous sounds'
    let
      ffiAudio = (defaultFFIAudio audioCtx unitCache) { buffers = pure sounds }
    unsubscribe <-
      H.liftEffect
        $ subscribe
            (run (pure unit) ({ position: _ } <$> position mouse) { easingAlgorithm } (FFIAudio ffiAudio) piece)
            (const $ pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
