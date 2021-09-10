module Sector.App where

import Prelude

import Control.Applicative.Indexed ((:*>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Parallel (parallel, sequential)
import Control.Plus (empty)
import Control.Promise (toAffE)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Either (either)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Traversable (for_, traverse)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, D2, D8, toInt, toInt')
import Data.Vec ((+>))
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
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Lacks)
import Record as R
import Math ((%))
import Record.Builder as Record
import Run.Except (throw)
import Run.Reader (ask)
import Run.State (get, modify)
import Type.Proxy (Proxy(..))
import WAGS.Change (ichange)
import WAGS.Control.Functions (imodifyRes)
import WAGS.Control.Functions.Validated (iloop, startUsingWithHint)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create.Optionals (gain, playBuf, speaker)
import WAGS.Graph.AudioUnit (OnOff(..), TGain, TPlayBuf, TSpeaker)
import WAGS.Graph.Parameter (ff)
import WAGS.Interpret (bufferDuration, close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.BufferPool (Buffy(..))
import WAGS.Lib.Cofree (heads, tails)
import WAGS.Lib.Lag (CfLag, makeLag)
import WAGS.Lib.Rate (ARate, Rate, CfRate, timeIs)
import WAGS.Lib.SimpleBuffer (SimpleBuffer, SimpleBufferCf, SimpleBufferHead, actualizeSimpleBuffer)
import WAGS.Lib.WAG (RunWag, ShortCircuit(..))
import WAGS.Math (calcSlope)
import WAGS.Run (Run, RunAudio, RunEngine, SceneI(..), run)
import WAGS.Template (fromTemplate)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

ntropi :: Behavior Number
ntropi =
  behavior \e ->
    makeEvent \f ->
      e `subscribe` \a2b -> (a2b <$> random) >>= f

globalFF = 0.03 :: Number

type Time = Number

type NSectors = D8 -- 8 sectors

type SectorRuns audio engine proof =
  V.Vec NSectors (SectorM audio engine proof Unit)

type RateInfo r =
  ( rate :: ARate
  , rateHistory :: Maybe (CfLag Number)
  | r
  )

type World = { buffer :: BrowserAudioBuffer }

type Graph =
  ( speaker :: TSpeaker /\ { gain :: Unit }
  , gain :: TGain /\ { buf :: Unit }
  , buf :: TPlayBuf /\ {}
  )

type Env = SceneI Unit World ()

type Res = Unit

type SectorM audio engine proof a = RunWag Env (Acc audio engine proof) audio engine proof Res Graph a

type ControlNext audio engine proof r = (next :: Cofree Identity (SectorM audio engine proof Unit) | r)

newtype Acc audio engine proof =
  Acc { | RateInfo (ControlNext audio engine proof ()) }

derive instance newtypeAcc :: Newtype (Acc audio engine proof) _

-- we're going to need a fixed point here
-- basically, at a certain condition, we go forward one modulo vec
-- that means that we need a check for the time and need to return the next free
-- if the time check fails
base
  :: forall i audio engine proof
   . Pos i
  => V.Vec i (SectorM audio engine proof {})
base = V.fill (const (pure {}))

erase
  :: forall a i audio engine proof
   . Nat i
  => V.Vec i (SectorM audio engine proof a)
  -> V.Vec i (SectorM audio engine proof Unit)
erase = asr (const $ const $ pure unit)

rates
  :: forall i r' audio engine proof
   . Nat i
  => Lacks "currentRate" r'
  => Lacks "modifiedTime" r'
  => V.Vec i (SectorM audio engine proof { | r' })
  -> V.Vec i (SectorM audio engine proof { currentRate :: Number, modifiedTime :: Number | r' })
rates = asr (const (makeRate (fromMaybe 1.0)))

makeRate
  :: forall r' audio engine proof
   . Lacks "currentRate" r'
  => Lacks "modifiedTime" r'
  => (Maybe Number -> Number)
  -> { | r' }
  -> SectorM audio engine proof { currentRate :: Number, modifiedTime :: Number | r' }
makeRate rateF a = do
  Acc { rate, rateHistory } <- get
  SceneI { time } <- ask
  let
    currentRate = rateF $ map
      (either identity snd <<< extract)
      rateHistory
    cf = rate { time, rate: currentRate }
  modify
    $ over Acc
    $ _
        { rate = unwrapCofree cf
        , rateHistory = Just $ maybe makeLag unwrapCofree rateHistory currentRate
        }
  pure
    $ R.insert (Proxy :: _ "currentRate") currentRate
    $ R.insert (Proxy :: _ "modifiedTime") (unwrap $ extract cf) a

-- all sector run
asr
  :: forall a b s audio engine proof
   . Nat s
  => (Int -> a -> SectorM audio engine proof b)
  -> V.Vec s (SectorM audio engine proof a)
  -> V.Vec s (SectorM audio engine proof b)
asr f = mapWithIndex (\i a -> a >>= f i)

-- update sector run
usr
  :: forall a i s audio engine proof
   . Nat i
  => Lt i s
  => i
  -> (Int -> a -> SectorM audio engine proof a)
  -> V.Vec s (SectorM audio engine proof a)
  -> V.Vec s (SectorM audio engine proof a)
usr i m = V.modifyAt i (\a -> a >>= m (toInt i))

maybeAdvance
  :: forall s audio engine proof r
   . Nat s
  => V.Vec s (SectorM audio engine proof { modifiedTime :: Number, currentRate :: Number | r })
  -> V.Vec s (SectorM audio engine proof { modifiedTime :: Number, currentRate :: Number | r })
maybeAdvance = asr
  ( \i a -> do
      (SceneI { world: { buffer }, headroomInSeconds }) <- ask
      let
        dur = bufferDuration buffer
        peekAhead = (a.modifiedTime + (headroomInSeconds * a.currentRate)) % dur
        asInt = toInt' (Proxy :: _ s)
        condition
          | i == asInt - 1 = peekAhead < dur / 2.0
          | otherwise = peekAhead > (toNumber i) * dur / (toNumber asInt)
      if condition then do
        Acc { next } <- get
        modify $ over Acc $ _ { next = unwrap $ unwrapCofree next }
        extract next
        throw ShortCircuit
      else do
        pure a
  )

sector
  :: forall i audio engine proof
   . Pos i
  => V.Vec i (SectorM audio engine proof Unit)
sector =
  base
    # rates
    # maybeAdvance
    # erase

--------------
--------------
--------------
--------------
--------------
--------------

type NKeys
  = D2 -- 2 keys

type NBuf
  = D2 -- 2 buffers

type RBuf
  = Unit -- no extra info needed

type KeyBufs r
  =
  ( keyBufs :: V.Vec NKeys (SimpleBuffer NBuf)
  , rate :: ARate
  | r
  )

type WorldOld
  =
  { ntropi :: Number
  , mickey :: Maybe { x :: Int, y :: Int }
  , change :: Number
  , buffers :: V.Vec NKeys BrowserAudioBuffer
  }

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
   . SceneI trigger WorldOld analyserCbs
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
   . SceneI trigger WorldOld analyserCbs
  -> { keyBufs :: V.Vec NKeys (SimpleBufferHead NBuf)
     , rate :: Rate
     | r
     }
  -> _
keyBufGraph (SceneI { world }) { keyBufs, rate } =
  { keyBufs:
      fromTemplate (Proxy :: _ "instruments") (V.zipWithE (R.insert (Proxy :: _ "browserBuf")) world.buffers keyBufs) \i { buffers, browserBuf } ->
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
          Nothing -> gain 0.0 (playBuf { onOff: Off } browserBuf)
  }
  where
  xpos = maybe 0.0 (\{ x } -> toNumber x / 1000.0) world.mickey

  time = unwrap rate

---
-- change this to make sound
-- for example, you can try:
-- a /@\ speaker { unit0: gain (cos (pi * e.time) * -0.02 + 0.02) { osc0: sinOsc 440.0 } }
type AccOld
  =
  (
  | KeyBufs ()
  )

acc :: { | AccOld }
acc = mempty

scene :: forall trigger analyserCbs. SceneI trigger WorldOld analyserCbs -> { | AccOld } -> _
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

type ResOld
  = { x :: Additive Int, y :: Additive Int }

piece :: forall env analyserCbs. Scene (SceneI env WorldOld analyserCbs) RunAudio RunEngine Frame0 ResOld
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
                    [ HH.text "SECTOR-esque" ]
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
            (\({ res } :: Run ResOld ()) -> Log.info $ show res)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing }
