module Sector.App where

import Prelude

import Control.Applicative.Indexed ((:*>))
import Control.Comonad (class Comonad, class Extend, extract)
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
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
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
import Math ((%))
import Prim.Row (class Lacks)
import Record as R
import Record.Builder as Record
import Run.Except (throw)
import Run.Reader (ask)
import Run.State (get, modify, put)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import WAGS.Change (ichange)
import WAGS.Control.Functions (imodifyRes)
import WAGS.Control.Functions.Validated (iloop, startUsingWithHint)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create.Optionals (gain, playBuf, speaker)
import WAGS.Graph.AudioUnit (OnOff(..), TGain, TPlayBuf, TSpeaker)
import WAGS.Graph.Parameter (ff)
import WAGS.Interpret (class AudioInterpret, bufferDuration, close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.BufferPool (Buffy(..))
import WAGS.Lib.Cofree (heads, tails)
import WAGS.Lib.Lag (CfLag, makeLag)
import WAGS.Lib.Latch (Latch, CfLatch)
import WAGS.Lib.Rate (ARate, Rate, CfRate, timeIs)
import WAGS.Lib.SimpleBuffer (SimpleBuffer, SimpleBufferCf, SimpleBufferHead, actualizeSimpleBuffer)
import WAGS.Lib.WAG (RunWag, ShortCircuit(..), wag)
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

type RateAcc r =
  ( rate :: ARate
  , rateHistory :: Maybe (CfLag Number)
  | r
  )

type SectorAcc r =
  ( sectorCount :: Int
  , sectorLatch :: CfLatch Int
  | r
  )

type NextAcc audio engine proof r = (next :: Cofree Identity (SectorM audio engine proof Unit) | r)
type PrevSectorAcc r = (prevSector :: Maybe Int | r)

type World = { buffer :: BrowserAudioBuffer }

type Graph =
  ( speaker :: TSpeaker /\ { gain :: Unit }
  , gain :: TGain /\ { buf :: Unit }
  , buf :: TPlayBuf /\ {}
  )

type Env = SceneI Unit World ()

type Res = Unit

type SectorM audio engine proof a = RunWag Env (Acc audio engine proof) audio engine proof Res Graph a

newtype Acc audio engine proof =
  Acc { | RateAcc + PrevSectorAcc + SectorAcc + NextAcc audio engine proof () }

derive instance newtypeAcc :: Newtype (Acc audio engine proof) _

type CachedAcc audio engine proof r =
  ( cachedAcc :: Acc audio engine proof
  | r
  )

base
  :: forall i audio engine proof
   . Pos i
  => V.Vec i (SectorM audio engine proof { | CachedAcc audio engine proof () })
base = V.fill (const $ { cachedAcc: _ } <$> get)

erase
  :: forall a i audio engine proof
   . Nat i
  => V.Vec i (SectorM audio engine proof a)
  -> V.Vec i (SectorM audio engine proof Unit)
erase = asr (const $ const $ pure unit)

confirmSectorLatch
  :: forall a i audio engine proof
   . Nat i
  => V.Vec i (SectorM audio engine proof a)
  -> V.Vec i (SectorM audio engine proof a)
confirmSectorLatch = asr \i a -> do
    -- this will force the sector latch to be Nothing on the next iteration
    Acc { sectorCount, sectorLatch } <- get
    modify $ over Acc $ _
      { sectorCount = sectorCount
      , sectorLatch = unwrapCofree sectorLatch sectorCount
      }
    pure a


rates
  :: forall i r' audio engine proof
   . Nat i
  => Lacks "currentRate" r'
  => Lacks "modifiedTime" r'
  => V.Vec i (SectorM audio engine proof { | r' })
  -> V.Vec i (SectorM audio engine proof { | RateInfo r' })
rates = asr (const (makeRate (maybe 1.0 extract)))

data SectorSect a = SectorStart a | SectorCont a

derive instance functorSectorSect :: Functor SectorSect

instance extendSectorSec :: Extend SectorSect where
  extend wab i@(SectorStart _) = SectorStart $ wab i
  extend wab i@(SectorCont _) = SectorCont $ wab i

instance comonadSectorSec :: Comonad SectorSect where
  extract (SectorStart a) = a
  extract (SectorCont a) = a

type RateInfo r = (modifiedTime :: Number, currentRate :: SectorSect Number | r)
type SectorInfo r = (currentSector :: Int | r)

latchToSectorSec :: forall a b. CfLatch a -> b -> SectorSect b
latchToSectorSec cf b = case extract cf of
  Nothing -> SectorCont b
  Just _ -> SectorStart b

makeRate
  :: forall r' audio engine proof
   . Lacks "currentRate" r'
  => Lacks "modifiedTime" r'
  => (Maybe (SectorSect Number) -> Number)
  -> { | r' }
  -> SectorM audio engine proof { | RateInfo r' }
makeRate rateF a = do
  Acc { rate, rateHistory, sectorLatch } <- get
  SceneI { time } <- ask
  let
    latchF = latchToSectorSec sectorLatch
    currentRate = rateF $ map
      (latchF <<< either identity snd <<< extract)
      rateHistory
    cf = rate { time, rate: currentRate }
  modify
    $ over Acc
    $ _
        { rate = unwrapCofree cf
        , rateHistory = Just $ maybe makeLag unwrapCofree rateHistory currentRate
        }
  pure
    $ R.insert (Proxy :: _ "currentRate") (latchF currentRate)
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

advanceIfSectorEnds
  :: forall s audio engine proof r
   . Nat s
  => V.Vec s (SectorM audio engine proof { | RateInfo + CachedAcc audio engine proof r })
  -> V.Vec s (SectorM audio engine proof { | RateInfo + CachedAcc audio engine proof r })
advanceIfSectorEnds = asr \i a -> do
  SceneI { world: { buffer }, headroomInSeconds } <- ask
  let
    dur = bufferDuration buffer
    peekAhead = (a.modifiedTime + (headroomInSeconds * extract a.currentRate)) % dur
    asInt = toInt' (Proxy :: _ s)
    condition
      | i == asInt - 1 = peekAhead < dur / 2.0
      | otherwise = peekAhead > (toNumber i) * dur / (toNumber asInt)
  when condition do
    -- reset the state
    put a.cachedAcc
    Acc { next, sectorCount, sectorLatch } <- get
    -- increment the sector count
    let newSectorCount = sectorCount + 1
    modify $ over Acc $ _
      { sectorCount = newSectorCount
      , sectorLatch = unwrapCofree sectorLatch newSectorCount
      , next = unwrap $ unwrapCofree next
      , prevSector = Just i
      }
    -- run next
    extract next
    -- throw to skip to the end
    throw ShortCircuit
  pure a

doBufferAlignment
  :: forall s audio engine proof r
   . Nat s
  => AudioInterpret audio engine
  => V.Vec s (SectorM audio engine proof r)
  -> V.Vec s (SectorM audio engine proof r)
doBufferAlignment = asr \i a -> do
  Acc { prevSector, sectorLatch } <- get
  SceneI { world: { buffer } } <- ask
  when (prevSector /= Just i && isJust (extract sectorLatch))
    $ wag
        { buf:
            { onOff: OffOn
            , bufferOffset: (toNumber i) * bufferDuration buffer / toNumber (toInt' (Proxy :: _ s))
            }
        }
  pure a

sector
  :: forall i audio engine proof
   . Pos i
  => AudioInterpret audio engine
  => V.Vec i (SectorM audio engine proof Unit)
sector =
  base
    # rates -- set the current time
    # advanceIfSectorEnds -- move forward if the current time is out of this section
    # doBufferAlignment -- restart buffer if necessary
    # confirmSectorLatch -- assert the current sector
    # erase -- void everything

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
