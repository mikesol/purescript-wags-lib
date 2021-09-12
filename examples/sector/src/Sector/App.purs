module Sector.App where

import Prelude

import App.Markov (getObservations)
import Control.Apply (applySecond)
import Control.Comonad (class Comonad, class Extend, extract)
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Parallel (parallel, sequential)
import Control.Plus (empty)
import Control.Promise (toAffE)
import Data.Either (either)
import Data.Functor (voidLeft, voidRight)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.List ((:))
import Data.List as L
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (for_, traverse)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (class Lt, class Nat, class Pos, class Pred, D2, D32, D4, D8, d0, d1, d2, d3, d32, d4, d5, d6, d7, d8, toInt, toInt')
import Data.Vec ((+>))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Log
import FRP.Event (subscribe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Prim.Row (class Lacks)
import Record as R
import Run.Except (throw)
import Run.Reader (ask)
import Run.State (get, modify, put)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import WAGS.Change (change)
import WAGS.Control.Functions (start)
import WAGS.Control.Functions.Validated ((@|>), loop)
import WAGS.Control.Types (Frame0, Scene, WAG, Frame)
import WAGS.Graph.AudioUnit (OnOff(..), TGain, TPlayBuf, TSpeaker)
import WAGS.Interpret (class AudioInterpret, bufferDuration, close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.Lag (CfLag, makeLag)
import WAGS.Lib.Latch (CfLatch, makeLatchEq)
import WAGS.Lib.Rate (ARate, makeRate)
import WAGS.Lib.Run (RunWag, ShortCircuit(..), rChange, rModifyrRes, runWag)
import WAGS.Lib.Stream (cycle)
import WAGS.Lib.Vec (indexMod)
import WAGS.Patch (patch)
import WAGS.Run (Run, RunAudio, RunEngine, SceneI(..), run)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

type Time = Number
type MarkovChainLength = D32
type NObservations = D4
type GaussianDimensions = D2

type NSectors = D8 -- audio file sliced in 8

type SectorRuns audio engine =
  V.Vec NSectors (SectorM audio engine Unit)

type RateAcc r =
  ( rate :: ARate
  , rateHistory :: Maybe (CfLag Number)
  | r
  )

type SectorAcc r =
  ( sectorCount :: Int
  , sectorLatch :: CfLatch Int
  , sectorStartsAtUsingModifiedTime :: Number
  , sectorStartsAtUsingClockTime :: Number
  | r
  )

type PlayingNowAcc audio engine r = (playingNow :: Cofree Identity (SectorM audio engine Unit) | r)
type PrevSectorAcc r = (prevSector :: Maybe Int | r)

type World = { buffer :: BrowserAudioBuffer }

type AnalyserCbs = () :: Row Type

type Graph =
  ( speaker :: TSpeaker /\ { gain :: Unit }
  , gain :: TGain /\ { buf :: Unit }
  , buf :: TPlayBuf /\ {}
  )

type Env = SceneI Unit World AnalyserCbs

type Res = { currentSector :: Endo Function (Maybe Int) }

type SectorM audio engine a = RunWag Env (Acc audio engine) audio engine Res Graph a

newtype Acc audio engine =
  Acc { | RateAcc + PrevSectorAcc + SectorAcc + PlayingNowAcc audio engine () }

derive instance newtypeAcc :: Newtype (Acc audio engine) _

type CachedAcc audio engine r =
  ( cachedAcc :: Acc audio engine
  | r
  )

base
  :: forall i audio engine
   . Pos i
  => V.Vec i (SectorM audio engine { | CachedAcc audio engine () })
base = V.fill (const $ { cachedAcc: _ } <$> get)

erase
  :: forall a i audio engine
   . Nat i
  => V.Vec i (SectorM audio engine a)
  -> V.Vec i (SectorM audio engine Unit)
erase = asr (const $ const $ pure unit)

confirmSectorLatch
  :: forall a i audio engine
   . Nat i
  => V.Vec i (SectorM audio engine a)
  -> V.Vec i (SectorM audio engine a)
confirmSectorLatch = asr $ const $ voidLeft do
  -- this will force the sector latch to be Nothing on the next iteration
  Acc { sectorCount, sectorLatch } <- get
  modify $ over Acc $ _
    { sectorCount = sectorCount
    , sectorLatch = unwrapCofree sectorLatch sectorCount
    }

-- expand to add more stuff to res
writeRes
  :: forall a i audio engine
   . Nat i
  => V.Vec i (SectorM audio engine a)
  -> V.Vec i (SectorM audio engine a)
writeRes = asr (voidLeft <<< rModifyrRes <<< const <<< { currentSector: _ } <<< Endo <<< const <<< Just)

rates
  :: forall i r' audio engine
   . Nat i
  => Lacks "currentRate" r'
  => Lacks "modifiedTime" r'
  => V.Vec i (SectorM audio engine { | r' })
  -> V.Vec i (SectorM audio engine { | RateInfo r' })
rates = asr (const (useRate (maybe 1.0 extract)))

data SectorSect a = SectorStart a | SectorCont a

derive instance functorSectorSect :: Functor SectorSect

instance extendSectorSec :: Extend SectorSect where
  extend wab i@(SectorStart _) = SectorStart $ wab i
  extend wab i@(SectorCont _) = SectorCont $ wab i

instance comonadSectorSec :: Comonad SectorSect where
  extract (SectorStart a) = a
  extract (SectorCont a) = a

type RateInfo r = (modifiedTime :: Number, currentRate :: SectorSect Number | r)

latchToSectorSec :: forall a b. CfLatch a -> b -> SectorSect b
latchToSectorSec cf b = case extract cf of
  Nothing -> SectorCont b
  Just _ -> SectorStart b

useRate
  :: forall r' audio engine
   . Lacks "currentRate" r'
  => Lacks "modifiedTime" r'
  => (Maybe (SectorSect Number) -> Number)
  -> { | r' }
  -> SectorM audio engine { | RateInfo r' }
useRate rateF a = do
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
  :: forall a b s audio engine
   . Nat s
  => (Int -> a -> SectorM audio engine b)
  -> V.Vec s (SectorM audio engine a)
  -> V.Vec s (SectorM audio engine b)
asr f = mapWithIndex (\i a -> a >>= f i)

-- update sector run
usr
  :: forall a i s audio engine
   . Nat i
  => Lt i s
  => i
  -> (Int -> a -> SectorM audio engine a)
  -> V.Vec s (SectorM audio engine a)
  -> V.Vec s (SectorM audio engine a)
usr i m = V.modifyAt i (\a -> a >>= m (toInt i))

doAdvance
  :: forall s audio engine r
   . Nat s
  => Boolean
  -> Proxy s
  -> Int
  -> { | RateInfo + CachedAcc audio engine r }
  -> SectorM audio engine { | RateInfo + CachedAcc audio engine r }
doAdvance useModifiedTime px i a = do
  SceneI { world: { buffer }, time, headroomInSeconds } <- ask
  Acc { sectorStartsAtUsingModifiedTime, sectorStartsAtUsingClockTime } <- get
  let
    dur = bufferDuration buffer / toNumber (toInt' px)
    timeForCondition = if useModifiedTime then a.modifiedTime else time
    startTimeForCondition = if useModifiedTime then sectorStartsAtUsingModifiedTime else sectorStartsAtUsingClockTime
    condition = (headroomInSeconds * extract a.currentRate) + timeForCondition > startTimeForCondition + dur
  when condition do
    -- reset the state
    put a.cachedAcc
    Acc { playingNow, sectorCount, sectorLatch } <- get
    -- increment the sector count
    let
      newSectorCount = sectorCount + 1
      next = unwrap $ unwrapCofree playingNow
    modify $ over Acc $ _
      { sectorCount = newSectorCount
      , sectorLatch = unwrapCofree sectorLatch newSectorCount
      , playingNow = next
      , prevSector = Just i
      , sectorStartsAtUsingModifiedTime = a.modifiedTime
      , sectorStartsAtUsingClockTime = time
      }
    -- run next
    extract next
    -- throw to skip to the end
    throw ShortCircuit
  pure a

advanceIfSectorEnds
  :: forall s audio engine r
   . Nat s
  => Boolean
  -> V.Vec s (SectorM audio engine { | RateInfo + CachedAcc audio engine r })
  -> V.Vec s (SectorM audio engine { | RateInfo + CachedAcc audio engine r })
advanceIfSectorEnds useModifiedTime = asr (doAdvance useModifiedTime (Proxy :: _ s))

doBufferAlignment
  :: forall s audio engine r
   . Nat s
  => AudioInterpret audio engine
  => V.Vec s (SectorM audio engine r)
  -> V.Vec s (SectorM audio engine r)
doBufferAlignment = asr \i -> voidLeft do
  Acc { prevSector, sectorLatch } <- get
  SceneI { world: { buffer } } <- ask
  when (prevSector /= Just (i - 1) && isJust (extract sectorLatch))
    $ rChange
        { buf:
            { onOff: OffOn
            , bufferOffset: (toNumber i) * bufferDuration buffer / toNumber (toInt' (Proxy :: _ s))
            }
        }

-- the problem with applying the rate
-- is that the modified time won't be exactly equal to the position of the playhead in the buffer
-- this is the case for numerous reasons:
-- 1. we are scheduling in the future due to the easing algorithm
-- 2. there may be some form of interpolation on the rate, which means the integral will be different
-- 3. we may defer application of the rate slightly using ff, which will change the integral
--
-- in general, mixing the audioContext clock and the wags clock for sample accurate buffer operations
-- requires some sort of either tighter sync or error corrective
-- otherwise, there will be some type of overflow or underflow of the buffer
-- the accumulated error doesn't really matter when there's a lot of switching but it gets amplified
-- when the buffer doesn't change, which leads to one large error when a change does happen
applyRate
  :: forall s audio engine r
   . Nat s
  => AudioInterpret audio engine
  => V.Vec s (SectorM audio engine { currentRate :: SectorSect Number | r })
  -> V.Vec s (SectorM audio engine { currentRate :: SectorSect Number | r })
applyRate = asr
  $ const
  $ applySecond <$> rChange <<< { buf: _ } <<< { playbackRate: _ } <<< extract <<< _.currentRate <*> pure

sector
  :: forall audio engine
   . AudioInterpret audio engine
  => V.Vec NSectors (SectorM audio engine Unit)
sector =
  base
    # rates -- set the current time
    # advanceIfSectorEnds true -- move forward if the current time is out of this section
    # doBufferAlignment -- restart buffer if necessary
    # applyRate -- apply the rate
    # confirmSectorLatch -- assert the current sector
    # writeRes -- write the residual
    # erase -- void everything

ix :: forall i s a. Nat i => Lt i s => i -> V.Vec s a -> a
ix i v = V.index v i

order
  :: forall audio engine
   . AudioInterpret audio engine
  => Cofree Identity (SectorM audio engine Unit)
order = cycle
  $ map ((#) sector)
      ( ix d0
          :| ix d2
            : ix d1
            : ix d5
            : ix d4
            : ix d5
            : ix d3
            : ix d5
            : ix d6
            : ix d5
            : ix d7
            : Nil
      )

runScene
  :: forall audio engine proof
   . AudioInterpret audio engine
  => RunWag Env (Acc audio engine) audio engine Res Graph Unit
  -> WAG audio engine proof Res Graph (Acc audio engine)
  -> Frame Env audio engine proof Res Graph (Acc audio engine)
runScene = runWag

loopScene
  :: forall audio engine proof
   . AudioInterpret audio engine
  => WAG audio engine proof Res Graph (Acc audio engine)
  -> Frame Env audio engine proof Res Graph (Acc audio engine)
loopScene = runScene <<< extract <<< _.playingNow <<< unwrap <<< extract <*> identity

firstFrame
  :: forall audio engine
   . AudioInterpret audio engine
  => Acc audio engine
  -> Frame Env audio engine Frame0 Res Graph (Acc audio engine)
firstFrame acc env@(SceneI { world: { buffer } }) =
  start
    # patch { microphone: empty }
    # voidRight { gain: 1.0, buf: buffer }
    # change
    # voidRight acc
    # flip loopScene env

piece :: Acc RunAudio RunEngine -> Scene Env RunAudio RunEngine Frame0 Res
piece acc = firstFrame acc @|> loop loopScene

type MarkovOptions =
  { "A" :: V.Vec NSectors (V.Vec NSectors Number)
  , "Sigma" :: V.Vec NSectors (V.Vec GaussianDimensions (V.Vec GaussianDimensions Number))
  , mu :: V.Vec NSectors (V.Vec GaussianDimensions Number)
  , pi :: V.Vec NSectors Number
  , observations :: NObservations
  , time :: MarkovChainLength
  , states :: NSectors
  , dimensions :: GaussianDimensions
  }

markovOptions :: MarkovOptions
markovOptions =
  { observations: d4
  , time: d32
  , states: d8
  , dimensions: d2
  , pi: (0.15 +> 0.20 +> 0.65 +> 0.15 +> 0.20 +> 0.65 +> 0.15 +> 0.20 +> V.empty)
  , "A":
      ( (0.55 +> 0.15 +> 0.30 +> 0.55 +> 0.15 +> 0.30 +> 0.55 +> 0.15 +> V.empty)
          +> (0.45 +> 0.45 +> 0.10 +> 0.45 +> 0.45 +> 0.10 +> 0.45 +> 0.45 +> V.empty)
          +> (0.15 +> 0.20 +> 0.65 +> 0.15 +> 0.20 +> 0.65 +> 0.15 +> 0.20 +> V.empty)
          +> (0.55 +> 0.15 +> 0.30 +> 0.55 +> 0.15 +> 0.30 +> 0.55 +> 0.15 +> V.empty)
          +> (0.45 +> 0.45 +> 0.10 +> 0.45 +> 0.45 +> 0.10 +> 0.45 +> 0.45 +> V.empty)
          +> (0.15 +> 0.20 +> 0.65 +> 0.15 +> 0.20 +> 0.65 +> 0.15 +> 0.20 +> V.empty)
          +> (0.55 +> 0.15 +> 0.30 +> 0.55 +> 0.15 +> 0.30 +> 0.55 +> 0.15 +> V.empty)
          +> (0.45 +> 0.45 +> 0.10 +> 0.45 +> 0.45 +> 0.10 +> 0.45 +> 0.45 +> V.empty)
          +> V.empty
      )
  , mu:
      ( (-7.0 +> -8.0 +> V.empty)
          +> (-1.5 +> 3.7 +> V.empty)
          +> (-1.7 +> 1.2 +> V.empty)
          +> (-7.0 +> -8.0 +> V.empty)
          +> (-1.5 +> 3.7 +> V.empty)
          +> (-1.7 +> 1.2 +> V.empty)
          +> (-7.0 +> -8.0 +> V.empty)
          +> (-1.5 +> 3.7 +> V.empty)
          +> V.empty
      )
  , "Sigma":
      ( ( (0.12 +> -0.01 +> V.empty)
            +> (-0.01 +> 0.50 +> V.empty)
            +> V.empty
        )
          +>
            ( (0.21 +> 0.05 +> V.empty)
                +> (0.05 +> 0.03 +> V.empty)
                +> V.empty
            )
          +>
            ( (0.37 +> 0.35 +> V.empty)
                +> (0.35 +> 0.44 +> V.empty)
                +> V.empty
            )
          +>
            ( (0.12 +> -0.01 +> V.empty)
                +> (-0.01 +> 0.50 +> V.empty)
                +> V.empty
            )
          +>
            ( (0.21 +> 0.05 +> V.empty)
                +> (0.05 +> 0.03 +> V.empty)
                +> V.empty
            )
          +>
            ( (0.37 +> 0.35 +> V.empty)
                +> (0.35 +> 0.44 +> V.empty)
                +> V.empty
            )
          +>
            ( (0.12 +> -0.01 +> V.empty)
                +> (-0.01 +> 0.50 +> V.empty)
                +> V.empty
            )
          +>
            ( (0.21 +> 0.05 +> V.empty)
                +> (0.05 +> 0.03 +> V.empty)
                +> V.empty
            )
          +> V.empty
      )
  }

--------------------------------
---- halogen app below

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = mkCofree initialTime \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

type State
  =
  { unsubscribe :: Effect Unit
  , unsubscribeFromHalogen :: Maybe H.SubscriptionId
  , audioCtx :: Maybe AudioContext
  , currentSector :: Maybe Int
  , tfMarkov :: Maybe (V.Vec NObservations (V.Vec MarkovChainLength Int))
  }

data Action
  = StartAudio
  | StopAudio
  | ReportRes Res
  | Initialize

component :: forall query input output m. MonadEffect m => MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  , currentSector: Nothing
  , unsubscribeFromHalogen: Nothing
  , tfMarkov: Nothing
  }

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

render :: forall m. State -> H.ComponentHTML Action () m
render { tfMarkov, currentSector } = HH.div [ classes [ "w-screen", "h-screen" ] ]
  [ HH.div [ classes [ "flex", "flex-col", "w-full", "h-full" ] ]
      [ HH.div [ classes [ "flex-grow" ] ] []
      , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
          [ HH.div [ classes [ "flex-grow" ] ]
              []
          , HH.div [ classes [ "flex", "flex-col" ] ]
              case tfMarkov of
                Nothing ->
                  [ HH.h3 [ classes [ "text-center", "text-3xl", "font-bold" ] ]
                      [ HH.text "Training hidden Markov model using tfjs..." ]
                  ]
                Just _ ->
                  ( [ HH.h1 [ classes [ "text-center", "text-3xl", "font-bold" ] ]
                        [ HH.text "SECTOR-esque" ]
                    , HH.button
                        [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> StartAudio ]
                        [ HH.text "Start audio" ]
                    , HH.button
                        [ classes [ "text-2xl", "m-5", "bg-pink-500", "p-3", "rounded-lg", "text-white", "hover:bg-pink-400" ], HE.onClick \_ -> StopAudio ]
                        [ HH.text "Stop audio" ]
                    ] <> maybe []
                      ( \cs ->
                          [ HH.h3 [ classes [ "text-center", "text-3xl", "font-bold" ] ]
                              [ HH.text $ "Current Sector: " <> show cs ]
                          ]
                      )
                      currentSector
                  )
          , HH.div [ classes [ "flex-grow" ] ] []
          ]
      , HH.div [ classes [ "flex-grow" ] ] []
      ]
  ]

vecToNonEmptyList :: forall n nm1. Pred n nm1 => V.Vec n ~> NonEmpty List
vecToNonEmptyList v = uc.head :| (L.fromFoldable $ V.toArray $ uc.tail)
  where
  uc = V.uncons v

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    Log.info "starting training"
    obs <- H.liftAff $ toAffE $ getObservations markovOptions
    Log.info "finished training"
    H.modify_ _ { tfMarkov = Just obs }
  ReportRes res -> do
    H.modify_ _ { currentSector = unwrap res.currentSector Nothing }
    pure unit
  StartAudio -> do
    handleAction StopAudio
    { emitter, listener } <- H.liftEffect HS.create
    unsubscribeFromHalogen <- H.subscribe emitter
    { tfMarkov } <- H.get
    audioCtx <- H.liftEffect context
    unitCache <- H.liftEffect makeUnitCache
    -- in case we want several, we use a traversable
    -- to be able to grab them all later
    let sound' = Identity "https://freesound.org/data/previews/320/320873_527080-hq.mp3"
    sound <- H.liftAff $ sequential $ traverse (parallel <<< toAffE <<< decodeAudioDataFromUri audioCtx) sound'
    let
      ffiAudio = defaultFFIAudio audioCtx unitCache
      acc = Acc
        { playingNow: maybe order (cycle <<< map (indexMod sector) <<< vecToNonEmptyList <<< flip V.index d0) tfMarkov
        , sectorCount: 0
        , sectorLatch: makeLatchEq 0
        , prevSector: Nothing
        , rate: makeRate { prevTime: 0.0, startsAt: 0.0 }
        , rateHistory: Nothing
        , sectorStartsAtUsingModifiedTime: 0.0
        , sectorStartsAtUsingClockTime: 0.0
        }
    unsubscribe <-
      H.liftEffect
        $ subscribe
            ( run (pure unit)
                (pure { buffer: (unwrap sound) })
                { easingAlgorithm }
                ffiAudio
                (piece acc)
            )
            (\({ res } :: Run Res ()) -> HS.notify listener (ReportRes res))
    H.modify_ _
      { unsubscribe = unsubscribe
      , audioCtx = Just audioCtx
      , unsubscribeFromHalogen = Just unsubscribeFromHalogen
      }
  StopAudio -> do
    { unsubscribe, unsubscribeFromHalogen, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ unsubscribeFromHalogen H.unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing, unsubscribeFromHalogen = Nothing }
