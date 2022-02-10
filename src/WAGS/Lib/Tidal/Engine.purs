module WAGS.Lib.Tidal.Engine where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Compactable (compact)
import Data.Either (Either)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Int (toNumber)
import Data.Lens (_1, over)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num (class Pos)
import Data.Variant.Either (either)
import Data.Variant.Maybe as VM
import Data.Vec ((+>))
import Data.Vec as V
import Effect.Aff (Aff)
import Effect.Random (randomInt)
import FRP.Behavior (Behavior, behavior)
import FRP.Event (Event, makeEvent, subscribe)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Lacks)
import Prim.RowList (class RowToList)
import Random.LCG (mkSeed)
import Record as Record
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Create.Optionals (analyser, gain, loopBuf, playBuf, recorder, speaker, subgraph, tumult)
import WAGS.Graph.AudioUnit (_off, _offOn, _on)
import WAGS.Graph.Parameter (ff)
import WAGS.Interpret (bufferDuration)
import WAGS.Lib.BufferPool (AScoredBufferPool, Buffy(..), CfScoredBufferPool, makeScoredBufferPool)
import WAGS.Lib.Learn (FullSceneBuilder, AnalysersCb, usingcr)
import WAGS.Lib.Tidal.Download (downloadSilence, initialBuffers)
import WAGS.Lib.Tidal.FX (calm)
import WAGS.Lib.Tidal.Types (class HomogenousToVec, Acc, BufferUrl, CycleInfo, DroneNote(..), Globals, IsFresh, NBuf, Next, NextCycle, RBuf, Sample(..), SampleCache, TheFuture, TidalRes, TimeIs(..), UnsampledTimeIs(..), ExternalControl, h2v')
import WAGS.Run (SceneI(..))
import WAGS.Subgraph (SubSceneSig)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer, MediaRecorderCb)

globalFF = 0.03 :: Number
globalSize = 5 :: Int

acc
  :: forall event
   . Monoid event
  => SceneI
       { interactivity :: event
       }
       { buffers :: SampleCache
       , entropy :: Int
       , externalControl :: ExternalControl
       , rcdr :: MediaRecorderCb
       , theFuture ::
           IsFresh event
           -> { clockTime :: Number }
           -> TheFuture event
       , silence :: BrowserAudioBuffer
       }
       AnalysersCb
  -> Acc event
acc (SceneI { time: time', trigger, world: { theFuture: theFutureFromWorld } }) =
  { buffers: fromHomogeneous
      $ map (emptyPool <<< _.next <<< unwrap)
      $ homogeneous ewf
  , justInCaseTheLastEvent
  }
  where
  justInCaseTheLastEvent = { isFresh: false, value: mempty }
  event = maybe justInCaseTheLastEvent ({ isFresh: true, value: _ } <<< _.interactivity) trigger
  theFuture = theFutureFromWorld event { clockTime: time' }
  ewf = let { earth, wind, fire, lambert, hendricks, ross } = unwrap theFuture in { earth, wind, fire, lambert, hendricks, ross }

sampleF
  :: Sample
  -> Boolean
  -> BrowserAudioBuffer
  -> Object
       { buffer ::
           { backwards :: BrowserAudioBuffer
           , forward :: BrowserAudioBuffer
           }
       , url :: BufferUrl
       }
  -> BrowserAudioBuffer
sampleF sample forward silence =
  maybe silence
    (if forward then _.buffer.forward else _.buffer.backwards) <<< Object.lookup (unwrap sample)

silenceSample = Sample "intentionalSilenceForInternalUseOnly" :: Sample

internal0
  :: forall event
   . SubSceneSig "singleton" ()
       { buf :: Maybe (Buffy (RBuf event))
       , seed :: Int
       , event :: IsFresh event
       , silence :: BrowserAudioBuffer
       , externalControl :: ExternalControl
       , buffers :: SampleCache
       , time :: Number
       }
internal0 = (const { initialEntropies: { volume: 0.5, rate: 0.5, bufferOffset: 0.5, sample: 0.5, forward: 0.5 } }) # SG.loopUsingScene
  \{ time
   , silence
   , buffers
   , externalControl
   , event
   , seed
   , buf: buf'
   }
   { initialEntropies: initialEntropiesOld } -> case buf' of
    Nothing ->
      { control: { initialEntropies: initialEntropiesOld }
      , scene: { singleton: gain 0.0 (playBuf { onOff: _off } silence) }
      }
    Just
      ( Buffy
          { starting
          , startTime
          , rest:
              { sampleFoT
              , forwardFoT
              , rateFoT
              , bufferOffsetFoT
              , volumeFoT
              , duration
              , cycleStartsAt
              , currentCycle
              , littleCycleDuration
              , bigCycleDuration
              }
          }
      ) -> { newSeed: mkSeed seed, size: globalSize } # evalGen do
      volumeEntropy <- arbitrary
      rateEntropy <- arbitrary
      offsetEntropy <- arbitrary
      sampleEntropy <- arbitrary
      forwardEntropy <- arbitrary
      initialEntropies <-
        if starting then { volume: _, rate: _, sample: _, bufferOffset: _, forward: _ }
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> arbitrary
        else pure initialEntropiesOld
      let
        sampleTime = time - startTime
        bigCycleTime = time - cycleStartsAt
        littleCycleTime = time - (cycleStartsAt + (toNumber currentCycle * littleCycleDuration))
        normalizedBigCycleTime = bigCycleTime / bigCycleDuration
        normalizedLittleCycleTime = littleCycleTime / littleCycleDuration
        normalizedClockTime = 0.0 -- cuz it's infinite :-P
        thisIsUnsampledTime initialEntropy' entropy' =
          UnsampledTimeIs
            { bigCycleTime
            , littleCycleTime
            , event
            , externalControl
            , clockTime: time
            , normalizedClockTime
            , normalizedBigCycleTime
            , normalizedLittleCycleTime
            , littleCycleDuration
            , bigCycleDuration
            , initialEntropy: initialEntropy'
            , entropy: entropy'
            }
        forward = forwardFoT (thisIsUnsampledTime initialEntropies.forward forwardEntropy)
        buf = sampleF (either ((#) (thisIsUnsampledTime initialEntropies.sample sampleEntropy)) identity sampleFoT)
          forward
          silence
          buffers
        normalizedSampleTime = sampleTime / duration
        thisIsTime initialEntropy' entropy' =
          TimeIs
            { sampleTime
            , event
            , externalControl
            , bigCycleTime
            , littleCycleTime
            , clockTime: time
            , normalizedClockTime
            , normalizedSampleTime
            , normalizedBigCycleTime
            , normalizedLittleCycleTime
            , littleCycleDuration
            , bigCycleDuration
            , bufferDuration: bufferDuration buf
            , initialEntropy: initialEntropy'
            , entropy: entropy'
            }
        vol = ff globalFF $ pure $ volumeFoT (thisIsTime initialEntropies.volume volumeEntropy)
      pure
        { control: { initialEntropies }
        , scene:
            { singleton:
                gain
                  vol
                  ( playBuf
                      { onOff:
                          ff globalFF
                            $
                              if starting then
                                ff (max 0.0 (startTime - time)) (pure _offOn)
                              else
                                pure _on
                      , bufferOffset: bufferOffsetFoT (thisIsTime initialEntropies.bufferOffset offsetEntropy)
                      , playbackRate: ff globalFF $ pure $ rateFoT (thisIsTime initialEntropies.rate rateEntropy)
                      }
                      buf
                  )
            }
        }

internal1
  :: forall event
   . SubSceneSig "singleton" ()
       { fng :: { future :: CfScoredBufferPool (Next event) NBuf (RBuf event), globals :: Globals event, seed :: Int }
       , externalControl :: ExternalControl
       , buffers :: SampleCache
       , event :: IsFresh event
       , silence :: BrowserAudioBuffer
       , time :: Number
       }
internal1 = (const emptyLags) # SG.loopUsingScene
  \{ time, buffers, event, externalControl, silence, fng: { future, globals, seed } }
   { timeLag
   , volumeLag
   } -> { newSeed: mkSeed seed, size: globalSize } # evalGen do
    volumeEntropy <- arbitrary
    tumultEntropy <- arbitrary
    seeds <- sequence (V.fill (const arbitrary))
    let
      prevTime = extract timeLag
      prevVolume = extract volumeLag
      thisIsTime = wrap
        { clockTime: time
        , adulteratedClockTime: time
        , externalControl
        , event
        , entropy: volumeEntropy
        }
      volumeNow = (unwrap globals).gain $ wrap { timeWas: prevTime, timeIs: thisIsTime, valWas: prevVolume }
    pure
      { control:
          { volumeLag: unwrapCofree volumeLag volumeNow
          , timeLag: unwrapCofree timeLag thisIsTime
          }
      , scene:
          { singleton: gain (ff globalFF $ pure $ volumeNow)
              { tmlt: tumult
                  ( (unwrap globals).fx
                      ( wrap
                          { clockTime: time
                          , adulteratedClockTime: time
                          , event
                          , externalControl
                          , buffers
                          , silence
                          , entropy: tumultEntropy
                          }
                      )
                  )
                  { voice: subgraph
                      (V.zipWithE (/\) seeds (extract future))
                      (const $ const $ internal0)
                      (const $ uncurry { time, buffers, event, externalControl, silence, seed: _, buf: _ })
                      {}
                  }
              }
          }
      }
  where
  emptyLags =
    { timeLag: makeLag
    , volumeLag: makeLag
    }

type CfLag a
  = Cofree ((->) a) (VM.Maybe a)

makeLag :: forall a. CfLag a
makeLag = VM.nothing :< go
  where
  go old = VM.just old :< go

droneSg
  :: forall event
   . SubSceneSig "singleton" ()
       { buf :: VM.Maybe (DroneNote event)
       , silence :: BrowserAudioBuffer
       , buffers :: SampleCache
       , event :: IsFresh event
       , externalControl :: ExternalControl
       , seed :: Int
       , time :: Number
       }
droneSg = (const emptyLags)
  # SG.loopUsingScene
      \{ time: time'
       , silence
       , buffers
       , event
       , externalControl
       , seed
       , buf: buf'
       }
       { timeLag, rateLag, volumeLag, loopStartLag, loopEndLag } ->
        buf' # VM.maybe'
          ( \_ ->
              { control: emptyLags
              , scene: { singleton: gain 0.0 { dronetmlt: tumult calm { voice: loopBuf { onOff: _off } silence } } }
              }
          )
          ( \( DroneNote
                 { sample
                 , forward
                 , volumeFoT
                 , loopStartFoT
                 , loopEndFoT
                 , rateFoT
                 , tumultFoT
                 }
             ) -> { newSeed: mkSeed seed, size: globalSize } # evalGen do
              volumeEntropy <- arbitrary
              loopStartEntropy <- arbitrary
              loopEndEntropy <- arbitrary
              rateEntropy <- arbitrary
              tumultEntropy <- arbitrary
              let
                thisIsTime entropy' =
                  { clockTime: time'
                  , adulteratedClockTime: time'
                  , externalControl
                  , event
                  , entropy: entropy'
                  }
                buf = sampleF sample forward silence buffers
                prevTime = extract timeLag
                prevVolume = extract volumeLag
                volumeNow = volumeFoT $ wrap
                  { timeWas: prevTime
                  , timeIs: wrap $ thisIsTime volumeEntropy
                  , valWas: prevVolume
                  }
                prevLoopStart = extract loopStartLag
                loopStartNow = loopStartFoT $ wrap
                  { timeWas: prevTime
                  , timeIs: wrap $ thisIsTime loopStartEntropy
                  , valWas: prevLoopStart
                  }
                prevLoopEnd = extract loopEndLag
                loopEndNow = loopEndFoT $ wrap
                  { timeWas: prevTime
                  , timeIs: wrap $ thisIsTime loopEndEntropy
                  , valWas: prevLoopEnd
                  }
                prevRate = extract rateLag
                rateNow = rateFoT $ wrap
                  { timeWas: prevTime
                  , timeIs: wrap $ thisIsTime rateEntropy
                  , valWas: prevRate
                  }
                vol = ff globalFF $ pure $ volumeNow
              pure
                { control:
                    { rateLag: unwrapCofree rateLag rateNow
                    , volumeLag: unwrapCofree volumeLag volumeNow
                    , loopStartLag: unwrapCofree loopStartLag loopStartNow
                    , loopEndLag: unwrapCofree loopEndLag loopEndNow
                    -- for now, the previous time gets no entropy
                    -- we can refactor to change this later (slightly more computationally expensive)
                    , timeLag: unwrapCofree timeLag (wrap $ thisIsTime 0.0)
                    }
                , scene:
                    { singleton:
                        gain vol
                          { dronetmlt: tumult (tumultFoT (wrap $ Record.union { buffers, silence } (thisIsTime tumultEntropy)))
                              { voice: loopBuf
                                  { onOff: ff globalFF $ pure _on
                                  , loopStart: loopStartNow
                                  , loopEnd: loopEndNow
                                  , playbackRate: ff globalFF $ pure $ rateNow
                                  }
                                  buf
                              }
                          }
                    }
                }
          )
  where
  emptyLags =
    { timeLag: makeLag
    , rateLag: makeLag
    , volumeLag: makeLag
    , loopStartLag: makeLag
    , loopEndLag: makeLag
    }

thePresent
  :: forall trigger world event
   . Lacks "theFuture" world
  => Behavior (IsFresh event -> { clockTime :: Number } -> TheFuture event)
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
  -> AudioContext /\ Aff
       ( Event
           { | trigger
           } /\ Behavior
           { theFuture ::
               IsFresh event
               -> { clockTime :: Number }
               -> TheFuture event
           | world
           }
       )
thePresent behv (ac /\ aff) = ac /\ do
  trigger /\ world <- aff
  let newWorld = Record.insert (Proxy :: _ "theFuture") <$> behv <*> world
  pure (trigger /\ newWorld)

interactivity
  :: forall trigger world event
   . Lacks "interactivity" trigger
  => Event event
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
  -> AudioContext /\ Aff (Event { interactivity :: event | trigger } /\ Behavior { | world })
interactivity ev = (map <<< map) (over _1 (\e -> Record.insert (Proxy :: _ "interactivity") <$> ev <*> e))

h2v :: forall r rl n a. RowToList r rl => HomogenousToVec rl r n a => { | r } -> V.Vec n a
h2v = h2v' (Proxy :: _ rl)

emptyPool :: forall event n. Pos n => NextCycle event -> AScoredBufferPool (Next event) n (RBuf event)
emptyPool nc = makeScoredBufferPool
  { startsAt: 0.0
  , noteStream: \{ time, headroomInSeconds } ->
      ( ( _.func $ unwrap
            $ nc
        )
          { currentCount: 0.0
          , prevCycleEnded: 0.0
          , time
          , headroomInSeconds
          }
      ) # map \{ startsAfter, rest } ->
        { startsAfter
        , rest:
            { rest: const rest
            , duration: const $ const $ const $ Just rest.duration
            }
        }
  }

ntropi :: Behavior Int
ntropi =
  behavior \e ->
    makeEvent \f ->
      e `subscribe` \a2b -> (a2b <$> randomInt bottom top) >>= f

addEntropy
  :: forall trigger world
   . Lacks "entropy" world
  => AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { entropy :: Int | world })
addEntropy (ac /\ aff) = ac /\ do
  trigger /\ world <- aff
  pure (trigger /\ (Record.insert (Proxy :: _ "entropy") <$> ntropi <*> world))

makeExternalCtrl
  :: forall trigger world
   . Lacks "externalControl" world
  => Behavior ExternalControl
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { externalControl :: ExternalControl | world })
makeExternalCtrl externalControl (ac /\ aff) = ac /\ do
  trigger /\ world <- aff
  pure (trigger /\ (Record.insert (Proxy :: _ "externalControl") <$> externalControl <*> world))

makeRecorder
  :: forall trigger world
   . Lacks "rcdr" world
  => Behavior MediaRecorderCb
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { rcdr :: MediaRecorderCb | world })
makeRecorder recorder (ac /\ aff) = ac /\ do
  trigger /\ world <- aff
  pure (trigger /\ (Record.insert (Proxy :: _ "rcdr") <$> recorder <*> world))

extractCycleInfo :: forall event. RBuf event -> CycleInfo
extractCycleInfo
  { cycleStartsAt
  , bigCycleDuration
  , littleCycleDuration
  , currentCycle
  , bigStartsAt
  , duration
  , littleStartsAt
  } =
  { cycleStartsAt
  , bigCycleDuration
  , littleCycleDuration
  , currentCycle
  , bigStartsAt
  , littleStartsAt
  , duration
  }

engine
  :: forall event
   . Monoid event
  => Event event
  -> Behavior (IsFresh event -> { clockTime :: Number } -> TheFuture event)
  -> Behavior ExternalControl
  -> Behavior MediaRecorderCb
  -> Either (Behavior SampleCache) (AudioContext -> Aff SampleCache)
  -> FullSceneBuilder
       ( interactivity :: event
       )
       ( buffers :: SampleCache
       , rcdr :: MediaRecorderCb
       , externalControl :: ExternalControl
       , entropy :: Int
       , theFuture ::
           IsFresh event
           -> { clockTime :: Number }
           -> TheFuture event
       , silence :: BrowserAudioBuffer
       )
       TidalRes
engine dmo evt ctrl mrc bsc = usingcr
  ( interactivity dmo
      <<< thePresent evt
      <<< makeExternalCtrl ctrl
      <<< makeRecorder mrc
      <<< initialBuffers bsc
      <<< addEntropy
      <<< downloadSilence
  )
  acc
  \( SceneI
       { time: time'
       , headroomInSeconds
       , trigger
       , world:
           { buffers
           , externalControl
           , theFuture: theFutureFromWorld
           , silence
           , rcdr
           , entropy: entropy'
           }
       , analyserCallbacks: { myAnalyser }
       }
   )
   control ->
    let
      event = maybe control.justInCaseTheLastEvent ({ isFresh: true, value: _ } <<< _.interactivity) trigger
      theFuture = theFutureFromWorld event { clockTime: time' }
      ewf = let { earth, wind, fire, lambert, hendricks, ross } = unwrap theFuture in { earth, wind, fire, lambert, hendricks, ross }
      -- to actualize is the next cycle, which could be the current cycle
      toActualize =
        map
          ( { time: time'
            , headroomInSeconds
            , input: _
            } <<< { next: _ } <<< _.next <<< unwrap
          )
          $ homogeneous ewf
      myGlobals = map (_.globals <<< unwrap) $ homogeneous ewf
      -- buffers consume the next input
      -- what's returned is a bank of buffers
      actualized = homogeneous control.buffers <*> toActualize
      forTemplate = h2v (fromHomogeneous $ ({ future: _, globals: _ } <$> actualized <*> myGlobals))
      -- to populate the monoid, we "cheat" and take the most recent value
      -- we can anchor off of this
      -- this is not very performant - it would be better to have more of this
      -- information at the top level so that we did not need to do an extra loop
      -- to extract it
      res = fromHomogeneous
        $ map
            ( map (extractCycleInfo <<< _.rest <<< unwrap)
                <<< compact
                <<< V.toArray
                <<< extract
            )
            actualized
    in
      { res
      , control: { buffers: fromHomogeneous (map unwrapCofree actualized), justInCaseTheLastEvent: event }
      , scene: { newSeed: mkSeed entropy', size: globalSize } # evalGen do
          seeds <- sequence (V.fill (const $ map { seed: _ } arbitrary))
          seedsDrone <- sequence (V.fill (const arbitrary))
          let vec = V.zipWithE Record.union forTemplate seeds
          pure $ speaker
            { recorder: recorder rcdr
                { analyse: analyser myAnalyser
                    { analysed: gain 1.0
                        { subs: subgraph vec (const $ const $ internal1)
                            ( const $
                                { time: time'
                                , buffers
                                , event
                                , externalControl
                                , silence
                                , fng: _
                                }
                            )
                            {}
                        , drones: subgraph
                            (V.zipWithE (/\) seedsDrone ((unwrap theFuture).air +> (unwrap theFuture).heart +> V.empty))
                            (const $ const $ droneSg)
                            ( const $ uncurry
                                { time: time'
                                , buffers
                                , silence
                                , event
                                , externalControl
                                , seed: _
                                , buf: _
                                }
                            )
                            {}
                        }
                    }
                }
            }
      }
