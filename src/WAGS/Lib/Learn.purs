module WAGS.Lib.Learn where

import Prelude

import CSS (rgba)
import CSS as CSS
import CSS.Common as CSSC
import CSS.Cursor as CSSCur
import CSS.Flexbox as CSSF
import CSS.TextAlign as CSST
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, hoistCofree, (:<))
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Promise (toAffE)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Identity (Identity(..))
import Data.Lens (_1, over, set)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Symbol (class IsSymbol)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (D4)
import Effect (Effect)
import Effect.Aff (Aff, joinFiber, launchAff, launchAff_, makeAff, try)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import FRP.Behavior (Behavior)
import FRP.Event (Event, subscribe)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Change (class Change)
import WAGS.Control.Functions.Graph (loopUsingScene, loopUsingSceneWithRes)
import WAGS.Control.Functions.Subgraph as SG
import WAGS.Control.Types (Frame0, Scene, SubScene)
import WAGS.Create (class Create)
import WAGS.Create.Optionals (constant, gain, playBuf, sinOsc, speaker, subgraph)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Paramable (paramize)
import WAGS.Graph.Parameter (class Timed, AudioOnOff, AudioParameter, ff, lensParam)
import WAGS.Interpret (class AudioInterpret, close, constant0Hack, context, contextResume, contextState, decodeAudioDataFromUri, makeFFIAudioSnapshot)
import WAGS.Lib.BufferPool (AScoredBufferPool, Buffy(..), makeBufferPoolWithAnchor)
import WAGS.Lib.Learn.Duration (Duration(..), Rest(..), longest)
import WAGS.Lib.Learn.FofT (class FofT, toFofT)
import WAGS.Lib.Learn.Note (Note(..), NoteOrRest, Sequenced(..), noteFromDefaults_, noteStreamToSequence, seq)
import WAGS.Lib.Learn.Pitch (Pitch(..))
import WAGS.Lib.Learn.Volume (Volume(..))
import WAGS.Lib.Piecewise (APFofT, makePiecewise)
import WAGS.Lib.Score (makeScore)
import WAGS.Lib.Stream (deadEnd)
import WAGS.Run (BehavingRun, RunAudio, RunEngine, BehavingScene(..), run)
import WAGS.Validation (class GraphIsRenderable)
import WAGS.WebAPI (AnalyserNode, AnalyserNodeCb, AudioContext, BrowserAudioBuffer)

-- global fast forward to avoid clicks and pops
gff :: forall timed. Timed timed => timed -> timed
gff = ff 0.03

type Analysers
  = (myAnalyser :: Maybe AnalyserNode)

type AnalysersCb
  = (myAnalyser :: AnalyserNodeCb)

class ToScene a res | a -> res where
  toScene :: a -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun res Analysers) }

newtype FullScene trigger world res
  = FullScene
  { triggerWorld :: AudioContext -> Aff (Event { | trigger } /\ Behavior { | world })
  , piece :: Scene (BehavingScene { | trigger } { | world } AnalysersCb) RunAudio RunEngine Frame0 res
  }

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = initialTime :< \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

defaultTriggerWorld :: AudioContext -> Aff (Event {} /\ Behavior {})
defaultTriggerWorld = pure (pure (pure {} /\ pure {}))

nothing :: Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
nothing =
  makeFullScene
    $ FullScene
        { triggerWorld: defaultTriggerWorld
        , piece: loopUsingScene (const $ const $ { control: unit, scene: speaker { c: constant 0.0 } }) mempty
        }

makePitch
  :: forall pitchF
   . FofT pitchF
  => Pitch pitchF
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makePitch = makeNote <<< (\p -> noteFromDefaults_ (_ { duration = longest, pitch = p }))

instance toScenePitch :: FofT pitchF => ToScene (Pitch pitchF) Unit where
  toScene = makePitch

makeNote
  :: forall pitchF durationF volumeF
   . FofT pitchF
  => FofT durationF
  => FofT volumeF
  => Note pitchF durationF volumeF
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeNote (Note n) =
  makeFunctionOfTimeLoop \time ->
    let
      duration = toFofT $ unwrap n.duration

      volume = toFofT $ unwrap n.volume

      pitch = toFofT $ unwrap n.pitch
    in
      speaker
        { gain:
            gain (gff $ paramize (if time < (duration time) then (volume time) else 0.0))
              (sinOsc (gff $ paramize $ (pitch time)))
        }

instance toSceneNote ::
  ( FofT volumeF
  , FofT durationF
  , FofT pitchF
  ) =>
  ToScene (Note volumeF durationF pitchF) Unit where
  toScene = makeNote

makeArrayPitch
  :: forall pitchF
   . FofT pitchF
  => Array (Pitch pitchF)
  -> Aff
       { audioCtx :: AudioContext
       , event :: Event (BehavingRun Unit Analysers)
       }
makeArrayPitch =
  A.uncons
    >>> case _ of
      Nothing -> nothing
      Just { head, tail } -> makeNonEmptyArrayPitch (head :| tail)

instance toSceneArrayPitch :: FofT pitchF => ToScene (Array (Pitch pitchF)) Unit where
  toScene = makeArrayPitch

makeArrayNote
  :: forall pitchF volumeF
   . FofT pitchF
  => FofT volumeF
  => Array (Note pitchF Identity volumeF)
  -> Aff
       { audioCtx :: AudioContext
       , event :: Event (BehavingRun Unit Analysers)
       }
makeArrayNote =
  A.uncons
    >>> case _ of
      Nothing -> nothing
      Just { head, tail } -> makeNonEmptyArrayNote (head :| tail)

instance toSceneArrayNote ::
  ( FofT pitchF
  , FofT volumeF
  ) =>
  ToScene (Array (Note pitchF Identity volumeF)) Unit where
  toScene = makeArrayNote

makeArrayNoteOrRest
  :: forall pitchF volumeF
   . FofT pitchF
  => FofT volumeF
  => Array (NoteOrRest pitchF Identity volumeF Identity)
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeArrayNoteOrRest =
  A.uncons
    >>> case _ of
      Nothing -> nothing
      Just { head, tail } -> makeNonEmptyArrayNoteOrRest (head :| tail)

instance toSceneArrayNoteOrRest ::
  ( FofT pitchF
  , FofT volumeF
  ) =>
  ToScene (Array (NoteOrRest pitchF Identity volumeF Identity)) Unit where
  toScene = makeArrayNoteOrRest

makeArraySequencedNote
  :: forall volumeF pitchF
   . FofT volumeF
  => FofT pitchF
  => Array (Sequenced (Note volumeF Identity pitchF))
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeArraySequencedNote =
  A.uncons
    >>> case _ of
      Nothing -> nothing
      Just { head, tail } -> makeNonEmptyArraySequencedNote (head :| tail)

instance toSceneArraySequencedNote ::
  ( FofT pitchF
  , FofT volumeF
  ) =>
  ToScene (Array (Sequenced (Note volumeF Identity pitchF))) Unit where
  toScene = makeArraySequencedNote

makeNonEmptyArrayPitch
  :: forall pitchF
   . FofT pitchF
  => NonEmpty Array (Pitch pitchF)
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeNonEmptyArrayPitch = makeNonEmptyArrayNote <<< map (noteFromDefaults_ <<< set (prop (Proxy :: _ "pitch")))

instance toSceneNonEmptyArrayPitch :: FofT pitchF => ToScene (NonEmpty Array (Pitch pitchF)) Unit where
  toScene = makeNonEmptyArrayPitch

makeNonEmptyArrayNote
  :: forall volumeF pitchF
   . FofT volumeF
  => FofT pitchF
  => NonEmpty Array (Note volumeF Identity pitchF)
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeNonEmptyArrayNote = makeNonEmptyArraySequencedNote <<< seq

instance toSceneNonEmptyArrayNote ::
  ( FofT pitchF
  , FofT volumeF
  ) =>
  ToScene (NonEmpty Array (Note pitchF Identity volumeF)) Unit where
  toScene = makeNonEmptyArrayNote

makeNonEmptyArrayNoteOrRest
  :: forall volumeF pitchF
   . FofT volumeF
  => FofT pitchF
  => NonEmpty Array (NoteOrRest volumeF Identity pitchF Identity)
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeNonEmptyArrayNoteOrRest = makeArraySequencedNote <<< seq

instance toSceneNonEmptyArrayNoteOrRest ::
  ( FofT pitchF
  , FofT volumeF
  ) =>
  ToScene (NonEmpty Array (NoteOrRest pitchF Identity volumeF Identity)) Unit where
  toScene = makeNonEmptyArrayNoteOrRest

makeNonEmptyArraySequencedNote
  :: forall volumeF pitchF
   . FofT volumeF
  => FofT pitchF
  => NonEmpty Array (Sequenced (Note volumeF Identity pitchF))
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeNonEmptyArraySequencedNote (a@(Sequenced { note: n' }) :| b) =
  makeCofreeSequencedNote
    $ deadEnd
    $ a
        :| b
        <> [ Sequenced { startsAfter: top, note: n' } ]

instance toSceneNonEmptyArraySequencedNote ::
  ( FofT volumeF
  , FofT pitchF
  ) =>
  ToScene (NonEmpty Array (Sequenced (Note volumeF Identity pitchF))) Unit where
  toScene = makeNonEmptyArraySequencedNote

makeCofreePitch
  :: forall pitchF
   . FofT pitchF
  => Cofree Identity (Pitch pitchF)
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeCofreePitch = makeCofreeNote <<< map (noteFromDefaults_ <<< set (prop (Proxy :: _ "pitch")))

instance toSceneCofreePitch :: FofT pitchF => ToScene (Cofree Identity (Pitch pitchF)) Unit where
  toScene = makeCofreePitch

makeCofreeNote
  :: forall volumeF pitchF
   . FofT volumeF
  => FofT pitchF
  => Cofree Identity (Note volumeF Identity pitchF)
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeCofreeNote = makeCofreeSequencedNote <<< noteStreamToSequence (Rest $ Identity 0.0)

instance toSceneCofreeNote ::
  ( FofT volumeF
  , FofT pitchF
  ) =>
  ToScene (Cofree Identity (Note volumeF Identity pitchF)) Unit where
  toScene = makeCofreeNote

noSine :: CTOR.Gain AudioParameter /\ (CTOR.SinOsc AudioOnOff AudioParameter /\ {})
noSine = gain 0.0 (sinOsc 0.1)

{-
subgraph
  :: forall n inputs info terminus env r
   . Pos n
  => V.Vec n info
  -> ( forall audio engine
        . AudioInterpret audio engine
       => Int
       -> info
       -> SubScene terminus inputs env audio engine Frame0 Unit
     )
  -> (Int -> info -> env)
  -> r
  -> (CTOR.Subgraph inputs (V.Vec n info) (AsSubgraph terminus inputs info env) (Int -> info -> env)) /\ r
subgraph vec sg ev = Tuple (CTOR.Subgraph vec (AsSubgraph sg) ev)
-}
makeCofreeSequencedNote
  :: forall volumeF durationF pitchF
   . FofT volumeF
  => FofT durationF
  => FofT pitchF
  => Cofree Identity (Sequenced (Note volumeF durationF pitchF))
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeCofreeSequencedNote notes' =
  makeFullScene
    $ FullScene
        { triggerWorld: defaultTriggerWorld
        , piece:
            loopUsingScene
              ( \(BehavingScene { time: time', headroomInSeconds: headroomInSeconds }) { oscSimple } ->
                  let
                    actualized = oscSimple { time: time', headroomInSeconds, input: unit }

                    internal
                      :: forall audio engine
                       . AudioInterpret audio engine
                      => SubScene "singleton" () { time :: Number, buf :: Maybe (Buffy ({ note :: Note volumeF durationF pitchF, pw :: APFofT })) } audio engine Frame0 Unit
                    internal =
                      mempty
                        # SG.loopUsingScene \{ time, buf } _ ->
                            { control: unit
                            , scene:
                                { singleton:
                                    case buf of
                                      Just (Buffy { rest: { note: Note { pitch: Pitch pitch, volume: Volume volume }, pw } }) ->
                                        gain
                                          ( (gff (pw { time, headroomInSeconds }))
                                              * (set lensParam (toFofT volume time) mempty)
                                          )
                                          (sinOsc (gff $ paramize $ (toFofT pitch time)))
                                      Nothing -> noSine
                                }
                            }
                  in
                    { control: { oscSimple: unwrapCofree actualized }
                    , scene:
                        speaker
                          { piece: subgraph (map ({ time: time', buf: _ }) (extract actualized)) (const internal) {}
                          }
                    }
              )
              (const { oscSimple: emitter })
        }
  where
  pwl dr v = ((0.0 /\ 0.0) :| ((min (dr * 0.3) 0.1) /\ v) : ((min (dr * 0.45) 0.3) /\ v * 0.3) : (dr /\ 0.0) : Nil)

  emitter :: AScoredBufferPool Unit D4 ({ note :: Note volumeF durationF pitchF, pw :: APFofT })
  emitter =
    makeBufferPoolWithAnchor
      ( (map <<< map <<< map)
          -- todo: unhard-code duration to pwl? possible, but would it be computatoinally expensive
          (\{ offset, rest: note@(Note { duration: Duration d }) } -> { duration: \_ _ t -> (Just (toFofT d t)), offset, rest: \st -> let pw = makePiecewise (map (over _1 (add st)) (pwl 1.0 1.0)) in { note, pw } })
          (makeScore { startsAt: 0.0, noteStream: \_ -> hoistCofree (\(Identity i) _ -> i) $ map (\(Sequenced { startsAfter: Rest (Identity s), note }) -> { startsAfter: s, rest: note }) notes' })
      )

instance toSceneCofreeSequencedNote ::
  ( FofT volumeF
  , FofT durationF
  , FofT pitchF
  ) =>
  ToScene (Cofree Identity (Sequenced (Note volumeF durationF pitchF))) Unit where
  toScene = makeCofreeSequencedNote

makeLoop
  :: forall scene graph
   . Create scene () graph
  => GraphIsRenderable graph
  => Change scene graph
  => { | scene }
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeLoop scene =
  makeFullScene
    $ FullScene
        { triggerWorld: defaultTriggerWorld
        , piece: loopUsingScene (const $ const $ { control: unit, scene }) mempty
        }

instance toSceneLoop ::
  ( Create scene () graph
  , GraphIsRenderable graph
  , Change scene graph
  ) =>
  ToScene { | scene } Unit where
  toScene = makeLoop

makeFunctionOfTimeLoop
  :: forall scene graph
   . Create scene () graph
  => GraphIsRenderable graph
  => Change scene graph
  => (Number -> { | scene })
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeFunctionOfTimeLoop scene =
  makeFullScene
    $ FullScene
        { triggerWorld: defaultTriggerWorld
        , piece: loopUsingScene (\(BehavingScene { time }) -> const $ { control: unit, scene: scene time }) mempty
        }

instance toSceneFunctionOfTimeLoop ::
  ( Create scene () graph
  , GraphIsRenderable graph
  , Change scene graph
  ) =>
  ToScene (Number -> { | scene }) Unit where
  toScene = makeFunctionOfTimeLoop

newtype WithControl control scene
  = WithControl
  ( control
      /\ (Number -> control -> { control :: control, scene :: { | scene } })
  )

withControl :: forall control scene. control -> (Number -> control -> { control :: control, scene :: { | scene } }) -> WithControl control scene
withControl control scene = WithControl (control /\ scene)

makeFunctionOfTimeAndControlLoop
  :: forall control scene graph
   . Create scene () graph
  => GraphIsRenderable graph
  => Change scene graph
  => (WithControl control scene)
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeFunctionOfTimeAndControlLoop (WithControl (control /\ scene)) =
  makeFullScene
    $ FullScene
        { triggerWorld: defaultTriggerWorld
        , piece: loopUsingScene (\(BehavingScene { time }) -> scene time) (const control)
        }

instance toSceneFunctionOfTimeAndControlLoop ::
  ( Create scene () graph
  , GraphIsRenderable graph
  , Change scene graph
  ) =>
  ToScene (WithControl control scene) Unit where
  toScene = makeFunctionOfTimeAndControlLoop

makeString :: String -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun Unit Analysers) }
makeString url =
  makeFullScene
    $ FullScene
        { triggerWorld:
            \audioCtx -> do
              buffer <- decodeAudioDataFromUri audioCtx url
              pure (pure {} /\ pure { buffer })
        , piece:
            loopUsingScene
              ( \(BehavingScene { world: { buffer } }) ->
                  const $ { control: unit, scene: speaker { b: playBuf buffer } }
              )
              mempty
        }

instance toSceneString :: ToScene String Unit where
  toScene = makeString

class MatchBuffersRL :: forall k1 k2. k1 -> k2 -> Row Type -> Row Type -> Constraint
class MatchBuffersRL buffersSRL buffersRL buffersS buffers | buffersSRL -> buffersRL buffersS buffers where
  getBuffersRL :: AudioContext -> Proxy buffersSRL -> Proxy buffersRL -> { | buffersS } -> Aff { | buffers }

instance matchBuffersRLNil :: MatchBuffersRL RL.Nil RL.Nil () () where
  getBuffersRL _ _ _ _ = pure {}

instance matchBuffersRLCons ::
  ( IsSymbol key
  , Cons key String buffersS' buffersS
  , Cons key BrowserAudioBuffer buffers' buffers
  , Lacks key buffersS'
  , Lacks key buffers'
  , MatchBuffersRL x y buffersS' buffers'
  ) =>
  MatchBuffersRL (RL.Cons key String x) (RL.Cons key BrowserAudioBuffer y) buffersS buffers where
  getBuffersRL audioCtx _ _ a = do
    buffer <- decodeAudioDataFromUri audioCtx (Record.get (Proxy :: _ key) a)
    rest <- getBuffersRL audioCtx (Proxy :: _ x) (Proxy :: _ y) (Record.delete (Proxy :: _ key) a)
    pure $ Record.insert (Proxy :: _ key) buffer rest

class MatchBuffers buffersS buffers | buffersS -> buffers where
  getBuffers :: AudioContext -> { | buffersS } -> Aff { | buffers }

instance matchBuffersAll ::
  ( RL.RowToList buffersS buffersSRL
  , RL.RowToList buffers buffersRL
  , MatchBuffersRL buffersSRL buffersRL buffersS buffers
  ) =>
  MatchBuffers buffersS buffers where
  getBuffers ctx a = getBuffersRL ctx (Proxy :: _ buffersSRL) (Proxy :: _ buffersRL) a

newtype FullSceneBuilder trigger world res
  = FullSceneBuilder
  { triggerWorld :: AudioContext /\ Aff (Event {} /\ Behavior {}) -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
  , piece :: Scene (BehavingScene { | trigger } { | world } AnalysersCb) RunAudio RunEngine Frame0 res
  }

using
  :: forall trigger world scene graph
   . Create scene () graph
  => GraphIsRenderable graph
  => Change scene graph
  => ( AudioContext /\ Aff (Event {} /\ Behavior {})
       -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
     )
  -> (BehavingScene { | trigger } { | world } AnalysersCb -> { | scene })
  -> FullSceneBuilder trigger world Unit
using triggerWorld = usingc triggerWorld mempty <<< map (const <<< { control: unit, scene: _ })

usingc
  :: forall trigger world scene graph control
   . Create scene () graph
  => GraphIsRenderable graph
  => Change scene graph
  => ( AudioContext /\ Aff (Event {} /\ Behavior {})
       -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
     )
  -> (BehavingScene { | trigger } { | world } AnalysersCb -> control)
  -> (BehavingScene { | trigger } { | world } AnalysersCb -> control -> { scene :: { | scene }, control :: control })
  -> FullSceneBuilder trigger world Unit
usingc triggerWorld control piece = FullSceneBuilder { triggerWorld, piece: loopUsingScene piece control }

usingcr
  :: forall trigger world scene graph control res
   . Create scene () graph
  => Monoid res
  => GraphIsRenderable graph
  => Change scene graph
  => ( AudioContext /\ Aff (Event {} /\ Behavior {})
       -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
     )
  -> (BehavingScene { | trigger } { | world } AnalysersCb -> control)
  -> (BehavingScene { | trigger } { | world } AnalysersCb -> control -> { scene :: { | scene }, control :: control, res :: res })
  -> FullSceneBuilder trigger world res
usingcr triggerWorld control piece = FullSceneBuilder { triggerWorld, piece: loopUsingSceneWithRes piece control }

buffers
  :: forall buffersS buffers trigger world
   . Lacks "buffers" world
  => MatchBuffers buffersS buffers
  => { | buffersS }
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { buffers :: { | buffers } | world })
buffers bf (ac /\ aff) =
  ac
    /\ do
      trigger /\ world <- aff
      b' <- b
      pure (trigger /\ (Record.insert (Proxy :: _ "buffers") b' <$> world))
  where
  b = getBuffers ac bf

makeFullSceneUsing
  :: forall trigger world res
   . Monoid res
  => FullSceneBuilder trigger world res
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun res Analysers) }
makeFullSceneUsing (FullSceneBuilder { triggerWorld, piece }) =
  makeFullScene
    $ FullScene
        { triggerWorld: \audioContext -> snd $ triggerWorld (audioContext /\ pure (pure {} /\ pure {}))
        , piece
        }

instance toSceneFullSceneUsing ::
  ( Monoid res
  ) =>
  ToScene (FullSceneBuilder trigger world res) res where
  toScene = makeFullSceneUsing

makeFullScene
  :: forall trigger world res
   . Monoid res
  => FullScene trigger world res
  -> Aff { audioCtx :: AudioContext, event :: Event (BehavingRun res Analysers) }
makeFullScene (FullScene { triggerWorld, piece }) = do
  audioCtx <- liftEffect context
  waStatus <- liftEffect $ contextState audioCtx
  -- void the constant 0 hack
  -- this will result in a very slight performance decrease but makes iOS and Mac more sure
  _ <- liftEffect $ constant0Hack audioCtx
  when (waStatus /= "running") (toAffE $ contextResume audioCtx)
  ffiAudio <- liftEffect $ makeFFIAudioSnapshot audioCtx
  trigger /\ world <- triggerWorld audioCtx
  pure { audioCtx, event: run trigger world { easingAlgorithm } (ffiAudio) piece }

instance toSceneFullScene ::
  ( Monoid res
  ) =>
  ToScene (FullScene trigger world res) res where
  toScene = makeFullScene

data WagsState
  = WagsPlaying
  | WagsStopped
  | WagsLoading

type State'
  =
  ( unsubscribe :: Effect Unit
  , audioCtx :: Maybe AudioContext
  , wagsState :: WagsState
  )

type State
  = { | State' }

data Action
  = StartAudio
  | StopAudio

component
  :: forall toScene res query input output m
   . MonadEffect m
  => MonadAff m
  => ToScene toScene res
  => toScene
  -> H.Component query input output m
component i =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction (toScene i), finalize = Just StopAudio }
    }

type AffCb a
  = (Error -> Effect Unit) -> (a -> Effect Unit) -> Effect Unit

acb2Aff :: AffCb ~> Aff
acb2Aff acb =
  makeAff \f -> do
    acb (f <<< Left) (f <<< Right)
    mempty

type Player
  =
  AffCb Unit
  -> AffCb Unit
  -> (Error -> Effect Unit)
  -> Effect (Effect Unit)

player
  :: forall toScene res
   . ToScene toScene res
  => toScene
  -> Player
player i loadingCb loadedCb errorCb = do
  x <-
    try do
      fib <-
        launchAff do
          acb2Aff loadingCb
          { audioCtx, event } <- aff
          acb2Aff loadedCb
          unsubscribe <- liftEffect $ subscribe event (\(_ :: BehavingRun res Analysers) -> pure unit)
          pure { audioCtx, unsubscribe }
      pure
        $ launchAff_ do
            { audioCtx, unsubscribe } <- joinFiber fib
            liftEffect do
              close audioCtx
              unsubscribe
  case x of
    Left e -> errorCb e *> pure (pure unit)
    Right r -> pure r
  where
  aff = toScene i

minicomponent
  :: forall toScene res query input output m
   . MonadEffect m
  => MonadAff m
  => ToScene toScene res
  => toScene
  -> H.Component query input output m
minicomponent i =
  H.mkComponent
    { initialState
    , render: miniRender
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction (toScene i), finalize = Just StopAudio }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  , wagsState: WagsStopped
  }

buttonCSS :: String -> String
buttonCSS rgb = """align-items:flex-start;appearance:none;background-color:""" <> rgb <> """;background-image:none;border-bottom-color:rgb(229, 231, 235);border-bottom-left-radius:8px;border-bottom-right-radius:8px;border-bottom-style:solid;border-bottom-width:0px;border-image-outset:0;border-image-repeat:stretch;border-image-slice:100%;border-image-source:none;border-image-width:1;border-left-color:rgb(229, 231, 235);border-left-style:solid;border-left-width:0px;border-right-color:rgb(229, 231, 235);border-right-style:solid;border-right-width:0px;border-top-color:rgb(229, 231, 235);border-top-left-radius:8px;border-top-right-radius:8px;border-top-style:solid;border-top-width:0px;box-sizing:border-box;color:rgb(255, 255, 255);cursor:pointer;display:block;font-family:ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe:UI", Roboto, "Helvetica:Neue", Arial, "Noto:Sans", sans-serif, "Apple:Color:Emoji", "Segoe:UI:Emoji", "Segoe:UI:Symbol", "Noto:Color:Emoji";font-size:24px;font-stretch:100%;font-style:normal;font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal;font-weight:400;height:56px;letter-spacing:normal;line-height:32px;margin-bottom:20px;margin-left:20px;margin-right:20px;margin-top:20px;padding-bottom:12px;padding-left:12px;padding-right:12px;padding-top:12px;tab-size:4;text-align:center;text-indent:0px;text-rendering:auto;text-shadow:none;text-size-adjust:100%;text-transform:none;width:172.5px;word-spacing:0px;writing-mode:horizontal-tb;-webkit-border-image:none;"""

onCSS = buttonCSS "rgb(99, 102, 241)" :: String

loadingCss = buttonCSS "rgb(154, 36, 23)" :: String

offCSS = buttonCSS "rgb(236, 72, 153)" :: String

containerCss = "display:-ms-flexbox;display:-webkit-flex;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;-webkit-flex-wrap:nowrap;-ms-flex-wrap:nowrap;flex-wrap:nowrap;-webkit-justify-content:center;-ms-flex-pack:center;justify-content:center;-webkit-align-content:stretch;-ms-flex-line-pack:stretch;align-content:stretch;-webkit-align-items:center;-ms-flex-align:center;align-items:center;" :: String

flex1Css = "-webkit-order:0;-ms-flex-order: 0;order: 0;-webkit-flex: 1 1 auto;-ms-flex: 1 1 auto;flex: 1 1 auto;-webkit-align-self: auto;-ms-flex-item-align: auto;align-self: auto;" :: String

flex2Css = "-webkit-order: 0;-ms-flex-order: 0;order: 0;-webkit-flex: 0 1 auto;-ms-flex: 0 1 auto;flex: 0 1 auto;-webkit-align-self: auto;-ms-flex-item-align: auto;align-self: auto;" :: String

flex3Css = "-webkit-order: 0;-ms-flex-order: 0;order: 0;-webkit-flex: 1 1 auto;-ms-flex: 1 1 auto;flex: 1 1 auto;-webkit-align-self: auto;-ms-flex-item-align: auto;align-self: auto;" :: String

render :: forall m. State -> H.ComponentHTML Action () m
render { wagsState } = case wagsState of
  WagsPlaying ->
    HH.button
      [ HE.onClick \_ -> StopAudio, HP.style offCSS ]
      [ HH.text "Stop audio" ]
  WagsStopped ->
    HH.button
      [ HE.onClick \_ -> StartAudio, HP.style onCSS ]
      [ HH.text "Start audio" ]
  WagsLoading -> HH.div [ HP.style loadingCss ] [ HH.text "Loading..." ]

bStyle :: ∀ i r. HP.IProp (style ∷ String | r) i
bStyle =
  HCSS.style do
    CSS.display CSS.inlineBlock
    CSS.padding (0.25 # CSS.em) (1.0 # CSS.em) (0.25 # CSS.em) (1.0 # CSS.em)
    CSS.border CSS.solid (0.1 # CSS.em) CSS.black
    CSS.margin (0.0 # CSS.em) (0.3 # CSS.em) (0.3 # CSS.em) (0.0 # CSS.em)
    CSS.borderRadius (0.12 # CSS.em) (0.12 # CSS.em) (0.12 # CSS.em) (0.12 # CSS.em)
    CSS.boxSizing CSS.borderBox
    CSS.textDecoration CSS.noneTextDecoration
    CSS.fontFamily [ "Roboto" ] (CSS.sansSerif :| [])
    CSS.fontWeight (CSS.weight 300.0)
    CSS.color CSS.black
    CSS.cursor CSSCur.pointer
    CSS.backgroundColor (rgba 0 0 0 0.0)
    CSST.textAlign CSST.center
    CSSF.flex 0.0 1.0 CSSC.auto

-- transition: all 0.2s;
miniRender :: forall m. State -> H.ComponentHTML Action () m
miniRender { wagsState } =
  HH.div
    [ HCSS.style do
        CSS.display CSS.flex
        CSS.flexDirection CSS.row
        CSS.justifyContent CSS.spaceBetween
    ]
    [ case wagsState of
        WagsPlaying ->
          HH.button
            [ HE.onClick \_ -> StopAudio, bStyle ]
            [ HH.text "Stop audio" ]
        WagsStopped ->
          HH.button
            [ HE.onClick \_ -> StartAudio, bStyle ]
            [ HH.text "Start audio" ]
        WagsLoading -> HH.button [ bStyle ] [ HH.text "Loading..." ]
    , HH.p
        [ HCSS.style do
            CSS.fontFamily [ "Roboto" ] (CSS.sansSerif :| [])
            CSS.fontWeight (CSS.weight 300.0)
        ]
        [ HH.text "made with ❤️ using ", HH.a [ HP.href "https://github.com/mikesol/purescript-wags" ] [ HH.text "wags" ] ]
    ]

handleAction
  :: forall res analysers output m
   . MonadEffect m
  => MonadAff m
  => Aff { audioCtx :: AudioContext, event :: Event (BehavingRun res analysers) }
  -> Action
  -> H.HalogenM State Action () output m Unit
handleAction aff = case _ of
  StartAudio -> do
    H.modify_ _ { wagsState = WagsLoading }
    { audioCtx, event } <- H.liftAff aff
    unsubscribe <- H.liftEffect $ subscribe event (\(_ :: BehavingRun res analysers) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx, wagsState = WagsPlaying }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing, wagsState = WagsStopped }

---
type ParentSlots
  = (audio :: forall q. H.Slot q Void Unit)

_audio = Proxy :: Proxy "audio"

type ParentState
  = {}

type ParentAction
  = Void

flexTop = """width:100%;height:100%;flex-direction:column;display:flex;""" :: String

flexRows = "flex-grow:1;display:block;" :: String

parent :: forall query input output m. (forall query2 input2 output2. H.Component query2 input2 output2 m) -> H.Component query input output m
parent audio =
  H.mkComponent
    { initialState: iS
    , render: rdr
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = absurd }
    }
  where
  iS :: input -> ParentState
  iS _ = {}

  rdr :: ParentState -> H.ComponentHTML ParentAction ParentSlots m
  rdr _ =
    HH.div [ HP.style containerCss ]
      [ HH.div [ HP.style flex1Css ] []
      , HH.div [ HP.style flex2Css ] [ HH.slot_ _audio unit audio unit ]
      , HH.div [ HP.style flex3Css ] []
      ]

play
  :: forall toScene res
   . ToScene toScene res
  => toScene
  -> Effect Unit
play toScene =
  runHalogenAff do
    awaitBody >>= runUI (parent (component toScene)) unit

miniplay
  :: forall toScene res
   . ToScene toScene res
  => toScene
  -> Effect Unit
miniplay toScene =
  runHalogenAff do
    awaitBody >>= runUI (minicomponent toScene) unit
