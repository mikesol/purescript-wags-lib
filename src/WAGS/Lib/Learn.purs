module WAGS.Lib.Learn where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Promise (toAffE)
import Data.Array as A
import Data.Foldable (for_)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.Lens (_1, over)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Symbol (class IsSymbol)
import Data.Traversable (sequence)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (D4)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Behavior (Behavior)
import FRP.Event (Event, subscribe)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Math (pow)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Change (class Change)
import WAGS.Control.Functions.Validated (freeze, loopUsingScene, (@!>))
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (class Create, icreate)
import WAGS.Create.Optionals (constant, gain, sinOsc, playBuf, speaker)
import WAGS.Graph.AudioUnit (APOnOff)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Parameter (AudioParameter_, AudioParameter, ff)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.BufferPool (AHotBufferPool', Buffy(..), makeHotBufferPoolWithRest)
import WAGS.Lib.Piecewise (makePiecewise)
import WAGS.Lib.Stream (stops)
import WAGS.Run (class AnalyserRefs, class Analysers, class MakeAnalyserCallbacks, Run, RunAudio, RunEngine, SceneI(..), run)
import WAGS.Template (fromTemplate)
import WAGS.Validation (class GraphIsRenderable)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

-- global fast forward to avoid clicks and pops
gff :: AudioParameter_ ~> AudioParameter_
gff = ff 0.03

class ToScene a res analysers | a -> res analysers where
  toScene :: a -> Aff { audioCtx :: AudioContext, event :: Event (Run res analysers) }

newtype FullScene trigger world analyserCallbacks res =
  FullScene
    { triggerWorld :: AudioContext -> Aff (Event { | trigger } /\ Behavior { | world })
    , piece :: Scene (SceneI { | trigger } { | world } analyserCallbacks) RunAudio RunEngine Frame0 res
    }

type EmptyAnalysers :: forall k. Row k
type EmptyAnalysers = ()

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = initialTime :< \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

defaultTriggerWorld :: AudioContext -> Aff (Event {} /\ Behavior {})
defaultTriggerWorld = pure (pure (pure {} /\ pure {}))

nothing :: Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
nothing = makeFullScene $ FullScene
  { triggerWorld: defaultTriggerWorld
  , piece: loopUsingScene (const $ const $ { control: unit, scene: speaker { c: constant 0.0 } }) unit
  }

midiToCps :: Number -> Number
midiToCps i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

makeInt :: Int -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeInt = makeNumber <<< midiToCps <<< toNumber

instance toSceneInt :: ToScene Int Unit EmptyAnalysers
  where
  toScene = makeInt

makeNumber :: Number -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNumber = makeFunctionOfTimeNumber <<< const

instance toSceneNumber :: ToScene Number Unit EmptyAnalysers
  where
  toScene = makeNumber

makeArrayInt :: Array Int -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayInt = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayInt (head :| tail)

instance toSceneArrayInt :: ToScene (Array Int) Unit EmptyAnalysers
  where
  toScene = makeArrayInt

makeArrayNumber :: Array Number -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayNumber = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayNumber (head :| tail)

instance toSceneArrayNumber :: ToScene (Array Number) Unit EmptyAnalysers
  where
  toScene = makeArrayNumber

makeNonEmptyArrayInt :: NonEmpty Array Int -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayInt = makeNonEmptyArrayNumber <<< map (midiToCps <<< toNumber)

instance toSceneNonEmptyArrayInt :: ToScene (NonEmpty Array Int) Unit EmptyAnalysers
  where
  toScene = makeNonEmptyArrayInt

makeNonEmptyArrayNumber :: NonEmpty Array Number -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayNumber (a :| b) = makeNonEmptyListNumber (a :| L.fromFoldable b)

instance toSceneNonEmptyArrayNumber :: ToScene (NonEmpty Array Number) Unit EmptyAnalysers
  where
  toScene = makeNonEmptyArrayNumber

makeListInt :: List Int -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeListInt = L.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyListInt (head :| tail)

instance toSceneListInt :: ToScene (List Int) Unit EmptyAnalysers
  where
  toScene = makeListInt

makeListNumber :: List Number -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeListNumber = L.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyListNumber (head :| tail)

instance toSceneListNumber :: ToScene (List Number) Unit EmptyAnalysers
  where
  toScene = makeListNumber

makeNonEmptyListInt :: NonEmpty List Int -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyListInt = makeNonEmptyListNumber <<< map (midiToCps <<< toNumber)

instance toSceneNonEmptyListInt :: ToScene (NonEmpty List Int) Unit EmptyAnalysers
  where
  toScene = makeNonEmptyListInt

makeNonEmptyListNumber :: NonEmpty List Number -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyListNumber = makeCofreeMaybeNumber <<< stops

instance toSceneNonEmptyListNumber :: ToScene (NonEmpty List Number) Unit EmptyAnalysers
  where
  toScene = makeNonEmptyListNumber

makeCofreeInt :: Cofree Identity Int -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeInt = makeCofreeNumber <<< map (midiToCps <<< toNumber)

instance toSceneCofreeInt :: ToScene (Cofree Identity Int) Unit EmptyAnalysers
  where
  toScene = makeCofreeInt

makeCofreeNumber :: Cofree Identity Number -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeNumber = makeCofreeFunctionOfTimeNumber <<< map const

instance toSceneCofreeNumber :: ToScene (Cofree Identity Number) Unit EmptyAnalysers
  where
  toScene = makeCofreeNumber

makeCofreeMaybeNumber :: Cofree Identity (Maybe Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeMaybeNumber = makeCofreeFunctionOfTimeMaybeNumber <<< map const

instance toSceneCofreeMaybeNumber :: ToScene (Cofree Identity (Maybe Number)) Unit EmptyAnalysers
  where
  toScene = makeCofreeMaybeNumber

makeFunctionOfTimeInt :: (Number -> Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeFunctionOfTimeInt = makeFunctionOfTimeNumber <<< map (midiToCps <<< toNumber)

instance toSceneFunctionOfTimeInt :: ToScene (Number -> Int) Unit EmptyAnalysers
  where
  toScene = makeFunctionOfTimeInt

makeFunctionOfTimeNumber :: (Number -> Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeFunctionOfTimeNumber f = makeFullScene $ FullScene
  { triggerWorld: defaultTriggerWorld
  , piece: (\(SceneI { time }) -> icreate (speaker { gain: gain (gff $ pure 0.3) (sinOsc (gff $ pure $ f time)) }) $> unit) @!> freeze
  }

instance toSceneFunctionOfTimeNumber :: ToScene (Number -> Number) Unit EmptyAnalysers
  where
  toScene = makeFunctionOfTimeNumber

makeArrayFunctionOfTimeInt :: Array (Number -> Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayFunctionOfTimeInt = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayFunctionOfTimeInt (head :| tail)

instance toSceneArrayFunctionOfTimeInt :: ToScene (Array (Number -> Int)) Unit EmptyAnalysers
  where
  toScene = makeArrayFunctionOfTimeInt

makeArrayFunctionOfTimeNumber :: Array (Number -> Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayFunctionOfTimeNumber = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayFunctionOfTimeNumber (head :| tail)

instance toSceneArrayFunctionOfTimeNumber :: ToScene (Array (Number -> Number)) Unit EmptyAnalysers
  where
  toScene = makeArrayFunctionOfTimeNumber

makeNonEmptyArrayFunctionOfTimeInt :: NonEmpty Array (Number -> Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayFunctionOfTimeInt = makeNonEmptyArrayFunctionOfTimeNumber <<< (map <<< map) (midiToCps <<< toNumber)

instance toSceneNonEmptyArrayFunctionOfTimeInt :: ToScene (NonEmpty Array (Number -> Int)) Unit EmptyAnalysers
  where
  toScene = makeNonEmptyArrayFunctionOfTimeInt

makeNonEmptyArrayFunctionOfTimeNumber :: NonEmpty Array (Number -> Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayFunctionOfTimeNumber (a :| b) = makeNonEmptyListFunctionOfTimeNumber (a :| L.fromFoldable b)

instance toSceneNonEmptyArrayFunctionOfTimeNumber :: ToScene (NonEmpty Array (Number -> Number)) Unit EmptyAnalysers
  where
  toScene = makeNonEmptyArrayFunctionOfTimeNumber

makeNonEmptyListFunctionOfTimeInt :: NonEmpty List (Number -> Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyListFunctionOfTimeInt = makeNonEmptyListFunctionOfTimeNumber <<< (map <<< map) (midiToCps <<< toNumber)

instance toScenemakeNonEmptyListFunctionOfTimeInt :: ToScene (NonEmpty List (Number -> Int)) Unit EmptyAnalysers
  where
  toScene = makeNonEmptyListFunctionOfTimeInt

makeNonEmptyListFunctionOfTimeNumber :: NonEmpty List (Number -> Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyListFunctionOfTimeNumber = makeCofreeFunctionOfTimeMaybeNumber <<< map sequence <<< stops

instance toSceneNonEmptyListFunctionOfTimeNumber :: ToScene (NonEmpty List (Number -> Number)) Unit EmptyAnalysers
  where
  toScene = makeNonEmptyListFunctionOfTimeNumber

makeCofreeFunctionOfTimeInt :: Cofree Identity (Number -> Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeFunctionOfTimeInt = makeCofreeFunctionOfTimeNumber <<< (map <<< map) (midiToCps <<< toNumber)

instance toSceneCofreeFunctionOfTimeInt :: ToScene (Cofree Identity (Number -> Int)) Unit EmptyAnalysers
  where
  toScene = makeCofreeFunctionOfTimeInt

noSine :: CTOR.Gain AudioParameter /\ (CTOR.SinOsc APOnOff AudioParameter /\ {})
noSine = gain 0.0 (sinOsc 0.1)

makeCofreeFunctionOfTimeNumber :: Cofree Identity (Number -> Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeFunctionOfTimeNumber = makeCofreeFunctionOfTimeMaybeNumber <<< ((map <<< map) Just)

instance toSceneCofreeFunctionOfTimeNumber :: ToScene (Cofree Identity (Number -> Number)) Unit EmptyAnalysers
  where
  toScene = makeCofreeFunctionOfTimeNumber

makeCofreeFunctionOfTimeMaybeNumber :: Cofree Identity (Number -> Maybe Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeFunctionOfTimeMaybeNumber notes' = makeFullScene $ FullScene
  { triggerWorld: defaultTriggerWorld
  , piece: loopUsingScene
      ( \(SceneI { time, headroomInSeconds: headroomInSeconds }) { oscSimple } ->
          let
            actualized = oscSimple { time, headroomInSeconds, freq: 1.0 }
          in
            { control: { oscSimple: unwrapCofree actualized }
            , scene: speaker
                { piece: fromTemplate (Proxy :: _ "sinOsc") (extract actualized) \_ -> case _ of
                    Just (Buffy { startTime, rest: pitch }) -> pitch time # maybe noSine \pitch' -> gain
                      (gff (makePiecewise (map (over _1 (add startTime)) pwl) { time, headroomInSeconds }))
                      (sinOsc (gff $ pure $ pitch'))

                    Nothing -> noSine
                }
            }
      )
      { oscSimple: emitter }
  }
  where
  fixedDuration = 1.0
  pwl = ((0.0 /\ 0.0) :| ((min (fixedDuration * 0.3) 0.1) /\ 1.0) : ((min (fixedDuration * 0.45) 0.3) /\ 0.2) : (fixedDuration /\ 0.0) : Nil)

  emitter :: AHotBufferPool' D4 (Number -> Maybe Number)
  emitter = makeHotBufferPoolWithRest { startsAt: 0.0, rest: notes' }

instance toSceneCofreeFunctionOfTimeMaybeNumber :: ToScene (Cofree Identity (Number -> Maybe Number)) Unit EmptyAnalysers
  where
  toScene = makeCofreeFunctionOfTimeMaybeNumber

makeLoop
  :: forall scene graph
   . Create scene () graph
  => GraphIsRenderable graph
  => Change scene graph
  => { | scene }
  -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeLoop scene = makeFullScene $ FullScene
  { triggerWorld: defaultTriggerWorld
  , piece: loopUsingScene (const $ const $ { control: unit, scene }) unit
  }

instance toSceneLoop ::
  ( Create scene () graph
  , GraphIsRenderable graph
  , Change scene graph
  ) =>
  ToScene { | scene } Unit EmptyAnalysers
  where
  toScene = makeLoop

makeFunctionOfTimeLoop
  :: forall scene graph
   . Create scene () graph
  => GraphIsRenderable graph
  => Change scene graph
  => (Number -> { | scene })
  -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeFunctionOfTimeLoop scene = makeFullScene $ FullScene
  { triggerWorld: defaultTriggerWorld
  , piece: loopUsingScene (\(SceneI { time }) -> const $ { control: unit, scene: scene time }) unit
  }

instance toSceneFunctionOfTimeLoop ::
  ( Create scene () graph
  , GraphIsRenderable graph
  , Change scene graph
  ) =>
  ToScene (Number -> { | scene }) Unit EmptyAnalysers
  where
  toScene = makeFunctionOfTimeLoop

newtype WithControl control scene = WithControl
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
  -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeFunctionOfTimeAndControlLoop (WithControl (control /\ scene)) = makeFullScene $ FullScene
  { triggerWorld: defaultTriggerWorld
  , piece: loopUsingScene (\(SceneI { time }) -> scene time) control
  }

instance toSceneFunctionOfTimeAndControlLoop ::
  ( Create scene () graph
  , GraphIsRenderable graph
  , Change scene graph
  ) =>
  ToScene (WithControl control scene) Unit EmptyAnalysers
  where
  toScene = makeFunctionOfTimeAndControlLoop

makeString :: String -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeString url = makeFullScene $ FullScene
  { triggerWorld: \audioCtx -> do
      buffer <- toAffE $ decodeAudioDataFromUri audioCtx url
      pure (pure {} /\ pure { buffer })
  , piece: loopUsingScene
      ( \(SceneI { world: { buffer } }) ->
          const $ { control: unit, scene: speaker { b: playBuf buffer } }
      )
      unit
  }

instance toSceneString :: ToScene String Unit EmptyAnalysers
  where
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
    buffer <- toAffE $ decodeAudioDataFromUri audioCtx (Record.get (Proxy :: _ key) a)
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

newtype FullSceneBuilder trigger world analyserCallbacks res =
  FullSceneBuilder
    { triggerWorld :: AudioContext /\ Aff (Event {} /\ Behavior {}) -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
    , piece :: Scene (SceneI { | trigger } { | world } analyserCallbacks) RunAudio RunEngine Frame0 res
    }

using
  :: forall trigger world scene graph
   . Create scene () graph
  => GraphIsRenderable graph
  => Change scene graph
  => ( AudioContext /\ Aff (Event {} /\ Behavior {})
       -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
     )
  -> (SceneI { | trigger } { | world } () -> { | scene })
  -> FullSceneBuilder trigger world EmptyAnalysers Unit
using triggerWorld piece = FullSceneBuilder
  { triggerWorld
  , piece: loopUsingScene (\x y -> { control: y, scene: piece x }) unit
  }

usingc
  :: forall trigger world scene graph control
   . Create scene () graph
  => GraphIsRenderable graph
  => Change scene graph
  => ( AudioContext /\ Aff (Event {} /\ Behavior {})
       -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
     )
  -> control
  -> (SceneI { | trigger } { | world } EmptyAnalysers -> control -> { scene :: { | scene }, control :: control })
  -> FullSceneBuilder trigger world EmptyAnalysers Unit
usingc triggerWorld control piece = FullSceneBuilder { triggerWorld, piece: loopUsingScene piece control }

buffers
  :: forall buffersS buffers trigger world
   . Lacks "buffers" world
  => MatchBuffers buffersS buffers
  => { | buffersS }
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { buffers :: { | buffers } | world })
buffers bf (ac /\ aff) = ac /\ do
  trigger /\ world <- aff
  b' <- b
  pure (trigger /\ (Record.insert (Proxy :: _ "buffers") b' <$> world))
  where
  b = getBuffers ac bf

makeFullSceneUsing
  :: forall analysersRL analysers analyserCallbacks analyserRefs trigger world res
   . RL.RowToList analysers analysersRL
  => AnalyserRefs analysersRL analyserRefs
  => MakeAnalyserCallbacks analysersRL analyserRefs analyserCallbacks
  => Analysers analysersRL analyserRefs analysers
  => Monoid res
  => FullSceneBuilder trigger world analyserCallbacks res
  -> Aff { audioCtx :: AudioContext, event :: Event (Run res analysers) }
makeFullSceneUsing (FullSceneBuilder { triggerWorld, piece }) = makeFullScene $ FullScene
  { triggerWorld: \audioContext -> snd $ triggerWorld (audioContext /\ pure (pure {} /\ pure {}))
  , piece
  }

instance toSceneFullSceneUsing ::
  ( RL.RowToList analysers analysersRL
  , AnalyserRefs analysersRL analyserRefs
  , MakeAnalyserCallbacks analysersRL analyserRefs analyserCallbacks
  , Analysers analysersRL analyserRefs analysers
  , Monoid res
  ) =>
  ToScene (FullSceneBuilder trigger world analyserCallbacks res) res analysers
  where
  toScene = makeFullSceneUsing

makeFullScene
  :: forall analysersRL analysers analyserCallbacks analyserRefs trigger world res
   . RL.RowToList analysers analysersRL
  => AnalyserRefs analysersRL analyserRefs
  => MakeAnalyserCallbacks analysersRL analyserRefs analyserCallbacks
  => Analysers analysersRL analyserRefs analysers
  => Monoid res
  => FullScene trigger world analyserCallbacks res
  -> Aff { audioCtx :: AudioContext, event :: Event (Run res analysers) }
makeFullScene (FullScene { triggerWorld, piece }) = do
  audioCtx <- liftEffect context
  unitCache <- liftEffect makeUnitCache
  trigger /\ world <- triggerWorld audioCtx
  pure { audioCtx, event: run trigger world { easingAlgorithm } (defaultFFIAudio audioCtx unitCache) piece }

instance toSceneFullScene ::
  ( RL.RowToList analysers analysersRL
  , AnalyserRefs analysersRL analyserRefs
  , MakeAnalyserCallbacks analysersRL analyserRefs analyserCallbacks
  , Analysers analysersRL analyserRefs analysers
  , Monoid res
  ) =>
  ToScene (FullScene trigger world analyserCallbacks res) res analysers
  where
  toScene = makeFullScene

type State
  =
  { unsubscribe :: Effect Unit
  , audioCtx :: Maybe AudioContext
  , playing :: Boolean
  }

data Action
  = StartAudio
  | StopAudio

component
  :: forall toScene res analysers query input output m
   . MonadEffect m
  => MonadAff m
  => ToScene toScene res analysers
  => toScene
  -> H.Component query input output m
component i =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction (toScene i), finalize = Just StopAudio }
    }

initialState :: forall input. input -> State
initialState _ =
  { unsubscribe: pure unit
  , audioCtx: Nothing
  , playing: false
  }

render :: forall m. State -> H.ComponentHTML Action () m
render { playing } = do
  HH.div_
    $
      if playing then
        [ HH.button
            [ HE.onClick \_ -> StopAudio ]
            [ HH.text "Stop audio" ]
        ]
      else
        [ HH.button
            [ HE.onClick \_ -> StartAudio ]
            [ HH.text "Start audio" ]
        ]

handleAction
  :: forall res analysers output m
   . MonadEffect m
  => MonadAff m
  => Aff { audioCtx :: AudioContext, event :: Event (Run res analysers) }
  -> Action
  -> H.HalogenM State Action () output m Unit
handleAction aff = case _ of
  StartAudio -> do
    { audioCtx, event } <- H.liftAff aff
    unsubscribe <- H.liftEffect $ subscribe event (\(_ :: Run res analysers) -> pure unit)
    H.modify_ _ { unsubscribe = unsubscribe, audioCtx = Just audioCtx, playing = true }
  StopAudio -> do
    { unsubscribe, audioCtx } <- H.get
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _ { unsubscribe = pure unit, audioCtx = Nothing, playing = false }

play
  :: forall toScene res analysers
   . ToScene toScene res analysers
  => toScene
  -> Effect Unit
play toScene = runHalogenAff do
  body <- awaitBody
  runUI (component toScene) unit body