module WAGS.Lib.Learn where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Promise (toAffE)
import Data.Array as A
import Data.Filterable (filter)
import Data.Foldable (for_)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.Lens (_1, over)
import Data.List (List(..), (:))
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (under)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (lcmap)
import Data.Symbol (class IsSymbol)
import Data.Traversable (sequence)
import Data.Tuple (snd, uncurry)
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
import WAGS.Lib.BufferPool (AScoredBufferPool, Buffy(..), makeBufferPoolWithAnchor)
import WAGS.Lib.Cofree (identityToMoore)
import WAGS.Lib.Piecewise (makePiecewise)
import WAGS.Lib.Score (makeScore)
import WAGS.Lib.Stream (deadEnd, stops)
import WAGS.Run (Run, RunAudio, RunEngine, SceneI(..), run)
import WAGS.Template (fromTemplate)
import WAGS.Validation (class GraphIsRenderable)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

-- global fast forward to avoid clicks and pops
gff :: AudioParameter_ ~> AudioParameter_
gff = ff 0.03

class ToScene a res | a -> res where
  toScene :: a -> Aff { audioCtx :: AudioContext, event :: Event (Run res EmptyAnalysers) }

newtype FullScene trigger world res =
  FullScene
    { triggerWorld :: AudioContext -> Aff (Event { | trigger } /\ Behavior { | world })
    , piece :: Scene (SceneI { | trigger } { | world } EmptyAnalysers) RunAudio RunEngine Frame0 res
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

instance toSceneInt :: ToScene Int Unit
  where
  toScene = makeInt

makeNumber :: Number -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNumber = makeFunctionOfTimeNumber <<< const

instance toSceneNumber :: ToScene Number Unit
  where
  toScene = makeNumber

makeArrayInt :: Array Int -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayInt = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayInt (head :| tail)

instance toSceneArrayInt :: ToScene (Array Int) Unit
  where
  toScene = makeArrayInt

makeArrayScoreInt :: Array (Number /\ Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayScoreInt = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayScoreInt (head :| tail)

instance toSceneArrayScoreInt :: ToScene (Array (Number /\ Int)) Unit
  where
  toScene = makeArrayScoreInt

makeArrayNumber :: Array Number -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayNumber = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayNumber (head :| tail)

instance toSceneArrayNumber :: ToScene (Array Number) Unit
  where
  toScene = makeArrayNumber

makeArrayScoreNumber :: Array (Number /\ Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayScoreNumber = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayScoreNumber (head :| tail)

instance toSceneArrayScoreNumber :: ToScene (Array (Number /\ Number)) Unit
  where
  toScene = makeArrayScoreNumber

makeNonEmptyArrayInt :: NonEmpty Array Int -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayInt = makeNonEmptyArrayNumber <<< map (midiToCps <<< toNumber)

instance toSceneNonEmptyArrayInt :: ToScene (NonEmpty Array Int) Unit
  where
  toScene = makeNonEmptyArrayInt

makeNonEmptyArrayScoreInt :: NonEmpty Array (Number /\ Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayScoreInt = makeNonEmptyArrayScoreNumber <<< (map <<< map) (midiToCps <<< toNumber)

instance toSceneNonEmptyArrayScoreInt :: ToScene (NonEmpty Array (Number /\ Int)) Unit
  where
  toScene = makeNonEmptyArrayScoreInt

makeNonEmptyArrayNumber :: NonEmpty Array Number -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayNumber = makeCofreeMaybeNumber <<< stops

instance toSceneNonEmptyArrayNumber :: ToScene (NonEmpty Array Number) Unit
  where
  toScene = makeNonEmptyArrayNumber

makeNonEmptyArrayScoreNumber :: NonEmpty Array (Number /\ Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayScoreNumber (a :| b) = makeNonEmptyListScoreNumber (a :| L.fromFoldable b)

instance toSceneNonEmptyScoreArrayNumber :: ToScene (NonEmpty Array (Number /\ Number)) Unit
  where
  toScene = makeNonEmptyArrayScoreNumber

makeListInt :: List Int -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeListInt = L.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyListInt (head :| tail)

instance toSceneListInt :: ToScene (List Int) Unit
  where
  toScene = makeListInt

makeListScoreInt :: List (Number /\ Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeListScoreInt = L.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyListScoreInt (head :| tail)

instance toSceneListScoreInt :: ToScene (List (Number /\ Int)) Unit
  where
  toScene = makeListScoreInt

makeListNumber :: List Number -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeListNumber = L.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyListNumber (head :| tail)

instance toSceneListNumber :: ToScene (List Number) Unit
  where
  toScene = makeListNumber

makeListScoreNumber :: List (Number /\ Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeListScoreNumber = L.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyListScoreNumber (head :| tail)

instance toSceneListScoreNumber :: ToScene (List (Number /\ Number)) Unit
  where
  toScene = makeListScoreNumber

makeNonEmptyListInt :: NonEmpty List Int -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyListInt = makeNonEmptyListNumber <<< map (midiToCps <<< toNumber)

instance toSceneNonEmptyListInt :: ToScene (NonEmpty List Int) Unit
  where
  toScene = makeNonEmptyListInt

makeNonEmptyListScoreInt :: NonEmpty List (Number /\ Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyListScoreInt = makeNonEmptyListScoreNumber <<< (map <<< map) (midiToCps <<< toNumber)

instance toSceneNonEmptyListScoreInt :: ToScene (NonEmpty List (Number /\ Int)) Unit
  where
  toScene = makeNonEmptyListScoreInt

makeNonEmptyListNumber :: NonEmpty List Number -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyListNumber = makeCofreeMaybeNumber <<< stops

instance toSceneNonEmptyListNumber :: ToScene (NonEmpty List Number) Unit
  where
  toScene = makeNonEmptyListNumber

makeNonEmptyListScoreNumber :: NonEmpty List (Number /\ Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyListScoreNumber = makeCofreeScoreMaybeNumber <<< (deadEnd <<< under NonEmptyList (flip NEL.snoc (1.0 /\ Nothing)) <<< map (map Just))

instance toSceneNonEmptyScoreListNumber :: ToScene (NonEmpty List (Number /\ Number)) Unit
  where
  toScene = makeNonEmptyListScoreNumber

makeCofreeInt :: Cofree Identity Int -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeInt = makeCofreeNumber <<< map (midiToCps <<< toNumber)

instance toSceneCofreeInt :: ToScene (Cofree Identity Int) Unit
  where
  toScene = makeCofreeInt

makeCofreeScoreInt :: Cofree Identity (Number /\ Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeScoreInt = makeCofreeScoreNumber <<< (map <<< map) (midiToCps <<< toNumber)

instance toSceneCofreeScoreInt :: ToScene (Cofree Identity (Number /\ Int)) Unit
  where
  toScene = makeCofreeScoreInt

makeCofreeNumber :: Cofree Identity Number -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeNumber = makeCofreeFunctionOfTimeNumber <<< map const

instance toSceneCofreeNumber :: ToScene (Cofree Identity Number) Unit
  where
  toScene = makeCofreeNumber

makeCofreeScoreNumber :: Cofree Identity (Number /\ Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeScoreNumber = makeCofreeFunctionOfTimeScoreNumber <<< map const

instance toSceneCofreeScoreNumber :: ToScene (Cofree Identity (Number /\ Number)) Unit
  where
  toScene = makeCofreeScoreNumber

makeCofreeMaybeNumber :: Cofree Identity (Maybe Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeMaybeNumber = makeCofreeFunctionOfTimeMaybeNumber <<< map const

instance toSceneCofreeMaybeNumber :: ToScene (Cofree Identity (Maybe Number)) Unit
  where
  toScene = makeCofreeMaybeNumber

makeCofreeScoreMaybeInt :: Cofree Identity (Number /\ Maybe Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeScoreMaybeInt = makeCofreeFunctionOfTimeScoreMaybeInt <<< map const

instance toSceneCofreeScoreMaybeInt :: ToScene (Cofree Identity (Number /\ Maybe Int)) Unit
  where
  toScene = makeCofreeScoreMaybeInt

makeCofreeScoreMaybeNumber :: Cofree Identity (Number /\ Maybe Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeScoreMaybeNumber = makeCofreeFunctionOfTimeScoreMaybeNumber <<< map const

instance toSceneCofreeScoreMaybeNumber :: ToScene (Cofree Identity (Number /\ Maybe Number)) Unit
  where
  toScene = makeCofreeScoreMaybeNumber

makeFunctionOfTimeInt :: (Number -> Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeFunctionOfTimeInt = makeFunctionOfTimeNumber <<< map (midiToCps <<< toNumber)

instance toSceneFunctionOfTimeInt :: ToScene (Number -> Int) Unit
  where
  toScene = makeFunctionOfTimeInt

makeFunctionOfTimeNumber :: (Number -> Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeFunctionOfTimeNumber f = makeFullScene $ FullScene
  { triggerWorld: defaultTriggerWorld
  , piece: (\(SceneI { time }) -> icreate (speaker { gain: gain (gff $ pure 0.3) (sinOsc (gff $ pure $ f time)) }) $> unit) @!> freeze
  }

instance toSceneFunctionOfTimeNumber :: ToScene (Number -> Number) Unit
  where
  toScene = makeFunctionOfTimeNumber

makeArrayFunctionOfTimeInt :: Array (Number -> Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayFunctionOfTimeInt = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayFunctionOfTimeInt (head :| tail)

instance toSceneArrayFunctionOfTimeInt :: ToScene (Array (Number -> Int)) Unit
  where
  toScene = makeArrayFunctionOfTimeInt

makeArrayFunctionOfTimeNumber :: Array (Number -> Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayFunctionOfTimeNumber = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayFunctionOfTimeNumber (head :| tail)

instance toSceneArrayFunctionOfTimeNumber :: ToScene (Array (Number -> Number)) Unit
  where
  toScene = makeArrayFunctionOfTimeNumber

makeNonEmptyArrayFunctionOfTimeInt :: NonEmpty Array (Number -> Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayFunctionOfTimeInt = makeNonEmptyArrayFunctionOfTimeNumber <<< (map <<< map) (midiToCps <<< toNumber)

instance toSceneNonEmptyArrayFunctionOfTimeInt :: ToScene (NonEmpty Array (Number -> Int)) Unit
  where
  toScene = makeNonEmptyArrayFunctionOfTimeInt

makeNonEmptyArrayFunctionOfTimeNumber :: NonEmpty Array (Number -> Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayFunctionOfTimeNumber (a :| b) = makeNonEmptyListFunctionOfTimeNumber (a :| L.fromFoldable b)

instance toSceneNonEmptyArrayFunctionOfTimeNumber :: ToScene (NonEmpty Array (Number -> Number)) Unit
  where
  toScene = makeNonEmptyArrayFunctionOfTimeNumber

makeNonEmptyListFunctionOfTimeInt :: NonEmpty List (Number -> Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyListFunctionOfTimeInt = makeNonEmptyListFunctionOfTimeNumber <<< (map <<< map) (midiToCps <<< toNumber)

instance toScenemakeNonEmptyListFunctionOfTimeInt :: ToScene (NonEmpty List (Number -> Int)) Unit
  where
  toScene = makeNonEmptyListFunctionOfTimeInt

makeNonEmptyListFunctionOfTimeNumber :: NonEmpty List (Number -> Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyListFunctionOfTimeNumber = makeCofreeFunctionOfTimeMaybeNumber <<< map sequence <<< stops

instance toSceneNonEmptyListFunctionOfTimeNumber :: ToScene (NonEmpty List (Number -> Number)) Unit
  where
  toScene = makeNonEmptyListFunctionOfTimeNumber

makeCofreeFunctionOfTimeInt :: Cofree Identity (Number -> Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeFunctionOfTimeInt = makeCofreeFunctionOfTimeNumber <<< (map <<< map) (midiToCps <<< toNumber)

instance toSceneCofreeFunctionOfTimeInt :: ToScene (Cofree Identity (Number -> Int)) Unit
  where
  toScene = makeCofreeFunctionOfTimeInt

noSine :: CTOR.Gain AudioParameter /\ (CTOR.SinOsc APOnOff AudioParameter /\ {})
noSine = gain 0.0 (sinOsc 0.1)

makeCofreeFunctionOfTimeNumber :: Cofree Identity (Number -> Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeFunctionOfTimeNumber = makeCofreeFunctionOfTimeMaybeNumber <<< ((map <<< map) Just)

instance toSceneCofreeFunctionOfTimeNumber :: ToScene (Cofree Identity (Number -> Number)) Unit
  where
  toScene = makeCofreeFunctionOfTimeNumber

makeCofreeFunctionOfTimeMaybeInt:: Cofree Identity (Number -> Maybe Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeFunctionOfTimeMaybeInt = makeCofreeFunctionOfTimeScoreMaybeInt <<< (map <<< map) ((/\) 1.0)

instance toSceneCofreeFunctionOfTimeMaybeInt :: ToScene (Cofree Identity (Number -> Maybe Int)) Unit
  where
  toScene = makeCofreeFunctionOfTimeMaybeInt

makeCofreeFunctionOfTimeMaybeNumber :: Cofree Identity (Number -> Maybe Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeFunctionOfTimeMaybeNumber = makeCofreeFunctionOfTimeScoreMaybeNumber <<< (map <<< map) ((/\) 1.0)

instance toSceneCofreeFunctionOfTimeMaybeNumber :: ToScene (Cofree Identity (Number -> Maybe Number)) Unit
  where
  toScene = makeCofreeFunctionOfTimeMaybeNumber

makeCofreeFunctionOfTimeScoreNumber :: Cofree Identity (Number -> Number /\ Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeFunctionOfTimeScoreNumber = makeCofreeFunctionOfTimeScoreMaybeNumber <<< (map <<< map <<< map) Just

instance toSceneCofreeFunctionOfTimeScoreNumber :: ToScene (Cofree Identity (Number -> Number /\ Number)) Unit
  where
  toScene = makeCofreeFunctionOfTimeScoreNumber


makeCofreeFunctionOfTimeScoreMaybeInt:: Cofree Identity (Number -> Number /\ Maybe Int) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeFunctionOfTimeScoreMaybeInt = makeCofreeFunctionOfTimeScoreMaybeNumber <<< (map <<< map <<< map <<< map) (midiToCps <<< toNumber)

instance toSceneCofreeFunctionOfTimeScoreMaybeInt :: ToScene (Cofree Identity (Number -> Number /\ Maybe Int)) Unit
  where
  toScene = makeCofreeFunctionOfTimeScoreMaybeInt

makeCofreeFunctionOfTimeScoreMaybeNumber :: Cofree Identity (Number -> Number /\ Maybe Number) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeFunctionOfTimeScoreMaybeNumber notes' = makeFullScene $ FullScene
  { triggerWorld: defaultTriggerWorld
  , piece: loopUsingScene
      ( \(SceneI { time, headroomInSeconds: headroomInSeconds }) { oscSimple } ->
          let
            actualized = oscSimple { time, headroomInSeconds }
          in
            { control: { oscSimple: unwrapCofree actualized }
            , scene: speaker
                { piece: fromTemplate (Proxy :: _ "sinOsc") (extract actualized) \_ -> case _ of
                    Just (Buffy { startTime, rest: pitch }) -> pitch # maybe noSine \pitch' -> gain
                      (gff (makePiecewise (map (over _1 (add startTime)) (pwl 1.0)) { time, headroomInSeconds }))
                      (sinOsc (gff $ pure $ pitch'))

                    Nothing -> noSine
                }
            }
      )
      { oscSimple: emitter }
  }
  where
  pwl dr = ((0.0 /\ 0.0) :| ((min (dr * 0.3) 0.1) /\ 0.6) : ((min (dr * 0.45) 0.3) /\ 0.2) : (dr /\ 0.0) : Nil)

  emitter :: AScoredBufferPool D4 (Maybe Number)
  emitter = makeBufferPoolWithAnchor ((map <<< map) (filter (isJust <<< _.rest)) (makeScore { startsAt: 0.0, rest: (map <<< map) (uncurry { duration: _, rest: _ }) (identityToMoore (map (lcmap _.time) notes')) }))

instance toSceneCofreeFunctionOfTimeScoreMaybeNumber :: ToScene (Cofree Identity (Number -> Number /\ Maybe Number)) Unit
  where
  toScene = makeCofreeFunctionOfTimeScoreMaybeNumber

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
  ToScene { | scene } Unit
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
  ToScene (Number -> { | scene }) Unit
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
  ToScene (WithControl control scene) Unit
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

instance toSceneString :: ToScene String Unit
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

newtype FullSceneBuilder trigger world res =
  FullSceneBuilder
    { triggerWorld :: AudioContext /\ Aff (Event {} /\ Behavior {}) -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
    , piece :: Scene (SceneI { | trigger } { | world } EmptyAnalysers) RunAudio RunEngine Frame0 res
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
  -> FullSceneBuilder trigger world Unit
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
  -> FullSceneBuilder trigger world Unit
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
  :: forall trigger world res
   . Monoid res
  => FullSceneBuilder trigger world res
  -> Aff { audioCtx :: AudioContext, event :: Event (Run res EmptyAnalysers) }
makeFullSceneUsing (FullSceneBuilder { triggerWorld, piece }) = makeFullScene $ FullScene
  { triggerWorld: \audioContext -> snd $ triggerWorld (audioContext /\ pure (pure {} /\ pure {}))
  , piece
  }

instance toSceneFullSceneUsing ::
  ( Monoid res
  ) =>
  ToScene (FullSceneBuilder trigger world res) res
  where
  toScene = makeFullSceneUsing

makeFullScene
  :: forall trigger world res
   . Monoid res
  => FullScene trigger world res
  -> Aff { audioCtx :: AudioContext, event :: Event (Run res EmptyAnalysers) }
makeFullScene (FullScene { triggerWorld, piece }) = do
  audioCtx <- liftEffect context
  unitCache <- liftEffect makeUnitCache
  trigger /\ world <- triggerWorld audioCtx
  pure { audioCtx, event: run trigger world { easingAlgorithm } (defaultFFIAudio audioCtx unitCache) piece }

instance toSceneFullScene ::
  ( Monoid res
  ) =>
  ToScene (FullScene trigger world res) res
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
  :: forall toScene res
   . ToScene toScene res
  => toScene
  -> Effect Unit
play toScene = runHalogenAff do
  body <- awaitBody
  runUI (component toScene) unit body