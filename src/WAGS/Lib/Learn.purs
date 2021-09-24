module WAGS.Lib.Learn where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Promise (toAffE)
import Data.Array as A
import Data.Foldable (for_)
import Data.Identity (Identity(..))
import Data.Lens (_1, over, set)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (lcmap)
import Data.Symbol (class IsSymbol)
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
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Change (class Change)
import WAGS.Control.Functions.Validated (loopUsingScene)
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (class Create)
import WAGS.Create.Optionals (constant, gain, sinOsc, playBuf, speaker)
import WAGS.Graph.AudioUnit (APOnOff)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Parameter (AudioParameter_, AudioParameter, ff)
import WAGS.Interpret (close, context, decodeAudioDataFromUri, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.BufferPool (AScoredBufferPool, Buffy(..), makeBufferPoolWithAnchor)
import WAGS.Lib.Cofree (annihalateIdentity, hoistHoistCofree)
import WAGS.Lib.Learn.Duration (Duration(..), Rest(..))
import WAGS.Lib.Learn.Note (Note(..), NoteOrRest, Sequenced(..), noteFromDefaults_, noteFromPitch_, noteStreamToSequence, seq)
import WAGS.Lib.Learn.Pitch (Pitch(..))
import WAGS.Lib.Learn.Volume (Volume(..))
import WAGS.Lib.Piecewise (APFofT, makePiecewise)
import WAGS.Lib.Score (makeScore)
import WAGS.Lib.Stream (deadEnd)
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

makePitch :: Pitch -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makePitch = makeFunctionOfTimePitch <<< const

instance toScenePitch :: ToScene Pitch Unit
  where
  toScene = makePitch

makeNote :: Note -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNote = makeFunctionOfTimeNote <<< const

instance toSceneNote :: ToScene Note Unit
  where
  toScene = makeNote

makeArrayPitch :: Array Pitch -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayPitch = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayPitch (head :| tail)

instance toSceneArrayPitch :: ToScene (Array Pitch) Unit
  where
  toScene = makeArrayPitch

makeArrayNote :: Array Note -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayNote = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayNote (head :| tail)

instance toSceneArrayNote :: ToScene (Array Note) Unit
  where
  toScene = makeArrayNote

makeArrayNoteOrRest :: Array NoteOrRest -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayNoteOrRest = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayNoteOrRest (head :| tail)

instance toSceneArrayNoteOrRest :: ToScene (Array NoteOrRest) Unit
  where
  toScene = makeArrayNoteOrRest

makeArraySequencedNote :: Array (Sequenced Note) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArraySequencedNote = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArraySequencedNote (head :| tail)

instance toSceneArraySequencedNote :: ToScene (Array (Sequenced Note)) Unit
  where
  toScene = makeArraySequencedNote

makeNonEmptyArrayPitch :: NonEmpty Array Pitch -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayPitch = makeNonEmptyArrayNote <<< map (noteFromDefaults_ <<< set (prop (Proxy :: _ "pitch")))

instance toSceneNonEmptyArrayPitch :: ToScene (NonEmpty Array Pitch) Unit
  where
  toScene = makeNonEmptyArrayPitch

makeNonEmptyArrayNote :: NonEmpty Array Note -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayNote = makeNonEmptyArraySequencedNote <<< map unwrap <<< seq <<< map Identity

instance toSceneNonEmptyArrayNote :: ToScene (NonEmpty Array Note) Unit
  where
  toScene = makeNonEmptyArrayNote

makeNonEmptyArrayNoteOrRest :: NonEmpty Array NoteOrRest -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayNoteOrRest = makeArraySequencedNote <<< map unwrap <<< seq <<< map Identity

instance toSceneNonEmptyArrayNoteOrRest :: ToScene (NonEmpty Array NoteOrRest) Unit
  where
  toScene = makeNonEmptyArrayNoteOrRest

makeNonEmptyArraySequencedNote :: NonEmpty Array (Sequenced Note) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArraySequencedNote (a :| b) = makeCofreeSequencedNote
  $ deadEnd
  $ a :| b <> [ Sequenced { startsAfter: top, note: noteFromDefaults_ identity } ]

instance toSceneNonEmptyArraySequencedNote :: ToScene (NonEmpty Array (Sequenced Note)) Unit
  where
  toScene = makeNonEmptyArraySequencedNote

makeCofreePitch :: Cofree Identity Pitch -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreePitch = makeCofreeFunctionOfTimePitch <<< map const

instance toSceneCofreePitch :: ToScene (Cofree Identity Pitch) Unit
  where
  toScene = makeCofreePitch

makeCofreeNote :: Cofree Identity Note -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeNote = makeCofreeFunctionOfTimeNote <<< map const

instance toSceneCofreeNote :: ToScene (Cofree Identity Note) Unit
  where
  toScene = makeCofreeNote

makeCofreeSequencedNote :: Cofree Identity (Sequenced Note) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeSequencedNote = makeFunctionCofreeFunctionOfTimeSequencedNote <<< annihalateIdentity <<< map const

instance toSceneCofreeSequencedNote :: ToScene (Cofree Identity (Sequenced Note)) Unit
  where
  toScene = makeCofreeSequencedNote

makeFunctionOfTimePitch :: (Number -> Pitch) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeFunctionOfTimePitch = makeFunctionOfTimeNote <<< map noteFromPitch_

instance toSceneFunctionOfTimePitch :: ToScene (Number -> Pitch) Unit
  where
  toScene = makeFunctionOfTimePitch

makeFunctionOfTimeNote :: (Number -> Note) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeFunctionOfTimeNote f = makeFunctionOfTimeLoop \time -> let Note n = f time in speaker { gain: gain (gff $ pure (if time < unwrap n.duration then unwrap n.volume else 0.0)) (sinOsc (gff $ pure $ unwrap $ n.pitch)) }

instance toSceneFunctionOfTimeNote :: ToScene (Number -> Note) Unit
  where
  toScene = makeFunctionOfTimeNote

makeArrayFunctionOfTimePitch :: Array (Number -> Pitch) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeArrayFunctionOfTimePitch = A.uncons >>> case _ of
  Nothing -> nothing
  Just { head, tail } -> makeNonEmptyArrayFunctionOfTimePitch (head :| tail)

instance toSceneArrayFunctionOfTimePitch :: ToScene (Array (Number -> Pitch)) Unit
  where
  toScene = makeArrayFunctionOfTimePitch

makeNonEmptyArrayFunctionOfTimePitch :: NonEmpty Array (Number -> Pitch) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayFunctionOfTimePitch = makeNonEmptyArrayFunctionOfTimeNote <<< (map <<< map) (noteFromDefaults_ <<< set (prop (Proxy :: _ "pitch")))

instance toSceneNonEmptyArrayFunctionOfTimePitch :: ToScene (NonEmpty Array (Number -> Pitch)) Unit
  where
  toScene = makeNonEmptyArrayFunctionOfTimePitch

makeNonEmptyArrayFunctionOfTimeNote :: NonEmpty Array (Number -> Note) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayFunctionOfTimeNote = makeNonEmptyArrayFunctionOfTimeSequencedNote <<< seq

instance toSceneNonEmptyArrayFunctionOfTimeNote :: ToScene (NonEmpty Array (Number -> Note)) Unit
  where
  toScene = makeNonEmptyArrayFunctionOfTimeNote

makeNonEmptyArrayFunctionOfTimeSequencedNote :: NonEmpty Array (Number -> Sequenced Note) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeNonEmptyArrayFunctionOfTimeSequencedNote (a :| b) = makeFunctionCofreeFunctionOfTimeSequencedNote
  $ annihalateIdentity
  $ deadEnd
  $ a :| b <> [ const $ Sequenced { startsAfter: top, note: noteFromDefaults_ identity } ]

instance toSceneNonEmptyArrayFunctionOfTimeSequencedNote :: ToScene (NonEmpty Array (Number -> Sequenced Note)) Unit
  where
  toScene = makeNonEmptyArrayFunctionOfTimeSequencedNote

noSine :: CTOR.Gain AudioParameter /\ (CTOR.SinOsc APOnOff AudioParameter /\ {})
noSine = gain 0.0 (sinOsc 0.1)

makeCofreeFunctionOfTimePitch :: Cofree Identity (Number -> Pitch) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeFunctionOfTimePitch = makeCofreeFunctionOfTimeNote <<< (map <<< map) (noteFromDefaults_ <<< set (prop (Proxy :: _ "pitch")))

instance toSceneCofreeFunctionOfTimePitch :: ToScene (Cofree Identity (Number -> Pitch)) Unit
  where
  toScene = makeCofreeFunctionOfTimePitch

makeCofreeFunctionOfTimeNote :: Cofree Identity (Number -> Note) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeCofreeFunctionOfTimeNote = makeFunctionCofreeFunctionOfTimeSequencedNote <<< map (noteStreamToSequence (Rest 0.0)) <<< annihalateIdentity

instance toSceneCofreeFunctionOfTimeNote :: ToScene (Cofree Identity (Number -> Note)) Unit
  where
  toScene = makeCofreeFunctionOfTimeNote

makeFunctionCofreeFunctionOfTimeSequencedNote :: (Number -> Cofree ((->) Number) (Sequenced Note)) -> Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
makeFunctionCofreeFunctionOfTimeSequencedNote notes' = makeFullScene $ FullScene
  { triggerWorld: defaultTriggerWorld
  , piece: loopUsingScene
      ( \(SceneI { time, headroomInSeconds: headroomInSeconds }) { oscSimple } ->
          let
            actualized = oscSimple { time, headroomInSeconds }
          in
            { control: { oscSimple: unwrapCofree actualized }
            , scene: speaker
                { piece: fromTemplate (Proxy :: _ "sinOsc") (extract actualized) \_ -> case _ of
                    Just (Buffy { rest: { note: Note { pitch: Pitch pitch }, pw } }) -> gain
                      (gff (pw { time, headroomInSeconds }))
                      (sinOsc (gff $ pure $ pitch))

                    Nothing -> noSine
                }
            }
      )
      { oscSimple: emitter }
  }
  where
  pwl dr v = ((0.0 /\ 0.0) :| ((min (dr * 0.3) 0.1) /\ v) : ((min (dr * 0.45) 0.3) /\ v * 0.3) : (dr /\ 0.0) : Nil)

  emitter :: AScoredBufferPool D4 ({ note :: Note, pw :: APFofT Number })
  emitter = makeBufferPoolWithAnchor
    ( (map <<< map <<< map)
        (\{ offset, rest: note@(Note { duration: Duration d, volume: Volume v }) } -> { duration: const $ const $ const (Just d), offset, rest: \st -> let pw = makePiecewise (map (over _1 (add st)) (pwl d v)) in { note, pw } })
        (makeScore { startsAt: 0.0, cf: (map <<< map) (\(Sequenced { startsAfter: Rest s, note }) -> { startsAfter: s, rest: note }) (hoistHoistCofree (lcmap _.time) notes') })
    )

instance toSceneFunctionCofreeFunctionOfTimeSequencedNote :: ToScene (Number -> Cofree ((->) Number) (Sequenced Note)) Unit
  where
  toScene = makeFunctionCofreeFunctionOfTimeSequencedNote

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

buttonCSS :: String -> String
buttonCSS rgb = """align-items:flex-start;appearance:none;background-color:""" <> rgb <> """;background-image:none;border-bottom-color:rgb(229, 231, 235);border-bottom-left-radius:8px;border-bottom-right-radius:8px;border-bottom-style:solid;border-bottom-width:0px;border-image-outset:0;border-image-repeat:stretch;border-image-slice:100%;border-image-source:none;border-image-width:1;border-left-color:rgb(229, 231, 235);border-left-style:solid;border-left-width:0px;border-right-color:rgb(229, 231, 235);border-right-style:solid;border-right-width:0px;border-top-color:rgb(229, 231, 235);border-top-left-radius:8px;border-top-right-radius:8px;border-top-style:solid;border-top-width:0px;box-sizing:border-box;color:rgb(255, 255, 255);cursor:pointer;display:block;font-family:ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe:UI", Roboto, "Helvetica:Neue", Arial, "Noto:Sans", sans-serif, "Apple:Color:Emoji", "Segoe:UI:Emoji", "Segoe:UI:Symbol", "Noto:Color:Emoji";font-size:24px;font-stretch:100%;font-style:normal;font-variant-caps:normal;font-variant-east-asian:normal;font-variant-ligatures:normal;font-variant-numeric:normal;font-weight:400;height:56px;letter-spacing:normal;line-height:32px;margin-bottom:20px;margin-left:20px;margin-right:20px;margin-top:20px;padding-bottom:12px;padding-left:12px;padding-right:12px;padding-top:12px;tab-size:4;text-align:center;text-indent:0px;text-rendering:auto;text-shadow:none;text-size-adjust:100%;text-transform:none;width:172.5px;word-spacing:0px;writing-mode:horizontal-tb;-webkit-border-image:none;"""

onCSS = buttonCSS "rgb(99, 102, 241)" :: String
offCSS = buttonCSS "rgb(236, 72, 153)" :: String

containerCss = "display:-ms-flexbox;display:-webkit-flex;display:flex;-webkit-flex-direction:row;-ms-flex-direction:row;flex-direction:row;-webkit-flex-wrap:nowrap;-ms-flex-wrap:nowrap;flex-wrap:nowrap;-webkit-justify-content:center;-ms-flex-pack:center;justify-content:center;-webkit-align-content:stretch;-ms-flex-line-pack:stretch;align-content:stretch;-webkit-align-items:center;-ms-flex-align:center;align-items:center;" :: String

flex1Css = "-webkit-order:0;-ms-flex-order: 0;order: 0;-webkit-flex: 1 1 auto;-ms-flex: 1 1 auto;flex: 1 1 auto;-webkit-align-self: auto;-ms-flex-item-align: auto;align-self: auto;" :: String
flex2Css = "-webkit-order: 0;-ms-flex-order: 0;order: 0;-webkit-flex: 0 1 auto;-ms-flex: 0 1 auto;flex: 0 1 auto;-webkit-align-self: auto;-ms-flex-item-align: auto;align-self: auto;" :: String
flex3Css = "-webkit-order: 0;-ms-flex-order: 0;order: 0;-webkit-flex: 1 1 auto;-ms-flex: 1 1 auto;flex: 1 1 auto;-webkit-align-self: auto;-ms-flex-item-align: auto;align-self: auto;" :: String

render :: forall m. State -> H.ComponentHTML Action () m
render { playing } =
  if playing then
    HH.button
      [ HE.onClick \_ -> StopAudio, HP.style offCSS ]
      [ HH.text "Stop audio" ]

  else
    HH.button
      [ HE.onClick \_ -> StartAudio, HP.style onCSS ]
      [ HH.text "Start audio" ]

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

---
type ParentSlots = (audio :: forall q. H.Slot q Void Unit)

_audio = Proxy :: Proxy "audio"

type ParentState = {}

type ParentAction = Void

flexTop = """width:100%;height:100%;flex-direction:column;display:flex;""" :: String
flexRows = "flex-grow:1;display:block;" :: String

parent :: forall query input output m. (forall query2 input2 output2. H.Component query2 input2 output2 m) -> H.Component query input output m
parent audio =
  H.mkComponent
    { initialState: iS
    , render: rdr
    , eval: H.mkEval $ H.defaultEval
        { handleAction = absurd }
    }
  where
  iS :: input -> ParentState
  iS _ = {}

  rdr :: ParentState -> H.ComponentHTML ParentAction ParentSlots m
  rdr _ = HH.div [ HP.style containerCss ]
    [ HH.div [ HP.style flex1Css ] []
    , HH.div [ HP.style flex2Css ] [ HH.slot_ _audio unit audio unit ]
    , HH.div [ HP.style flex3Css ] []
    ]

play
  :: forall toScene res
   . ToScene toScene res
  => toScene
  -> Effect Unit
play toScene = runHalogenAff do
  body <- awaitBody
  runUI (parent (component toScene)) unit body