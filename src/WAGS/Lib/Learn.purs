module WAGS.Lib.Learn where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Array as A
import Data.Foldable (for_)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.Lens (_1, over)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (D4)
import Data.Vec as V
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
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import WAGS.Control.Functions.Validated (freeze, loopUsingScene, (@!>))
import WAGS.Control.Types (Frame0, Scene)
import WAGS.Create (icreate)
import WAGS.Create.Optionals (constant, gain, sinOsc, speaker)
import WAGS.Graph.AudioUnit (APOnOff)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Parameter (AudioParameter_, AudioParameter, ff)
import WAGS.Interpret (close, context, defaultFFIAudio, makeUnitCache)
import WAGS.Lib.BufferPool (AHotBufferPool, AHotBufferPool', Buffy(..), makeHotBufferPool)
import WAGS.Lib.Piecewise (makePiecewise)
import WAGS.Lib.Stream (stops)
import WAGS.Lib.Vec (foldV)
import WAGS.Run (class AnalyserRefs, class Analysers, class MakeAnalyserCallbacks, Run, RunAudio, RunEngine, SceneI(..), run)
import WAGS.Template (fromTemplate)
import WAGS.WebAPI (AudioContext)

-- global fast forward to avoid clicks and pops
gff :: AudioParameter_ ~> AudioParameter_
gff = ff 0.03

class ToScene a res analysers | a -> res analysers where
  toScene :: a -> Aff { audioCtx :: AudioContext, event :: Event (Run res analysers) }

newtype FullScene trigger world analyserCallbacks res =
  FullScene
    { trigger :: Aff (Event trigger)
    , world :: Aff (Behavior world)
    , piece :: Scene (SceneI trigger world analyserCallbacks) RunAudio RunEngine Frame0 res
    }

type EmptyAnalysers :: forall k. Row k
type EmptyAnalysers = ()

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = initialTime :< \adj -> fOf $ max 20 (initialTime - adj)
  in
    fOf 20

nothing :: Aff { audioCtx :: AudioContext, event :: Event (Run Unit EmptyAnalysers) }
nothing = makeFullScene $ FullScene
  { trigger: pure (pure unit)
  , world: pure (pure unit)
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
  { trigger: pure (pure unit)
  , world: pure (pure unit)
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
  { trigger: pure (pure unit)
  , world: pure (pure unit)
  , piece: loopUsingScene
      ( \(SceneI { time, headroomInSeconds: headroom }) { oscSimple } ->
          let
            actualized = oscSimple { time, headroom, freq: 1.0 }
          in
            { control: { oscSimple: unwrapCofree actualized }
            , scene: speaker
                { piece: fromTemplate (Proxy :: _ "sinOsc") (extract actualized) \_ -> case _ of
                    Just (Buffy { startTime, pitch }) -> pitch time # maybe noSine \pitch' -> gain
                      (gff (makePiecewise (map (over _1 (add startTime)) pwl) { time, headroom }))
                      (sinOsc (gff $ pure $ pitch'))

                    Nothing -> noSine
                }
            }
      )
      { oscSimple: withNotes }
  }
  where
  fixedDuration = 1.0
  pwl = ((0.0 /\ 0.0) :| ((min (fixedDuration * 0.3) 0.1) /\ 1.0) : ((min (fixedDuration * 0.45) 0.3) /\ 0.2) : (fixedDuration /\ 0.0) : Nil)

  pool :: AHotBufferPool D4
  pool = makeHotBufferPool { startsAt: 0.0 }

  withNotes :: AHotBufferPool' D4 (duration :: Number, pitch :: Number -> Maybe Number)
  withNotes = pool # map \cf ->
    let
      go prevNotes notes cfcm =
        let
          vec = V.zipWithE { buffy: _, pnote: _ } (extract cfcm) prevNotes
          newCf /\ newVec = foldV
            ( \({ notes } /\ { buffy, pnote }) -> case buffy of
                Just (Buffy { starting, startTime }) ->
                  { notes:
                      if starting then
                        unwrap $ unwrapCofree notes
                      else notes
                  } /\
                    ( Just $ Buffy
                        { starting
                        , startTime
                        , pitch: if starting then extract notes else pnote
                        , duration: fixedDuration
                        }
                    )
                Nothing -> { notes } /\ Nothing
            )
            { notes }
            vec
        in
          newVec :<
            go
              ( map
                  (\(a /\ b) -> maybe b (unwrap >>> _.pitch) a)
                  (V.zipWithE (/\) newVec prevNotes)
              )
              newCf.notes <<< unwrapCofree cfcm

    in
      go (V.fill (const $ const Nothing)) notes' cf

instance toSceneCofreeFunctionOfTimeMaybeNumber :: ToScene (Cofree Identity (Number -> Maybe Number)) Unit EmptyAnalysers
  where
  toScene = makeCofreeFunctionOfTimeMaybeNumber

makeFullScene
  :: forall analysersRL analysers analyserCallbacks analyserRefs trigger world res
   . RL.RowToList analysers analysersRL
  => AnalyserRefs analysersRL analyserRefs
  => MakeAnalyserCallbacks analysersRL analyserRefs analyserCallbacks
  => Analysers analysersRL analyserRefs analysers
  => Monoid res
  => FullScene trigger world analyserCallbacks res
  -> Aff { audioCtx :: AudioContext, event :: Event (Run res analysers) }
makeFullScene (FullScene { trigger, world, piece }) = do
  audioCtx <- liftEffect context
  unitCache <- liftEffect makeUnitCache
  { audioCtx, event: _ } <$>
    ( run
        <$> trigger
        <*> world
        <*> pure { easingAlgorithm }
        <*> pure (defaultFFIAudio audioCtx unitCache)
        <*> pure piece
    )

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