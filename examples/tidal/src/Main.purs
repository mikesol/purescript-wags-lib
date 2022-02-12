module Main where

import Prelude

import CSS as CSS
import Data.Array (fold)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.ArrayBuffer.Typed (toArray)
import Data.Foldable (for_)
import Data.Lens (set, traversed)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))
import Data.UInt (toInt)
import Data.Variant.Either (right)
import Data.Variant.Maybe as VM
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import FRP.Event (Event, subscribe)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Heterogeneous.Mapping (hmap)
import Math ((%))
import Record as R
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import WAGS.Create.Optionals (highpass, pan)
import WAGS.Interpret (close, getByteFrequencyData)
import WAGS.Lib.HList (HNil(..), (:), type (:))
import WAGS.Lib.Learn (class ToScene, Analysers, State', WagsState(..), bStyle, initialState, toScene)
import WAGS.Lib.Learn.Oscillator (lfo)
import WAGS.Lib.Tidal (tdl)
import WAGS.Lib.Tidal.Cycle (noteFromSample_, Cycle)
import WAGS.Lib.Tidal.FX (fx, goodbye, hello)
import WAGS.Lib.Tidal.Synth (synth)
import WAGS.Lib.Tidal.Tidal (changeRate, changeVolume, lnr, lnv, lvt, make, mseq, nefy, i, nl, onTag, parse, s)
import WAGS.Lib.Tidal.Types (AFuture, Note, FoT, TidalRes, FoT_)
import WAGS.Run (Run)
import WAGS.WebAPI (AudioContext)

m2 = 4.0 * 1.0 * 60.0 / 111.0 :: Number

data Action
  = StartAudio
  | StopAudio
  | UpdateRes TidalRes
  | Freqz (Array String)

wag :: AFuture
wag = wag0 <> wag1 <> wag2 <> wag3

wag3 :: AFuture
wag3 = make 2.0 { earth: s $ (map <<< map) (synth {}) $ parse "c4 d4 e4 f4 g4 a4 b4 c5" }

wag1 :: AFuture
wag1 = make 2.0 { earth: s "hh hh hh hh" }

wag2 :: AFuture
wag2 = make 2.0
  { earth: s
      $ mseq 2.0
          ( 0.0 /\ noteFromSample_ (wrap "bd")
              :|
                [ 0.33 /\ nl (_ { s = "hh" })
                , 0.66 /\ nl (_ { s = "hh:3", v = const 0.5 })
                , 1.0 /\ nl (_ { s = "future" })
                , 1.33 /\ nl (_ { s = "chin" })
                , 1.66 /\ nl (_ { s = "hh:4" })
                ]
          )

  }

wag0 :: AFuture
wag0 =
  make (m2 * 2.0)
    { earth: s
        $ set (traversed <<< traversed <<< lnr)
            (lcmap unwrap \{ normalizedLittleCycleTime: t } -> 1.0 + t * 0.1)
        $ parse (Proxy :: _ "tink:1;t0 tink:2;t1 tink:3;t2 tink:0;t3 tink:4;t4 tink:2;t5 tink:3;t6 tink:1;t7 tink:2;t8 tink:0;t9 tink:3;t10 ")
    , wind:
        map
          ( set lvt
              ( lcmap unwrap \{ clockTime } ->
                  let
                    mody = clockTime % (m2 * 2.0)
                  in
                    fx
                      ( goodbye $ highpass (200.0 + mody * 100.0) hello
                      )
              )
          ) $ s $ onTag "ph" (changeRate \{ normalizedSampleTime: t } -> min 1.2 (1.0 + t * 0.3))
          $ onTag "print" (changeVolume \{ normalizedSampleTime: _ } -> 0.2)
          $ onTag "pk" (changeRate \{ normalizedSampleTime: t } -> 0.7 - t * 0.2)
          $ onTag "kt" (changeRate \{ normalizedSampleTime: t } -> min 1.0 (0.6 + t * 0.8))
          $ parse "psr:3 ~ [~ chin*4] ~ [psr:3;ph psr:3;ph ~ ] , [~ ~ ~ psr:1;print] kurt:5;kt , ~ ~ pluck:1;pk ~ ~ ~ ~ ~ "
    , fire:
        map
          ( set lvt
              ( lcmap unwrap \{ clockTime } -> fx
                  ( goodbye $ pan (lfo { phase: 0.0, amp: 1.0, freq: 0.2 } clockTime + 0.0)
                      { myhp: highpass (lfo { phase: 0.0, amp: 2000.0, freq: 0.4 } clockTime + 2000.0) hello
                      }
                  )
              )
          ) $ s "~ ~ ~ ~ ~ ~ speechless:2 ~"
    , title: "lo fi"
    }

type State = { res :: TidalRes, unsubscribeFromHalogen :: Maybe H.SubscriptionId, freqz :: Array String | State' }

miniRender :: forall m. State -> H.ComponentHTML Action () m
miniRender { wagsState, res, freqz } = HH.div
  [ HCSS.style do
      CSS.display CSS.flex
      CSS.flexDirection CSS.column
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
  , HH.h1_ [ HH.text "Cycle info" ]
  , HH.p_ [ HH.code_ [ HH.text $ JSON.writeJSON res ] ]
  , HH.p
      [ HCSS.style do
          CSS.fontFamily [ "Roboto" ] (CSS.sansSerif :| [])
          CSS.fontWeight (CSS.weight 300.0)
      ]
      [ HH.text "made with ❤️ using ", HH.a [ HP.href "https://github.com/mikesol/purescript-wags" ] [ HH.text "wags" ] ]
  , HH.div_ (map (HH.p_ <<< pure <<< HH.text) freqz)
  ]

handleAction
  :: forall output m
   . MonadEffect m
  => MonadAff m
  => Aff { audioCtx :: AudioContext, event :: Event (Run TidalRes Analysers) }
  -> Action
  -> H.HalogenM State Action () output m Unit
handleAction aff = case _ of
  UpdateRes res -> H.modify_ _ { res = res }
  Freqz freqz -> H.modify_ _ { freqz = freqz }
  StartAudio -> do
    { emitter, listener } <- H.liftEffect HS.create
    unsubscribeFromHalogen <- H.subscribe emitter
    H.modify_ _ { wagsState = WagsLoading }
    { audioCtx, event } <- H.liftAff aff
    unsubscribe <- H.liftEffect $ subscribe event \({ res, analysers: { myAnalyser } } :: Run TidalRes Analysers) -> do
      HS.notify listener (UpdateRes res)
      for_ myAnalyser \myAnalyser' -> do
        frequencyData <- getByteFrequencyData myAnalyser'
        arr <- toArray frequencyData
        HS.notify listener (Freqz ((map (fold <<< flip Array.replicate ">" <<< add 1 <<< toInt) arr)))
        pure unit
    H.modify_ _
      { unsubscribe = unsubscribe
      , unsubscribeFromHalogen = Just unsubscribeFromHalogen
      , audioCtx = Just audioCtx
      , wagsState = WagsPlaying
      }
  StopAudio -> do
    { unsubscribe, audioCtx, unsubscribeFromHalogen } <- H.get
    for_ unsubscribeFromHalogen H.unsubscribe
    H.liftEffect unsubscribe
    for_ audioCtx (H.liftEffect <<< close)
    H.modify_ _
      { unsubscribe = pure unit
      , audioCtx = Nothing
      , wagsState = WagsStopped
      , unsubscribeFromHalogen = Nothing
      }

minicomponent
  :: forall toScene query input output m
   . MonadEffect m
  => MonadAff m
  => ToScene toScene TidalRes
  => toScene
  -> H.Component query input output m
minicomponent i =
  H.mkComponent
    { initialState: map
        ( R.union
            { freqz: []
            , res: mempty :: TidalRes
            , unsubscribeFromHalogen: Nothing
            }
        )
        initialState
    , render: miniRender
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction (toScene i), finalize = Just StopAudio }
    }

main :: Effect Unit
main = runHalogenAff do
  awaitBody >>= runUI (minicomponent (tdl wag)) unit
