module Main where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (deferCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Array as A
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories, runStorybook, proxy)
import Math (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Create.Optionals (speaker, playBuf, loopBuf)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.Emitter (makeEmitter)
import WAGS.Lib.Learn (buffers, component, using, usingc)
import WAGS.Lib.Stream (cycle)
import WAGS.Run (SceneI(..))

type Slots = (audio :: forall q. H.Slot q Void Unit)

_audio = Proxy :: Proxy "audio"

type ParentState = {}

type ParentAction = Void

parent :: forall query input output m. String -> (forall query2 input2 output2. H.Component query2 input2 output2 m) -> H.Component query input output m
parent label audio =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = absurd }
    }
  where
  initialState :: input -> ParentState
  initialState _ = {}

  render :: ParentState -> H.ComponentHTML ParentAction Slots m
  render _ =
    HH.div_ [ HH.p_ [ HH.text label ], HH.slot_ _audio unit audio unit ]

stories :: Stories Aff
stories = Object.fromFoldable
  [ Tuple "" $ proxy (parent "Simple sine wave" (component 60))
  , Tuple "array" $ proxy (parent "Array of notes" (component [ 60, 62, 64, 68, 73, 49, 41 ]))
  , Tuple "cyclic array" $ proxy (parent "Cyclic array of notes" (component $ cycle $ 60 :| [ 62, 64, 68, 73, 49, 41 ]))
  , Tuple "freq array" $ proxy (parent "Array of detuned notes" (component $ cycle $ 440.0 :| [ 447.0, 483.0, 499.9999 ]))
  , Tuple "buffer" $ proxy (parent "Play a buffer" (component "https://freesound.org/data/previews/493/493864_5583677-hq.mp3"))
  , Tuple "cofree" $ proxy
      ( parent "Cofree comonad full of notes"
          ( component
              ( let
                  f x b =
                    deferCofree \_ -> Tuple x
                      $ Identity
                      $ f (x + (if b then 2 else -2))
                          (if x >= 70 then false else if x <= 60 then true else b)
                in
                  f 60 true
              )
          )
      )
  , Tuple "loop buffer"
      ( proxy
          ( parent "Loop a buffer"
              ( component
                  $ using (buffers { loopy: "https://freesound.org/data/previews/493/493864_5583677-hq.mp3" })
                      \(SceneI { world: { buffers: { loopy } } }) -> speaker $ loopBuf loopy
              )
          )
      )
  , Tuple "emitter"
      ( proxy
          ( parent "Change on emit"
              ( component
                  $ usingc (buffers { loopy: "https://freesound.org/data/previews/493/493864_5583677-hq.mp3" }) (makeEmitter { startsAt: 0.0 })
                      \( SceneI
                           { world: { buffers: { loopy } }
                           , time
                           , headroomInSeconds
                           }
                       )
                       emitter ->
                        let
                          emitted = emitter { time, headroomInSeconds, freq: sin (pi * time) + 2.0 }
                        in
                          { control: unwrapCofree emitted
                          , scene:
                              speaker $
                                playBuf { onOff: ff 0.04 $ pure $ if A.null (extract emitted) then On else OffOn } loopy
                          }
              )
          )
      )
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitBody >>=
    runStorybook
      { stories
      , logo: Nothing
      }
