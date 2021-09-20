module Main where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Array as A
import Data.Lens (over)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Storybook (Stories, runStorybook, proxy)
import Type.Proxy (Proxy(..))
import WAGS.Create.Optionals (speaker, playBuf, loopBuf)
import WAGS.Graph.AudioUnit (OnOff(..))
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.Cofree (ana)
import WAGS.Lib.Emitter (makeEmitter)
import WAGS.Lib.Learn (buffers, component, using, usingc)
import WAGS.Lib.Learn.Duration (allegro, crochet, minim, semibreve)
import WAGS.Lib.Learn.Note (duration, note, noteFromPitch, pitch, repeat, seqI)
import WAGS.Lib.Learn.Pitch (bFlat4, c4, c5, d4, e4, fSharp4, g4, gSharp4, majorThird, wholeTone, transpose)
import WAGS.Lib.Learn.Volume (mezzoForte)
import WAGS.Run (SceneI(..))
import Wags.Learn.Oscillator (lfo)

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
  [ Tuple "" $ proxy (parent "Simple sine wave" (component c4))
  , Tuple "array" $ proxy (parent "Array of notes" (component [ c4, d4, e4, fSharp4, gSharp4, bFlat4, c5 ]))
  , Tuple "cyclic array" $ proxy (parent "Cyclic array of notes" (component $ repeat $ seqI $ map noteFromPitch $ c4 :| [ d4, e4, fSharp4, gSharp4, bFlat4, c5 ]))
  , Tuple "buffer" $ proxy (parent "Play a buffer" (component "https://freesound.org/data/previews/24/24623_130612-hq.mp3"))
  , Tuple "cofree" $ proxy
      ( parent "Cofree comonad full of notes"
          ( component $ map _.note $ ana
              ( \{ note, rising } ->
                  if note >= c5 then { note: bFlat4, rising: false }
                  else if note <= c4 then { note: d4, rising: true }
                  else { note: (if rising then add else sub) note wholeTone, rising }
              )
              { note: c4, rising: true }
          )
      )
  , Tuple "loop buffer"
      ( proxy
          ( parent "Loop a buffer"
              ( component
                  $ using (buffers { loopy: "https://freesound.org/data/previews/24/24623_130612-hq.mp3" })
                      \(SceneI { world: { buffers: { loopy } } }) -> speaker $ loopBuf loopy
              )
          )
      )
  , Tuple "emitter"
      ( proxy
          ( parent "Change on emit"
              ( component
                  $ usingc (buffers { loopy: "https://freesound.org/data/previews/110/110212_1751865-hq.mp3" }) (makeEmitter { startsAt: 0.0 })
                      \( SceneI
                           { world: { buffers: { loopy } }
                           , time
                           , headroomInSeconds
                           }
                       )
                       emitter ->
                        let
                          emitted = emitter { time, headroomInSeconds, freq: lfo { amp: 1.5, freq: 1.0, phase: 0.0 } time + 3.0 }
                        in
                          { control: unwrapCofree emitted
                          , scene:
                              speaker $
                                playBuf { onOff: ff 0.04 $ pure $ if A.null (extract emitted) then On else OffOn } loopy
                          }
              )
          )
      )
  , Tuple "mary had a little lamb" $ proxy
      ( parent "mary had a little lamb"
          ( component $ repeat $ seqI $
              let
                n = note mezzoForte
              in
                n crochet e4 :|
                  [ n crochet d4
                  , n crochet c4
                  , n crochet d4
                  , n crochet e4
                  , n crochet e4
                  , n minim e4
                  , n crochet d4
                  , n crochet d4
                  , n minim d4
                  , n crochet e4
                  , n crochet g4
                  , n minim g4
                  , n crochet e4
                  , n crochet d4
                  , n crochet c4
                  , n crochet d4
                  , n crochet e4
                  , n crochet e4
                  , n crochet e4
                  , n crochet e4
                  , n crochet d4
                  , n crochet d4
                  , n crochet e4
                  , n crochet d4
                  , n semibreve c4
                  ]
          )
      )
  , Tuple "mary had a little lamb allegro & transposed major third" $ proxy
      ( parent "mary had a little lamb allegro & transposed major third"
          ( component
              $ repeat
              $ seqI
              $ map (over duration allegro <<< over pitch (transpose majorThird))
                  let
                    n = note mezzoForte
                  in
                    n crochet e4 :|
                      [ n crochet d4
                      , n crochet c4
                      , n crochet d4
                      , n crochet e4
                      , n crochet e4
                      , n minim e4
                      , n crochet d4
                      , n crochet d4
                      , n minim d4
                      , n crochet e4
                      , n crochet g4
                      , n minim g4
                      , n crochet e4
                      , n crochet d4
                      , n crochet c4
                      , n crochet d4
                      , n crochet e4
                      , n crochet e4
                      , n crochet e4
                      , n crochet e4
                      , n crochet d4
                      , n crochet d4
                      , n crochet e4
                      , n crochet d4
                      , n semibreve c4
                      ]
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
