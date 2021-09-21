module Main where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Array as A
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
import WAGS.Lib.Learn.Duration (crochet, crochetRest, dottedMinim, minim, semibreve)
import WAGS.Lib.Learn.Note (just, note, noteFromPitch, repeat, seq, then_)
import WAGS.Lib.Learn.Pitch (a4, a5, b4, b5, bFlat4, c4, c5, cSharp4, d4, d5, d6, e4, e5, fSharp4, fSharp5, fSharp6, g4, g5, gSharp4, gSharp5, majorThird, wholeTone)
import WAGS.Lib.Learn.Tempo (allegro)
import WAGS.Lib.Learn.Transpose (transpose)
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
  , Tuple "cyclic array" $ proxy (parent "Cyclic array of notes" (component $ repeat $ seq $ map noteFromPitch $ c4 :| [ d4, e4, fSharp4, gSharp4, bFlat4, c5 ]))
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
          ( component $ repeat $ seq $
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
              $ seq
              $ map (allegro <<< transpose majorThird)
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
  , Tuple "blue danube (with rests)" $ proxy
      ( parent "blue danube (with rests)"
          ( component
              $ repeat
              $ seq
              $ map allegro
                  let
                    n = note mezzoForte crochet
                    ndm = note mezzoForte dottedMinim
                    nm = note mezzoForte minim
                    nj = just <<< n
                    nmj = just <<< nm
                    cell p1 p2 p3 p4 p5 p6 p7 p8 p9 = nj p1 :|
                      [ nj p2
                      , nj p3
                      , nj p4
                      , n p5 `then_` crochetRest
                      , nj p6
                      , n p7 `then_` crochetRest
                      , nj p8
                      , n p9 `then_` crochetRest
                      ]
                    app (a :| b) (c :| d) = a :| (b <> [ c ] <> d)
                  in
                    cell d4 d4 fSharp4 a4 a4 a5 a5 fSharp5 fSharp5
                      `app` cell d4 d4 fSharp4 a4 a4 a5 a5 g5 g5
                      `app` cell cSharp4 cSharp4 e4 b4 b4 b5 b5 g5 g5
                      `app` cell cSharp4 cSharp4 e4 b4 b4 b5 b5 fSharp5 fSharp5
                      `app` cell d4 d4 fSharp4 a4 d5 d6 d6 a5 a5
                      `app` cell d4 d4 fSharp4 a4 d5 d6 d6 b5 b5
                      `app`
                        ( nj e5 :|
                            [ nj e5
                            , nj g5
                            , nj b5
                            , ndm b5 `then_` crochetRest
                            , nj gSharp5
                            , nj a5
                            , ndm fSharp6 `then_` crochetRest
                            , nj d6
                            , nj fSharp5
                            , nmj fSharp5
                            , nj e5
                            , nmj b5
                            , nj a5
                            , nmj d5
                            ]
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
