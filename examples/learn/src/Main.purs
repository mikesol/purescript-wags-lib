module Main where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Array as A
import Data.Function (on)
import Data.Lens (over)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
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
import WAGS.Graph.AudioUnit (OnOff, _on, _offOn)
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.Cofree (ana)
import WAGS.Lib.Comonad (rewrap)
import WAGS.Lib.Emitter (makeEmitter)
import WAGS.Lib.Learn (buffers, component, using, usingc)
import WAGS.Lib.Learn.Duration (crochet, crochetRest, dottedMinim, minim, semibreve)
import WAGS.Lib.Learn.Note (accelerando, note, noteFromPitch_, note_, repeat, rs, seq)
import WAGS.Lib.Learn.Oscillator (lfo)
import WAGS.Lib.Learn.Pitch (a4, a5, at, b4, b5, bFlat4, c4, c5, cSharp4, d4, d5, d6, e4, e5, fSharp4, fSharp5, fSharp6, fuse, g4, g5, gSharp4, gSharp5, majorThird, wholeTone, fot2)
import WAGS.Lib.Learn.Tempo (allegro)
import WAGS.Lib.Learn.Transpose (transpose)
import WAGS.Lib.Learn.Volume (mezzoForte)
import WAGS.Lib.Lens (_NonEmptyT)
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
  [ Tuple "" $ proxy (parent "Simple sine wave" (component c4))
  , Tuple "array" $ proxy (parent "Array of notes" (component [ c4, d4, e4, fSharp4, gSharp4, bFlat4, c5 ]))
  , Tuple "cyclic array" $ proxy (parent "Cyclic array of notes" (component $ repeat $ seq $ map noteFromPitch_ $ c4 :| [ d4, e4, fSharp4, gSharp4, bFlat4, c5 ]))
  , Tuple "buffer" $ proxy (parent "Play a buffer" (component "https://freesound.org/data/previews/24/24623_130612-hq.mp3"))
  , Tuple "cofree" $ proxy
      ( parent "Cofree comonad full of notes"
          ( component $ map fuse $ (map <<< map) _.note $ ana
              ( (<*>) \time { note, rising } ->
                  if on (>=) (at time) note c5 then { note: bFlat4, rising: false }
                  else if on (<=) (at time) note c4 then { note: d4, rising: true }
                  else { note: (if rising then add else sub) note wholeTone, rising }
              )
              (const { note: c4, rising: true })
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
                                playBuf { onOff: ff 0.04 $ pure $ if A.null (extract emitted) then _on else _offOn } loopy
                          }
              )
          )
      )
  , Tuple "mary had a little lamb" $ proxy
      ( parent "mary had a little lamb"
          ( component $ repeat $ seq $
              let
                n = note_ mezzoForte
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
                    n = note_ mezzoForte
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
  , Tuple "try ps page" $ proxy
      ( parent "try ps page"
          ( component
              $ repeat
              $ accelerando
              $ seq
              $ map noteFromPitch_
              $ c4 :| [ d4, e4, fSharp4, gSharp4, bFlat4, c5, bFlat4, gSharp4, fSharp4, e4, d4, c4 ]
          )
      )
  , Tuple "try ps page with applicatives" $ proxy
      ( parent "try ps page with applicatives"
          ( component
              $ repeat
              $ accelerando
              $ seq
              $ map noteFromPitch_
              $ over (_NonEmptyT <<< ix 1) (fot2 ((+) <<< lfo { amp: 20.0, freq: 16.0, phase: 0.0 }))
              $ map (over _Newtype (rewrap :: _ -> Number -> Number))
              $ c4 :| [ d4, e4, fSharp4, gSharp4, bFlat4, c5, bFlat4, gSharp4, fSharp4, e4, d4, c4 ]
          )
      )
  , Tuple "blue danube (with rests)" $ proxy
      ( parent "blue danube (with rests)"
          ( component
              $ seq
              $ map allegro
                  let
                    n = note mezzoForte crochet
                    ndm = note mezzoForte dottedMinim
                    nm = note mezzoForte minim
                    cell p1 p2 p3 p4 p5 p6 p7 p8 p9 = n p1 :|
                      [ n p2
                      , n p3
                      , n p4
                      , n p5
                      , rs crochetRest
                      , n p6
                      , n p7
                      , rs crochetRest
                      , n p8
                      , n p9
                      , rs crochetRest
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
                        ( n e5 :|
                            [ n e5
                            , n g5
                            , n b5
                            , nm b5
                            , rs crochetRest
                            , n gSharp5
                            , n a5
                            , ndm fSharp6
                            , rs crochetRest
                            , n d6
                            , n fSharp5
                            , nm fSharp5
                            , n e5
                            , nm b5
                            , n a5
                            , nm d5
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
