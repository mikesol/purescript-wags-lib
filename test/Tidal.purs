module Test.Tidal where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.Array.NonEmpty as NEA
import Data.Newtype (wrap)
import Data.Variant.Maybe (Maybe, just, maybe, nothing)
import Data.NonEmpty ((:|))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (runParser)
import WAGS.Lib.Tidal.Cycle (Cycle(..), cycleFromSample, cycleFromSample')
import WAGS.Lib.Tidal.Samples as S
import WAGS.Lib.Tidal.Cycle (internal, simultaneous, branching, singleton)
import WAGS.Lib.Tidal.Tidal (c2s, cycleP_, parseWithBrackets, unrest)
import WAGS.Lib.Tidal.Types (Note(..), NoteInTime(..))
import Data.Variant.Either (right)

runWrapped
  :: String
  -> Either
       { error :: String
       , pos :: Int
       }
       (Cycle (Maybe (Note Unit)))
runWrapped = parseWithBrackets

testTidal :: Spec Unit
testTidal = do
  describe "Tests parser" do
    it "Works on simple imput" do
      runWrapped
        "tabla2:42" `shouldEqual` Right (cycleFromSample S.tabla2_42__Sample)
      runWrapped
        "  tabla2:42" `shouldEqual` Right (cycleFromSample S.tabla2_42__Sample)
      runWrapped
        "tabla2:42 hc:0" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.tabla2_42__Sample) :| [ (cycleFromSample S.hc_0__Sample) ]) })
    it "Evenly divides cycles" do
      runWrapped
        "tabla2:42 hc:0 hc:2" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.tabla2_42__Sample) :| [ (cycleFromSample S.hc_0__Sample), (cycleFromSample S.hc_2__Sample) ]) })
      runWrapped
        "tabla2:42 hc:0 hc:2 hc:1" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.tabla2_42__Sample) :| [ (cycleFromSample S.hc_0__Sample), (cycleFromSample S.hc_2__Sample), (cycleFromSample S.hc_1__Sample) ]) })
    it "Fixes a bug where cycles were crunched towards then end" do
      runWrapped
        "tabla2:42 hc:0 [tabla2:42 hc:2] hc:1 tabla2:42 [tabla2:42 hc:2] hc:0 hc:1" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.tabla2_42__Sample) :| [ (cycleFromSample S.hc_0__Sample), (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.tabla2_42__Sample) :| [ (cycleFromSample S.hc_2__Sample) ]) }), (cycleFromSample S.hc_1__Sample), (cycleFromSample S.tabla2_42__Sample), (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.tabla2_42__Sample) :| [ (cycleFromSample S.hc_2__Sample) ]) }), (cycleFromSample S.hc_0__Sample), (cycleFromSample S.hc_1__Sample) ]) })
    it "Works on internal cycles" do
      runWrapped
        "[tabla2:42 hc:0] tabla2:42" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.tabla2_42__Sample) :| [ (cycleFromSample S.hc_0__Sample) ]) } :| [ (cycleFromSample S.tabla2_42__Sample) ]) })
    it "Works on internal cycles 2" do
      runWrapped
        "tabla2:42*2 tabla2:41" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.tabla2_42__Sample) :| [ (cycleFromSample S.tabla2_42__Sample) ]) } :| [ (cycleFromSample S.tabla2_41__Sample) ]) })
    it "Works on branching cycles" do
      runWrapped
        "[tabla2:42 <hc:0 tech:0>] tabla2:42" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.tabla2_42__Sample) :| [ (branching { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.hc_0__Sample) :| [ (cycleFromSample S.tech_0__Sample) ]) }) ]) } :| [ (cycleFromSample S.tabla2_42__Sample) ]) })
    it "Works on dual groups" do
      runWrapped
        "[tabla2 tabla2] [diphone2 <sitar latibro>]" `shouldEqual` Right
        ( internal
            { env: { weight: 1.0, tag: nothing }
            , cycles: NEA.fromNonEmpty
                ( internal
                    { env: { weight: 1.0, tag: nothing }
                    , cycles: NEA.fromNonEmpty
                        ( (cycleFromSample S.tabla2_0__Sample)
                            :| [ (cycleFromSample S.tabla2_0__Sample) ]
                        )
                    } :|
                    [ internal
                        { env: { weight: 1.0, tag: nothing }
                        , cycles: NEA.fromNonEmpty
                            ( (cycleFromSample S.diphone2_0__Sample)
                                :| [ (branching { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.sitar_0__Sample) :| [ (cycleFromSample S.latibro_0__Sample) ]) }) ]
                            )
                        }
                    ]

                )
            }
        )
    it "Works on simultaneous cycles" do
      runWrapped
        "diphone2 diphone2  , tech:0 tech:2" `shouldEqual` Right
        ( simultaneous
            { env: { weight: 1.0, tag: nothing }
            , cycles: NEA.fromNonEmpty
                ( internal
                    { env: { weight: 1.0, tag: nothing }
                    , cycles: NEA.fromNonEmpty
                        ( (cycleFromSample S.diphone2_0__Sample)
                            :| [ (cycleFromSample S.diphone2_0__Sample) ]
                        )
                    } :|
                    [ internal
                        { env: { weight: 1.0, tag: nothing }
                        , cycles: NEA.fromNonEmpty
                            ( (cycleFromSample S.tech_0__Sample)
                                :| [ (cycleFromSample S.tech_2__Sample) ]
                            )
                        }
                    ]
                )
            }
        )
    it "Works on simultaneous cycles 2" do
      runWrapped
        "[tabla2:42 <hc:0 tech:0>] , tabla2:42" `shouldEqual` Right (simultaneous { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.tabla2_42__Sample) :| [ (branching { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.hc_0__Sample) :| [ (cycleFromSample S.tech_0__Sample) ]) }) ]) } :| [ (cycleFromSample S.tabla2_42__Sample) ]) })
    it "Works on weights" do
      runWrapped
        "[tabla2:42 _ <hc:0 tech:0>] , tabla2:42 _ _" `shouldEqual` Right (simultaneous { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample' 2.0 S.tabla2_42__Sample) :| [ (branching { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.hc_0__Sample) :| [ (cycleFromSample S.tech_0__Sample) ]) }) ]) } :| [ (cycleFromSample' 3.0 S.tabla2_42__Sample) ]) })
    it "Works on branching cycles with internal cycle" do
      runWrapped
        "[tabla2:42 <[hc:0 hc:0] tech:0>] tabla2:42" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.tabla2_42__Sample) :| [ (branching { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.hc_0__Sample) :| [ (cycleFromSample S.hc_0__Sample) ]) }) :| [ (cycleFromSample S.tech_0__Sample) ]) }) ]) } :| [ (cycleFromSample S.tabla2_42__Sample) ]) })
    it "Works on branching cycles with internal simultenaity" do
      runWrapped
        "[tabla2:42 <[hc:0 , hc:1] tech:0>] tabla2:42" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.tabla2_42__Sample) :| [ (branching { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((simultaneous { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample S.hc_0__Sample) :| [ (cycleFromSample S.hc_1__Sample) ]) }) :| [ (cycleFromSample S.tech_0__Sample) ]) }) ]) } :| [ (cycleFromSample S.tabla2_42__Sample) ]) })
  describe "Test cycle to sequence" do
    let run = map (flip c2s $ wrap 1.0) <<< runWrapped
    it "Works on simple imput" do
      run "tabla2:42" `shouldEqual` Right (pure (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [])))
      run "tabla2:42 hc:0" `shouldEqual` Right (pure (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])))
    it "Works on internal cycles" do
      run "[tabla2:42 hc:0] tabla2:42" `shouldEqual` Right (pure (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])))
    it "Works on tags" do
      run "[tabla2:42 hc:0] tabla2:42;foobar" `shouldEqual` Right (pure (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: just "foobar", duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])))
    it "Works on branching cycles" do
      run "[tabla2:42 <hc:0 tech:0>] tabla2:42" `shouldEqual` Right (NEA.fromNonEmpty ((NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) :| [ (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tech_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) ]))
    it "Works on dual groups" do
      run "[tabla2 tabla2] [diphone2 <sitar latibro>]" `shouldEqual` Right (NEA.fromNonEmpty ((NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.diphone2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.75, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.sitar_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) :| [ (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.diphone2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.75, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.latibro_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) ]))
    it "Works on weights groups" do
      run "[tabla2 _ _ tabla2] [diphone2 <sitar latibro>]" `shouldEqual` Right (NEA.fromNonEmpty ((NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.375, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.125, startsAt: 0.375, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.diphone2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.75, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.sitar_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) :| [ (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.375, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.125, startsAt: 0.375, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.diphone2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.75, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.latibro_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) ]))
    it "Works on simultaneous cycles" do
      run "[tabla2:42 <hc:0 ~>] , hc:0" `shouldEqual` Right (NEA.fromNonEmpty ((NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) :| [ (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: nothing } ])) ]))
  describe "Test cycle to sequence without rests" do
    let run = map (unrest <<< flip c2s (wrap 1.0)) <<< runWrapped
    it "Works on simple imput" do
      run "tabla2:42" `shouldEqual` Right (pure (([ NoteInTime { tag: nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])))
      run "tabla2:42 hc:0" `shouldEqual` Right (pure (([ NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])))
    it "Works on internal cycles" do
      run "[tabla2:42 hc:0] tabla2:42" `shouldEqual` Right (pure (([ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])))
    it "Works on branching cycles" do
      run "[tabla2:42 <hc:0 tech:0>] tabla2:42" `shouldEqual` Right (([ (([ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])), (([ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Note { sampleFoT: right S.tech_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) ]))
    it "Works on simultaneous cycles" do
      run "[tabla2:42 <hc:0 ~>] , hc:0" `shouldEqual` Right (([ (([ NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])), (([ NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) ]))
