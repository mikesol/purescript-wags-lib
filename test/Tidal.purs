module Test.Tidal where

import Prelude

import Data.Either (Either(..))
import Data.Array.NonEmpty as NEA
import Data.Newtype (wrap)
import Data.Variant.Maybe (Maybe, just, nothing)
import Data.NonEmpty ((:|))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import WAGS.Lib.Tidal.Cycle (Cycle, branching, cycleFromSample, cycleFromSample', internal, simultaneous)
import WAGS.Lib.Tidal.FX (goodbye, hello, fx)
import WAGS.Lib.Tidal.Tidal (c2s, parseWithBrackets, unrest)
import WAGS.Lib.Tidal.Types (Note(..), Sample(..), NoteInTime(..))
import Data.Variant.Either (right)

runWrapped
  :: String
  -> Either
       { error :: String
       , pos :: Int
       }
       (Cycle (Maybe (Note Unit)))
runWrapped = parseWithBrackets

tabla2_42__Sample = Sample "tabla2:42" :: Sample
tabla2_0__Sample = Sample "tabla2" :: Sample
tabla2_41__Sample = Sample "tabla2:41" :: Sample
tech_0__Sample = Sample "tech:0" :: Sample
hc_0__Sample = Sample "hc:0" :: Sample
hc_1__Sample = Sample "hc:1" :: Sample
hc_2__Sample = Sample "hc:2" :: Sample
diphone2_0__Sample = Sample "diphone2" :: Sample
sitar_0__Sample = Sample "sitar" :: Sample
latibro_0__Sample = Sample "latibro" :: Sample
tech_2__Sample = Sample "tech:2" :: Sample

testTidal :: Spec Unit
testTidal = do
  describe "Tests parser" do
    it "Works on simple imput" do
      runWrapped
        "tabla2:42" `shouldEqual` Right (cycleFromSample tabla2_42__Sample)
      runWrapped
        "  tabla2:42" `shouldEqual` Right (cycleFromSample tabla2_42__Sample)
      runWrapped
        "tabla2:42 hc:0" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample tabla2_42__Sample) :| [ (cycleFromSample hc_0__Sample) ]) })
    it "Evenly divides cycles" do
      runWrapped
        "tabla2:42 hc:0 hc:2" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample tabla2_42__Sample) :| [ (cycleFromSample hc_0__Sample), (cycleFromSample hc_2__Sample) ]) })
      runWrapped
        "tabla2:42 hc:0 hc:2 hc:1" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample tabla2_42__Sample) :| [ (cycleFromSample hc_0__Sample), (cycleFromSample hc_2__Sample), (cycleFromSample hc_1__Sample) ]) })
    it "Fixes a bug where cycles were crunched towards then end" do
      runWrapped
        "tabla2:42 hc:0 [tabla2:42 hc:2] hc:1 tabla2:42 [tabla2:42 hc:2] hc:0 hc:1" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample tabla2_42__Sample) :| [ (cycleFromSample hc_0__Sample), (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample tabla2_42__Sample) :| [ (cycleFromSample hc_2__Sample) ]) }), (cycleFromSample hc_1__Sample), (cycleFromSample tabla2_42__Sample), (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample tabla2_42__Sample) :| [ (cycleFromSample hc_2__Sample) ]) }), (cycleFromSample hc_0__Sample), (cycleFromSample hc_1__Sample) ]) })
    it "Works on internal cycles" do
      runWrapped
        "[tabla2:42 hc:0] tabla2:42" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample tabla2_42__Sample) :| [ (cycleFromSample hc_0__Sample) ]) } :| [ (cycleFromSample tabla2_42__Sample) ]) })
    it "Works on internal cycles 2" do
      runWrapped
        "tabla2:42*2 tabla2:41" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample tabla2_42__Sample) :| [ (cycleFromSample tabla2_42__Sample) ]) } :| [ (cycleFromSample tabla2_41__Sample) ]) })
    it "Works on branching cycles" do
      runWrapped
        "[tabla2:42 <hc:0 tech:0>] tabla2:42" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample tabla2_42__Sample) :| [ (branching { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample hc_0__Sample) :| [ (cycleFromSample tech_0__Sample) ]) }) ]) } :| [ (cycleFromSample tabla2_42__Sample) ]) })
    it "Works on dual groups" do
      runWrapped
        "[tabla2 tabla2] [diphone2 <sitar latibro>]" `shouldEqual` Right
        ( internal
            { env: { weight: 1.0, tag: nothing }
            , cycles: NEA.fromNonEmpty
                ( internal
                    { env: { weight: 1.0, tag: nothing }
                    , cycles: NEA.fromNonEmpty
                        ( (cycleFromSample tabla2_0__Sample)
                            :| [ (cycleFromSample tabla2_0__Sample) ]
                        )
                    } :|
                    [ internal
                        { env: { weight: 1.0, tag: nothing }
                        , cycles: NEA.fromNonEmpty
                            ( (cycleFromSample diphone2_0__Sample)
                                :| [ (branching { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample sitar_0__Sample) :| [ (cycleFromSample latibro_0__Sample) ]) }) ]
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
                        ( (cycleFromSample diphone2_0__Sample)
                            :| [ (cycleFromSample diphone2_0__Sample) ]
                        )
                    } :|
                    [ internal
                        { env: { weight: 1.0, tag: nothing }
                        , cycles: NEA.fromNonEmpty
                            ( (cycleFromSample tech_0__Sample)
                                :| [ (cycleFromSample tech_2__Sample) ]
                            )
                        }
                    ]
                )
            }
        )
    it "Works on simultaneous cycles 2" do
      runWrapped
        "[tabla2:42 <hc:0 tech:0>] , tabla2:42" `shouldEqual` Right (simultaneous { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample tabla2_42__Sample) :| [ (branching { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample hc_0__Sample) :| [ (cycleFromSample tech_0__Sample) ]) }) ]) } :| [ (cycleFromSample tabla2_42__Sample) ]) })
    it "Works on weights" do
      runWrapped
        "[tabla2:42 _ <hc:0 tech:0>] , tabla2:42 _ _" `shouldEqual` Right (simultaneous { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample' 2.0 tabla2_42__Sample) :| [ (branching { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample hc_0__Sample) :| [ (cycleFromSample tech_0__Sample) ]) }) ]) } :| [ (cycleFromSample' 3.0 tabla2_42__Sample) ]) })
    it "Works on branching cycles with internal cycle" do
      runWrapped
        "[tabla2:42 <[hc:0 hc:0] tech:0>] tabla2:42" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample tabla2_42__Sample) :| [ (branching { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample hc_0__Sample) :| [ (cycleFromSample hc_0__Sample) ]) }) :| [ (cycleFromSample tech_0__Sample) ]) }) ]) } :| [ (cycleFromSample tabla2_42__Sample) ]) })
    it "Works on branching cycles with internal simultenaity" do
      runWrapped
        "[tabla2:42 <[hc:0 , hc:1] tech:0>] tabla2:42" `shouldEqual` Right (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty (internal { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample tabla2_42__Sample) :| [ (branching { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((simultaneous { env: { weight: 1.0, tag: nothing }, cycles: NEA.fromNonEmpty ((cycleFromSample hc_0__Sample) :| [ (cycleFromSample hc_1__Sample) ]) }) :| [ (cycleFromSample tech_0__Sample) ]) }) ]) } :| [ (cycleFromSample tabla2_42__Sample) ]) })
  describe "Test cycle to sequence" do
    let run = map (flip c2s $ wrap 1.0) <<< runWrapped
    it "Works on simple imput" do
      run "tabla2:42" `shouldEqual` Right (pure (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [])))
      run "tabla2:42 hc:0" `shouldEqual` Right (pure (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])))
    it "Works on internal cycles" do
      run "[tabla2:42 hc:0] tabla2:42" `shouldEqual` Right (pure (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: just $ Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])))
    it "Works on tags" do
      run "[tabla2:42 hc:0] tabla2:42;foobar" `shouldEqual` Right (pure (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: just $ Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: just "foobar", duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])))
    it "Works on branching cycles" do
      run "[tabla2:42 <hc:0 tech:0>] tabla2:42" `shouldEqual` Right (NEA.fromNonEmpty ((NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: just $ Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) :| [ (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tech_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) ]))
    it "Works on dual groups" do
      run "[tabla2 tabla2] [diphone2 <sitar latibro>]" `shouldEqual` Right (NEA.fromNonEmpty ((NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right diphone2_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.75, cycleDuration: 1.0, note: just $ Note { sampleFoT: right sitar_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) :| [ (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right diphone2_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.75, cycleDuration: 1.0, note: just $ Note { sampleFoT: right latibro_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) ]))
    it "Works on weights groups" do
      run "[tabla2 _ _ tabla2] [diphone2 <sitar latibro>]" `shouldEqual` Right (NEA.fromNonEmpty ((NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.375, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.125, startsAt: 0.375, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right diphone2_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.75, cycleDuration: 1.0, note: just $ Note { sampleFoT: right sitar_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) :| [ (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.375, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 0.125, startsAt: 0.375, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right diphone2_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.75, cycleDuration: 1.0, note: just $ Note { sampleFoT: right latibro_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) ]))
    it "Works on simultaneous cycles" do
      run "[tabla2:42 <hc:0 ~>] , hc:0" `shouldEqual` Right (NEA.fromNonEmpty ((NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: just $ Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) :| [ (NEA.fromNonEmpty (NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| [ NoteInTime { tag: nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: just $ Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: nothing } ])) ]))
  describe "Test cycle to sequence without rests" do
    let run = map (unrest <<< flip c2s (wrap 1.0)) <<< runWrapped
    it "Works on simple imput" do
      run "tabla2:42" `shouldEqual` Right (pure (([ NoteInTime { tag: nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])))
      run "tabla2:42 hc:0" `shouldEqual` Right (pure (([ NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])))
    it "Works on internal cycles" do
      run "[tabla2:42 hc:0] tabla2:42" `shouldEqual` Right (pure (([ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])))
    it "Works on branching cycles" do
      run "[tabla2:42 <hc:0 tech:0>] tabla2:42" `shouldEqual` Right (([ (([ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])), (([ NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Note { sampleFoT: right tech_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) ]))
    it "Works on simultaneous cycles" do
      run "[tabla2:42 <hc:0 ~>] , hc:0" `shouldEqual` Right (([ (([ NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])), (([ NoteInTime { tag: nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right tabla2_42__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } }, NoteInTime { tag: nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: right hc_0__Sample, tumultFoT: const $ fx $ goodbye hello, rateFoT: const 1.0, forwardFoT: const true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } ])) ]))
