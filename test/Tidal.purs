module Test.Tidal where

import Prelude

import Data.Either (Either(..))
import Data.List ((:))
import Data.List.Types (List(..), NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.StringParser (runParser)
import WAGS.Lib.Tidal.Cycle (Cycle(..), noteFromSample, noteFromSample')
import WAGS.Lib.Tidal.Samples as S
import WAGS.Lib.Tidal.Tidal (c2s, cycleP_, parseWithBrackets, unrest)
import WAGS.Lib.Tidal.Types (Note(..), NoteInTime(..), _right)

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
        "tabla2:42" `shouldEqual` Right (noteFromSample S.tabla2_42__Sample)
      runWrapped
        "  tabla2:42" `shouldEqual` Right (noteFromSample S.tabla2_42__Sample)
      runWrapped
        "tabla2:42 hc:0" `shouldEqual` Right (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.tabla2_42__Sample) :| (noteFromSample S.hc_0__Sample) : Nil) })
    it "Evenly divides cycles" do
      runWrapped
        "tabla2:42 hc:0 hc:2" `shouldEqual` Right (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.tabla2_42__Sample) :| (noteFromSample S.hc_0__Sample) : (noteFromSample S.hc_2__Sample) : Nil) })
      runWrapped
        "tabla2:42 hc:0 hc:2 hc:1" `shouldEqual` Right (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.tabla2_42__Sample) :| (noteFromSample S.hc_0__Sample) : (noteFromSample S.hc_2__Sample) : (noteFromSample S.hc_1__Sample)  : Nil) })
    it "Fixes a bug where cycles were crunched towards then end" do
      runWrapped
        "tabla2:42 hc:0 [tabla2:42 hc:2] hc:1 tabla2:42 [tabla2:42 hc:2] hc:0 hc:1" `shouldEqual` Right (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ( (noteFromSample S.tabla2_42__Sample) :| (noteFromSample S.hc_0__Sample) : (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.tabla2_42__Sample) :| (noteFromSample S.hc_2__Sample)  : Nil) }) : (noteFromSample S.hc_1__Sample)  : (noteFromSample S.tabla2_42__Sample)  : (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.tabla2_42__Sample) :| (noteFromSample S.hc_2__Sample)  : Nil) }) : (noteFromSample S.hc_0__Sample) : (noteFromSample S.hc_1__Sample) : Nil) })
    it "Works on internal cycles" do
      runWrapped
        "[tabla2:42 hc:0] tabla2:42" `shouldEqual` Right (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.tabla2_42__Sample) :| (noteFromSample S.hc_0__Sample) : Nil) } :| (noteFromSample S.tabla2_42__Sample) : Nil) })
    it "Works on internal cycles 2" do
      runWrapped
        "tabla2:42*2 tabla2:41" `shouldEqual` Right (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.tabla2_42__Sample) :| (noteFromSample S.tabla2_42__Sample) : Nil) } :| (noteFromSample S.tabla2_41__Sample) : Nil) })
    it "Works on branching cycles" do
      runWrapped
        "[tabla2:42 <hc:0 tech:0>] tabla2:42" `shouldEqual` Right (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.tabla2_42__Sample) :| (Branching { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.hc_0__Sample) :| (noteFromSample S.tech_0__Sample) : Nil) }) : Nil) } :| (noteFromSample S.tabla2_42__Sample) : Nil) })
    it "Works on dual groups" do
      runWrapped
        "[tabla2 tabla2] [diphone2 <sitar latibro>]" `shouldEqual` Right
        ( Internal
            { env: { weight: 1.0, tag: Nothing }
            , nel: NonEmptyList
                ( Internal
                    { env: { weight: 1.0, tag: Nothing }
                    , nel: NonEmptyList
                        ( (noteFromSample S.tabla2_0__Sample)
                            :| (noteFromSample S.tabla2_0__Sample) : Nil
                        )
                    } :|
                    Internal
                      { env: { weight: 1.0, tag: Nothing }
                      , nel: NonEmptyList
                          ( (noteFromSample S.diphone2_0__Sample)
                              :| (Branching { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.sitar_0__Sample) :| (noteFromSample S.latibro_0__Sample) : Nil) }) : Nil
                          )
                      } : Nil

                )
            }
        )
    it "Works on simultaneous cycles" do
      runWrapped
        "diphone2 diphone2  , tech:0 tech:2" `shouldEqual` Right
        ( Simultaneous
            { env: { weight: 1.0, tag: Nothing }
            , nel: NonEmptyList
                ( Internal
                    { env: { weight: 1.0, tag: Nothing }
                    , nel: NonEmptyList
                        ( (noteFromSample S.diphone2_0__Sample)
                            :| (noteFromSample S.diphone2_0__Sample) : Nil
                        )
                    } :|
                    Internal
                      { env: { weight: 1.0, tag: Nothing }
                      , nel: NonEmptyList
                          ( (noteFromSample S.tech_0__Sample)
                              :| (noteFromSample S.tech_2__Sample) : Nil
                          )
                      } : Nil
                )
            }
        )
    it "Works on simultaneous cycles 2" do
      runWrapped
        "[tabla2:42 <hc:0 tech:0>] , tabla2:42" `shouldEqual` Right (Simultaneous { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.tabla2_42__Sample) :| (Branching { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.hc_0__Sample) :| (noteFromSample S.tech_0__Sample) : Nil) }) : Nil) } :| (noteFromSample S.tabla2_42__Sample) : Nil) })
    it "Works on weights" do
      runWrapped
        "[tabla2:42 _ <hc:0 tech:0>] , tabla2:42 _ _" `shouldEqual` Right (Simultaneous { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample' 2.0 S.tabla2_42__Sample) :| (Branching { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.hc_0__Sample) :| (noteFromSample S.tech_0__Sample) : Nil) }) : Nil) } :| (noteFromSample' 3.0 S.tabla2_42__Sample) : Nil) })
    it "Works on branching cycles with internal cycle" do
      runWrapped
        "[tabla2:42 <[hc:0 hc:0] tech:0>] tabla2:42" `shouldEqual` Right (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.tabla2_42__Sample) :| (Branching { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.hc_0__Sample) :| (noteFromSample S.hc_0__Sample) : Nil) }) :| (noteFromSample S.tech_0__Sample) : Nil) }) : Nil) } :| (noteFromSample S.tabla2_42__Sample) : Nil) })
    it "Works on branching cycles with internal simultenaity" do
      runWrapped
        "[tabla2:42 <[hc:0 , hc:1] tech:0>] tabla2:42" `shouldEqual` Right (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList (Internal { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.tabla2_42__Sample) :| (Branching { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((Simultaneous { env: { weight: 1.0, tag: Nothing }, nel: NonEmptyList ((noteFromSample S.hc_0__Sample) :| (noteFromSample S.hc_1__Sample) : Nil) }) :| (noteFromSample S.tech_0__Sample) : Nil) }) : Nil) } :| (noteFromSample S.tabla2_42__Sample) : Nil) })
  describe "Test cycle to sequence" do
    let run = map (flip c2s $ wrap 1.0) <<< runWrapped
    it "Works on simple imput" do
      run "tabla2:42" `shouldEqual` Right (pure (NonEmptyList (NoteInTime { tag: Nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| Nil)))
      run "tabla2:42 hc:0" `shouldEqual` Right (pure (NonEmptyList (NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)))
    it "Works on internal cycles" do
      run "[tabla2:42 hc:0] tabla2:42" `shouldEqual` Right (pure (NonEmptyList (NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)))
    it "Works on tags" do
      run "[tabla2:42 hc:0] tabla2:42;foobar" `shouldEqual` Right (pure (NonEmptyList (NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Just "foobar", duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)))
    it "Works on branching cycles" do
      run "[tabla2:42 <hc:0 tech:0>] tabla2:42" `shouldEqual` Right (NonEmptyList ((NonEmptyList (NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)) :| (NonEmptyList (NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tech_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)) : Nil))
    it "Works on dual groups" do
      run "[tabla2 tabla2] [diphone2 <sitar latibro>]" `shouldEqual` Right (NonEmptyList ((NonEmptyList (NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.5, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.diphone2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.75, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.sitar_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)) :| (NonEmptyList (NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.5, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.diphone2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.75, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.latibro_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)) : Nil))
    it "Works on weights groups" do
      run "[tabla2 _ _ tabla2] [diphone2 <sitar latibro>]" `shouldEqual` Right (NonEmptyList ((NonEmptyList (NoteInTime { tag: Nothing, duration: 0.375, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| NoteInTime { tag: Nothing, duration: 0.125, startsAt: 0.375, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.5, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.diphone2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.75, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.sitar_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)) :| (NonEmptyList (NoteInTime { tag: Nothing, duration: 0.375, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| NoteInTime { tag: Nothing, duration: 0.125, startsAt: 0.375, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.5, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.diphone2_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.75, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.latibro_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)) : Nil))
    it "Works on simultaneous cycles" do
      run "[tabla2:42 <hc:0 ~>] , hc:0" `shouldEqual` Right (NonEmptyList ((NonEmptyList (NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| NoteInTime { tag: Nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)) :| (NonEmptyList (NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } :| NoteInTime { tag: Nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: Just $ Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Nothing } : Nil)) : Nil))
  describe "Test cycle to sequence without rests" do
    let run = map (unrest <<< flip c2s (wrap 1.0)) <<< runWrapped
    it "Works on simple imput" do
      run "tabla2:42" `shouldEqual` Right (pure ((NoteInTime { tag: Nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)))
      run "tabla2:42 hc:0" `shouldEqual` Right (pure ((NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)))
    it "Works on internal cycles" do
      run "[tabla2:42 hc:0] tabla2:42" `shouldEqual` Right (pure ((NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)))
    it "Works on branching cycles" do
      run "[tabla2:42 <hc:0 tech:0>] tabla2:42" `shouldEqual` Right ((((NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)) : ((NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.25, startsAt: 0.25, cycleDuration: 1.0, note: Note { sampleFoT: _right S.tech_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)) : Nil))
    it "Works on simultaneous cycles" do
      run "[tabla2:42 <hc:0 ~>] , hc:0" `shouldEqual` Right ((((NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.5, cycleDuration: 1.0, note: Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)) : ((NoteInTime { tag: Nothing, duration: 0.5, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: _right S.tabla2_42__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : NoteInTime { tag: Nothing, duration: 1.0, startsAt: 0.0, cycleDuration: 1.0, note: Note { sampleFoT: _right S.hc_0__Sample, rateFoT: const 1.0, forward: true, bufferOffsetFoT: const 0.0, volumeFoT: const 1.0 } } : Nil)) : Nil))
