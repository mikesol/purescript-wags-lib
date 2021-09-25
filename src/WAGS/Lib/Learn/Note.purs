module WAGS.Lib.Learn.Note where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Monad.State (evalState, get, put)
import Data.Array.NonEmpty (fromNonEmpty, toArray)
import Data.Compactable (compact)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Lens (Lens', over, view)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, inj, match)
import Math (pow)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import WAGS.Lib.Learn.Duration (Duration(..), Rest(..), crochet)
import WAGS.Lib.Learn.Pitch (Pitch, middleC)
import WAGS.Lib.Learn.Volume (Volume, mezzoForte)

type Note' volumeF durationF pitchF =
  { volume :: Volume volumeF
  , duration :: Duration durationF
  , pitch :: Pitch pitchF
  }

newtype Note volumeF durationF pitchF = Note (Note' volumeF durationF pitchF)

derive instance newtypeNote :: Newtype (Note volumeF durationF pitchF) _

newtype NoteOrRest volumeF durationF pitchF restF = NoteOrRest
  (Variant (note :: Note volumeF durationF pitchF, rest :: Rest restF))

derive instance newtypeNoteOrRest :: Newtype (NoteOrRest volumeF durationF pitchF restF) _

nt
  :: forall volumeF durationF pitchF restF
   . Note volumeF durationF pitchF
  -> NoteOrRest volumeF durationF pitchF restF
nt = NoteOrRest <<< inj (Proxy :: _ "note")

rs
  :: forall volumeF durationF pitchF restF
   . Rest restF
  -> NoteOrRest volumeF durationF pitchF restF
rs = NoteOrRest <<< inj (Proxy :: _ "rest")

newtype Sequenced note = Sequenced { startsAfter :: Rest Identity, note :: note }

derive instance newtypeSequence :: Newtype (Sequenced note) _
derive instance functorSequenced :: Functor Sequenced

class IntableIndex a where
  indexToInt :: a -> Int

instance intableIndexInt :: IntableIndex Int where
  indexToInt = identity

instance intableIndexMaybeInt :: IntableIndex (Maybe Int) where
  indexToInt = maybe 0 (add 1)

note_
  :: forall volumeF durationF pitchF
   . Volume volumeF
  -> Duration durationF
  -> Pitch pitchF
  -> Note volumeF durationF pitchF
note_ v d p = Note { volume: v, duration: d, pitch: p }

noteFromDefaults_
  :: forall volumeF durationF pitchF
   . (Note' Identity Identity ((->) Number) -> Note' volumeF durationF pitchF)
  -> Note volumeF durationF pitchF
noteFromDefaults_ = Note <<< (#) { volume: mezzoForte, duration: crochet, pitch: middleC }

noteFromPitch_ :: forall pitchF. Pitch pitchF -> Note Identity Identity pitchF
noteFromPitch_ p = noteFromDefaults_ (_ { pitch = p })

note
  :: forall volumeF durationF pitchF restF
   . Volume volumeF
  -> Duration durationF
  -> Pitch pitchF
  -> NoteOrRest volumeF durationF pitchF restF
note v d p = nt $ Note { volume: v, duration: d, pitch: p }

noteFromDefaults
  :: forall volumeF durationF pitchF restF
   . (Note' Identity Identity ((->) Number) -> Note' volumeF durationF pitchF)
  -> NoteOrRest volumeF durationF pitchF restF
noteFromDefaults = nt <<< Note <<< (#) { volume: mezzoForte, duration: crochet, pitch: middleC }

noteFromPitch :: forall pitchF restF. Pitch pitchF -> NoteOrRest Identity Identity pitchF restF
noteFromPitch p = noteFromDefaults (_ { pitch = p })

sNote
  :: forall volumeF pitchF
   . Rest Identity
  -> Volume volumeF
  -> Duration Identity
  -> Pitch pitchF
  -> Sequenced (Note volumeF Identity pitchF)
sNote s v d p = Sequenced { startsAfter: s, note: Note { volume: v, duration: d, pitch: p } }

volume :: forall volumeF durationF pitchF. Lens' (Note volumeF durationF pitchF) (Volume volumeF)
volume = unto Note <<< prop (Proxy :: _ "volume")

pitch :: forall volumeF durationF pitchF. Lens' (Note volumeF durationF pitchF) (Pitch pitchF)
pitch = unto Note <<< prop (Proxy :: _ "pitch")

duration :: forall volumeF durationF pitchF. Lens' (Note volumeF durationF pitchF) (Duration durationF)
duration = unto Note <<< prop (Proxy :: _ "duration")

startsAfter :: forall note. Lens' (Sequenced note) (Rest Identity)
startsAfter = unto Sequenced <<< prop (Proxy :: _ "startsAfter")

accelerando
  :: forall f i note
   . IntableIndex i
  => FunctorWithIndex i f
  => f (Sequenced note)
  -> f (Sequenced note)
accelerando = mapWithIndex (over startsAfter <<< mul <<< coerce <<< flip pow 0.6 <<< div 1.0 <<< add 1.0 <<< toNumber <<< indexToInt)

rallentando :: forall f i note. IntableIndex i => FunctorWithIndex i f => f (Sequenced note) -> f (Sequenced note)
rallentando = mapWithIndex (over startsAfter <<< mul <<< coerce <<< flip pow 0.35 <<< add 1.0 <<< toNumber <<< indexToInt)

class Seq (t :: Type -> Type) a (u :: Type -> Type) b | t a -> u b where
  seq :: t a -> u b

instance seqFNoteOrRest ::
  Seq (NonEmpty Array)
    (NoteOrRest volumeF Identity pitchF Identity)
    Array
    (Sequenced (Note volumeF Identity pitchF)) where
  seq = compact <<< toArray <<< fromNonEmpty <<< flip evalState (Rest $ Identity 0.0)
    <<< traverse \(NoteOrRest v) -> v # match
      { note: \n -> do
          s <- get
          put $ Rest $ unwrap $ view duration n
          pure $ Just $ Sequenced $ { startsAfter: s, note: n }
      , rest: \n -> do
          s <- get
          put $ (n + s)
          pure $ Nothing
      }

instance seqFNote ::
  Traversable t =>
  Seq t (Note volumeF Identity pitchF) t (Sequenced (Note volumeF Identity pitchF)) where
  seq = flip evalState (Rest $ Identity 0.0)
    <<< traverse \n -> do
      s <- get
      put $ (Rest <<< unwrap <<< view duration) n
      pure $ (Sequenced ({ startsAfter: s, note: n }))

repeatL
  :: forall volumeF pitchF
   . NonEmpty List (Sequenced (Note volumeF Identity pitchF))
  -> Cofree Identity (Sequenced (Note volumeF Identity pitchF))
repeatL l = go l
  where
  modHead
    :: Note volumeF Identity pitchF
    -> NonEmpty List (Sequenced (Note volumeF Identity pitchF))
    -> NonEmpty List (Sequenced (Note volumeF Identity pitchF))
  modHead (Note { duration: Duration rest }) ((Sequenced { note: n }) :| b) =
    Sequenced { startsAfter: Rest rest, note: n } :| b
  go (s@(Sequenced { note: n }) :| Nil) = deferCofree \_ -> s /\ Identity (go (modHead n l))
  go (s :| (Cons b c)) = deferCofree \_ -> s /\ Identity (go (b :| c))

class Repeat f where
  repeat
    :: forall volumeF pitchF
     . NonEmpty f (Sequenced (Note volumeF Identity pitchF))
    -> Cofree Identity (Sequenced (Note volumeF Identity pitchF))

instance repeatList :: Repeat List where
  repeat = repeatL

instance repeatArray :: Repeat Array where
  repeat (a :| b) = repeatL (a :| L.fromFoldable b)

noteStreamToSequence
  :: forall f volumeF pitchF
   . Functor f
  => Rest Identity
  -> Cofree f (Note volumeF Identity pitchF)
  -> Cofree f (Sequenced (Note volumeF Identity pitchF))
noteStreamToSequence x cf =
  let
    nte = extract cf
  in
    deferCofree \_ ->
      Sequenced { startsAfter: x, note: nte }
        /\ (map (noteStreamToSequence (Rest (coerce $ view duration nte))) (unwrapCofree cf))

