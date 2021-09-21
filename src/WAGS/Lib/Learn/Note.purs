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
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, inj, match)
import Math (pow)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import WAGS.Lib.Learn.Duration (Duration(..), Rest(..), crochet)
import WAGS.Lib.Learn.Pitch (Pitch, middleC)
import WAGS.Lib.Learn.Volume (Volume, mezzoForte)

type Note' = { volume :: Volume, duration :: Duration, pitch :: Pitch }
newtype Note = Note Note'

derive instance newtypeNote :: Newtype Note _

newtype NoteOrRest = NoteOrRest (Variant (note :: Note, rest :: Rest))

derive instance newtypeNoteOrRest :: Newtype NoteOrRest _

nt :: Note -> NoteOrRest
nt = NoteOrRest <<< inj (Proxy :: _ "note")

rs :: Rest -> NoteOrRest
rs = NoteOrRest <<< inj (Proxy :: _ "rest")

newtype Sequenced note = Sequenced { startsAfter :: Rest, note :: note }

derive instance newtypeSequence :: Newtype (Sequenced note) _
derive instance functorSequenced :: Functor Sequenced

class IntableIndex a where
  indexToInt :: a -> Int

instance intableIndexInt :: IntableIndex Int where
  indexToInt = identity

instance intableIndexMaybeInt :: IntableIndex (Maybe Int) where
  indexToInt = maybe 0 (add 1)

note_ :: Volume -> Duration -> Pitch -> Note
note_ v d p = Note { volume: v, duration: d, pitch: p }

noteFromDefaults_ :: (Note' -> Note') -> Note
noteFromDefaults_ = Note <<< (#) { volume: mezzoForte, duration: crochet, pitch: middleC }

noteFromPitch_ :: Pitch -> Note
noteFromPitch_ p = noteFromDefaults_ (_ { pitch = p })

note :: Volume -> Duration -> Pitch -> NoteOrRest
note v d p = nt $ Note { volume: v, duration: d, pitch: p }

noteFromDefaults :: (Note' -> Note') -> NoteOrRest
noteFromDefaults = nt <<< Note <<< (#) { volume: mezzoForte, duration: crochet, pitch: middleC }

noteFromPitch :: Pitch -> NoteOrRest
noteFromPitch p = noteFromDefaults (_ { pitch = p })

sNote :: Rest -> Volume -> Duration -> Pitch -> Sequenced Note
sNote s v d p = Sequenced { startsAfter: s, note: Note { volume: v, duration: d, pitch: p } }

volume :: Lens' Note Volume
volume = unto Note <<< prop (Proxy :: _ "volume")

pitch :: Lens' Note Pitch
pitch = unto Note <<< prop (Proxy :: _ "pitch")

duration :: Lens' Note Duration
duration = unto Note <<< prop (Proxy :: _ "duration")

startsAfter :: forall note. Lens' (Sequenced note) Rest
startsAfter = unto Sequenced <<< prop (Proxy :: _ "startsAfter")

accelerando :: forall f i note. IntableIndex i => FunctorWithIndex i f => f (Sequenced note) -> f (Sequenced note)
accelerando = mapWithIndex (over startsAfter <<< mul <<< coerce <<< flip pow 0.6 <<< div 1.0 <<< add 1.0 <<< toNumber <<< indexToInt)

rallentando :: forall f i note. IntableIndex i => FunctorWithIndex i f => f (Sequenced note) -> f (Sequenced note)
rallentando = mapWithIndex (over startsAfter <<< mul <<< coerce <<< flip pow 0.35 <<< add 1.0 <<< toNumber <<< indexToInt)

class Seq (t :: Type -> Type) a (u :: Type -> Type) b | t a -> u b where
  seq :: t a -> u b

instance seqFNoteOrRest :: (Traversable f, Applicative f) => Seq (NonEmpty Array) (f NoteOrRest) Array (f (Sequenced Note)) where
  seq = compact <<< toArray <<< fromNonEmpty <<< map sequence <<< flip evalState (Rest 0.0)
    <<< traverse
      ( traverse \(NoteOrRest v) -> v # match
          { note: \n -> do
              s <- get
              put $ Rest $ unwrap $ view duration n
              pure $ Just $ Sequenced $ { startsAfter: s, note: n }
          , rest: \n -> do
              s <- get
              put $ (n + s)
              pure $ Nothing
          }
      )

instance seqNoteOrRest :: Seq (NonEmpty Array) NoteOrRest Array (Sequenced Note) where
  seq = map unwrap <<< seq <<< map Identity

instance seqFNote :: (Traversable t, Applicative f) => Seq t (f Note) t (f (Sequenced Note)) where
  seq = flip evalState (pure (Rest 0.0))
    <<< traverse \n -> do
      s <- get
      put $ map (Rest <<< unwrap <<< view duration) n
      pure $ (Sequenced <$> ({ startsAfter: _, note: _ } <$> s <*> n))

instance seqNote :: Traversable t => Seq t Note t (Sequenced Note) where
  seq = map unwrap <<< seq <<< map Identity

repeatL :: NonEmpty List (Sequenced Note) -> Cofree Identity (Sequenced Note)
repeatL l = go l
  where
  modHead :: Note -> NonEmpty List (Sequenced Note) -> NonEmpty List (Sequenced Note)
  modHead (Note { duration: Duration rest }) ((Sequenced { note: n }) :| b) =
    Sequenced { startsAfter: Rest rest, note: n } :| b
  go (s@(Sequenced { note: n }) :| Nil) = deferCofree \_ -> s /\ Identity (go (modHead n l))
  go (s :| (Cons b c)) = deferCofree \_ -> s /\ Identity (go (b :| c))

class Repeat f where
  repeat :: NonEmpty f (Sequenced Note) -> Cofree Identity (Sequenced Note)

instance repeatList :: Repeat List where
  repeat = repeatL

instance repeatArray :: Repeat Array where
  repeat (a :| b) = repeatL (a :| L.fromFoldable b)

noteStreamToSequence
  :: forall f
   . Functor f
  => Rest
  -> Cofree f Note
  -> Cofree f (Sequenced Note)
noteStreamToSequence x cf =
  let
    nte = extract cf
  in
    deferCofree \_ ->
      Sequenced { startsAfter: x, note: nte }
        /\ (map (noteStreamToSequence (Rest (coerce $ view duration nte))) (unwrapCofree cf))

