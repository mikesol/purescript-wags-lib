module WAGS.Lib.Learn.Note where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Monad.State (evalState, get, put)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Lens (Lens', lens, over, view)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.List as L
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested ((/\))
import Math (pow)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import WAGS.Lib.Learn.Duration (Duration(..), Rest(..), crochet)
import WAGS.Lib.Learn.Pitch (Pitch, middleC)
import WAGS.Lib.Learn.Volume (Volume, mezzoForte)

type Note' = { volume :: Volume, duration :: Duration, pitch :: Pitch }
newtype Note = Note Note'

derive instance newtypeNote :: Newtype Note _

data NoteWithRest
  = Before { durationBefore :: Rest, note :: Note }
  | After { durationAfter :: Rest, note :: Note }
  | BeforeAndAfter { durationBefore :: Rest, durationAfter :: Rest, note :: Note }
  | JustNote { note :: Note }

just :: Note -> NoteWithRest
just = JustNote <<< { note: _ }

class Then_ a b | a -> b where
  then_ :: a -> b -> NoteWithRest

instance then_NoteRest :: Then_ Note Rest where
  then_ n r = After { durationAfter: r, note: n }

instance then_RestNote :: Then_ Rest Note where
  then_ r n = Before { durationBefore: r, note: n }

restBefore :: NoteWithRest -> Rest
restBefore = case _ of
  Before { durationBefore } -> durationBefore
  BeforeAndAfter { durationBefore } -> durationBefore
  _ -> Rest 0.0

restAfter :: NoteWithRest -> Rest
restAfter = case _ of
  After { durationAfter } -> durationAfter
  BeforeAndAfter { durationAfter } -> durationAfter
  _ -> Rest 0.0

toNote :: Lens' NoteWithRest Note
toNote = lens
  ( case _ of
      Before { note: n } -> n
      After { note: n } -> n
      BeforeAndAfter { note: n } -> n
      JustNote { note: n } -> n

  )
  ( case _ of
      Before b -> \n -> Before $ b { note = n }
      After a -> \n -> After $ a { note = n }
      BeforeAndAfter ba -> \n -> BeforeAndAfter $ ba { note = n }
      JustNote jn -> \n -> JustNote $ jn { note = n }
  )

newtype Sequenced note = Sequenced { startsAfter :: Rest, note :: note }

derive instance newtypeSequence :: Newtype (Sequenced note) _
derive instance functorSequenced :: Functor Sequenced

class IntableIndex a where
  indexToInt :: a -> Int

instance intableIndexInt :: IntableIndex Int where
  indexToInt = identity

instance intableIndexMaybeInt :: IntableIndex (Maybe Int) where
  indexToInt = maybe 0 (add 1)

note :: Volume -> Duration -> Pitch -> Note
note v d p = Note { volume: v, duration: d, pitch: p }

noteFromDefaults :: (Note' -> Note') -> Note
noteFromDefaults = Note <<< (#) { volume: mezzoForte, duration: crochet, pitch: middleC }

noteFromPitch :: Pitch -> Note
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

class Seq a b | a -> b where
  seq :: forall t. Traversable t => t a -> t b

instance seqFNoteWithRest :: Applicative f => Seq (f NoteWithRest) (f (Sequenced Note)) where
  seq = flip evalState (pure (Rest 0.0))
    <<< traverse \n -> do
      s <- get
      put $ map (\nwr -> restBefore nwr + restAfter nwr + (Rest $ unwrap $ view duration $ view toNote nwr)) n
      pure $ (Sequenced <$> ({ startsAfter: _, note: _ } <$> s <*> (view toNote <$> n)))

instance seqFNote :: Applicative f => Seq (f Note) (f (Sequenced Note)) where
  seq = seq <<< (map <<< map) (JustNote <<< { note: _ })

instance seqNoteWithRest :: Seq NoteWithRest (Sequenced Note) where
  seq = map unwrap <<< seq <<< map Identity

instance seqNote :: Seq Note (Sequenced Note) where
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
  -> Cofree f NoteWithRest
  -> Cofree f (Sequenced Note)
noteStreamToSequence x cf =
  let
    nwr = extract cf
    nt = view toNote nwr
  in
    deferCofree \_ ->
      Sequenced { startsAfter: x + restBefore nwr, note: nt }
        /\ (map (noteStreamToSequence (Rest (coerce $ view duration nt) + restAfter nwr)) (unwrapCofree cf))

