module WAGS.Lib.Learn.Note where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Monad.State (evalState, get, put)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Lens (Lens', over, view)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Record (prop)
import Data.List as L
import Data.List (List(..))
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple.Nested ((/\))
import Math (pow)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import WAGS.Lib.Learn.Duration (Duration(..), Gap(..), crochet)
import WAGS.Lib.Learn.Pitch (Pitch, middleC)
import WAGS.Lib.Learn.Volume (Volume, mezzoForte)

newtype Sequenced note = Sequenced { startsAfter :: Gap, note :: note }

derive instance newtypeSequence :: Newtype (Sequenced note) _
derive instance functorSequenced :: Functor Sequenced

type Note' = { volume :: Volume, duration :: Duration, pitch :: Pitch }
newtype Note = Note Note'

derive instance newtypeNote :: Newtype Note _

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

sNote :: Gap -> Volume -> Duration -> Pitch -> Sequenced Note
sNote s v d p = Sequenced { startsAfter: s, note: Note { volume: v, duration: d, pitch: p } }

volume :: Lens' Note Volume
volume = unto Note <<< prop (Proxy :: _ "volume")

pitch :: Lens' Note Pitch
pitch = unto Note <<< prop (Proxy :: _ "pitch")

duration :: Lens' Note Duration
duration = unto Note <<< prop (Proxy :: _ "duration")

startsAfter :: forall note. Lens' (Sequenced note) Gap
startsAfter = unto Sequenced <<< prop (Proxy :: _ "startsAfter")

accelerando :: forall f i note. IntableIndex i => FunctorWithIndex i f => f (Sequenced note) -> f (Sequenced note)
accelerando = mapWithIndex (over startsAfter <<< mul <<< coerce <<< flip pow 0.6 <<< div 1.0 <<< add 1.0 <<< toNumber <<< indexToInt)

rallentando :: forall f i note. IntableIndex i => FunctorWithIndex i f => f (Sequenced note) -> f (Sequenced note)
rallentando = mapWithIndex (over startsAfter <<< mul <<< coerce <<< flip pow 0.35 <<< add 1.0 <<< toNumber <<< indexToInt)

seq :: forall t f. Applicative f => Traversable t => t (f Note) -> t (f (Sequenced Note))
seq = flip evalState (pure (Gap 0.0))
  <<< traverse \n -> do
    s <- get
    put $ map (\(Note { duration: Duration gap }) -> Gap gap) n
    pure $ (Sequenced <$> ({ startsAfter: _, note: _ } <$> s <*> n))

seqI :: forall t. Traversable t => t Note -> t (Sequenced Note)
seqI = map unwrap <<< seq <<< map Identity

repeatL :: NonEmpty List (Sequenced Note) -> Cofree Identity (Sequenced Note)
repeatL l = go l
  where
  modHead :: Note -> NonEmpty List (Sequenced Note) -> NonEmpty List (Sequenced Note)
  modHead (Note { duration: Duration gap }) ((Sequenced { note: n }) :| b) =
    Sequenced { startsAfter: Gap gap, note: n } :| b
  go (s@(Sequenced { note: n }) :| Nil) = deferCofree \_ -> s /\ Identity (go (modHead n l))
  go (s :| (Cons b c)) = deferCofree \_ -> s /\ Identity (go (b :| c))

class Repeat f where
  repeat :: NonEmpty f (Sequenced Note) -> Cofree Identity (Sequenced Note)

instance repeatList :: Repeat List where
  repeat = repeatL

instance repeatArray :: Repeat Array where
  repeat (a :| b) = repeatL (a :| L.fromFoldable b)

noteStreamToSequence'
  :: forall f
   . Applicative f
  => Number
  -> Cofree Identity (f Note)
  -> Cofree Identity (f (Sequenced Note))
noteStreamToSequence' startsAt =
  let
    f x z =
      let
        ez = extract z
      in
        deferCofree \_ ->
          ( Sequenced <$>
              ({ startsAfter: _, note: _ } <$> x <*> ez)
          ) /\ (Identity $ f (map (Gap <<< unwrap <<< view duration) ez) (unwrap $ unwrapCofree z))
  in
    f (pure $ Gap startsAt)

noteStreamToSequence
  :: forall f
   . Applicative f
  => Cofree Identity (f Note)
  -> Cofree Identity (f (Sequenced Note))
noteStreamToSequence = noteStreamToSequence' 0.0
