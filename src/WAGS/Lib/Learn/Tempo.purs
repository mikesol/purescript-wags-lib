module WAGS.Lib.Learn.Tempo where

import Prelude

import Data.Identity (Identity(..))
import Data.Lens (over)
import Data.Newtype (unwrap)
import Data.Variant (case_, on)
import Type.Proxy (Proxy(..))
import WAGS.Lib.Learn.Duration (Duration(..), Rest(..))
import WAGS.Lib.Learn.Note (Note, NoteOrRest, duration, nt, rs)

class Tempo a where
  tempo :: Duration Identity -> a -> a

instance tempoPitch :: Applicative f => Tempo (Duration f) where
  tempo (Duration (Identity d))= mul (Duration (pure d))

instance tempoRest :: Applicative f => Tempo (Rest f) where
  tempo (Duration (Identity d)) = mul (Rest (pure d))

instance tempoNote :: Applicative f => Tempo (Note volumeF f pitchF) where
  tempo = over duration <<< tempo

instance tempoNoteWithRest :: Applicative f => Tempo (NoteOrRest volumeF f pitchF f) where
  tempo val =
    ( case_
        # on (Proxy :: _ "note") (nt <<< tempo val)
        # on (Proxy :: _ "rest") (rs <<< tempo val)
    )
      <<< unwrap

larghissimo :: forall a. Tempo a => a -> a
larghissimo = tempo $ Duration $ pure $ 1.0 / 0.35

grave :: forall a. Tempo a => a -> a
grave = tempo $ Duration $ pure $ 1.0 / 0.6

largo :: forall a. Tempo a => a -> a
largo = tempo $ Duration $ pure $ 1.0 / 0.8

lento :: forall a. Tempo a => a -> a
lento = tempo $ Duration $ pure $ 1.0 / 0.8

larghetto :: forall a. Tempo a => a -> a
larghetto = tempo $ Duration $ pure $ 1.0 / 1.0

adagio :: forall a. Tempo a => a -> a
adagio = tempo $ Duration $ pure $ 1.0 / 1.2

andante :: forall a. Tempo a => a -> a
andante = tempo $ Duration $ pure $ 1.0 / 1.4

moderato :: forall a. Tempo a => a -> a
moderato = tempo $ Duration $ pure $ 1.0 / 1.8

allegretto :: forall a. Tempo a => a -> a
allegretto = tempo $ Duration $ pure $ 1.0 / 1.8

allegro :: forall a. Tempo a => a -> a
allegro = tempo $ Duration $ pure $ 1.0 / 2.1

allegroVivace :: forall a. Tempo a => a -> a
allegroVivace = tempo $ Duration $ pure $ 1.0 / 2.6

presto :: forall a. Tempo a => a -> a
presto = tempo $ Duration $ pure $ 1.0 / 3.0

prestissimo :: forall a. Tempo a => a -> a
prestissimo = tempo $ Duration $ pure $ 1.0 / 3.25
