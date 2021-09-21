module WAGS.Lib.Learn.Tempo where

import Prelude

import Data.Lens as Lens
import Data.Newtype as NT
import WAGS.Lib.Learn.Duration (Duration(..), Rest(..))
import WAGS.Lib.Learn.Note (Note, NoteWithRest(..), duration)

class Tempo a where
  tempo :: Number -> a -> a

instance tempoNumber :: Tempo Number where
  tempo = mul

instance tempoDuration :: Tempo Duration where
  tempo = NT.over Duration <<< mul

instance tempoRest :: Tempo Rest where
  tempo = NT.over Rest <<< mul

instance tempoNote :: Tempo Note where
  tempo = Lens.over duration <<< tempo

instance tempoNoteWithRest :: Tempo NoteWithRest where
  tempo n = case _ of
    Before { durationBefore: dB, note: nt } -> Before { durationBefore: tempo n dB, note: tempo n nt }
    After { durationAfter: dA, note: nt } -> After { durationAfter: tempo n dA, note: tempo n nt }
    BeforeAndAfter { durationBefore: dB, durationAfter: dA, note: nt } ->
      BeforeAndAfter { durationBefore: tempo n dB, durationAfter: tempo n dA, note: tempo n nt }
    JustNote { note: nt } -> JustNote { note: tempo n nt }

instance tempoFNumber :: Functor f => Tempo (f Number) where
  tempo = map <<< mul

instance tempoFDuration :: Functor f => Tempo (f Duration) where
  tempo = map <<< (NT.over Duration <<< mul)

instance tempoFRest :: Functor f => Tempo (f Rest) where
  tempo = map <<< (NT.over Rest <<< mul)

instance tempoFNote :: Functor f => Tempo (f Note) where
  tempo = map <<< (Lens.over duration <<< tempo)

larghissimo :: forall a. Tempo a => a -> a
larghissimo = tempo $ 1.0 / 0.35

grave :: forall a. Tempo a => a -> a
grave = tempo $ 1.0 / 0.6

largo :: forall a. Tempo a => a -> a
largo = tempo $ 1.0 / 0.8

lento :: forall a. Tempo a => a -> a
lento = tempo $ 1.0 / 0.8

larghetto :: forall a. Tempo a => a -> a
larghetto = tempo $ 1.0 / 1.0

adagio :: forall a. Tempo a => a -> a
adagio = tempo $ 1.0 / 1.2

andante :: forall a. Tempo a => a -> a
andante = tempo $ 1.0 / 1.4

moderato :: forall a. Tempo a => a -> a
moderato = tempo $ 1.0 / 1.8

allegretto :: forall a. Tempo a => a -> a
allegretto = tempo $ 1.0 / 1.8

allegro :: forall a. Tempo a => a -> a
allegro = tempo $ 1.0 / 2.1

allegroVivace :: forall a. Tempo a => a -> a
allegroVivace = tempo $ 1.0 / 2.6

presto :: forall a. Tempo a => a -> a
presto = tempo $ 1.0 / 3.0

prestissimo :: forall a. Tempo a => a -> a
prestissimo = tempo $ 1.0 / 3.25
