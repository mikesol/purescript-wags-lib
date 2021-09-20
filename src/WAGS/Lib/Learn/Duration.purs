module WAGS.Lib.Learn.Duration where

import Prelude

import Data.Newtype (class Newtype, over)

newtype Duration = Duration Number

derive instance newtypeDuration :: Newtype Duration _
derive newtype instance boundedDuration :: Bounded Duration
derive newtype instance divisionringDuration :: DivisionRing Duration
derive newtype instance eqDuration :: Eq Duration
derive newtype instance commutativeRingDuration :: CommutativeRing Duration
derive newtype instance euclideanRingDuration :: EuclideanRing Duration
derive newtype instance ordDuration :: Ord Duration
derive newtype instance ringDuration :: Ring Duration
derive newtype instance semiringDuration :: Semiring Duration
derive newtype instance showDuration :: Show Duration

newtype Gap = Gap Number

derive instance newtypeGap :: Newtype Gap _
derive newtype instance boundedGap :: Bounded Gap
derive newtype instance divisionringGap :: DivisionRing Gap
derive newtype instance eqGap :: Eq Gap
derive newtype instance commutativeRingGap :: CommutativeRing Gap
derive newtype instance euclideanRingGap :: EuclideanRing Gap
derive newtype instance ordGap :: Ord Gap
derive newtype instance ringGap :: Ring Gap
derive newtype instance semiringGap :: Semiring Gap
derive newtype instance showGap :: Show Gap

hemidemisemiquaver = Duration 0.0625 :: Duration
demisemiquaver = Duration 0.125 :: Duration
semiquaver = Duration 0.25 :: Duration
quaver = Duration 0.5 :: Duration
crochet = Duration 1.0 :: Duration
minim = Duration 2.0 :: Duration
semibreve = Duration 4.0 :: Duration
breve = Duration 8.0 :: Duration

dot :: Duration -> Duration
dot = over Duration (mul 1.5)

dottedHemidemisemiquaver = dot hemidemisemiquaver :: Duration
dottedDemisemiquaver = dot demisemiquaver :: Duration
dottedSemiquaver = dot semiquaver :: Duration
dottedQuaver = dot quaver :: Duration
dottedCrochet = dot crochet :: Duration
dottedMinim = dot minim :: Duration
dottedSemibreve = dot semibreve :: Duration
dottedBreve = dot breve :: Duration

class Tempo a where
  tempo :: Number -> a -> a

instance tempoNumber :: Tempo Number where
  tempo = mul

instance tempoDuration :: Tempo Duration where
  tempo = over Duration <<< mul

instance tempoFNumber :: Functor f => Tempo (f Number) where
  tempo = map <<< mul

instance tempoFDuration :: Functor f => Tempo (f Duration) where
  tempo = map <<< (over Duration <<< mul)

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

