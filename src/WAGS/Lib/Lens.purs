module WAGS.Lib.Lens where

import Prelude

import Data.Lens (Lens, lens)
import Data.NonEmpty as N

_NonEmptyH :: forall f a. Lens (N.NonEmpty f a) (N.NonEmpty f a) a a
_NonEmptyH = lens N.head (flip N.NonEmpty <<< N.tail)

_NonEmptyT :: forall f a. Lens (N.NonEmpty f a) (N.NonEmpty f a) (f a) (f a)
_NonEmptyT = lens N.tail (N.NonEmpty <<< N.head)