module Test.Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (zipWith)
import Data.Foldable (fold)
import Data.Ord (abs)
import Data.Traversable (sequence)
import Effect.Exception (Error)
import Test.Spec.Assertions (fail)


epsilon :: forall e. Semiring e => e
epsilon = sixteen * sixteen
  where
  two = one + one
  four = two * two
  sixteen = four * four

class ShouldEqualIsh t where
  shouldEqualIsh :: forall m. MonadThrow Error m => t -> t -> m Unit

instance shouldEqualIshArray :: (ShouldEqualIsh t) => ShouldEqualIsh (Array t) where
  shouldEqualIsh v1 v2 = map fold $ sequence $ zipWith shouldEqualIsh v1 v2
else instance shouldEqualIshN :: (Show t
  , Eq t
  , Ord t
  , EuclideanRing t) => ShouldEqualIsh t where
  shouldEqualIsh v1 v2 = when (abs (v1 - v2) >= epsilon) $
      fail $ show v1 <> " ≠ ish " <> show v2
