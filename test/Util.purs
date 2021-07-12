module Test.Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (zipWith)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Traversable (sequence)
import Effect.Exception (Error)
import Test.Spec.Assertions (fail, shouldEqual)
import WAGS.Graph.AudioUnit (OnOff)
import WAGS.Graph.Parameter (AudioParameter_(..))


epsilon :: forall e. EuclideanRing e => e
epsilon = one `div` (sixteen * sixteen)
  where
  two = one + one
  four = two * two
  sixteen = four * four

class ShouldEqualIsh t where
  shouldEqualIsh :: forall m. MonadThrow Error m => t -> t -> m Unit

instance shouldEqualIshArray :: (ShouldEqualIsh t) => ShouldEqualIsh (Array t) where
  shouldEqualIsh v1 v2 = map fold $ sequence $ zipWith shouldEqualIsh v1 v2
else instance shouldEqualIshOnOff :: ShouldEqualIsh OnOff where
  shouldEqualIsh = shouldEqual
else instance shouldEqualIshMaybe :: (ShouldEqualIsh t) => ShouldEqualIsh (Maybe t) where
  shouldEqualIsh (Just v1) (Just v2) = shouldEqualIsh v1 v2
  shouldEqualIsh Nothing Nothing = pure unit
  shouldEqualIsh _ _ = fail "Not equalish - one is nothing, the other is something"
else instance shouldEqualIshAP :: (ShouldEqualIsh t) => ShouldEqualIsh (AudioParameter_ t) where
  shouldEqualIsh (AudioParameter v1) (AudioParameter v2) = do
    shouldEqualIsh v1.timeOffset v2.timeOffset
    shouldEqualIsh v1.param v2.param
else instance shouldEqualIshN :: (Show t
  , Eq t
  , Ord t
  , EuclideanRing t) => ShouldEqualIsh t where
  shouldEqualIsh v1 v2 = when (abs (v1 - v2) >= epsilon) $
      fail $ show v1 <> " ≠ ish " <> show v2
