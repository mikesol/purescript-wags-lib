module Test.Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (zipWith)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Foldable (fold, for_)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Data.Variant (match)
import Effect.Exception (Error)
import Test.Spec.Assertions (fail, shouldEqual)
import WAGS.Graph.Parameter (AudioEnvelope(..), AudioParameter(..), AudioParameterCancellation(..), AudioSingleNumber(..), OnOff)

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
else instance shouldEqualIshMaybe :: (ShouldEqualIsh t, Show t) => ShouldEqualIsh (Maybe t) where
  shouldEqualIsh (Just v1) (Just v2) = shouldEqualIsh v1 v2
  shouldEqualIsh Nothing Nothing = pure unit
  shouldEqualIsh a b = fail ("Not equalish - one is " <> show a <> " the other is " <> show b)
else instance shouldEqualIshASN :: ShouldEqualIsh AudioSingleNumber where
  shouldEqualIsh (AudioSingleNumber v1) (AudioSingleNumber v2) = do
    shouldEqualIsh v1.timeOffset v2.timeOffset
    shouldEqualIsh v1.param v2.param
else instance shouldEqualIshAE :: ShouldEqualIsh AudioEnvelope where
  shouldEqualIsh (AudioEnvelope v1) (AudioEnvelope v2) = do
    shouldEqualIsh v1.timeOffset v2.timeOffset
    shouldEqualIsh v1.duration v2.duration
    shouldEqualIsh v1.values v2.values
else instance shouldEqualIshAC :: ShouldEqualIsh AudioParameterCancellation where
  shouldEqualIsh (AudioParameterCancellation v1) (AudioParameterCancellation v2) = do
    shouldEqualIsh v1.timeOffset v2.timeOffset
    shouldEqual v1.hold v2.hold
else instance shouldEqualIshAP :: ShouldEqualIsh AudioParameter where
  shouldEqualIsh (AudioParameter v1) (AudioParameter v2) =
    let
      failure = fail ("Unequal params: " <> show v1 <> " " <> show v2)
    in
      match
        { singleNumber: \v11 -> match
            { singleNumber: \v22 -> shouldEqualIsh v11 v22
            , cancellation: const failure
            , envelope: const failure
            }
            v2
        , cancellation: \v11 -> match
            { singleNumber: const failure
            , cancellation: \v22 -> shouldEqualIsh v11 v22
            , envelope: const failure
            }
            v2
        , envelope: \v11 -> match
            { singleNumber: const failure
            , cancellation: const failure
            , envelope: \v22 -> shouldEqualIsh v11 v22
            }
            v2
        }
        v1
else instance shouldEqualIshNEA :: ShouldEqualIsh a => ShouldEqualIsh (NonEmptyArray a) where
  shouldEqualIsh v1 v2 = do
    shouldEqual (NEA.length v1) (NEA.length v2)
    for_ (NEA.zip v1 v2) \(aa /\ bb) -> shouldEqualIsh aa bb
else instance shouldEqualIshN ::
  ( Show t
  , Eq t
  , Ord t
  , EuclideanRing t
  ) =>
  ShouldEqualIsh t where
  shouldEqualIsh v1 v2 = when (abs (v1 - v2) >= epsilon)
    $ fail
    $ show v1 <> " ≠ ish " <> show v2
