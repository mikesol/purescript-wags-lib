module WAGS.Lib.Latch where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Data.Maybe (Maybe(..))
import Data.Variant (match)
import WAGS.Graph.Parameter (AudioParameter(..), AudioSingleNumber(..))

type Latch v
  = Maybe v

type CfLatch v
  = Cofree ((->) v) (Maybe v)

type ALatch v
  = v -> CfLatch v

asXNor :: forall v. (v -> v -> Boolean) -> Maybe v -> Maybe v -> Boolean
asXNor f = case _, _ of
  Nothing, Nothing -> true
  Just _, Nothing -> false
  Nothing, Just _ -> false
  Just x, Just y -> f x y

makeLatch :: forall v. (v -> v -> Boolean) -> ALatch v
makeLatch cmp = go Nothing
  where
  go :: Latch v -> v -> CfLatch v
  go old new = (if asXNor cmp old (Just new) then Nothing else Just new) :< go (Just new)

makeLatchWithoutInitialBlip :: forall v. (v -> v -> Boolean) -> ALatch v
makeLatchWithoutInitialBlip cmp = go <$> Just <*> identity
  where
  go :: Latch v -> v -> CfLatch v
  go old new = (if asXNor cmp old (Just new) then Nothing else Just new) :< go (Just new)

makeLatchEq :: forall v. Eq v => ALatch v
makeLatchEq = makeLatch eq

makeLatchWithoutInitialBlipEq :: forall v. Eq v => ALatch v
makeLatchWithoutInitialBlipEq = makeLatchWithoutInitialBlip eq

--

type MakeLatchAP a
  = AudioParameter -> a

type LatchAP
  = Maybe AudioParameter

type CfLatchAP
  = Cofree ((->) (AudioParameter)) (LatchAP)

type ALatchAP
  = AudioParameter -> CfLatchAP

cmpAP :: AudioParameter -> AudioParameter -> Boolean
cmpAP (AudioParameter a) (AudioParameter b) = match
  { singleNumber: \(AudioSingleNumber pa) ->
      match
        { singleNumber: \(AudioSingleNumber pb) -> pa.param == pb.param
        , envelope: const false
        , cancellation: const false
        }
        b
  , envelope: const false
  , cancellation: const false
  }
  a

makeLatchAP :: ALatchAP
makeLatchAP = makeLatch cmpAP

makeLatchAPWithoutInitialBlip ::  ALatchAP
makeLatchAPWithoutInitialBlip = makeLatchWithoutInitialBlip cmpAP
