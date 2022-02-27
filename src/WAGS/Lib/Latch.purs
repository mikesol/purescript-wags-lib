module WAGS.Lib.Latch where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Data.Maybe (Maybe(..))
import WAGS.Graph.Parameter (AudioSingleNumber(..))

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
  = AudioSingleNumber -> a

type LatchAP
  = Maybe AudioSingleNumber

type CfLatchAP
  = Cofree ((->) (AudioSingleNumber)) (LatchAP)

type ALatchAP
  = AudioSingleNumber -> CfLatchAP

cmpAP :: AudioSingleNumber -> AudioSingleNumber -> Boolean
cmpAP (AudioSingleNumber a) (AudioSingleNumber b) = a.param == b.param

makeLatchAP :: ALatchAP
makeLatchAP = makeLatch cmpAP

makeLatchAPWithoutInitialBlip ::  ALatchAP
makeLatchAPWithoutInitialBlip = makeLatchWithoutInitialBlip cmpAP
