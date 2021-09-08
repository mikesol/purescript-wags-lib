module WAGS.Lib.Latch where

import Prelude

import Control.Comonad.Cofree (Cofree, (:<))
import Data.Maybe (Maybe(..))
import WAGS.Graph.Parameter (AudioParameter_(..))

type MakeLatchAP v a
  = AudioParameter_ v -> a

type LatchAP v
  = Maybe (AudioParameter_ v)

type CfLatchAP v
  = Cofree ((->) (AudioParameter_ v)) (LatchAP v)

type ALatchAP v
  = AudioParameter_ v -> CfLatchAP v

cmpAP :: forall v. Eq v => Maybe (AudioParameter_ v) -> Maybe (AudioParameter_ v) -> Boolean
cmpAP Nothing Nothing = true

cmpAP (Just _) (Nothing) = false

cmpAP (Nothing) (Just _) = false

cmpAP (Just (AudioParameter { param: p0 })) (Just (AudioParameter { param: p1 })) = p0 == p1

makeLatchAP :: forall v. Eq v => ALatchAP v
makeLatchAP = go Nothing
  where
  go :: LatchAP v -> AudioParameter_ v -> CfLatchAP v
  go old new = (if old `cmpAP` Just new then Nothing else Just new) :< go (Just new)

makeLatchAPWithoutInitialBlip :: forall v. Eq v => ALatchAP v
makeLatchAPWithoutInitialBlip = go <$> Just <*> identity
  where
  go :: LatchAP v -> AudioParameter_ v -> CfLatchAP v
  go old new = (if old `cmpAP` Just new then Nothing else Just new) :< go (Just new)
