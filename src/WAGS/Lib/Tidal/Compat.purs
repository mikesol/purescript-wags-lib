module WAGS.Lib.Tidal.Compat where

import Prelude hiding (append)

import Data.Array (uncons)
import Data.Lens (_1, over, set)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicate)
import Data.Variant.Maybe as VM
import WAGS.Lib.Tidal.Cycle (Cycle, r)
import WAGS.Lib.Tidal.Tidal (b, i, lcw)
import WAGS.Lib.Tidal.Types (Note)

-- todo
-- slow: hard to implement, need to think it through
-- randcat: need to add top-level entropy
-- flatpat: need to create more typeclasses for patterns a la tidal
-- run: see above
-- scan: see above
every
  :: forall a
   . Int
  -> (Cycle a -> Cycle a)
  -> Cycle a
  -> Cycle a
every ix
  | ix <= 0 = const identity
  | ix == 1 = ($)
  | otherwise = flip (<*>) (replicate (ix - 1)) <<< map b

fast
  :: Int
  -> Cycle
       ~> Cycle
fast ix
  | ix <= 1 = identity
  | otherwise = i <*> replicate (ix - 1)

fastGap
  :: forall event
   . Int
  -> Cycle (VM.Maybe (Note event))
  -> Cycle (VM.Maybe (Note event))
fastGap ix
  | ix <= 1 = identity
  | otherwise = flip i (replicate (ix - 1) r)

fastcat
  :: forall event
   . Array (Cycle (VM.Maybe (Note event)))
  -> Cycle (VM.Maybe (Note event))
fastcat = uncons >>> case _ of
  Just { head, tail } -> i head tail
  Nothing -> r

slowcat
  :: forall event
   . Array (Cycle (VM.Maybe (Note event)))
  -> Cycle (VM.Maybe (Note event))
slowcat = uncons >>> case _ of
  Just { head, tail } -> b head tail
  Nothing -> r

cat
  :: forall event
   . Array (Cycle (VM.Maybe (Note event)))
  -> Cycle (VM.Maybe (Note event))
cat = slowcat

append
  :: forall event
   . Cycle (VM.Maybe (Note event))
  -> Cycle (VM.Maybe (Note event))
  -> Cycle (VM.Maybe (Note event))
append a b = cat [ a, b ]

slowAppend
  :: forall event
   . Cycle (VM.Maybe (Note event))
  -> Cycle (VM.Maybe (Note event))
  -> Cycle (VM.Maybe (Note event))
slowAppend = append

fastAppend
  :: forall event
   . Cycle (VM.Maybe (Note event))
  -> Cycle (VM.Maybe (Note event))
  -> Cycle (VM.Maybe (Note event))
fastAppend a b = fastcat [ a, b ]

timeCat
  :: forall event
   . Array (Number /\ Cycle (VM.Maybe (Note event)))
  -> Cycle (VM.Maybe (Note event))
timeCat = fastcat <<< map
  (lcmap (over _1 mul) $ (uncurry $ over lcw))

wedge :: forall event. Number -> Cycle (VM.Maybe (Note event)) -> Cycle (VM.Maybe (Note event)) -> Cycle (VM.Maybe (Note event))
wedge n
  | n <= 0.0 = \_ b -> b
  | n >= 1.0 = \a _ -> a
  | otherwise = \a b -> timeCat [ n /\ a, (1.0 - n) /\ b ]

brak :: forall event. Cycle (VM.Maybe (Note event)) -> Cycle (VM.Maybe (Note event))
brak c = b c [ i (set lcw 0.25 r) [ set lcw 0.5 c, set lcw 0.25 r ] ]
