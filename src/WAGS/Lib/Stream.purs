module WAGS.Lib.Stream where

import Prelude

import Control.Comonad.Cofree (Cofree, deferCofree)
import Data.Identity (Identity)
import Data.List (List(..))
import Data.List.NonEmpty (last)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested ((/\))

stream :: forall a. Maybe a -> NonEmpty List a -> Cofree Identity a
stream i l = go l
  where
  go (a :| Nil) = deferCofree \_ -> a /\ wrap (maybe (go l) (\i' -> go (i' :| Nil)) i)
  go (a :| (Cons b c)) = deferCofree \_ -> a /\ wrap (go (b :| c))

deadEnd :: forall a. NonEmpty List a -> Cofree Identity a
deadEnd l = stream (Just (last (wrap l))) l

cycle :: forall a. NonEmpty List a -> Cofree Identity a
cycle = stream Nothing

always :: forall a. a -> Cofree Identity a
always = cycle <<< flip (:|) Nil