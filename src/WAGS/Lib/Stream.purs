module WAGS.Lib.Stream where

import Prelude

import Control.Comonad.Cofree (Cofree, deferCofree)
import Control.Plus (empty)
import Data.Identity (Identity)
import Data.Lazy (force)
import Data.List (List(..))
import Data.List as L
import Data.List.Lazy as LLazy
import Data.List.NonEmpty (NonEmptyList(..), last)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (under, wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Tuple.Nested ((/\))

stream :: forall a. Maybe a -> NonEmpty List a -> Cofree Identity a
stream i l = go l
  where
  go (a :| Nil) = deferCofree \_ -> a /\ wrap (maybe (go l) (\i' -> go (i' :| Nil)) i)
  go (a :| (Cons b c)) = deferCofree \_ -> a /\ wrap (go (b :| c))

streamLazy :: forall a. Maybe a -> NonEmpty LLazy.List a -> Cofree Identity a
streamLazy i l = go l
  where
  go :: NonEmpty LLazy.List a -> Cofree Identity a
  go (a :| (LLazy.List x)) = deferCofree \_ -> case force x of
    LLazy.Nil -> a /\ wrap (maybe (go l) (\i' -> go (i' :| empty)) i)
    (LLazy.Cons b c) -> a /\ wrap (go (b :| c))

class Stops f where
  stops :: forall a. NonEmpty f a -> Cofree Identity (Maybe a)

stopsL :: forall a. NonEmpty List a -> Cofree Identity (Maybe a)
stopsL = deadEnd <<< under NonEmptyList (flip NEL.snoc Nothing) <<< map Just

instance stopsList :: Stops List where
  stops = stopsL

instance stopsArray :: Stops Array where
  stops (a :| b) = stopsL (a :| L.fromFoldable b)

class DeadEnd f where
  deadEnd :: NonEmpty f ~> Cofree Identity

deadEndL :: NonEmpty List ~> Cofree Identity
deadEndL = stream <<< Just <<< last <<< wrap <*> identity

instance deadEndList :: DeadEnd List where
  deadEnd = deadEndL

instance deadEndArray :: DeadEnd Array where
  deadEnd (a :| b) = deadEndL (a :| L.fromFoldable b)

class Cycle f where
  cycle :: NonEmpty f ~> Cofree Identity

cycleL :: forall a. NonEmpty List a -> Cofree Identity a
cycleL = stream Nothing

instance cycleList :: Cycle List where
  cycle = cycleL

instance cycleListLazy :: Cycle LLazy.List where
  cycle = streamLazy Nothing

instance cycleArray :: Cycle Array where
  cycle (a :| b) = cycleL (a :| L.fromFoldable b)

always :: forall a. a -> Cofree Identity a
always = cycleL <<< flip (:|) Nil