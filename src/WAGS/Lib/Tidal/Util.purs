module WAGS.Lib.Tidal.Util where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree (Cofree, (:<))
import Data.Compactable (compact)
import Data.List (List(..), fold, (:))
import Data.Map as Map
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Event as Event
import Foreign.Object as O
import WAGS.WebAPI (AudioContext)
import WAGS.Lib.Tidal.Download (getBuffersUsingCache)
import WAGS.Lib.Tidal.Samples (nameToSampleO, sampleToUrls)
import WAGS.Lib.Tidal.Types (DroneNote(..), NextCycle(..), Sample(..), SampleCache, TheFuture(..), Voice(..))

r2b :: Ref.Ref ~> Behavior
r2b r = behavior \e -> Event.makeEvent \f -> Event.subscribe e \v -> Ref.read r >>= f <<< v

bindToN :: Int -> (List ~> List)
bindToN n
  | n <= 0 = const Nil
  | otherwise = case _ of
      Nil -> Nil
      (a : b) -> a : bindToN (n - 1) b

easingAlgorithm :: Cofree ((->) Int) Int
easingAlgorithm =
  let
    fOf initialTime = initialTime :< \adj -> fOf $ max 15 (initialTime - adj)
  in
    fOf 15

v2s :: forall event. Voice event -> Set.Set Sample
v2s (Voice { next: NextCycle { samples } }) = samples

d2s :: forall event. DroneNote event -> Sample
d2s (DroneNote { sample }) = sample

doDownloads
  :: forall a event
   . AudioContext
  -> Ref.Ref SampleCache
  -> (a -> Effect Unit)
  -> (a -> TheFuture event)
  -> a
  -> Aff Unit
doDownloads ctx rf = doDownloads' ctx { read: Ref.read rf, write: flip Ref.write rf }

doDownloads'
  :: forall a event
   . AudioContext
  -> { read :: Effect SampleCache, write :: SampleCache -> Effect Unit }
  -> (a -> Effect Unit)
  -> (a -> TheFuture event)
  -> a
  -> Aff Unit
doDownloads' audioContext { read, write } push lock key = do
  cache <- liftEffect read
  let
    sets = Set.fromFoldable preload
      <> fold (map v2s [ earth, wind, fire ])
      <> (Set.fromFoldable $ compact ((map <<< map) d2s [ air, heart ]))
    samplesToUrl = Set.toMap sets # Map.mapMaybeWithKey \samp@(Sample k) _ -> Map.lookup samp sounds <|> do
      nm <- O.lookup k nameToSampleO
      url <- Map.lookup nm sampleToUrls
      pure url
  newMap <- getBuffersUsingCache samplesToUrl audioContext cache
  liftEffect do
    write newMap
    push key
  where
  TheFuture { earth, wind, fire, air, heart, sounds, preload } = lock key