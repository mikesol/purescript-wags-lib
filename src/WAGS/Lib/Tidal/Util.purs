module WAGS.Lib.Tidal.Util where

import Prelude

import Control.Alt ((<|>))
import Control.Comonad.Cofree (Cofree, (:<))
import Data.Compactable (compact)
import Data.List (List(..), fold, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Data.Variant.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Event as Event
import Foreign.Object as Object
import WAGS.Lib.Tidal.Download (getBuffersUsingCache)
import WAGS.Lib.Tidal.ObjectHack as OH
import WAGS.Lib.Tidal.Samples (sampleToUrls)
import WAGS.Lib.Tidal.Types (DroneNote(..), NextCycle(..), Sample, SampleCache, TheFuture(..), Voice(..))
import WAGS.WebAPI (AudioContext)

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

v2s :: forall event. Voice event -> Array Sample
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
    sets = Set.fromFoldable
      ( preload
          <> fold (map v2s [ earth, wind, fire, lambert, hendricks, ross ])
          <> (compact $ (map (maybe Nothing Just)) $ ((map <<< map) d2s [ water, heart ]))
      )
    samplesToUrl = OH.catMaybes $ Object.mapWithKey
      (\k _ -> Object.lookup k sounds <|> Object.lookup k sampleToUrls)
      (Object.fromFoldable (Set.map (\samp -> (unwrap samp) /\ unit) sets))
  newMap <- getBuffersUsingCache samplesToUrl audioContext cache
  liftEffect do
    write newMap
    push key
  where
  TheFuture { earth, wind, fire, lambert, hendricks, ross, water, heart, sounds, preload } = lock key