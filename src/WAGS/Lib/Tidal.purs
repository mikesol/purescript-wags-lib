module WAGS.Lib.Tidal where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import WAGS.Lib.Learn (FullSceneBuilder)
import WAGS.Lib.Tidal.Engine (engine)
import WAGS.Lib.Tidal.Types (IsFresh, SampleCache, TheFuture, TidalRes)
import WAGS.Lib.Tidal.Util (doDownloads)
import WAGS.WebAPI (BrowserAudioBuffer)

type AFuture = TheFuture Unit

-- todo: we may want to cache buffers between runs in trypurescript
-- if that's the case, we'll need to accept some form of ref instead of
-- downloading fresh each time
tdl
  :: AFuture
  -> FullSceneBuilder
       ( theFuture :: IsFresh Unit -> { clockTime :: Number } -> AFuture
       , interactivity :: Unit
       )
       ( buffers :: SampleCache
       , entropy :: Int
       , silence :: BrowserAudioBuffer
       )
       TidalRes
tdl = tdlx <<< pure

tdlx
  :: ({ clockTime :: Number } -> AFuture)
  -> FullSceneBuilder
       ( theFuture :: IsFresh Unit -> { clockTime :: Number } -> AFuture
       , interactivity :: Unit
       )
       ( buffers :: SampleCache
       , entropy :: Int
       , silence :: BrowserAudioBuffer
       )
       TidalRes
tdlx aFuture = engine (pure unit) (pure $ const aFuture) $ Right \ac -> do
  rf <- liftEffect $ Ref.new Map.empty
  doDownloads ac rf (const $ pure unit) ((#) { clockTime: 0.0 }) aFuture
  liftEffect $ Ref.read rf