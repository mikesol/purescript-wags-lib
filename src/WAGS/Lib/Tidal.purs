module WAGS.Lib.Tidal where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import WAGS.Lib.Learn (FullSceneBuilder)
import WAGS.WebAPI (BrowserAudioBuffer)
import WAGS.Lib.Tidal.Engine (engine)
import WAGS.Lib.Tidal.Types (IsFresh, SampleCache, TheFuture)
import WAGS.Lib.Tidal.Util (doDownloads)

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
       Unit
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
       Unit
tdlx aFuture = engine (pure unit) (pure $ const aFuture) $ Right \ac -> do
  rf <- liftEffect $ Ref.new Map.empty
  doDownloads ac rf (const $ pure unit) (aFuture { clockTime: 0.0 })
  liftEffect $ Ref.read rf