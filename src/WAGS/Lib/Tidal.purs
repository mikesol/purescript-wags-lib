module WAGS.Lib.Tidal where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (wrap)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.Object as Object
import WAGS.Lib.Tidal.Engine (engine)
import WAGS.Lib.Tidal.Types (TidalFSB, emptyCtrl)
import WAGS.Lib.Tidal.Types as T
import WAGS.Lib.Tidal.Util (doDownloads)

-- todo: we may want to cache buffers between runs in trypurescript
-- if that's the case, we'll need to accept some form of ref instead of
-- downloading fresh each time
tdl :: T.AFuture -> TidalFSB
tdl = tdlx <<< pure

tdlx :: ({ clockTime :: Number } -> T.AFuture) -> TidalFSB
tdlx aFuture = engine (pure unit) (pure $ const aFuture) (pure $ emptyCtrl) (pure (wrap mempty)) $ Right \ac -> do
  rf <- liftEffect $ Ref.new Object.empty
  doDownloads ac rf (const $ pure unit) ((#) { clockTime: 0.0 }) aFuture
  liftEffect $ Ref.read rf