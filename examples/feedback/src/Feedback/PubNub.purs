module Feedback.PubNub where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Data.Either (either)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Exception (error)
import Feedback.Types (IncomingEvent)
import FRP.Event (Event, create)
import Foreign (Foreign)
import Simple.JSON (readImpl, writeImpl)

data PubNub

foreign import pubnub_ :: (Foreign -> Effect Unit) -> Effect PubNub

pubnub :: (IncomingEvent -> Effect Unit) -> Effect PubNub
pubnub f = pubnub_ ((=<<) f <<< either (throwError <<< error <<< show) pure <<< runExcept <<< readImpl)

pubnubEvent :: Effect (Event IncomingEvent /\ PubNub)
pubnubEvent =  create >>= map <$> ((/\) <<< _.event) <*> (pubnub <<< _.push)

foreign import publish_ :: PubNub -> Foreign -> Effect Unit

publish :: PubNub -> Foreign -> Effect Unit
publish p = publish_ p <<< writeImpl