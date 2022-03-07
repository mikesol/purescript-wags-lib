module Feedback.PubNub where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Data.Either (either)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Exception (error)
import FRP.Event (Event, create)
import Feedback.Types (IncomingEvent(..))
import Foreign (Foreign, ForeignError(..), fail)
import Simple.JSON (readImpl, writeImpl)
import Simple.JSON as JSON

data PubNub

data Click = Click

instance toJsonClick :: JSON.WriteForeign Click where
  writeImpl = const $ writeImpl "CLICK"

instance fromJsonClick :: JSON.ReadForeign Click where
  readImpl = readImpl >=>
    (if _ then _ else _)
      <$> (eq "CLICK")
      <*> (const $ pure Click)
      <*> (fail <<< ForeignError <<< (<>) "Not CLICK: ")

newtype UIEvent = UIEvent
  ( Variant
      ( mouse ::
          { x :: Number
          , y :: Number
          }
      , click :: Click
      )
  )

derive instance newtypeuiEvent :: Newtype UIEvent _
derive newtype instance toJSONUIEvent :: JSON.ReadForeign UIEvent
derive newtype instance fromJSONUIEvent :: JSON.WriteForeign UIEvent

newtype PubNubMessage = PubNubMessage
  ( Variant
      ( music :: IncomingEvent
      , ui :: UIEvent
      )
  )

derive instance newtypePubNubPubNubMessage :: Newtype PubNubMessage _
derive newtype instance toJSONPubNubPubNubMessage :: JSON.ReadForeign PubNubMessage
derive newtype instance fromJSONPubNubPubNubMessage :: JSON.WriteForeign PubNubMessage

newtype PubNubEvent = PubNubEvent
  { publisher :: String
  , message :: PubNubMessage
  }

derive instance newtypePubNubEvent :: Newtype PubNubEvent _
derive newtype instance toJSONPubNubEvent :: JSON.ReadForeign PubNubEvent
derive newtype instance fromJSONPubNubEvent :: JSON.WriteForeign PubNubEvent

foreign import pubnub_ :: (Foreign -> Effect Unit) -> Effect PubNub

pubnub :: (PubNubEvent -> Effect Unit) -> Effect PubNub
pubnub f = pubnub_ ((=<<) f <<< either (throwError <<< error <<< show) pure <<< runExcept <<< readImpl)

pubnubEvent :: Effect (Event PubNubEvent /\ PubNub)
pubnubEvent = create >>= map <$> ((/\) <<< _.event) <*> (pubnub <<< _.push)

foreign import publish_ :: PubNub -> Foreign -> Effect Unit

publish :: PubNub -> PubNubMessage -> Effect Unit
publish p = publish_ p <<< writeImpl