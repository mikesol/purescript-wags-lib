module WAGS.Lib.Tidal.Download where

import Prelude

import Data.Array as A
import Data.Either (Either(..), either)
import Data.Int (toNumber)
import Data.Map (Map)
import WAGS.Lib.Tidal.ObjectHack as OH
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), ParAff, delay, error, parallel, sequential, throwError, try)
import Effect.Class (liftEffect)
import Effect.Class.Console as Log
import FRP.Behavior (Behavior)
import FRP.Event (Event)
import Foreign.Object as Object
import Prim.Row (class Lacks)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Interpret (decodeAudioDataFromUri)
import WAGS.Lib.Tidal.Types (BufferUrl(..), ForwardBackwards, Sample, SampleCache)
import WAGS.WebAPI (AudioContext, BrowserAudioBuffer)

data ArrayBuffer

class Sounds (rl :: RowList Type) (r :: Row Type) where
  sounds' :: forall proxy. proxy rl -> { | r } -> Map Sample BufferUrl

instance soundsNil :: Sounds RL.Nil r where
  sounds' _ _ = Map.empty

instance soundsCons :: (Row.Cons k String r' r, Sounds c r, Row.Lacks k r', IsSymbol k) => Sounds (RL.Cons k String c) r where
  sounds' _ r = Map.insert (wrap (reflectSymbol (Proxy :: _ k))) (wrap (Record.get (Proxy :: _ k) r)) (sounds' (Proxy :: _ c) r)

chunks :: forall a. Int -> Array a -> Array (Array a)
chunks _ [] = []
chunks n xs = pure (A.take n xs) <> (chunks n $ A.drop n xs)

sounds :: forall r rl. RowToList r rl => Sounds rl r => { | r } -> Map Sample BufferUrl
sounds = sounds' (Proxy :: _ rl)

mapped :: AudioContext -> BufferUrl -> Aff { url :: BufferUrl, buffer :: ForwardBackwards }
mapped audioCtx url@(BufferUrl bf) = backoff do
  forward <- decodeAudioDataFromUri audioCtx bf
  backwards <- liftEffect $ reverseAudioBuffer audioCtx forward
  pure { url, buffer: { forward, backwards } }


getBuffersUsingCache
  :: Object.Object BufferUrl
  -> AudioContext
  -> Object.Object { url :: BufferUrl, buffer :: ForwardBackwards }
  -> Aff (Object.Object { url :: BufferUrl, buffer :: ForwardBackwards })
getBuffersUsingCache nameToUrl audioCtx alreadyDownloaded = do
  res <- Object.union <$> newBuffers <*> pure alreadyDownloaded
  pure res
  where
  toDownload :: Object.Object BufferUrl
  toDownload = OH.catMaybes $ Object.mapWithKey
    ( \k v -> case Object.lookup k alreadyDownloaded of
        Nothing -> Just v
        Just { url } -> if url == v then Nothing else Just v
    )
    nameToUrl

  toDownloadArr :: Array (String /\ BufferUrl)
  toDownloadArr = Object.toUnfoldable toDownload

  traversed :: Array (String /\ BufferUrl) -> ParAff (Array (String /\ { url :: BufferUrl, buffer :: ForwardBackwards }))
  traversed = traverse \(k /\ v) -> parallel $ backoff $ ((/\) k <$> mapped audioCtx v)
  newBuffers = map (Object.fromFoldable <<< join) $ (traverse (sequential <<< traversed) (chunks 100 toDownloadArr))

backoff :: Aff ~> Aff
backoff aff = go 0
  where
  go n
    | n >= 3 = throwError $ error "Maximum download tries exceeded"
    | otherwise = try aff >>= case _ of
        Left err -> Log.error (show err) *> delay (Milliseconds (toNumber (n + 1) * 1000.0)) *> go (n + 1)
        Right val -> pure val

foreign import reverseAudioBuffer :: AudioContext -> BrowserAudioBuffer -> Effect BrowserAudioBuffer

downloadSilence
  :: forall trigger world
   . Lacks "silence" world
  => AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { silence :: BrowserAudioBuffer | world })
downloadSilence (ac /\ aff) = ac /\ do
  trigger /\ world <- aff
  b' <- b
  pure (trigger /\ (Record.insert (Proxy :: _ "silence") b' <$> world))
  where
  b = backoff $ decodeAudioDataFromUri ac "https://freesound.org/data/previews/459/459659_4766646-lq.mp3"

initialBuffers
  :: forall trigger world
   . Lacks "buffers" world
  => Either (Behavior SampleCache) (AudioContext -> Aff SampleCache)
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { | world })
  -> AudioContext /\ Aff (Event { | trigger } /\ Behavior { buffers :: SampleCache | world })
initialBuffers bf (ac /\ aff) = ac /\ do
  trigger /\ world <- aff
  bhv <- either pure (map pure <<< (#) ac) bf
  pure (trigger /\ (Record.insert (Proxy :: _ "buffers") <$> bhv <*> world))