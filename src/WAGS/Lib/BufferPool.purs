module WAGS.Lib.BufferPool where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Data.Array as A
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty)
import Data.Tuple.Nested (type (/\))
import Data.Typelevel.Num (class Pos, toInt')
import Data.Vec as V
import Type.Proxy (Proxy(..))
import WAGS.Graph.AudioUnit (APOnOff, OnOff(..))
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.Blip (makeBlip)
import WAGS.Lib.Cofree (convolveComonadCofreeChooseB)
import WAGS.Lib.Emitter (fEmitter, makeEmitter)

type TimeHeadroomOffsets rest
  = { time :: Number, headroom :: Number, offsets :: Array { offset :: Number, rest :: rest } }

type TimeHeadroomFreq
  = { time :: Number, headroom :: Number, freq :: Number }

type MakeBufferPoolWithRest rest a
  = TimeHeadroomOffsets rest -> a

type MakeHotBufferPool a
  = TimeHeadroomFreq -> a

type MakeSnappyBufferPool a
  = TimeHeadroomFreq -> a

newtype Buffy rest
  = Buffy { starting :: Boolean, startTime :: Number, rest :: rest }

derive instance newtypeBuffy :: Newtype (Buffy rest) _

instance semigroupBuffy :: Semigroup rest => Semigroup (Buffy rest) where
  append (Buffy x) (Buffy y) = Buffy
    { starting: x.starting || y.starting
    , startTime: min x.startTime y.startTime
    , rest: x.rest <> y.rest
    }

type CfBufferPool n rest = Cofree ((->) (TimeHeadroomOffsets rest)) (BuffyVec n rest)
type CfHotBufferPool n = Cofree ((->) TimeHeadroomFreq) (BuffyVec n Unit)
type CfSnappyBufferPool n = Cofree ((->) TimeHeadroomFreq) (BuffyVec n Unit)

type BuffyVec (n :: Type) (rest :: Type)
  = V.Vec n (Maybe (Buffy rest))

type ABufferPool n rest
  = MakeBufferPoolWithRest rest (CfBufferPool n rest)

type AHotBufferPool n
  = MakeHotBufferPool (CfHotBufferPool n)

type ASnappyBufferPool n
  = MakeSnappyBufferPool (CfSnappyBufferPool n)

makeBufferPool'
  :: forall n r
   . Pos n
  => Proxy n
  -> Maybe Number
  -> Maybe (NonEmpty List (Number /\ Number))
  -> ABufferPool n r
makeBufferPool' _ _ _ = go 0 (V.fill (const (Nothing :: Maybe (Buffy r))))
  where
  len :: Int
  len = toInt' (Proxy :: _ n)

  -- cPos myPos
  maybeBufferToGainOnOff
    :: Int
    -> TimeHeadroomOffsets r
    -> Int
    -> Maybe (Buffy r)
    -> Maybe (Buffy r)
  maybeBufferToGainOnOff cPos { time, offsets } myPos Nothing
    | Just { offset, rest } <- A.index offsets (myPos - cPos `mod` len) = Just (Buffy { startTime: time, starting: true, rest })
    | otherwise = Nothing

  maybeBufferToGainOnOff cPos { time, offsets } myPos (Just (Buffy b))
    | Just { offset, rest } <- A.index offsets (myPos - cPos `mod` len) = Just (Buffy { startTime: time, starting: true, rest })
    | otherwise = Just (Buffy { startTime: b.startTime, rest: b.rest, starting: false })

  go
    :: Int
    -> V.Vec n (Maybe (Buffy r))
    -> TimeHeadroomOffsets r
    -> CfBufferPool n r
  go cIdx v tht@{ offsets } = internalStates :< go ((cIdx + A.length offsets) `mod` len) internalStates
    where
    internalStates = mapWithIndex (maybeBufferToGainOnOff cIdx tht) v

makeBufferPool
  :: forall n r
   . Pos n
  => Maybe Number
  -> Maybe (NonEmpty List (Number /\ Number))
  -> ABufferPool n r
makeBufferPool = makeBufferPool' (Proxy :: _ n)

makeHotBufferPool
  :: forall n
   . Pos n
  => { prevTime :: Number, startsAt :: Number }
  -> Maybe Number
  -> Maybe (NonEmpty List (Number /\ Number))
  -> AHotBufferPool n
makeHotBufferPool ptsa dur pwf = convolveComonadCofreeChooseB
  ( \e b cont ({ time, headroom, freq } :: TimeHeadroomFreq) ->
      let
        enow = e { time, headroom, freq }
        bnow = b { time, headroom, offsets: map { rest: unit, offset: _ } (extract enow) }
      in
        cont enow bnow
  )
  (makeEmitter ptsa)
  (makeBufferPool' (Proxy :: _ n) dur pwf)

makeSnappyBufferPool
  :: forall n
   . Pos n
  => Maybe Number
  -> Maybe (NonEmpty List (Number /\ Number))
  -> ASnappyBufferPool n
makeSnappyBufferPool dur pwf = convolveComonadCofreeChooseB
  ( \e b cont ({ time, headroom, freq } :: TimeHeadroomFreq) ->
      let
        emitted = fEmitter freq { time, headroom }
        enow = e (isJust emitted)
        bnow =
          b
            { time
            , headroom
            , offsets:
                if unwrap (extract enow) then
                  fromMaybe [] (pure <<< { offset: _, rest: unit } <$> emitted)
                else []
            }
      in
        cont enow bnow
  )
  makeBlip
  (makeBufferPool dur pwf)

type Time
  = Number

bOnOff :: forall r. Time -> Maybe (Buffy r) -> APOnOff
bOnOff time = maybe (pure Off) (unwrap >>> \{ starting, startTime } -> if starting then ff (max (startTime - time) 0.0) (pure OffOn) else pure On)
