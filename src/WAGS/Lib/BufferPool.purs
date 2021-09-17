module WAGS.Lib.BufferPool where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, hoistCofree, (:<))
import Data.Array as A
import Data.FunctorWithIndex (mapWithIndex)
import Data.Identity (Identity)
import Data.Lens (over, traversed)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (lcmap)
import Data.Typelevel.Num (class Pos, toInt')
import Data.Vec as V
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Graph.AudioUnit (APOnOff, OnOff(..))
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.Blip (makeBlip)
import WAGS.Lib.Cofree (combineComonadCofreeChooseB)
import WAGS.Lib.Emitter (fEmitter, makeEmitter')
import WAGS.Lib.Score (MakeScore, CfRest, makeScore)

type TimeOffsetsRest rest
  = { time :: Number, offsets :: Array { offset :: Number, rest :: rest } }

type TimeOffsets
  = { time :: Number, offsets :: Array { offset :: Number } }

type TimeHeadroomFreq
  = { time :: Number, headroomInSeconds :: Number, freq :: Number }

type TimeHeadroom
  = { time :: Number, headroomInSeconds :: Number }

type MakeBufferPoolWithRest rest a
  = TimeOffsetsRest rest -> a

type MakeBufferPool a
  = TimeOffsets -> a

type MakeHotBufferPool a
  = TimeHeadroomFreq -> a

type MakeScoredBufferPool a
  = TimeHeadroom -> a

type MakeSnappyBufferPool a
  = TimeHeadroomFreq -> a

newtype Buffy rest
  = Buffy { starting :: Boolean, startTime :: Number, rest :: rest }

derive instance newtypeBuffy :: Newtype (Buffy rest) _
derive newtype instance showBuffy :: Show rest => Show (Buffy rest)
derive newtype instance eqBuffy :: Eq rest => Eq (Buffy rest)
derive newtype instance ordBuffy :: Ord rest => Ord (Buffy rest)
instance semigroupBuffy :: Semigroup rest => Semigroup (Buffy rest) where
  append (Buffy x) (Buffy y) = Buffy { starting: x.starting || y.starting, startTime: min x.startTime y.startTime, rest: x.rest <> y.rest }

type CfBufferPool' n r = Cofree ((->) (TimeOffsetsRest r)) (BuffyVec' n r)
type CfHotBufferPool' n r = Cofree ((->) TimeHeadroomFreq) (BuffyVec' n r)
type CfSnappyBufferPool' n r = Cofree ((->) TimeHeadroomFreq) (BuffyVec' n r)
type CfBufferPoolWithRest n = CfBufferPool' n Unit
type CfBufferPool n = Cofree ((->) TimeOffsets) (BuffyVec' n Unit)
type CfHotBufferPool n = CfHotBufferPool' n Unit
type CfSnappyBufferPool n = CfSnappyBufferPool' n Unit
type CfScoredBufferPool n r = Cofree ((->) TimeHeadroom) (BuffyVec' n r)

type BuffyVec' (n :: Type) (r :: Type)
  = V.Vec n (Maybe (Buffy r))

type BuffyVec n = BuffyVec' n Unit

type ABufferPool' n r
  = MakeBufferPoolWithRest r (CfBufferPool' n r)

type AHotBufferPool' n r
  = MakeHotBufferPool (CfHotBufferPool' n r)

type ASnappyBufferPool' n r
  = MakeSnappyBufferPool (CfSnappyBufferPool' n r)

type ABufferPool n
  = MakeBufferPool (Cofree ((->) TimeOffsets) (BuffyVec' n Unit))

type AHotBufferPool n
  = AHotBufferPool' n Unit

type ASnappyBufferPool n
  = ASnappyBufferPool' n Unit

type AScoredBufferPool n r
  = MakeScoredBufferPool (CfScoredBufferPool n r)

makeBufferPoolWithRest'
  :: forall n r
   . Pos n
  => Proxy n
  -> ABufferPool' n r
makeBufferPoolWithRest' _ = go 0 (V.fill (const (Nothing :: Maybe (Buffy r))))
  where
  len :: Int
  len = toInt' (Proxy :: _ n)

  -- cPos myPos
  maybeBufferToGainOnOff
    :: Int
    -> TimeOffsetsRest r
    -> Int
    -> Maybe (Buffy r)
    -> Maybe (Buffy r)
  -- for now, we normalize offset to 0 as negative emitters make sense theoretically but not when starting a buffer
  maybeBufferToGainOnOff cPos { time, offsets } myPos Nothing
    | Just { offset, rest } <- A.index offsets (myPos - cPos `mod` len) = Just (Buffy { startTime: time + (max 0.0 offset), starting: true, rest })
    | otherwise = Nothing

  maybeBufferToGainOnOff cPos { time, offsets } myPos (Just (Buffy b))
    | Just { offset, rest } <- A.index offsets (myPos - cPos `mod` len) = Just (Buffy { startTime: time + (max 0.0 offset), starting: true, rest })
    | otherwise = Just (Buffy { startTime: b.startTime, starting: false, rest: b.rest })

  go
    :: Int
    -> V.Vec n (Maybe (Buffy r))
    -> TimeOffsetsRest r
    -> CfBufferPool' n r
  go cIdx v tht@{ offsets } = internalStates :< go ((cIdx + A.length offsets) `mod` len) internalStates
    where
    internalStates = mapWithIndex (maybeBufferToGainOnOff cIdx tht) v

makeBufferPoolWithRest
  :: forall n r
   . Pos n
  => ABufferPool' n r
makeBufferPoolWithRest = makeBufferPoolWithRest' (Proxy :: _ n)

unitRest :: forall a. (TimeOffsetsRest Unit -> a) -> TimeOffsets -> a
unitRest = lcmap (over (prop (Proxy :: _ "offsets") <<< traversed) (Record.union { rest: unit }))

makeBufferPool
  :: forall n
   . Pos n
  => ABufferPool n
makeBufferPool = unitRest $ map (hoistCofree unitRest) makeBufferPoolWithRest

makeBufferPoolWithAnchor
  :: forall input n rest
   . Pos n
  => ({ time :: Number | input } -> Cofree ((->) { time :: Number | input }) (Array { offset :: Number, rest :: rest }))
  -> { time :: Number | input }
  -> Cofree ((->) { time :: Number | input }) (V.Vec n (Maybe (Buffy rest)))
makeBufferPoolWithAnchor cf = combineComonadCofreeChooseB
  ( \cont e b (a@{ time }) ->
      let
        enow = e a
        bnow = b { time, offsets: extract enow }
      in
        cont enow bnow
  )
  cf
  (makeBufferPoolWithRest)

makeScoredBufferPool
  :: forall n rest
   . Pos n
  => { startsAt :: Number, rest :: MakeScore (CfRest rest) }
  -> AScoredBufferPool n rest
makeScoredBufferPool sar = makeBufferPoolWithAnchor (makeScore sar)

makeHotBufferPoolWithRest
  :: forall n rest
   . Pos n
  => { startsAt :: Number, rest :: Cofree Identity rest }
  -> AHotBufferPool' n rest
makeHotBufferPoolWithRest sar = makeBufferPoolWithAnchor (makeEmitter' sar)

makeHotBufferPool
  :: forall n
   . Pos n
  => { startsAt :: Number }
  -> AHotBufferPool n
makeHotBufferPool = makeHotBufferPoolWithRest <<< Record.union { rest: mempty :: Cofree Identity Unit }

makeSnappyBufferPool
  :: forall n
   . Pos n
  => ASnappyBufferPool n
makeSnappyBufferPool = combineComonadCofreeChooseB
  ( \cont e b ({ time, headroomInSeconds, freq } :: TimeHeadroomFreq) ->
      let
        emitted = fEmitter freq { time, headroomInSeconds }
        enow = e (isJust emitted)
        bnow =
          b
            { time
            , offsets:
                if unwrap (extract enow) then
                  fromMaybe [] (pure <<< { offset: _ } <$> emitted)
                else []
            }
      in
        cont enow bnow
  )
  makeBlip
  makeBufferPool

type Time
  = Number

bOnOff :: forall r. Time -> Maybe (Buffy r) -> APOnOff
bOnOff time = maybe (pure Off) (unwrap >>> \{ starting, startTime } -> if starting then ff (max (startTime - time) 0.0) (pure OffOn) else pure On)
