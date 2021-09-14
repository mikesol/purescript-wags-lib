module WAGS.Lib.BufferPool where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Data.Array as A
import Data.Eq (class EqRecord)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Typelevel.Num (class Pos, toInt')
import Data.Vec as V
import Prim.Row (class Lacks)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Graph.AudioUnit (APOnOff, OnOff(..))
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.Blip (makeBlip)
import WAGS.Lib.Cofree (combineComonadCofreeChooseB)
import WAGS.Lib.Emitter (fEmitter, makeEmitter)

type TimeOffsets
  = { time :: Number, offsets :: Array { offset :: Number } }

type TimeHeadroomFreq
  = { time :: Number, headroom :: Number, freq :: Number }

type MakeBufferPool a
  = TimeOffsets -> a

type MakeHotBufferPool a
  = TimeHeadroomFreq -> a

type MakeSnappyBufferPool a
  = TimeHeadroomFreq -> a

newtype Buffy r
  = Buffy { starting :: Boolean, startTime :: Number | r }

derive instance newtypeBuffy :: Newtype (Buffy r) _
instance showBuffy ::
  ( Show { | r }
  , Lacks "starting" r
  , Lacks "startTime" r
  ) =>
  Show (Buffy r) where
  show (Buffy x@{ starting, startTime }) = show $ "Buffy "
    <> (show { starting, startTime })
    <> " "
    <> (show x')

    where
    f = Record.delete (Proxy :: _ "starting") <<< Record.delete (Proxy :: _ "startTime")
    x' = f x

instance eqBuffy ::
  ( Eq { | r }
  , Lacks "starting" r
  , Lacks "startTime" r
  ) =>
  Eq (Buffy r) where
  eq (Buffy x) (Buffy y) = x.starting == y.starting && x.startTime == y.startTime && x' == y'
    where
    f = Record.delete (Proxy :: _ "starting") <<< Record.delete (Proxy :: _ "startTime")
    x' = f x
    y' = f x

instance ordBuffy ::
  ( RL.RowToList r rl
  , EqRecord rl r
  , Ord { | r }
  , Lacks "starting" r
  , Lacks "startTime" r
  ) =>
  Ord (Buffy r) where
  compare (Buffy x) (Buffy y) = if c0 == EQ then if c1 == EQ then c2 else c1 else c0
    where
    f = Record.delete (Proxy :: _ "starting") <<< Record.delete (Proxy :: _ "startTime")
    x' = f x
    y' = f x
    c0 = compare x.starting y.starting
    c1 = compare x.startTime y.startTime
    c2 = compare x' y'

instance semigroupBuffy ::
  ( Semigroup { | r }
  , Lacks "starting" r
  , Lacks "startTime" r
  ) =>
  Semigroup (Buffy r) where
  append (Buffy x) (Buffy y) = Buffy
    $ Record.insert (Proxy :: _ "starting") (x.starting || y.starting)
    $ Record.insert (Proxy :: _ "startTime") (min x.startTime y.startTime) (x' <> y')
    where
    f = Record.delete (Proxy :: _ "starting") <<< Record.delete (Proxy :: _ "startTime")
    x' = f x
    y' = f y

type CfBufferPool' n r = Cofree ((->) (TimeOffsets)) (BuffyVec' n r)
type CfHotBufferPool' n r = Cofree ((->) TimeHeadroomFreq) (BuffyVec' n r)
type CfSnappyBufferPool' n r = Cofree ((->) TimeHeadroomFreq) (BuffyVec' n r)
type CfBufferPool n = CfBufferPool' n ()
type CfHotBufferPool n = CfHotBufferPool' n ()
type CfSnappyBufferPool n = CfSnappyBufferPool' n ()

type BuffyVec' (n :: Type) (r :: Row Type)
  = V.Vec n (Maybe (Buffy r))

type BuffyVec n = BuffyVec' n ()

type ABufferPool' n r
  = MakeBufferPool (CfBufferPool' n r)

type AHotBufferPool' n r
  = MakeHotBufferPool (CfHotBufferPool' n r)

type ASnappyBufferPool' n r
  = MakeSnappyBufferPool (CfSnappyBufferPool' n r)

type ABufferPool n
  = ABufferPool' n ()

type AHotBufferPool n
  = AHotBufferPool' n ()

type ASnappyBufferPool n
  = ASnappyBufferPool' n ()

makeBufferPool'
  :: forall n
   . Pos n
  => Proxy n
  -> ABufferPool n
makeBufferPool' _ = go 0 (V.fill (const (Nothing :: Maybe (Buffy ()))))
  where
  len :: Int
  len = toInt' (Proxy :: _ n)

  -- cPos myPos
  maybeBufferToGainOnOff
    :: Int
    -> TimeOffsets
    -> Int
    -> Maybe (Buffy ())
    -> Maybe (Buffy ())
  -- for now, we normalize offset to 0 as negative emitters make sense theoretically but not when starting a buffer
  maybeBufferToGainOnOff cPos { time, offsets } myPos Nothing
    | Just { offset } <- A.index offsets (myPos - cPos `mod` len) = Just (Buffy { startTime: time + (max 0.0 offset), starting: true })
    | otherwise = Nothing

  maybeBufferToGainOnOff cPos { time, offsets } myPos (Just (Buffy b))
    | Just { offset } <- A.index offsets (myPos - cPos `mod` len) = Just (Buffy { startTime: time + (max 0.0 offset), starting: true })
    | otherwise = Just (Buffy { startTime: b.startTime, starting: false })

  go
    :: Int
    -> V.Vec n (Maybe (Buffy ()))
    -> TimeOffsets
    -> CfBufferPool n
  go cIdx v tht@{ offsets } = internalStates :< go ((cIdx + A.length offsets) `mod` len) internalStates
    where
    internalStates = mapWithIndex (maybeBufferToGainOnOff cIdx tht) v

makeBufferPool
  :: forall n
   . Pos n
  => ABufferPool n
makeBufferPool = makeBufferPool' (Proxy :: _ n)

makeHotBufferPool
  :: forall n
   . Pos n
  => { startsAt :: Number }
  -> AHotBufferPool n
makeHotBufferPool sa = combineComonadCofreeChooseB
  ( \cont e b ({ time, headroom, freq } :: TimeHeadroomFreq) ->
      let
        enow = e { time, headroom, freq }
        bnow = b { time, offsets: map { offset: _ } (extract enow) }
      in
        cont enow bnow
  )
  (makeEmitter sa)
  (makeBufferPool' (Proxy :: _ n))

makeSnappyBufferPool
  :: forall n
   . Pos n
  => ASnappyBufferPool n
makeSnappyBufferPool = combineComonadCofreeChooseB
  ( \cont e b ({ time, headroom, freq } :: TimeHeadroomFreq) ->
      let
        emitted = fEmitter freq { time, headroom }
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

bOnOff :: Time -> Maybe (Buffy ()) -> APOnOff
bOnOff time = maybe (pure Off) (unwrap >>> \{ starting, startTime } -> if starting then ff (max (startTime - time) 0.0) (pure OffOn) else pure On)
