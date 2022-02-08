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
import Data.Typelevel.Num (class Pos)
import Data.Vec as V
import Record as Record
import Type.Proxy (Proxy(..))
import WAGS.Graph.AudioUnit (APOnOff, _off, _offOn, _on)
import WAGS.Graph.Parameter (ff)
import WAGS.Lib.Blip (makeBlip)
import WAGS.Lib.Cofree (ana, combineComonadCofreeChooseB)
import WAGS.Lib.Emitter (fEmitter, makeEmitter')
import WAGS.Lib.Score (MakeScore', CfNoteStream', makeScore)

-- | rest is of the form startTime rest
-- | duration is of the form startTime -> starting -> time -> Maybe duration
-- | use the currying to perform expensive computations based on the start time
-- | and cheaper ones based on starting
type TimeOffsetsRestDuration rest
  =
  { time :: Number
  , offsets ::
      Array
        { offset :: Number
        , rest :: Number -> rest
        , duration :: Number -> Boolean -> Number -> Maybe Number
        }
  }

type TimeOffsets
  = { time :: Number, offsets :: Array { offset :: Number } }

type TimeHeadroomFreq
  = { time :: Number, headroomInSeconds :: Number, freq :: Number }

type TimeHeadroom
  = { time :: Number, headroomInSeconds :: Number }

type TimeHeadroomInput input
  = { time :: Number, headroomInSeconds :: Number, input :: input }

type MakeBufferPoolWithRest rest a
  = TimeOffsetsRestDuration rest -> a

type MakeBufferPool a
  = TimeOffsets -> a

type MakeHotBufferPool a
  = TimeHeadroomFreq -> a

type MakeScoredBufferPool input a
  = TimeHeadroomInput input -> a

type MakeSnappyBufferPool a
  = TimeHeadroomFreq -> a

newtype Buffy rest
  = Buffy
  { starting :: Boolean
  , startTime :: Number
  , rest :: rest
  , duration :: Maybe Number
  }

derive instance newtypeBuffy :: Newtype (Buffy rest) _
derive newtype instance showBuffy :: Show rest => Show (Buffy rest)
derive newtype instance eqBuffy :: Eq rest => Eq (Buffy rest)
derive newtype instance ordBuffy :: Ord rest => Ord (Buffy rest)
instance semigroupBuffy :: Semigroup rest => Semigroup (Buffy rest) where
  append (Buffy x) (Buffy y) = Buffy
    { starting: x.starting || y.starting
    , startTime: min x.startTime y.startTime
    , rest: x.rest <> y.rest
    , duration: case x.duration, y.duration of
        Just a, Just b -> Just $ max a b
        Nothing, b -> b
        a, Nothing -> a
    }

type CfBufferPool' n r = Cofree ((->) (TimeOffsetsRestDuration r)) (BuffyVec' n r)
type CfHotBufferPool' n r = Cofree ((->) TimeHeadroomFreq) (BuffyVec' n r)
type CfSnappyBufferPool' n r = Cofree ((->) TimeHeadroomFreq) (BuffyVec' n r)
type CfBufferPoolWithRest n = CfBufferPool' n Unit
type CfBufferPool n = Cofree ((->) TimeOffsets) (BuffyVec' n Unit)
type CfHotBufferPool n = CfHotBufferPool' n Unit
type CfSnappyBufferPool n = CfSnappyBufferPool' n Unit
type CfScoredBufferPool i n r = Cofree ((->) (TimeHeadroomInput i)) (BuffyVec' n r)

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

type AScoredBufferPool i n r
  = MakeScoredBufferPool i (CfScoredBufferPool i n r)

makeBufferPoolWithRest'
  :: forall n r
   . Pos n
  => Proxy n
  -> ABufferPool' n r
makeBufferPoolWithRest' _ = go (V.fill (const Nothing))
  where
  performAttribution time offset rest duration orig =
    let
      startTime = time + (max 0.0 offset)
      tag = rest startTime
      curried = duration startTime
    in
      { orig
      , val: Just $
          { durationF: curried false
          , buf: Buffy { startTime, starting: true, rest: tag, duration: curried true time }
          }
      }

  maybeBufferToGainOnOff
    :: TimeOffsetsRestDuration r
    -> Int
    -> { orig :: Int, val :: Maybe { durationF :: Number -> Maybe Number, buf :: Buffy r } }
    -> { orig :: Int, val :: Maybe { durationF :: Number -> Maybe Number, buf :: Buffy r } }
  maybeBufferToGainOnOff { time, offsets } myPos i@{ orig, val: Nothing }
    | Just { offset, rest, duration } <- A.index offsets myPos = performAttribution time offset rest duration orig
    | otherwise = i

  maybeBufferToGainOnOff
    { time, offsets }
    myPos
    { orig
    , val: Just { durationF, buf: Buffy b }
    }
    | Just { offset, rest, duration } <- A.index offsets myPos = performAttribution time offset rest duration orig
    | otherwise =
        { orig
        , val: Just $
            { durationF
            , buf: Buffy { startTime: b.startTime, starting: false, rest: b.rest, duration: b.duration }
            }
        }

  sortForAttribution
    :: { orig :: Int, val :: Maybe { durationF :: Number -> Maybe Number, buf :: Buffy r } }
    -> { orig :: Int, val :: Maybe { durationF :: Number -> Maybe Number, buf :: Buffy r } }
    -> Ordering
  sortForAttribution { val: valA } { val: valB } = case valA, valB of
    Just { buf: Buffy a }, Just { buf: Buffy b } -> compare (a.startTime + fromMaybe 0.0 a.duration) (b.startTime + fromMaybe 0.0 b.duration)
    Nothing, Nothing -> LT
    Nothing, Just _ -> LT
    Just _, Nothing -> GT

  go
    :: V.Vec n (Maybe { durationF :: Number -> Maybe Number, buf :: Buffy r })
    -> TimeOffsetsRestDuration r
    -> CfBufferPool' n r
  go v tht@{ time, offsets } = (map <<< map) _.buf internalStates :< go internalStates
    where
    internalStates =
      if
        A.null offsets then (map <<< map)
        ( \{ durationF, buf: Buffy { startTime, rest } } ->
            { durationF
            , buf: Buffy
                { startTime
                , starting: false
                , rest
                , duration: durationF time
                }
            }
        )
        v
      else
        map _.val
          $ V.sortBy (\a b -> compare a.orig b.orig)
          $ mapWithIndex (maybeBufferToGainOnOff tht)
          $ V.sortBy sortForAttribution
          $ mapWithIndex
              ( \orig a ->
                  { orig
                  , val: map
                      ( \{ durationF, buf: Buffy { startTime, rest } } ->
                          { durationF
                          , buf: Buffy { startTime, starting: false, rest, duration: durationF time }
                          }
                      )
                      a
                  }
              )
              v

makeBufferPoolWithRest
  :: forall n r
   . Pos n
  => ABufferPool' n r
makeBufferPoolWithRest = makeBufferPoolWithRest' (Proxy :: _ n)

unitRest :: forall a. (TimeOffsetsRestDuration Unit -> a) -> TimeOffsets -> a
unitRest = lcmap
  ( over (prop (Proxy :: _ "offsets") <<< traversed)
      ( Record.union
          { rest: const unit
          , duration: const $ const $ const Nothing
          }
      )
  )

makeBufferPool
  :: forall n
   . Pos n
  => ABufferPool n
makeBufferPool = unitRest $ map (hoistCofree unitRest) makeBufferPoolWithRest

makeBufferPoolWithAnchor
  :: forall input n rest
   . Pos n
  => ( { time :: Number | input }
       -> Cofree ((->) { time :: Number | input })
            ( Array
                { offset :: Number
                , rest :: Number -> rest
                , duration :: Number -> Boolean -> Number -> Maybe Number
                }
            )
     )
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
  :: forall n rest input
   . Pos n
  => { startsAt :: Number
     , noteStream ::
         MakeScore'
           ( CfNoteStream'
               { rest :: Number -> rest
               , duration :: Number -> Boolean -> Number -> Maybe Number
               }
               input
           )
           input
     }
  -> AScoredBufferPool input n rest
makeScoredBufferPool { startsAt, noteStream } = makeBufferPoolWithAnchor $ (map <<< map <<< map)
  (\{ offset, rest: { rest, duration } } -> { offset, rest, duration })
  (makeScore { startsAt, noteStream })

makeHotBufferPoolWithRest
  :: forall n rest
   . Pos n
  => { startsAt :: Number
     , rest ::
         Cofree Identity
           { rest :: Number -> rest
           , duration :: Number -> Boolean -> Number -> Maybe Number
           }
     }
  -> AHotBufferPool' n rest
makeHotBufferPoolWithRest { startsAt, rest: rest' } = makeBufferPoolWithAnchor
  $ (map <<< map <<< map)
      (\{ offset, rest: { rest, duration } } -> { offset, rest, duration })
      (makeEmitter' { startsAt, rest: rest' })

makeHotBufferPool
  :: forall n
   . Pos n
  => { startsAt :: Number }
  -> AHotBufferPool n
makeHotBufferPool = makeHotBufferPoolWithRest <<< Record.union { rest: ana identity { rest: const unit, duration: const $ const $ const Nothing } }

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
bOnOff time = maybe (pure _off) (unwrap >>> \{ starting, startTime } -> if starting then ff (max (startTime - time) 0.0) (pure _offOn) else pure _on)
