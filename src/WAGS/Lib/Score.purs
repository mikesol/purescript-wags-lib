-- | An emitter is an impulse that is emitted once in the case of comonads or as few times as possible in the case of functions.
module WAGS.Lib.Score where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

-------
type MakeScore a
  = { time :: Number, headroomInSeconds :: Number } -> a

type Note' rest
  = Array { offset :: Number, rest :: rest }

type Note = Note' Unit

type CfScore' rest = Cofree ((->) { time :: Number, headroomInSeconds :: Number }) (Note' rest)
type CfScore = CfScore' Unit

type AnScore' rest = MakeScore (CfScore' rest)
type AnScore = MakeScore (CfScore' Unit)

type CfRest rest = (Cofree ((->) { time :: Number, headroomInSeconds :: Number }) { duration :: Number, rest :: rest })

makeOffsets :: forall rest. { time :: Number, headroomInSeconds :: Number, playhead :: Number, rest :: MakeScore (CfRest rest) } -> { offsets :: Array { offset :: Number, rest :: rest }, playhead :: Number, rest :: MakeScore (CfRest rest) }
makeOffsets { time, headroomInSeconds, playhead, rest }
  | x <- rest { time, headroomInSeconds }
  , r <- extract x
  , time + headroomInSeconds >= playhead + r.duration = over (prop (Proxy :: Proxy "offsets"))
      (append [ { offset: playhead + r.duration - time, rest: r.rest } ])
      (makeOffsets { time, headroomInSeconds, playhead: playhead + r.duration, rest: unwrapCofree x })
  | otherwise = { offsets: [], playhead, rest }

makeScore :: forall rest. { startsAt :: Number, rest :: MakeScore (CfRest rest) } -> AnScore' rest
makeScore { startsAt, rest: r } = go r startsAt

  where
  go rest playhead { time, headroomInSeconds } =
    let
      o = makeOffsets { time, headroomInSeconds, playhead, rest }
    in
      o.offsets :< go o.rest o.playhead
