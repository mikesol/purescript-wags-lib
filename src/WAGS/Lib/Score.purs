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
type MakeScore' a input
  = { time :: Number, headroomInSeconds :: Number, input :: input } -> a

type MakeScore a = MakeScore' a Unit

type Note' rest
  = Array { offset :: Number, rest :: rest }

type Note = Note' Unit

type CfScore'' rest input = Cofree ((->) { time :: Number, headroomInSeconds :: Number, input :: input }) (Note' rest)
type CfScore' rest = CfScore'' rest Unit
type CfScore = CfScore' Unit

type AScore'' rest input = MakeScore' (CfScore'' rest input) input
type AScore' rest = AScore'' rest Unit
type AScore = AScore' Unit

type CfNoteStream' rest input = (Cofree ((->) { time :: Number, headroomInSeconds :: Number, input :: input }) { startsAfter :: Number, rest :: rest })

type CfNoteStream rest = CfNoteStream' rest Unit

makeOffsets :: forall rest input. { time :: Number, headroomInSeconds :: Number, playhead :: Number, rest :: MakeScore' (CfNoteStream' rest input) input, input :: input } -> { offsets :: Array { offset :: Number, rest :: rest }, playhead :: Number, rest :: MakeScore' (CfNoteStream' rest input) input }
makeOffsets { time, headroomInSeconds, playhead, rest, input }
  | x <- rest { time, headroomInSeconds, input }
  , r <- extract x
  , time + headroomInSeconds >= playhead + r.startsAfter = over (prop (Proxy :: Proxy "offsets"))
      (append [ { offset: playhead + r.startsAfter - time, rest: r.rest } ])
      (makeOffsets { time, headroomInSeconds, playhead: playhead + r.startsAfter, rest: unwrapCofree x, input })
  | otherwise = { offsets: [], playhead, rest }

makeScore :: forall rest input. { startsAt :: Number, noteStream :: MakeScore' (CfNoteStream' rest input) input } -> AScore'' rest input
makeScore { startsAt, noteStream } = go noteStream startsAt
  where
  go rest playhead { time, headroomInSeconds, input } =
    let
      o = makeOffsets { time, headroomInSeconds, playhead, rest, input }
    in
      o.offsets :< go o.rest o.playhead
