module WAGS.Lib.Run where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Run (match, peel)
import Run as Run
import Run.Except (Except)
import Run.Reader (Reader(..))
import Run.State (State(..))
import Type.Proxy (Proxy(..))
import WAGS.Change (class Change, change)
import WAGS.Control.Functions (modifyRes)
import WAGS.Control.Types (WAG, Frame)
import WAGS.Interpret (class AudioInterpret)

data ShortCircuit = ShortCircuit

newtype Wag audio engine res graph a = Wag (forall proof. WAG audio engine proof res graph Unit -> WAG audio engine proof res graph a)

derive instance functorWag :: Functor (Wag audio engine res graph)

type WAG_ audio engine res graph r = (wag :: Wag audio engine res graph | r)

_wag :: Proxy "wag"
_wag = Proxy

rChangeAt
  :: forall proxy t r s ch audio engine res graph
   . IsSymbol s
  => Row.Cons s (Wag audio engine res graph) t r
  => Change ch graph
  => AudioInterpret audio engine
  => proxy s
  -> { | ch }
  -> Run.Run r Unit
rChangeAt sym ch = Run.lift sym (Wag (\i -> change (i $> ch)))

rChange
  :: forall r ch audio engine res graph
   . Change ch graph
  => AudioInterpret audio engine
  => { | ch }
  -> Run.Run (WAG_ audio engine res graph r) Unit
rChange = rChangeAt _wag

rModifyrResAt
  :: forall proxy t r s audio engine res graph
   . IsSymbol s
  => Row.Cons s (Wag audio engine res graph) t r
  => proxy s
  -> (res -> res)
  -> Run.Run r res
rModifyrResAt sym f = Run.lift sym (Wag (\i -> modifyRes f i))

rModifyrRes
  :: forall r audio engine res graph
   . (res -> res)
  -> Run.Run (WAG_ audio engine res graph r) res
rModifyrRes = rModifyrResAt _wag

type RunWag env control audio engine res graph a =
  Run.Run
    ( wag :: Wag audio engine res graph
    , reader :: Reader env
    , state :: State control
    , except :: Except ShortCircuit
    )
    a

type RunLoop control audio engine proof res graph r =
  { wag :: WAG audio engine proof res graph Unit, control :: control | r }

type RunStep env control audio engine proof res graph a =
  RunLoop control audio engine proof res graph (monad :: RunWag env control audio engine res graph a)

type RunDone control audio engine proof res graph a =
  RunLoop control audio engine proof res graph (val :: a)

-- after the run,
-- runWag has the same signature as `loop`, allowing it to be a drop in replacement for loop
runWag
  :: forall env control audio engine proof res graph
   . AudioInterpret audio engine
  => RunWag env control audio engine res graph Unit
  -> WAG audio engine proof res graph control
  -> Frame env audio engine proof res graph control
runWag monad' wag' env' = res.wag $> res.control
  where
  res = tailRec go { wag: wag' $> unit, control: extract wag', monad: monad' }

  go
    :: forall a
     . RunStep env control audio engine proof res graph a
    -> Step
         (RunStep env control audio engine proof res graph a)
         (RunDone control audio engine proof res graph (Either ShortCircuit a))
  go { wag: w, control, monad } = case peel monad of
    Left var -> match
      { wag: \(Wag wg) -> let wg' = wg w in Loop { wag: wg' $> unit, control, monad: extract wg' }
      , reader: \(Reader f) -> Loop { wag: w, control, monad: f env' }
      , state: \(State s f) -> let control' = s control in Loop { wag: w, control: control', monad: f control' }
      , except: \_ -> Done { wag: w, control, val: Left ShortCircuit }
      }
      var
    Right val -> Done { wag: w, control, val: Right val }
