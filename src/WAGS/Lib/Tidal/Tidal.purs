module WAGS.Lib.Tidal.Tidal
  ( asScore
  , b
  , b'
  , b_
  , betwixt
  , c2s
  , class S
  , cycleP
  , cycleP_
  , derivative
  , djQuickCheck
  , focus
  , i
  , i'
  , i_
  , impatient
  , drone
  , intentionalSilenceForInternalUseOnly
  , l_j
  , l_r
  , lct
  , lcw
  , ldf
  , ldle
  , ldls
  , ldr
  , lds
  , ldt
  , ldv
  , lfb
  , lfc
  , lfd
  , lfl
  , lfn
  , lft
  , lnbo
  , changeBufferOffset
  , lnf
  , changeForward
  , lnr
  , changeRate
  , lns
  , changeSample
  , lnv
  , changeVolume
  , ltd
  , ltn
  , lts
  , ltt
  , lvg
  , lvt
  , addEffect
  , make
  , module WAGS.Lib.Tidal.Cycle
  , onTag
  , onTag'
  , onTagWithIndex
  , onTagWithIndex'
  , onTags
  , onTags'
  , onTagsC
  , onTagsC'
  , onTagsCWithIndex
  , onTagsCWithIndex'
  , onTagsWithIndex
  , onTagsWithIndex'
  , openFuture
  , openVoice
  , parse
  , parse_
  , plainly
  , rend
  , rendNit
  , rend_
  , s
  , s2f
  , sequentialcyclePInternal
  , u
  , unrest
  , when_
  , x
  , x'
  , x_
  ----- internal
  , parseWithBrackets
  , ident
  ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Comonad.Cofree ((:<))
import Control.Monad.State (evalState, get, put)
import Data.Array (fold, null, uncons, (..))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Either (Either(..), either, hush)
import Data.Filterable (compact, filter, filterMap, maybeBool)
import Data.Function (on)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Homogeneous.Record (fromHomogeneous, homogeneous)
import Data.Int (fromString, toNumber)
import Data.Lens (Lens', Prism', _Just, _Right, iso, lens, over, prism', set, traversed)
import Data.Lens.Iso.Newtype (unto)
import Data.Lens.Record (prop)
import Data.List (List(..), foldMap, foldl, (:))
import Data.List as L
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Profunctor (class Profunctor, lcmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Set as Set
import Data.String.CodeUnits as CU
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Typelevel.Num (D1)
import Data.Unfoldable (replicate)
import Data.Vec ((+>))
import Data.Vec as V
import Foreign.Object (Object)
import Foreign.Object as O
import Foreign.Object as Object
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record as Record
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf1, elements, frequency, resize)
import Text.Parsing.StringParser (Parser, fail, runParser, try)
import Text.Parsing.StringParser.CodeUnits (alphaNum, anyDigit, char, oneOf, satisfy, skipSpaces)
import Text.Parsing.StringParser.Combinators (between, many, many1, optionMaybe, sepBy1, sepEndBy, sepEndBy1)
import Type.Proxy (Proxy(..))
import WAGS.Create (class Create)
import WAGS.Create.Optionals (input)
import WAGS.Graph.AudioUnit as CTOR
import WAGS.Graph.Parameter (Maybe', _just, _maybe, _nothing)
import WAGS.Lib.Tidal.Cycle (Cycle(..), singleton, branching, simultaneous, internal, flattenCycle, intentionalSilenceForInternalUseOnly_, reverse)
import WAGS.Lib.Tidal.FX (WAGSITumult)
import WAGS.Lib.Tidal.SampleDurs (sampleToDur, sampleToDur')
import WAGS.Lib.Tidal.Samples (class ClockTime, clockTime, sample2drone)
import WAGS.Lib.Tidal.Samples as S
import WAGS.Lib.Tidal.Types (AH, AH', AfterMatter, BufferUrl, ClockTimeIs, CycleDuration(..), DroneNote(..), EWF, EWF', Either', FoT, Globals(..), NextCycle(..), Note(..), NoteInFlattenedTime(..), NoteInTime(..), O'Past, Sample(..), Tag, TheFuture(..), TimeIs', TimeIsAndWas, UnsampledTimeIs, Voice(..), ClockTimeIs', _either, _hush, _left, _right)
import Data.Variant (match)
import WAGS.Tumult (Tumultuous)
import WAGS.Tumult.Make (tumultuously)
import WAGS.Validation (class NodesCanBeTumultuous, class SubgraphIsRenderable)

-- | Only play the first cycle, and truncate/interrupt the playing cycle at the next sub-ending.
impatient :: NextCycle ~> NextCycle
impatient = over (unto NextCycle <<< prop (Proxy :: _ "force")) (const true)

make
  :: forall inRec overfull rest event
   . Union inRec
       ( EWF' (CycleDuration -> Voice event)
           ( AH' (Maybe' (DroneNote event))
               ( title :: String
               , sounds :: Object BufferUrl
               , preload :: Array Sample
               )
           )
       )
       overfull
  => Nub overfull
       ( EWF' (CycleDuration -> Voice event)
           ( AH' (Maybe' (DroneNote event))
               ( title :: String
               , sounds :: Object BufferUrl
               , preload :: Array Sample
               | rest
               )
           )
       )
  => Number
  -> { | inRec }
  -> TheFuture event
make cl rr = TheFuture $ Record.union
  ( fromHomogeneous $ map ((#) cycleDuration) $ homogeneous
      { earth: z.earth
      , wind: z.wind
      , fire: z.fire
      }
  )
  { air: z.air
  , heart: z.heart
  , title: z.title
  , sounds: z.sounds
  , preload: z.preload
  , cycleDuration
  }
  where
  cycleDuration = wrap cl
  z =
    Record.merge rr
      ( Record.union (openVoices :: OpenVoices event)
          $ Record.union (openDrones :: OpenDrones event)
              { title: "wagsi @ tidal"
              , preload: [] :: Array Sample
              , sounds: Object.empty :: Object BufferUrl
              }
      )
      ::
           { | EWF' (CycleDuration -> Voice event)
               ( AH' (Maybe' (DroneNote event))
                   ( title :: String
                   , sounds :: Object BufferUrl
                   , preload :: Array Sample
                   | rest
                   )
               )
           }

fx
  :: forall scene graph graphRL
   . RowToList graph graphRL
  => Create scene () graph
  => SubgraphIsRenderable graph "output" (voice :: Unit)
  => NodesCanBeTumultuous graphRL
  => { | scene }
  -> Tumultuous D1 "output" (voice :: Unit)
fx scene = tumultuously (scene +> V.empty)

hello :: CTOR.Input "voice" /\ {}
hello = input (Proxy :: _ "voice")

goodbye :: forall a. a -> { output :: a }
goodbye = { output: _ }

calm :: Tumultuous D1 "output" (voice :: Unit)
calm = fx $ goodbye hello

plainly :: NextCycle ~> Voice
plainly = Voice <<< { globals: Globals { gain: const 1.0, fx: const calm }, next: _ }

--- lenses
l_j :: forall a. Prism' (Maybe' a) a
l_j = prism' _just (_maybe Nothing Just)

l_r :: forall a b. Prism' (Either' a b) b
l_r = prism' _right _hush

lvg :: forall event. Lens' (Voice event) (O'Past event)
lvg = unto Voice <<< prop (Proxy :: _ "globals") <<< unto Globals <<< prop (Proxy :: _ "gain")

lvt :: forall event. Lens' (Voice event) (ClockTimeIs event -> Tumultuous D1 "output" (voice :: Unit))
lvt = unto Voice <<< prop (Proxy :: _ "globals") <<< unto Globals <<< prop (Proxy :: _ "fx")

addEffect
  :: forall event
   . ( ClockTimeIs' event
       -> Tumultuous D1 "output" (voice :: Unit)
     )
  -> Voice event
  -> Voice event
addEffect = set lvt <<< lcmap unwrap

lfn :: forall note. Lens' (NoteInFlattenedTime note) note
lfn = unto NoteInFlattenedTime <<< prop (Proxy :: _ "note")

lfb :: forall note. Lens' (NoteInFlattenedTime note) Number
lfb = unto NoteInFlattenedTime <<< prop (Proxy :: _ "bigStartsAt")

lfl :: forall note. Lens' (NoteInFlattenedTime note) Number
lfl = unto NoteInFlattenedTime <<< prop (Proxy :: _ "littleStartsAt")

lfc :: forall note. Lens' (NoteInFlattenedTime note) Int
lfc = unto NoteInFlattenedTime <<< prop (Proxy :: _ "currentCycle")

lfd :: forall note. Lens' (NoteInFlattenedTime note) Number
lfd = unto NoteInFlattenedTime <<< prop (Proxy :: _ "duration")

lft :: forall p note. Choice p => Strong p => p String String -> p (NoteInFlattenedTime note) (NoteInFlattenedTime note)
lft = unto NoteInFlattenedTime <<< prop (Proxy :: _ "tag") <<< _Just

ltn :: forall note. Lens' (NoteInTime note) note
ltn = unto NoteInTime <<< prop (Proxy :: _ "note")

lts :: forall note. Lens' (NoteInTime note) Number
lts = unto NoteInTime <<< prop (Proxy :: _ "startsAt")

ltd :: forall note. Lens' (NoteInTime note) Number
ltd = unto NoteInTime <<< prop (Proxy :: _ "duration")

ltt :: forall p note. Choice p => Strong p => p String String -> p (NoteInTime note) (NoteInTime note)
ltt = unto NoteInTime <<< prop (Proxy :: _ "tag") <<< _Just

setter' :: forall t163 t170 t171 t172 t175 t176 t177 t178. Traversable t170 => Profunctor t177 => Newtype t178 t175 => ((t163 -> t177 t178 t176) -> t171 -> t172) -> t177 t175 t176 -> t170 t171 -> t170 t172
setter' len = set (traversed <<< len) <<< lcmap unwrap

lns :: forall event. Lens' (Note event) (Either' (UnsampledTimeIs event -> Sample) Sample)
lns = unto Note <<< prop (Proxy :: _ "sampleFoT")

changeSample
  :: forall container event
   . Traversable container
  => Sample
  -> container (Note event)
  -> container (Note event)
changeSample = set (traversed <<< lns <<< iso (_either Left Right) (either _left _right) <<< _Right)

lnr :: forall event. Lens' (Note event) (FoT event)
lnr = unto Note <<< prop (Proxy :: _ "rateFoT")

type ChangeSig =
  forall container event
   . Traversable container
  => (TimeIs' event -> Number)
  -> container (Note event)
  -> container (Note event)

changeRate :: ChangeSig
changeRate = setter' lnr

lnbo :: forall event. Lens' (Note event) (FoT event)
lnbo = unto Note <<< prop (Proxy :: _ "bufferOffsetFoT")

changeBufferOffset :: ChangeSig
changeBufferOffset = setter' lnbo

lnf :: forall event. Lens' (Note event) Boolean
lnf = unto Note <<< prop (Proxy :: _ "forward")

changeForward
  :: forall container event
   . Traversable container
  => Boolean
  -> container (Note event)
  -> container (Note event)
changeForward = set (traversed <<< lnf)

lnv :: forall event. Lens' (Note event) (FoT event)
lnv = unto Note <<< prop (Proxy :: _ "volumeFoT")

changeVolume :: ChangeSig
changeVolume = setter' lnv

lds :: forall event. Lens' (DroneNote event) Sample
lds = unto DroneNote <<< prop (Proxy :: _ "sample")

ldr :: forall event. Lens' (DroneNote event) (O'Past event)
ldr = unto DroneNote <<< prop (Proxy :: _ "rateFoT")

ldls :: forall event. Lens' (DroneNote event) (O'Past event)
ldls = unto DroneNote <<< prop (Proxy :: _ "loopStartFoT")

ldle :: forall event. Lens' (DroneNote event) (O'Past event)
ldle = unto DroneNote <<< prop (Proxy :: _ "loopEndFoT")

ldf :: forall event. Lens' (DroneNote event) Boolean
ldf = unto DroneNote <<< prop (Proxy :: _ "forward")

ldv :: forall event. Lens' (DroneNote event) (O'Past event)
ldv = unto DroneNote <<< prop (Proxy :: _ "volumeFoT")

ldt :: forall event. Lens' (DroneNote event) (ClockTimeIs event -> WAGSITumult)
ldt = unto DroneNote <<< prop (Proxy :: _ "tumultFoT")

lcw :: forall note. Lens' (Cycle note) Number
lcw = lens getWeight
  ( unwrap >>> match {
      branching: \ii -> \weight -> branching $ ii { env = ii.env { weight = weight } },
      simultaneous: \ii -> \weight -> simultaneous $ ii { env = ii.env { weight = weight } },
      internal: \ii -> \weight -> internal $ ii { env = ii.env { weight = weight } },
      singleton: \ii -> \weight -> singleton $ ii { env = ii.env { weight = weight } }
  })

lct :: forall note. Lens' (Cycle note) (Maybe String)
lct = lens getTag
  ( unwrap >>> match
      { branching: \ii -> \tag -> branching $ ii { env = ii.env { tag = tag } }
      , simultaneous: \ii -> \tag -> simultaneous $ ii { env = ii.env { tag = tag } }
      , internal: \ii -> \tag -> internal $ ii { env = ii.env { tag = tag } }
      , singleton: \ii -> \tag -> singleton $ ii { env = ii.env { tag = tag } }
      }
  )

---

when_ :: forall a. (a -> Boolean) -> (a -> a) -> a -> a
when_ cond func aa = if cond aa then func aa else aa

focus :: forall a. (a -> Boolean) -> Prism' a a
focus = prism' identity <<< maybeBool

---
b :: forall event. Cycle (Maybe (Note event)) -> Array (Cycle (Maybe (Note event))) -> Cycle (Maybe (Note event))
b bx by = branching { env: { weight: 1.0, tag: Nothing }, cycles: NEA.fromNonEmpty (bx :| by) }

b' :: forall event. Cycle (Maybe (Note event)) -> Cycle (Maybe (Note event))
b' bx = b bx []

b_ :: Cycle (Maybe (Note Unit)) -> Array (Cycle (Maybe (Note Unit))) -> Cycle (Maybe (Note Unit))
b_ = b

i :: forall event. Cycle (Maybe (Note event)) -> Array (Cycle (Maybe (Note event))) -> Cycle (Maybe (Note event))
i sx sy = internal { env: { weight: 1.0, tag: Nothing }, cycles: NEA.fromNonEmpty (sx :| sy) }

i' :: forall event. Cycle (Maybe (Note event)) -> Cycle (Maybe (Note event))
i' sx = i sx []

i_ :: Cycle (Maybe (Note Unit)) -> Array (Cycle (Maybe (Note Unit))) -> Cycle (Maybe (Note Unit))
i_ = i

x :: forall event. Cycle (Maybe (Note event)) -> Array (Cycle (Maybe (Note event))) -> Cycle (Maybe (Note event))
x xx xy = simultaneous { env: { weight: 1.0, tag: Nothing }, cycles: NEA.fromNonEmpty (xx :| xy) }

x' :: forall event. Cycle (Maybe (Note event)) -> Cycle (Maybe (Note event))
x' sx = x sx []

x_ :: Cycle (Maybe (Note Unit)) -> Array (Cycle (Maybe (Note Unit))) -> Cycle (Maybe (Note Unit))
x_ = x

u :: Cycle (Maybe (Note Unit)) -> Cycle (Maybe (Note Unit))
u = identity

drone :: forall event. String -> Maybe' (DroneNote event)
drone = _just <<< sample2drone <<< Sample

nel2nea :: NonEmptyList ~> NonEmptyArray
nel2nea (NonEmptyList (aa :| bb)) = NEA.fromNonEmpty (aa :| A.fromFoldable bb)

sampleName :: Parser String
sampleName = map (CU.fromCharArray <<< NEA.toArray <<< nel2nea) (many1 $ oneOf [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', '~' ])

---
whiteSpace1 :: Parser String
whiteSpace1 = do
  cs <- many1 (satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t')
  pure (foldMap CU.singleton cs)

afterMatterP :: Parser AfterMatter
afterMatterP = do
  asInternal <-
    optionMaybe $
      ( ( fromString
            <<< CU.fromCharArray
            <<< A.fromFoldable <$> (char '*' *> many anyDigit)
        ) >>= maybe (fail "Could not parse int")
          ( \v -> case (NEA.fromArray $ replicate v unit) of
              Just vv -> pure vv
              Nothing -> fail "Number must be positive"
          )
      )
  pure { asInternal }

tagP :: Parser Tag
tagP = do
  tag <- optionMaybe (map (CU.fromCharArray <<< A.fromFoldable) (char ';' *> many1 alphaNum))
  pure { tag }

weightP :: Parser Number
weightP = do
  _ <- skipSpaces
  add 1.0 <<< toNumber <<< L.length <$> sepEndBy (char '_') whiteSpace1

sampleP :: forall event. Parser (Maybe (Note event))
sampleP = do
  possiblySample <- sampleName
  -- if it is in our original stock, there may be a level
  -- of indirection. for example, chin and chin:0 are the same sample
  -- otherwise, if it is not in the original stock, we use the name as-is
  case O.lookup possiblySample S.nameToSampleMNO of
    Nothing -> pure $ Just $ Note
      { sampleFoT: _right $ Sample possiblySample
      , bufferOffsetFoT: const 0.0
      , rateFoT: const 1.0
      , forward: true
      , volumeFoT: const 1.0
      }
    Just foundSample -> pure foundSample

branchingcyclePInternal :: forall event. Int -> Parser (Cycle (Maybe (Note event)))
branchingcyclePInternal lvl = branching <$> do
  cycles <- between (skipSpaces *> char '<' *> skipSpaces) (skipSpaces *> char '>' *> skipSpaces) do
    pure unit -- breaks recursion
    --  = spy (ident i <> "branchingcyclePInternal in between " <> show i) true
    map nel2nea $ sepBy1 (cyclePInternal (lvl + 1)) skipSpaces
  --  = spy (ident i <> "branchingcyclePInternal finished" <> show i) cycles
  --------- HACK
  --- if there is a single sequential we expand it
  { tag } <- tagP
  weight <- weightP
  pure { cycles, env: { weight, tag } }

ident ∷ Int → String
ident lvl = (fold (map (const " ") (0 .. (lvl - 1))))

simultaneouscyclePInternal :: forall event. Int -> Parser (Cycle (Maybe (Note event)))
simultaneouscyclePInternal lvl = simultaneous <$> do
  cycles <- between (skipSpaces *> char '[' *> skipSpaces) (skipSpaces *> char ']' *> skipSpaces) do
    pure unit -- breaks recursion
    --  = spy (ident i <> "simultaneouscyclePInternal in between " <> show i) true
    map nel2nea $ sepBy1 (sequentialcyclePInternal (lvl + 1)) (skipSpaces *> char ',' *> skipSpaces)
  --  = spy (ident i <> "simultaneouscyclePInternal finishing " <> show i) true
  { tag } <- tagP
  weight <- weightP
  --  = spy (ident i <> "simultaneouscyclePInternal got weights " <> show i) true
  pure { cycles, env: { weight, tag } }

sequentialcyclePInternal :: forall event. Int -> Parser (Cycle (Maybe (Note event)))
sequentialcyclePInternal lvl = map internal do
  pure unit -- breaks recursion
  --  = spy (ident i <> "rewinding to sequentialcyclePInternal starting many " <> show i) true
  cycles <- skipSpaces *> (map nel2nea $ sepEndBy1 (cyclePInternal (lvl + 1)) skipSpaces)
  --  = spy (ident i <> "rewinding to sequentialcyclePInternal ending many with length " <> (show $ NEA.length cycles) <> " " <> show i) true
  pure { cycles, env: { weight: 1.0, tag: Nothing } }

singleSampleP :: forall event. Int -> Parser (Cycle (Maybe (Note event)))
singleSampleP _ = do
  skipSpaces
  sample <- sampleP
  afterMatter <- afterMatterP
  skipSpaces
  { tag } <- tagP
  weight <- weightP
  pure $ case afterMatter.asInternal of
    Nothing -> singleton { env: { weight, tag }, val: sample }
    Just ntimes -> internal { env: { weight, tag }, cycles: map (const $ singleton { env: { weight: 1.0, tag: Nothing }, val: sample }) ntimes }

cyclePInternal :: forall event. Int -> Parser (Cycle (Maybe (Note event)))
cyclePInternal lvl = try (branchingcyclePInternal lvl)
  <|> try (simultaneouscyclePInternal lvl)
  <|> try (singleSampleP lvl)
  <|> fail "Could not parse cycle"

cycleP_ :: Parser (Cycle (Maybe (Note Unit)))
cycleP_ = cycleP

cycleP :: forall event. Parser (Cycle (Maybe (Note event)))
cycleP = go <$> cyclePInternal 0
  where
  c cycles env ctr =
    let
      h = NEA.head cycles
      t = NEA.tail cycles
    in
      if null t then go h else ctr { env, cycles: map go cycles }
  go = unwrap >>> match
    { branching: \{ cycles, env } -> c cycles env branching
    , simultaneous: \{ cycles, env } -> c cycles env simultaneous
    , internal: \{ cycles, env } -> c cycles env internal
    , singleton: \note -> singleton note
    }

flatMap :: forall event. NonEmptyArray (NonEmptyArray (NonEmptyArray (NoteInTime (Maybe (Note event))))) -> NonEmptyArray (NonEmptyArray (NoteInTime (Maybe (Note event))))
flatMap nea =
  let
    aa = NEA.head nea
    tt = NEA.tail nea
    ucd = uncons tt
  in
    ucd # maybe aa \ucd' ->
      let
        bb = ucd'.head
        cc = ucd'.tail
      in
        join $ aa # map \a' -> flatMap (NEA.fromNonEmpty (bb :| cc)) # map \bbb -> a' <> bbb

getWeight :: forall a. Cycle a -> Number
getWeight = unwrap >>> match
  { branching: _.env >>> _.weight
  , simultaneous: _.env >>> _.weight
  , internal: _.env >>> _.weight
  , singleton: _.env >>> _.weight
  }

getTag :: forall a. Cycle a -> Maybe String
getTag = unwrap >>> match
  { branching: _.env >>> _.tag
  , simultaneous: _.env >>> _.tag
  , internal: _.env >>> _.tag
  , singleton: _.env >>> _.tag
  }

onTagsCWithIndex :: forall a. (Set.Set String -> Boolean) -> (Int -> Cycle a -> Cycle a) -> Cycle a -> Cycle a
onTagsCWithIndex pf fff vvv = (go Set.empty 0 vvv).val
  where
  go' st ff ii env cycles = let nst = ((Set.fromFoldable env.tag) <> st) in let folded = foldl (\axc cyc -> axc <> (pure (go nst (NEA.last axc).i cyc))) (pure (go nst ii (NEA.head cycles))) (NEA.tail cycles) in { i: _.i $ NEA.last folded, val: ff { env, cycles: map _.val folded } }

  go :: Set.Set String -> Int -> Cycle a -> { i :: Int, val :: Cycle a }
  go st ii = unwrap >>> match
    { branching: \{ env, cycles } -> go' st branching ii env cycles
    , simultaneous: \{ env, cycles } -> go' st simultaneous ii env cycles
    , internal: \{ env, cycles } -> go' st internal ii env cycles
    , singleton: \{ env, val } -> let res = (Set.fromFoldable env.tag) <> st in if pf res then { i: ii + 1, val: fff ii $ singleton { env, val } } else { i: ii, val: singleton { env, val } }
    }

onTagsC :: forall a. (Set.Set String -> Boolean) -> (Cycle a -> Cycle a) -> Cycle a -> Cycle a
onTagsC pf f = onTagsCWithIndex pf \_ -> f

onTagsCWithIndex' :: forall a. (Set.Set String -> Boolean) -> (Int -> Cycle a -> Cycle a) -> Cycle a -> Cycle a
onTagsCWithIndex' pf fff vvv = (go Set.empty 0 vvv).val
  where
  go' st ff ii env cycles = let nst = st in let folded = foldl (\axc cyc -> axc <> (pure (go nst (NEA.last axc).i cyc))) (pure (go nst ii (NEA.head cycles))) (NEA.tail cycles) in { i: _.i $ NEA.last folded, val: ff { env, cycles: map _.val folded } }

  go :: Set.Set String -> Int -> Cycle a -> { i :: Int, val :: Cycle a }
  go st ii = unwrap >>> match
    { branching: \{ env, cycles } -> go' st branching ii env cycles
    , simultaneous: \{ env, cycles } -> go' st simultaneous ii env cycles
    , internal: \{ env, cycles } -> go' st internal ii env cycles
    , singleton: \{ env, val } -> let res = (Set.fromFoldable env.tag) <> st in if pf res then { i: ii + 1, val: fff ii $ singleton { env, val } } else { i: ii, val: singleton { env, val } }
    }

onTagsC' :: forall a. (Set.Set String -> Boolean) -> (Cycle a -> Cycle a) -> Cycle a -> Cycle a
onTagsC' pf f = onTagsCWithIndex' pf \_ -> f

onTagsWithIndex :: forall a. (Set.Set String -> Boolean) -> (Int -> a -> a) -> Cycle a -> Cycle a
onTagsWithIndex pf zz = onTagsCWithIndex pf (map <<< zz)

onTagsWithIndex' :: forall a. (Set.Set String -> Boolean) -> (Int -> a -> a) -> Cycle a -> Cycle a
onTagsWithIndex' pf zz = onTagsCWithIndex' pf (map <<< zz)

onTags :: forall a. (Set.Set String -> Boolean) -> (a -> a) -> Cycle a -> Cycle a
onTags pf = onTagsC pf <<< map

onTags' :: forall a. (Set.Set String -> Boolean) -> (a -> a) -> Cycle a -> Cycle a
onTags' pf = onTagsC' pf <<< map

onTagWithIndex :: forall a. String -> (Int -> a -> a) -> Cycle a -> Cycle a
onTagWithIndex = onTagsWithIndex <<< Set.member

onTagWithIndex' :: forall a. String -> (Int -> a -> a) -> Cycle a -> Cycle a
onTagWithIndex' = onTagsWithIndex' <<< Set.member

onTag :: forall a. String -> (a -> a) -> Cycle a -> Cycle a
onTag = onTags <<< Set.member

onTag' :: forall a. String -> (a -> a) -> Cycle a -> Cycle a
onTag' = onTags' <<< Set.member

cycleToSequence :: forall event. CycleDuration -> Cycle (Maybe (Note event)) -> NonEmptyArray (NonEmptyArray (NoteInTime (Maybe (Note event))))
cycleToSequence (CycleDuration cycleDuration) = go { currentSubdivision: cycleDuration, currentOffset: 0.0 }
  where
  go state = unwrap >>> match
    { branching: \{ cycles } -> join $ map (go state) cycles
    , simultaneous: \{ cycles } -> map (NEA.sortBy (compare `on` (unwrap >>> _.startsAt)))
        $ flatMap
        $ map (go state) cycles
    , internal: \{ cycles } -> seq state cycles
    , singleton: \{ env: { tag }, val } -> pure $ pure $ NoteInTime
        { duration: state.currentSubdivision
        , startsAt: state.currentOffset
        , note: val
        , cycleDuration
        , tag
        }
    }
  seq state cycles =
    let
      allWeights = foldl (+) 0.0 (map getWeight cycles)
      withStateInfo = evalState
        ( ( traverse \vv -> do
              ii <- get
              let wt = getWeight vv
              put $ ii + wt
              pure
                { currentSubdivision: state.currentSubdivision * wt / allWeights
                , currentOffset: state.currentOffset + state.currentSubdivision * ii / allWeights
                , vv
                }
          ) cycles
        )
        0.0
    in
      flatMap $ map
        (\{ currentSubdivision, currentOffset, vv } -> go { currentSubdivision, currentOffset } vv)
        withStateInfo

unrest :: forall event. NonEmptyArray (NonEmptyArray (NoteInTime (Maybe (Note event)))) -> Array (Array (NoteInTime (Note event)))
unrest = filter (not <<< A.null) <<< NEA.toArray <<< map go
  where
  go =
    filterMap
      ( \(NoteInTime { startsAt, duration, cycleDuration, tag, note }) ->
          NoteInTime <<< { startsAt, duration, cycleDuration, tag, note: _ } <$> note
      ) <<< NEA.toArray

asScore :: forall event. Boolean -> NonEmptyArray (NoteInFlattenedTime (Note event)) -> NextCycle event
asScore force flattened = NextCycle
  { force
  , samples: compact $ NEA.toArray $ map (unwrap >>> _.note >>> unwrap >>> _.sampleFoT >>> _hush) flattened
  , func: scoreInput
  }
  where
  scoreInput ccPce = go ccPce.currentCount ccPce.prevCycleEnded flattened
  go currentCount prevCycleEnded nea =
    let
      NoteInFlattenedTime aa :| bb = NEA.toNonEmpty nea
      st = prevCycleEnded + aa.bigStartsAt
    in
      { startsAfter: st - currentCount
      , rest:
          { sampleFoT: (unwrap aa.note).sampleFoT
          , forward: (unwrap aa.note).forward
          , cycleStartsAt: prevCycleEnded
          , rateFoT: (unwrap aa.note).rateFoT
          , bufferOffsetFoT: (unwrap aa.note).bufferOffsetFoT
          , volumeFoT: (unwrap aa.note).volumeFoT
          , bigCycleDuration: aa.bigCycleDuration
          , littleCycleDuration: aa.littleCycleDuration
          , currentCycle: aa.currentCycle
          , bigStartsAt: aa.bigStartsAt
          , littleStartsAt: aa.littleStartsAt
          , duration: aa.duration
          }
      } :<
        \{ time, headroomInSeconds, input: { next: (NextCycle nc) } } ->
          case uncons bb of
            Nothing -> nc.func
              { currentCount: st
              , prevCycleEnded: prevCycleEnded + aa.bigCycleDuration
              , time
              , headroomInSeconds
              }
            Just { head: cc, tail: dd } ->
              if nc.force && aa.positionInCycle == aa.elementsInCycle - 1 then nc.func
                { currentCount: st
                , prevCycleEnded: prevCycleEnded + aa.littleCycleDuration * toNumber (aa.currentCycle + 1)
                , time
                , headroomInSeconds
                }
              else go st prevCycleEnded (NEA.fromNonEmpty (cc :| dd))

flattenScore :: forall event. NonEmptyArray (NonEmptyArray (NoteInTime (Note event))) -> NonEmptyArray (NoteInFlattenedTime (Note event))
flattenScore l = flattened
  where
  ll = NEA.length l
  flattened = join $ mapWithIndex
    ( \ii -> mapWithIndex
        ( \jj (NoteInTime { note, duration, startsAt, cycleDuration, tag }) -> NoteInFlattenedTime
            { note
            , duration
            , bigStartsAt: startsAt + cycleDuration * toNumber ii
            , currentCycle: ii
            , elementsInCycle: NEA.length (NEA.head l)
            , nCycles: NEA.length l
            , positionInCycle: jj
            , littleStartsAt: startsAt
            , littleCycleDuration: cycleDuration
            , bigCycleDuration: cycleDuration * toNumber ll
            , tag
            }
        )
    )
    l

openVoice :: forall event. CycleDuration -> Voice event
openVoice = Voice
  <<<
    { globals: Globals { gain: const 0.0, fx: const calm }
    , next: _
    }
  <<< asScore false
  <<< pure
  <<< intentionalSilenceForInternalUseOnly

type OpenVoices event = { | EWF (CycleDuration -> Voice event) }

openVoices :: forall event. OpenVoices event
openVoices = fromHomogeneous
  $ map
      ( const $ s
          ( intentionalSilenceForInternalUseOnly_
              :: Cycle (Maybe (Note event))
          )
      )
  $ homogeneous (mempty :: { | EWF Unit })

type OpenDrones event = { | AH (Maybe' (DroneNote event)) }

openDrones :: forall event. OpenDrones event
openDrones = fromHomogeneous
  $ map (const _nothing)
  $ homogeneous (mempty :: { | AH Unit })

openFuture :: forall event. CycleDuration -> TheFuture event
openFuture cycleDuration = TheFuture
  $ Record.union
      ( fromHomogeneous $ map (const (openVoice cycleDuration))
          $ homogeneous (mempty :: { | EWF Unit })
      )
  $ Record.union
      ( fromHomogeneous $ map (const _nothing)
          $ homogeneous (mempty :: { | AH Unit })
      )
      { title: "wagsi @ tidal"
      , cycleDuration
      , sounds: (Object.empty :: Object BufferUrl)
      , preload: []
      }

intentionalSilenceForInternalUseOnly
  :: forall event. CycleDuration -> NoteInFlattenedTime (Note event)
intentionalSilenceForInternalUseOnly (CycleDuration cl) = NoteInFlattenedTime
  { note: Note
      { sampleFoT: _right $ S.intentionalSilenceForInternalUseOnly__Sample
      , rateFoT: const 1.0
      , forward: true
      , volumeFoT: const 1.0
      , bufferOffsetFoT: const 0.0
      }
  , bigStartsAt: 0.0
  , littleStartsAt: 0.0
  , duration: cl
  , elementsInCycle: 1
  , nCycles: 1
  , positionInCycle: 0
  , currentCycle: 0
  , bigCycleDuration: cl
  , littleCycleDuration: cl
  , tag: Nothing
  }

parseWithBrackets
  :: forall event
   . String
  -> Either
       { error :: String
       , pos :: Int
       }
       (Cycle (Maybe (Note event)))
parseWithBrackets = runParser cycleP
  <<< ("[ " <> _)
  <<< (_ <> " ]")

parse :: forall event. String -> Cycle (Maybe (Note event))
parse = fromMaybe intentionalSilenceForInternalUseOnly_
  <<< hush
  <<< parseWithBrackets

parse_ :: String -> Cycle (Maybe (Note Unit))
parse_ = parse

parseInternal :: forall event. String -> CycleDuration -> NextCycle event
parseInternal str dur = asScore false
  $ maybe (pure $ intentionalSilenceForInternalUseOnly dur) flattenScore
  $ join
  $ map
      ( NEA.fromArray
          <<< compact
          <<< map NEA.fromArray
          <<< (unrest <<< cycleToSequence dur)
      )
  $ hush
  $ parseWithBrackets str

rend :: forall event. Cycle (Maybe (Note event)) -> CycleDuration -> (NextCycle event)
rend cyn dur = asScore false
  $ maybe (pure (intentionalSilenceForInternalUseOnly dur)) flattenScore
  $ NEA.fromArray
  $ compact
  $ map NEA.fromArray
  $ unrest
  $ cycleToSequence dur
  $ cyn

rend_ :: Cycle (Maybe (Note Unit)) -> CycleDuration -> (NextCycle Unit)
rend_ = rend

rendNit :: forall event. NonEmptyArray (NonEmptyArray (NoteInTime (Maybe (Note event)))) -> NextCycle event
rendNit = asScore false <<< s2f

c2s :: forall event. Cycle (Maybe (Note event)) -> CycleDuration -> NonEmptyArray (NonEmptyArray (NoteInTime (Maybe (Note event))))
c2s = flip cycleToSequence

s2f
  :: forall event
   . NonEmptyArray (NonEmptyArray (NoteInTime (Maybe (Note event))))
  -> NonEmptyArray (NoteInFlattenedTime (Note event))
s2f = fromMaybe
  <$>
    ( pure <<< intentionalSilenceForInternalUseOnly
        <<< CycleDuration
        <<< _.cycleDuration
        <<< unwrap
        <<< NEA.head
        <<< NEA.head
    )
  <*>
    ( map flattenScore
        <<< NEA.fromArray
        <<< compact
        <<< map NEA.fromArray
        <<< unrest
    )

----------------
-- dj quickcheck --

qcSamples :: NonEmptyArray Sample
qcSamples = map fst $ fromMaybe (sampleToDur') $ NEA.fromArray $ NEA.filter ((>) 0.8 <<< snd) sampleToDur

genSingleSample :: Gen Sample
genSingleSample = elements qcSamples

genSingleton :: forall event. Gen (Cycle (Maybe (Note event)))
genSingleton =
  singleton <<< { env: { weight: 1.0, tag: Nothing }, val: _ } <<< Just <<< Note
    <<<
      { rateFoT: const 1.0
      , volumeFoT: const 1.0
      , forward: true
      , bufferOffsetFoT: const 0.0
      , sampleFoT: _
      }
    <<< _right <$> genSingleSample

genRest :: forall event. Gen (Cycle (Maybe (Note event)))
genRest = pure $ singleton { env: { weight: 1.0, tag: Nothing }, val: Nothing }

genSingletonOrRest :: forall event. Gen (Cycle (Maybe (Note event)))
genSingletonOrRest = frequency (wrap ((7.0 /\ genSingleton) :| (3.0 /\ genRest) : Nil))

genCycle :: forall event. Gen (Cycle (Maybe (Note event)))
genCycle = go 0
  where
  genSameInternal nn = do
    sn <- genSingleton
    ii <- case nn of
      0 -> elements $ NEA.fromNonEmpty (2 :| [ 3, 4, 5, 6 ])
      1 -> elements $ NEA.fromNonEmpty (2 :| [ 3, 4, 6 ])
      _ -> elements $ NEA.fromNonEmpty (2 :| [ 3, 4 ])
    pure $ internal { env: { weight: 1.0, tag: Nothing }, cycles: NEA.fromNonEmpty (sn :| A.replicate (ii - 1) sn) }
  genDiffInternal nn = do
    let gg = go (nn + 1)
    ii <- case nn of
      0 -> elements $ NEA.fromNonEmpty (2 :| [ 3, 4, 8 ])
      1 -> elements $ NEA.fromNonEmpty (2 :| [ 4 ])
      _ -> elements $ NEA.fromNonEmpty (2 :| [])
    cycles <- sequence $ NEA.fromNonEmpty (gg :| A.replicate (ii - 1) gg)
    pure $ internal { env: { weight: 1.0, tag: Nothing }, cycles }
  genBranching nn = resize 3 (branching <<< { env: { weight: 1.0, tag: Nothing }, cycles: _ } <$> arrayOf1 (go (nn + 1)))
  genSimultaneous nn = resize 3 (simultaneous <<< { env: { weight: 1.0, tag: Nothing }, cycles: _ } <$> arrayOf1 (go (nn + 1)))
  go 0 =
    let
      zz = 0
    in
      frequency
        ( wrap
            ( (1.0 /\ (genBranching zz))
                :| (1.0 /\ (genSimultaneous zz))
                  : (1.0 /\ (genDiffInternal zz))
                  : Nil
            )
        )
  go 1 =
    let
      zz = 1
    in
      frequency
        ( wrap
            ( (1.0 /\ (genBranching zz))
                :| (1.0 /\ (genSimultaneous zz))
                  : (2.0 /\ (genDiffInternal zz))
                  : (4.0 /\ (genSameInternal zz))
                  : (3.0 /\ genSingletonOrRest)
                  : Nil
            )
        )
  go 2 =
    let
      zz = 2
    in
      frequency
        ( wrap
            ( (1.0 /\ (genBranching zz))
                :| (1.0 /\ (genSimultaneous zz))
                  : (1.0 /\ (genDiffInternal zz))
                  : (4.0 /\ (genSameInternal zz))
                  : (8.0 /\ genSingletonOrRest)
                  : Nil
            )
        )
  go 3 =
    let
      zz = 3
    in
      frequency
        ( wrap
            ( (1.0 /\ (genBranching zz))
                :| (1.0 /\ genSimultaneous zz)
                  : (1.0 /\ (genDiffInternal zz))
                  : (4.0 /\ (genSameInternal zz))
                  : (10.0 /\ genSingletonOrRest)
                  : Nil
            )
        )
  go _ = genSingletonOrRest

genVoice :: forall event. Number -> CycleDuration -> Gen { cycle :: Cycle (Maybe (Note event)), voice :: Voice event }
genVoice vol cl = do
  let globals = Globals { gain: const vol, fx: const calm }
  cycle <- genCycle
  let next = asScore false $ s2f $ c2s cycle cl
  pure $ { cycle, voice: Voice { globals, next } }

djQuickCheck :: forall event. Gen { cycle :: Cycle (Maybe (Note event)), future :: TheFuture event }
djQuickCheck = do
  cl' <- arbitrary
  let cycleDuration = CycleDuration (1.0 + 3.0 * cl')
  { cycle, voice: earth } <- genVoice 1.0 cycleDuration
  wind <- pure (openVoice cycleDuration)
  fire <- pure (openVoice cycleDuration)
  pure $
    { cycle
    , future: TheFuture
        { earth
        , wind
        , fire
        , air: _nothing
        , heart: _nothing
        , title: "d j q u i c k c h e c k"
        , sounds: Object.empty
        , preload: []
        , cycleDuration
        }
    }

class S s event where
  s :: s -> CycleDuration -> Voice event

instance sString :: S String event where
  s = map plainly <<< parseInternal

instance sCycle :: S (Cycle (Maybe (Note event))) event where
  s = map plainly <<< rend

instance sNit :: S (NonEmptyArray (NonEmptyArray (NoteInTime (Maybe (Note event))))) event where
  s = map plainly <<< pure <<< rendNit

instance sNitFT :: S (CycleDuration -> (NonEmptyArray (NonEmptyArray (NoteInTime (Maybe (Note event)))))) event where
  s = map plainly <<< map rendNit

instance sNift :: S (NonEmptyArray (NoteInFlattenedTime (Note event))) event where
  s = map plainly <<< pure <<< asScore false

instance sNiftFT :: S (CycleDuration -> (NonEmptyArray (NoteInFlattenedTime (Note event)))) event where
  s = map plainly <<< map (asScore false)

instance sNextCycle :: S (NextCycle event) event where
  s = map plainly <<< pure

instance sNextCycleFT :: S (CycleDuration -> NextCycle event) event where
  s = map plainly

instance sVoice :: S (Voice event) event where
  s = const

instance sVoiceFT :: S (CycleDuration -> (Voice event)) event where
  s = identity

betwixt :: forall n. Ord n => n -> n -> n -> n
betwixt mn' mx' n = if n < mn then mn else if n > mx then mx else n
  where
  mn = min mn' mx'
  mx = max mn' mx'

derivative :: forall a. ClockTime a => (TimeIsAndWas a -> Number) -> Number -> TimeIsAndWas a -> Number
derivative f y v = fromMaybe' (\_ -> f v) do
  let v' = unwrap v
  let ti = v'.timeIs
  mti' <- v'.timeWas
  mn' <- v'.valWas
  pure $ (y * (clockTime ti - clockTime mti')) + mn'