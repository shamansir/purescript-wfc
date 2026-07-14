module WFC.Catalog where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Number (log)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import WFC.Pattern (Pattern(..), PatternId(..), VariantTag, taggedVariantsFor)

-- Whether a catalog pattern only exists because of the rotation/mirror
-- symmetry options, and if so, which transform(s) produced it. A pattern
-- that also occurs as a genuine unmodified window somewhere in the sample
-- is never in this map — see `originFromTag`/`mergeOrigin` below.
type PatternOrigin = { rotated :: Boolean, mirrored :: Boolean }

-- All pattern data derived from the input sample.
-- Constructed only via extractPatterns.
--
-- `patterns`/`weights`/`wLogW` are `Array`s, not `Map PatternId x` — since
-- `PatternId` is contiguous `0..T-1` by construction (see `accumulatePattern`,
-- which only ever assigns the next sequential id), a `PatternId`'s own
-- underlying `Int` *is* its index, so a plain `Array` gives O(1) access
-- (`patternOf`/`weightOf`/`wLogWOf` below) instead of a `Map`'s O(log T)
-- tree lookup — for free, since this table is built once (`finalize`) and
-- never mutated again, unlike `WFC.Wave`'s per-cell compatibility/entropy
-- state (see `docs/Optimisations.md`, where the same `Array`-vs-`Map`
-- tradeoff pointed the other way *because* those are mutated thousands of
-- times per solve and must stay cheap to snapshot for backtracking — this
-- table has neither concern). `origins` stays `Map`-backed: it's sparse
-- (most patterns have no entry) and only read for display purposes
-- (pattern-thumbnail badges), never on the solving hot path.
type PatternCatalog a =
  { patterns     :: Array (Pattern a)           -- index = id → pixel data
  , weights      :: Array Number                -- index = id → frequency count
  , wLogW        :: Array Number                -- index = id → weight * ln(weight)
  , size         :: Int                         -- N (pattern is N×N)
  , totalW       :: Number                      -- Σ weights
  , totalWLogW   :: Number                      -- Σ (w * ln w)
  , startEntropy :: Number                      -- initial entropy for all cells
  , origins      :: Map PatternId PatternOrigin -- only rotation/mirror-only patterns
  }

-- Every `PatternId` in the catalog, in order (id 0, 1, 2, ...).
patternIds :: forall a. PatternCatalog a -> Array PatternId
patternIds catalog =
  let t = Array.length catalog.patterns
  in if t <= 0 then [] else map PatternId (Array.range 0 (t - 1))

-- Same shape `Map.toUnfoldable catalog.patterns :: Array (Tuple PatternId _)`
-- used to have — a drop-in replacement at call sites that want every
-- pattern tagged with its own id (rendering a pattern list, etc.), not just
-- looking one up.
patternsWithIds :: forall a. PatternCatalog a -> Array (Tuple PatternId (Pattern a))
patternsWithIds catalog = Array.mapWithIndex (\i pat -> Tuple (PatternId i) pat) catalog.patterns

patternOf :: forall a. PatternCatalog a -> PatternId -> Maybe (Pattern a)
patternOf catalog (PatternId i) = Array.index catalog.patterns i

weightOf :: forall a. PatternCatalog a -> PatternId -> Number
weightOf catalog (PatternId i) = fromMaybe 0.0 (Array.index catalog.weights i)

wLogWOf :: forall a. PatternCatalog a -> PatternId -> Number
wLogWOf catalog (PatternId i) = fromMaybe 0.0 (Array.index catalog.wLogW i)

-- Per-pattern bookkeeping while accumulating: has this exact pixel content
-- ever been seen as a base (untransformed) window, and has it ever been
-- seen via a rotation and/or a mirror — tracked separately because the
-- same content can arise both ways (e.g. a plain window here, a rotated
-- variant of a different window there).
type OriginAcc = { sawOriginal :: Boolean, rotated :: Boolean, mirrored :: Boolean }

originFromTag :: VariantTag -> OriginAcc
originFromTag tag = { sawOriginal: not tag.rotated && not tag.mirrored, rotated: tag.rotated, mirrored: tag.mirrored }

mergeOrigin :: OriginAcc -> OriginAcc -> OriginAcc
mergeOrigin a b =
  { sawOriginal: a.sawOriginal || b.sawOriginal
  , rotated:     a.rotated || b.rotated
  , mirrored:    a.mirrored || b.mirrored
  }

type Accum a =
  { nextId   :: Int
  , byPixels :: Map (Pattern a) PatternId
  , patterns :: Map PatternId (Pattern a)
  , weights  :: Map PatternId Number
  , origins  :: Map PatternId OriginAcc
  }

emptyAccum :: forall a. Accum a
emptyAccum = { nextId: 0, byPixels: Map.empty, patterns: Map.empty, weights: Map.empty, origins: Map.empty }

accumulatePattern :: forall a. Ord a => Accum a -> VariantTag -> Pattern a -> Accum a
accumulatePattern acc tag pat =
  case Map.lookup pat acc.byPixels of
    Just pid ->
      acc
        { weights = Map.insertWith (+) pid 1.0 acc.weights
        , origins = Map.insertWith mergeOrigin pid (originFromTag tag) acc.origins
        }
    Nothing ->
      let pid = PatternId acc.nextId
      in acc
        { nextId   = acc.nextId + 1
        , byPixels = Map.insert pat pid acc.byPixels
        , patterns = Map.insert pid pat acc.patterns
        , weights  = Map.insert pid 1.0 acc.weights
        , origins  = Map.insert pid (originFromTag tag) acc.origins
        }

finalize :: forall a. Accum a -> Int -> PatternCatalog a
finalize acc n =
  -- `acc.patterns`/`acc.weights` are `Map PatternId x` with contiguous
  -- `0..T-1` keys (by construction), so `Map.toUnfoldable` — guaranteed
  -- ascending-by-key — hands them back already in the right order to just
  -- drop the keys and keep the values as a dense `Array`.
  let patternPairs = Map.toUnfoldable acc.patterns :: Array (Tuple PatternId (Pattern a))
      weightPairs  = Map.toUnfoldable acc.weights :: Array (Tuple PatternId Number)
      patterns   = map (\(Tuple _ pat) -> pat) patternPairs
      weights    = map (\(Tuple _ w) -> w) weightPairs
      wLogW      = map (\w -> w * log w) weights
      totalW     = foldl (+) 0.0 weights
      totalWLogW = foldl (+) 0.0 wLogW
      startEntropy =
        if totalW > 0.0
          then log totalW - totalWLogW / totalW
          else 0.0
      origins = Map.mapMaybe
        (\o -> if not o.sawOriginal && (o.rotated || o.mirrored)
                 then Just { rotated: o.rotated, mirrored: o.mirrored }
                 else Nothing)
        acc.origins
  in { patterns, weights, wLogW, size: n, totalW, totalWLogW, startEntropy, origins }

-- Extract the pixel at position (x, y) from the input grid, with optional wrapping.
sampleAt :: forall a. Int -> Int -> Boolean -> Array (Array a) -> Int -> Int -> a
sampleAt w h periodic grid x y =
  let x' = if periodic then x `mod` w else x
      y' = if periodic then y `mod` h else y
  in unsafePartial $ fromJust $
       Array.index grid y' >>= \row -> Array.index row x'

-- Extract the N×N pattern rooted at (px, py).
patternAt :: forall a. Int -> Boolean -> Int -> Int -> Array (Array a) -> Int -> Int -> Pattern a
patternAt n periodic w h grid px py = Pattern $ do
  dy <- Array.range 0 (n - 1)
  dx <- Array.range 0 (n - 1)
  pure $ sampleAt w h periodic grid (px + dx) (py + dy)

-- Extract all patterns from the input grid.
--   n             — pattern size (N×N)
--   periodic      — whether to wrap at edges
--   useRotations  — also extract each window's 90°/180°/270° rotations
--   useMirror     — also extract each window's horizontal reflection
--                    (combined with useRotations, its rotations too)
extractPatterns
  :: forall a. Ord a
  => Int
  -> Boolean
  -> Boolean
  -> Boolean
  -> Array (Array a)
  -> PatternCatalog a
extractPatterns n periodic useRotations useMirror grid =
  let h = Array.length grid
      w = fromMaybe 0 (map Array.length (Array.head grid))
      -- Valid top-left corners for pattern extraction
      yMax = if periodic then h     else h - n + 1
      xMax = if periodic then w     else w - n + 1
      positions = do
        y <- Array.range 0 (yMax - 1)
        x <- Array.range 0 (xMax - 1)
        pure { x, y }
      allVariants = positions >>= \{ x, y } ->
        taggedVariantsFor n useRotations useMirror (patternAt n periodic w h grid x y)
      acc = foldl (\a (Tuple tag pat) -> accumulatePattern a tag pat) emptyAccum allVariants
  in finalize acc n

-- The "ground" heuristic (see WFC.Propagate.applyGround) pins the
-- highest-numbered pattern id — the original C# WFC's `T-1`, the last
-- pattern whose pixel content was newly discovered during `extractPatterns`'s
-- row-major scan (see the `positions`/`allVariants` order above).
lastPatternId :: forall a. PatternCatalog a -> Maybe PatternId
lastPatternId cat =
  let n = Array.length cat.patterns
  in if n == 0 then Nothing else Just (PatternId (n - 1))
