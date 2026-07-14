module WFC.Wave where

import Prelude

import Data.Array as Array
import Data.Foldable (all, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (log)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import WFC.Catalog (PatternCatalog, WLogW(..), Weight(..), patternIds, weightOf, wLogWOf)
import WFC.CompatibilityMap (CompatibilityKey(..), CompatibilityMap)
import WFC.CompatibilityMap as CompatibilityMap
import WFC.Direction (Direction, DirectionIndex(..), allDirections, dirIndex)
import WFC.Grid (GridSize, OutputPeriodic(..), Pos(..), allPositions)
import WFC.Pattern (PatternId(..))
import WFC.Rules (AdjacencyRules, initialCompatCount)

-- Per-cell possibilities. Nothing = contradiction at this cell.
type Cell = Maybe (Set PatternId)

-- Per-position compat table: compatibility[pid][dir] = number of tiles in
-- the direction-dir neighbour of this position that still support pid
-- being here. Reaches 0 → pid must be banned. `PatternId × Direction`
-- folded into one combined key (via `CompatibilityMap`) instead of two
-- nested `Map`s — one tree and one fast `Int` comparator per update
-- instead of a `PatternId`-keyed tree containing a further
-- `Direction`-keyed tree.
--
-- (An earlier version of this also folded `Pos` into the same combined
-- key, giving one flat `P×T×4`-entry map. Measured slower, not faster: it
-- turned every `decrementCompatibility` into two full traversals of a much
-- *deeper* tree — `log(P×T×4)`, bigger than the outer `Pos` tree's
-- `log(P)` alone — instead of one `Map.alter` each on an outer `Pos`-keyed
-- tree with only `P` entries and an inner table shared by reference across
-- every position at `initWave`. Keeping `Pos` as the outer key (below, in
-- `Wave`'s own `compatibility` field) preserves that sharing.)
type CompatibilityCell = CompatibilityMap

-- Fold (pid, dir) into `CompatibilityCell`'s single combined key. Doesn't
-- depend on grid size/position, so a computed key stays valid across a
-- `resizeWave`; kept positions' `CompatibilityCell`s carry over unchanged,
-- same as `cells`.
compatibilityKey :: PatternId -> Direction -> CompatibilityKey
compatibilityKey (PatternId pid) dir =
  let DirectionIndex di = dirIndex dir
  in CompatibilityKey (pid * 4 + di)

-- Running totals behind a cell's Shannon entropy (`entropyFromStats` below),
-- kept incrementally in sync with `cells` by every single ban (see
-- `WFC.Propagate.processBan`) instead of being re-summed from a cell's whole
-- possibility set on every entropy query — the same running-sums trick the
-- original C# WFC's `Model.Ban()` uses (`sumsOfWeights`/
-- `sumsOfWeightLogWeights`), which is what makes `minEntropyPos` cheap to
-- call once per solving step instead of O(cells × possibilities per cell).
type EntropyStats = { sumW :: Number, sumWLogW :: Number }

type EntropyCache = Map Pos EntropyStats

-- Shannon entropy of a cell — distinct from `WFC.Catalog`'s `Weight`/`WLogW`
-- (the raw ingredients it's computed from), even though all three are
-- "just a Number". Lives here, not in `WFC.Entropy`, so `WFC.Entropy` (which
-- imports `WFC.Wave`) can import this type back without a cycle.
newtype Entropy = Entropy Number

derive newtype instance eqEntropy :: Eq Entropy
derive newtype instance ordEntropy :: Ord Entropy
derive newtype instance showEntropy :: Show Entropy

-- Same formula `WFC.Catalog.finalize`'s `startEntropy` and the old
-- from-scratch `WFC.Entropy.cellEntropy` both use, factored out so the
-- cached and non-cached paths can't drift apart.
entropyFromStats :: EntropyStats -> Entropy
entropyFromStats { sumW, sumWLogW } =
  Entropy (if sumW > 0.0 then log sumW - sumWLogW / sumW else 0.0)

-- Stats for a cell still in full superposition — just the catalog's own
-- precomputed totals, reused (structurally shared, not recomputed) for
-- every fresh cell rather than summing every pattern's weight per position.
statsForAll :: forall a. PatternCatalog a -> EntropyStats
statsForAll catalog = { sumW: catalog.totalW, sumWLogW: catalog.totalWLogW }

-- Stats for an arbitrary possibility set — the fallback used when a cell's
-- possibilities are set directly (`setCell`) rather than shrunk one ban at
-- a time, and by `WFC.Entropy.cellEntropy` for an explicit/hypothetical set.
statsForSet :: forall a. PatternCatalog a -> Set PatternId -> EntropyStats
statsForSet catalog possible =
  let pids = Set.toUnfoldable possible :: Array PatternId
      sumW = foldl (\acc pid -> let Weight w = weightOf catalog pid in acc + w) 0.0 pids
      sumWLogW = foldl (\acc pid -> let WLogW w = wLogWOf catalog pid in acc + w) 0.0 pids
  in { sumW, sumWLogW }

type Wave a =
  { cells         :: Map Pos Cell
  , compatibility :: Map Pos CompatibilityCell
  , entropy       :: EntropyCache
  , catalog       :: PatternCatalog a
  , rules         :: AdjacencyRules
  , size          :: GridSize
  , periodic      :: Boolean
  }

-- The shared per-position compat table for a freshly-superposed cell — same
-- `CompatibilityCell` value (structural sharing, not recomputed) reused for
-- every position, the same way a fresh `Cell`'s possibility set is one
-- shared `Set` reused everywhere rather than built once per position.
initialCompatibilityCell :: AdjacencyRules -> Array PatternId -> CompatibilityCell
initialCompatibilityCell rules ids =
  CompatibilityMap.fromFoldable $ do
    pid <- ids
    dir <- allDirections
    pure (Tuple (compatibilityKey pid dir) (initialCompatCount rules pid dir))

-- Create a wave where every cell is in full superposition.
initWave
  :: forall a
  .  PatternCatalog a
  -> AdjacencyRules
  -> GridSize
  -> OutputPeriodic
  -> Wave a
initWave catalog rules size (OutputPeriodic periodic) =
  let ids      = patternIds catalog
      allPids  = Set.fromFoldable ids
      initCell = Just allPids
      cellComp = initialCompatibilityCell rules ids
      initStats = statsForAll catalog
      positions = allPositions size
      cells    = Map.fromFoldable $ map (\pos -> Tuple pos initCell) positions
      compatibility = Map.fromFoldable $ map (\pos -> Tuple pos cellComp) positions
      entropy  = Map.fromFoldable $ map (\pos -> Tuple pos initStats) positions
  in { cells, compatibility, entropy, catalog, rules, size, periodic }

-- Resize a wave to `newSize`, keeping every cell/compat entry whose position
-- still falls within the new bounds untouched (same possibilities, same
-- propagation progress) and filling any newly-exposed positions with a
-- fresh full-superposition cell — the same "crop or extend, never restart"
-- resize a plain grid would get, just carrying the compat map along so
-- propagation on the new cells keeps working correctly. Positions dropped
-- by shrinking (e.g. a decision frame's own cell, in backtracking search)
-- simply stop existing in `cells`/`compat`; `getCellPossibilities` already
-- treats a missing position as a contradiction, so callers that still
-- reference a dropped position fail gracefully through the existing
-- contradiction-recovery path instead of needing special-case handling here.
resizeWave :: forall a. GridSize -> Wave a -> Wave a
resizeWave newSize wave =
  let
    ids       = patternIds wave.catalog
    initCell  = Just (Set.fromFoldable ids)
    cellComp  = initialCompatibilityCell wave.rules ids
    initStats = statsForAll wave.catalog
    inBounds (Pos { x, y }) = x >= 0 && x < newSize.width && y >= 0 && y < newSize.height
    keptCells         = Map.filterKeys inBounds wave.cells
    keptCompatibility = Map.filterKeys inBounds wave.compatibility
    keptEntropy       = Map.filterKeys inBounds wave.entropy
    newPos     = Array.filter (\p -> not (Map.member p keptCells)) (allPositions newSize)
    fillCells         = Map.fromFoldable (map (\pos -> Tuple pos initCell) newPos)
    fillCompatibility = Map.fromFoldable (map (\pos -> Tuple pos cellComp) newPos)
    fillEntropy       = Map.fromFoldable (map (\pos -> Tuple pos initStats) newPos)
  in
    wave
      { cells         = Map.union keptCells fillCells
      , compatibility = Map.union keptCompatibility fillCompatibility
      , entropy       = Map.union keptEntropy fillEntropy
      , size          = newSize
      }

-- Read a cell's possibility set.
getCellPossibilities :: forall a. Wave a -> Pos -> Maybe (Set PatternId)
getCellPossibilities wave pos = fromMaybe Nothing (Map.lookup pos wave.cells)

newtype FullyCollapsed = FullyCollapsed Boolean

derive newtype instance eqFullyCollapsed :: Eq FullyCollapsed
derive newtype instance showFullyCollapsed :: Show FullyCollapsed

-- True when every cell is collapsed to exactly one pattern.
isFullyCollapsed :: forall a. Wave a -> FullyCollapsed
isFullyCollapsed wave = FullyCollapsed (all isSingleton (Map.values wave.cells))
  where
    isSingleton Nothing   = false
    isSingleton (Just s)  = Set.size s == 1

-- Set a cell (used internally by collapse and propagate). Recomputes that
-- position's entropy cache entry from scratch to stay consistent — this
-- (unlike `WFC.Propagate.processBan`'s one-pattern-at-a-time decrement) has
-- no prior stats to adjust incrementally, since the new `Cell` isn't
-- necessarily a subset of whatever was there before.
setCell :: forall a. Pos -> Cell -> Wave a -> Wave a
setCell pos cell wave =
  wave
    { cells   = Map.insert pos cell wave.cells
    , entropy = Map.insert pos stats wave.entropy
    }
  where
    stats = case cell of
      Nothing -> { sumW: 0.0, sumWLogW: 0.0 }
      Just s  -> statsForSet wave.catalog s
