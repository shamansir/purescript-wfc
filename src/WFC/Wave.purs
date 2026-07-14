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
import WFC.Catalog (PatternCatalog)
import WFC.Direction (Direction, allDirections)
import WFC.Grid (GridSize, Pos(..), allPositions)
import WFC.Pattern (PatternId)
import WFC.Rules (AdjacencyRules, initialCompatCount)

-- Per-cell possibilities. Nothing = contradiction at this cell.
type Cell = Maybe (Set PatternId)

-- compat[pos][pid][dir] = number of tiles in the direction-dir neighbour of pos
-- that still support pid being at pos.  Reaches 0 → pid must be banned.
type CompatMap = Map Pos (Map PatternId (Map Direction Int))

-- Running totals behind a cell's Shannon entropy (`entropyFromStats` below),
-- kept incrementally in sync with `cells` by every single ban (see
-- `WFC.Propagate.processBan`) instead of being re-summed from a cell's whole
-- possibility set on every entropy query — the same running-sums trick the
-- original C# WFC's `Model.Ban()` uses (`sumsOfWeights`/
-- `sumsOfWeightLogWeights`), which is what makes `minEntropyPos` cheap to
-- call once per solving step instead of O(cells × possibilities per cell).
type EntropyStats = { sumW :: Number, sumWLogW :: Number }

type EntropyCache = Map Pos EntropyStats

-- Same formula `WFC.Catalog.finalize`'s `startEntropy` and the old
-- from-scratch `WFC.Entropy.cellEntropy` both use, factored out so the
-- cached and non-cached paths can't drift apart.
entropyFromStats :: EntropyStats -> Number
entropyFromStats { sumW, sumWLogW } =
  if sumW > 0.0 then log sumW - sumWLogW / sumW else 0.0

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
      sumW = foldl (\acc pid -> acc + fromMaybe 0.0 (Map.lookup pid catalog.weights)) 0.0 pids
      sumWLogW = foldl (\acc pid -> acc + fromMaybe 0.0 (Map.lookup pid catalog.wLogW)) 0.0 pids
  in { sumW, sumWLogW }

type Wave a =
  { cells    :: Map Pos Cell
  , compat   :: CompatMap
  , entropy  :: EntropyCache
  , catalog  :: PatternCatalog a
  , rules    :: AdjacencyRules
  , size     :: GridSize
  , periodic :: Boolean
  }

initialCompatEntry
  :: AdjacencyRules
  -> PatternId
  -> Map Direction Int
initialCompatEntry rules pid =
  Map.fromFoldable $ map (\dir -> Tuple dir (initialCompatCount rules pid dir)) allDirections

initialCellCompat :: AdjacencyRules -> Array PatternId -> Map PatternId (Map Direction Int)
initialCellCompat rules ids =
  Map.fromFoldable $ map (\pid -> Tuple pid (initialCompatEntry rules pid)) ids

-- Create a wave where every cell is in full superposition.
initWave
  :: forall a
  .  PatternCatalog a
  -> AdjacencyRules
  -> GridSize
  -> Boolean
  -> Wave a
initWave catalog rules size periodic =
  let ids      = map (\(Tuple pid _) -> pid)
                   (Map.toUnfoldable catalog.patterns :: Array (Tuple PatternId _))
      allPids  = Set.fromFoldable ids
      initCell = Just allPids
      cellComp = initialCellCompat rules ids
      initStats = statsForAll catalog
      positions = allPositions size
      cells    = Map.fromFoldable $ map (\pos -> Tuple pos initCell) positions
      compat   = Map.fromFoldable $ map (\pos -> Tuple pos cellComp) positions
      entropy  = Map.fromFoldable $ map (\pos -> Tuple pos initStats) positions
  in { cells, compat, entropy, catalog, rules, size, periodic }

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
    ids       = map (\(Tuple pid _) -> pid) (Map.toUnfoldable wave.catalog.patterns :: Array (Tuple PatternId _))
    initCell  = Just (Set.fromFoldable ids)
    cellComp  = initialCellCompat wave.rules ids
    initStats = statsForAll wave.catalog
    inBounds (Pos { x, y }) = x >= 0 && x < newSize.width && y >= 0 && y < newSize.height
    keptCells   = Map.filterKeys inBounds wave.cells
    keptCompat  = Map.filterKeys inBounds wave.compat
    keptEntropy = Map.filterKeys inBounds wave.entropy
    newPos     = Array.filter (\p -> not (Map.member p keptCells)) (allPositions newSize)
    fillCells   = Map.fromFoldable (map (\pos -> Tuple pos initCell) newPos)
    fillCompat  = Map.fromFoldable (map (\pos -> Tuple pos cellComp) newPos)
    fillEntropy = Map.fromFoldable (map (\pos -> Tuple pos initStats) newPos)
  in
    wave
      { cells   = Map.union keptCells fillCells
      , compat  = Map.union keptCompat fillCompat
      , entropy = Map.union keptEntropy fillEntropy
      , size    = newSize
      }

-- Read a cell's possibility set.
getCellPossibilities :: forall a. Wave a -> Pos -> Maybe (Set PatternId)
getCellPossibilities wave pos = fromMaybe Nothing (Map.lookup pos wave.cells)

-- True when every cell is collapsed to exactly one pattern.
isFullyCollapsed :: forall a. Wave a -> Boolean
isFullyCollapsed wave = all isSingleton (Map.values wave.cells)
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
