module WFC.Rules where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import WFC.Catalog (PatternCatalog, patternIds, patternOf)
import WFC.Direction (Direction, allDirections, dirIndex)
import WFC.Pattern (PatternId(..), Pattern(..), agrees)

-- propagator[dir][pid] = Array of pattern IDs that can be placed adjacent
-- in direction dir when pid is at the current cell.
--
-- `Array (Array (Array PatternId))` indexed `[dirIndex dir][pid]`, not
-- `Map Direction (Map PatternId (Array PatternId))` — built once
-- (`buildRules`/`WFC.Tiles.buildTiledRules`/`WFC.TileSet.buildTileSet`) and
-- read very hot afterwards (`lookupNeighbors`, 4×/ban in
-- `WFC.Propagate.processNeighbours`); `Direction` has only 4 values and
-- `PatternId` is contiguous `0..T-1`, so both index dimensions are dense
-- and a nested `Array` gives O(1) access at each level instead of a tree
-- traversal. See `docs/Optimisations.md` finding #5.
newtype AdjacencyRules = AdjacencyRules (Array (Array (Array PatternId)))

-- Freeze a nested `Map`-based accumulation (the natural shape to build
-- incrementally — `WFC.TileSet.buildTileSet` inserts one declared rule's
-- facts at a time via `Map.insertWith`/`Map.unionWith`) into the dense
-- `Array`-based `AdjacencyRules` actually used at solve time. `patternCount`
-- sizes each direction's dense per-pattern row (patterns with no rule at
-- all get an empty compat list, same as a missing `Map` entry would give).
fromNestedMap :: Int -> Map Direction (Map PatternId (Array PatternId)) -> AdjacencyRules
fromNestedMap patternCount byDir =
  AdjacencyRules $ map
    (\dir ->
      let byPid = fromMaybe Map.empty (Map.lookup dir byDir)
      in map (\i -> fromMaybe [] (Map.lookup (PatternId i) byPid)) idxs)
    allDirections
  where
    idxs = if patternCount <= 0 then [] else Array.range 0 (patternCount - 1)

-- Look up what patterns can exist in direction dir from pattern pid.
lookupNeighbors :: AdjacencyRules -> Direction -> PatternId -> Array PatternId
lookupNeighbors (AdjacencyRules rules) dir (PatternId pid) =
  fromMaybe [] (Array.index rules (dirIndex dir) >>= \byPid -> Array.index byPid pid)

-- Build adjacency rules from the pattern catalog.
-- Two patterns are compatible in direction d if their overlap regions agree.
buildRules :: forall a. Eq a => PatternCatalog a -> AdjacencyRules
buildRules catalog =
  let ids = patternIds catalog
      getPat pid = fromMaybe (Pattern []) (patternOf catalog pid)
      forDir dir =
        Map.fromFoldable $ map (\pid ->
          let pat     = getPat pid
              compat  = Array.filter (\pid2 -> agrees catalog.size dir pat (getPat pid2)) ids
          in Tuple pid compat
        ) ids
      byDir = Map.fromFoldable $ map (\dir -> Tuple dir (forDir dir)) allDirections
  in fromNestedMap (Array.length ids) byDir

-- Initial compatibility count for (pid, dir) at any position:
-- how many tiles in the neighbor in direction dir can support pid.
-- = |propagator[dir][pid]| — the set of patterns pid allows as its
-- direction-dir neighbour, before any of them have been ruled out.
initialCompatCount :: AdjacencyRules -> PatternId -> Direction -> Int
initialCompatCount rules pid dir = Array.length (lookupNeighbors rules dir pid)
