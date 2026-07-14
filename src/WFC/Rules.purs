module WFC.Rules where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import WFC.Catalog (PatternCatalog, patternIds, patternOf)
import WFC.CompatibilityMap (CompatibilityCount(..))
import WFC.Direction (Direction, DirectionIndex(..), allDirections, dirIndex)
import WFC.Pattern (Agrees(..), PatternId(..), PatternSize(..), Pattern(..), agrees)
import WFC.PatternMap (PatternCount(..), PatternMap)
import WFC.PatternMap as PatternMap

-- propagator[dir][pid] = Array of pattern IDs that can be placed adjacent
-- in direction dir when pid is at the current cell.
--
-- `Array (PatternMap (Array PatternId))` indexed `[dirIndex dir][pid]`, not
-- `Map Direction (Map PatternId (Array PatternId))` — built once
-- (`buildRules`/`WFC.Tiles.buildTiledRules`/`WFC.TileSet.buildTileSet`) and
-- read very hot afterwards (`lookupNeighbors`, 4×/ban in
-- `WFC.Propagate.processNeighbours`). `Direction` has only 4 values (a
-- plain `Array`, indexed by `dirIndex`, is dense enough on its own);
-- `PatternId` is contiguous `0..T-1`, which is exactly what `PatternMap`
-- is for. See `docs/Optimisations.md` finding #5.
newtype AdjacencyRules = AdjacencyRules (Array (PatternMap (Array PatternId)))

-- Freeze a nested `Map`-based accumulation (the natural shape to build
-- incrementally — `WFC.TileSet.buildTileSet` inserts one declared rule's
-- facts at a time via `Map.insertWith`/`Map.unionWith`) into the dense
-- `AdjacencyRules` actually used at solve time. `patternCount` sizes each
-- direction's dense `PatternMap` row (patterns with no rule at all get an
-- empty compat list, same as a missing `Map` entry would give).
fromNestedMap :: PatternCount -> Map Direction (Map PatternId (Array PatternId)) -> AdjacencyRules
fromNestedMap (PatternCount patternCount) byDir =
  AdjacencyRules $ map
    (\dir ->
      let byPid = fromMaybe Map.empty (Map.lookup dir byDir)
      in PatternMap.fromArray (map (\i -> fromMaybe [] (Map.lookup (PatternId i) byPid)) idxs))
    allDirections
  where
    idxs = if patternCount <= 0 then [] else Array.range 0 (patternCount - 1)

-- Look up what patterns can exist in direction dir from pattern pid.
lookupNeighbors :: AdjacencyRules -> Direction -> PatternId -> Array PatternId
lookupNeighbors (AdjacencyRules rules) dir pid =
  let DirectionIndex di = dirIndex dir
  in fromMaybe [] (Array.index rules di >>= \byPid -> PatternMap.index byPid pid)

-- Build adjacency rules from the pattern catalog.
-- Two patterns are compatible in direction d if their overlap regions agree.
buildRules :: forall a. Eq a => PatternCatalog a -> AdjacencyRules
buildRules catalog =
  let ids = patternIds catalog
      n = PatternSize catalog.size
      getPat pid = fromMaybe (Pattern []) (patternOf catalog pid)
      compatible dir pat pid2 = case agrees n dir pat (getPat pid2) of
        Agrees b -> b
      forDir dir =
        Map.fromFoldable $ map (\pid ->
          let pat     = getPat pid
              compat  = Array.filter (compatible dir pat) ids
          in Tuple pid compat
        ) ids
      byDir = Map.fromFoldable $ map (\dir -> Tuple dir (forDir dir)) allDirections
  in fromNestedMap (PatternCount (Array.length ids)) byDir

-- Initial compatibility count for (pid, dir) at any position:
-- how many tiles in the neighbor in direction dir can support pid.
-- = |propagator[dir][pid]| — the set of patterns pid allows as its
-- direction-dir neighbour, before any of them have been ruled out.
initialCompatCount :: AdjacencyRules -> PatternId -> Direction -> CompatibilityCount
initialCompatCount rules pid dir = CompatibilityCount (Array.length (lookupNeighbors rules dir pid))
