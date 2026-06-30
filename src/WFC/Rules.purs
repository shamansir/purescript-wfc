module WFC.Rules where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import WFC.Catalog (PatternCatalog)
import WFC.Direction (Direction, allDirections)
import WFC.Pattern (PatternId, Pattern(..), agrees)

-- propagator[dir][pid] = Array of pattern IDs that can be placed adjacent
-- in direction dir when pid is at the current cell.
newtype AdjacencyRules = AdjacencyRules (Map Direction (Map PatternId (Array PatternId)))

-- Look up what patterns can exist in direction dir from pattern pid.
lookupNeighbors :: AdjacencyRules -> Direction -> PatternId -> Array PatternId
lookupNeighbors (AdjacencyRules rules) dir pid =
  fromMaybe [] $ do
    byPid <- Map.lookup dir rules
    Map.lookup pid byPid

-- Build adjacency rules from the pattern catalog.
-- Two patterns are compatible in direction d if their overlap regions agree.
buildRules :: forall a. Eq a => PatternCatalog a -> AdjacencyRules
buildRules catalog =
  let pairs   = Map.toUnfoldable catalog.patterns :: Array (Tuple PatternId (Pattern a))
      ids     = map (\(Tuple pid _) -> pid) pairs
      getPat pid = fromMaybe (Pattern []) (Map.lookup pid catalog.patterns)
      forDir dir =
        Map.fromFoldable $ map (\pid ->
          let pat     = getPat pid
              compat  = Array.filter (\pid2 -> agrees catalog.size dir pat (getPat pid2)) ids
          in Tuple pid compat
        ) ids
  in AdjacencyRules $ Map.fromFoldable $ map (\dir -> Tuple dir (forDir dir)) allDirections

-- Initial compatibility count for (pid, dir) at any position:
-- how many tiles in the neighbor in direction dir can support pid.
-- = |propagator[opposite(dir)][pid]|
initialCompatCount :: AdjacencyRules -> PatternId -> Direction -> Int
initialCompatCount (AdjacencyRules rules) pid dir =
  fromMaybe 0 $ do
    byPid <- Map.lookup dir rules
    pure $ Array.length (fromMaybe [] (Map.lookup pid byPid))
