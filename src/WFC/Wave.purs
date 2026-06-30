module WFC.Wave where

import Prelude

import Data.Foldable (all)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import WFC.Catalog (PatternCatalog)
import WFC.Direction (Direction, allDirections)
import WFC.Grid (GridSize, Pos, allPositions)
import WFC.Pattern (PatternId)
import WFC.Rules (AdjacencyRules, initialCompatCount)

-- Per-cell possibilities. Nothing = contradiction at this cell.
type Cell = Maybe (Set PatternId)

-- compat[pos][pid][dir] = number of tiles in the direction-dir neighbour of pos
-- that still support pid being at pos.  Reaches 0 → pid must be banned.
type CompatMap = Map Pos (Map PatternId (Map Direction Int))

type Wave a =
  { cells    :: Map Pos Cell
  , compat   :: CompatMap
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
      positions = allPositions size
      cells    = Map.fromFoldable $ map (\pos -> Tuple pos initCell) positions
      compat   = Map.fromFoldable $ map (\pos -> Tuple pos cellComp) positions
  in { cells, compat, catalog, rules, size, periodic }

-- Read a cell's possibility set.
getCellPossibilities :: forall a. Wave a -> Pos -> Maybe (Set PatternId)
getCellPossibilities wave pos = fromMaybe Nothing (Map.lookup pos wave.cells)

-- True when every cell is collapsed to exactly one pattern.
isFullyCollapsed :: forall a. Wave a -> Boolean
isFullyCollapsed wave = all isSingleton (Map.values wave.cells)
  where
    isSingleton Nothing   = false
    isSingleton (Just s)  = Set.size s == 1

-- Set a cell (used internally by collapse and propagate).
setCell :: forall a. Pos -> Cell -> Wave a -> Wave a
setCell pos cell wave = wave { cells = Map.insert pos cell wave.cells }
