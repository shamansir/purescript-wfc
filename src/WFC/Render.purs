module WFC.Render where

import Prelude

import Data.Array as Array
import Data.Foldable (minimum)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Traversable (traverse)
import WFC.Catalog (PatternCatalog, patternOf)
import WFC.Grid (Pos(..))
import WFC.Pattern (Pattern(..), PatternId)
import WFC.Wave (Wave)

-- Extract the top-left pixel of a pattern — canonical output pixel for that cell.
topLeftPixel :: forall a. PatternCatalog a -> PatternId -> Maybe a
topLeftPixel catalog pid = do
  Pattern px <- patternOf catalog pid
  Array.head px

-- Read the single collapsed PatternId from a cell.
collapsedId :: Maybe (Set.Set PatternId) -> Maybe PatternId
collapsedId Nothing  = Nothing
collapsedId (Just s)
  | Set.size s == 1 = minimum s  -- Set is Foldable; minimum gives the only element
  | otherwise       = Nothing

-- Render a fully-collapsed wave into a 2D pixel grid.
-- Returns Nothing if any cell is not collapsed or is a contradiction.
renderWave :: forall a. Wave a -> Maybe (Array (Array a))
renderWave wave =
  let { width, height } = wave.size
      go y = traverse (goCell y) (Array.range 0 (width - 1))
      goCell y x = do
        let pos = Pos { x, y }
        cell <- Map.lookup pos wave.cells
        pid  <- collapsedId cell
        topLeftPixel wave.catalog pid
  in traverse go (Array.range 0 (height - 1))

-- Render with a fallback pixel for uncollapsed/contradiction cells.
renderWaveWith :: forall a. a -> Wave a -> Array (Array a)
renderWaveWith fallback wave =
  let { width, height } = wave.size
  in map (\y ->
    map (\x ->
      fromMaybe fallback $ do
        let pos = Pos { x, y }
        cell <- Map.lookup pos wave.cells
        pid  <- collapsedId cell
        topLeftPixel wave.catalog pid
    ) (Array.range 0 (width - 1))
  ) (Array.range 0 (height - 1))
