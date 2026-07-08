-- Public API for the WFC library.
module WFC
  ( module WFC.Algorithm
  , module WFC.Catalog
  , module WFC.Direction
  , module WFC.Grid
  , module WFC.Pattern
  , module WFC.Propagate
  , module WFC.Render
  , module WFC.Rules
  , module WFC.Wave
  ) where

import WFC.Algorithm (wfc, wfcWithRetry, step)
import WFC.Catalog (PatternCatalog, PatternOrigin, extractPatterns)
import WFC.Direction (Direction(..), allDirections, dirOffset, opposite)
import WFC.Grid (GridSize, Pos(..), allPositions, neighborPos)
import WFC.Pattern (Pattern(..), PatternId(..), VariantTag, agrees, rotate, reflect, variantsFor, taggedVariantsFor)
import WFC.Propagate (Contradiction(..), BanEvent, propagate)
import WFC.Render (renderWave, renderWaveWith, topLeftPixel)
import WFC.Rules (AdjacencyRules(..), buildRules, lookupNeighbors)
import WFC.Wave (Cell, CompatMap, Wave, initWave, isFullyCollapsed)
