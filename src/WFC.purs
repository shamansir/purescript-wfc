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
  , module WFC.TileSet
  , module WFC.TileSet.Symmetry
  , module WFC.TileSet.Xml
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
import WFC.TileSet (TileInstance(..), TileDef, NeighborRule, Subset, TileSetDef, RuleFact, rotateDirCW, expandRule, tileInstances, buildTileSet, selectSubset)
import WFC.TileSet.Symmetry (Symmetry(..), parseSymmetry, cardinality, distinctOrientations, rotateIndex, rotateIndexBy)
import WFC.TileSet.Xml (parseTileSetXml)
import WFC.Wave (Cell, CompatMap, Wave, initWave, isFullyCollapsed)
