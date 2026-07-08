module WFC.Tiles where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import WFC.Catalog (Accum, PatternCatalog, finalize)
import WFC.Direction (Direction(..), allDirections)
import WFC.Pattern (Pattern(..), PatternId(..))
import WFC.Rules (AdjacencyRules(..))

-- The classic Wang-tile mechanism: a tile's compatibility with its
-- neighbours is expressed per-side as a label, not as a hand-written list
-- of compatible neighbour tiles. Two tiles are compatible with B to the
-- right of A exactly when `A.sockets.right == B.sockets.left` — this is
-- inherently symmetric (the same fact, read from either tile's side),
-- unlike a directed pairwise adjacency list which would need the reverse
-- direction stated separately to stay consistent.
type Sockets =
  { left  :: String
  , right :: String
  , up    :: String
  , down  :: String
  }

-- A hand-authored tile: a value (rendered as one cell — a tile here is a
-- size-1 Pattern, not an N×N block like the overlapping model's patterns),
-- an explicit frequency weight (rather than an occurrence count mined from
-- a sample image), and its four socket labels.
type TileDef a =
  { value   :: a
  , weight  :: Number
  , sockets :: Sockets
  }

-- Rotation/symmetry variants aren't auto-generated (unlike
-- `WFC.Pattern.symmetryVariants` for the overlapping model) — a sample
-- author lists rotated tiles as separate `TileDef`s with rotated socket
-- labels; there's no tile graphic here for the engine to rotate itself.

-- Build a `PatternCatalog` directly from a hand-authored tile list. Every
-- other solving module (`WFC.Wave`, `WFC.Entropy`, `WFC.Collapse`,
-- `WFC.Propagate`, `WFC.Algorithm`, `WFC.Backtrack`, `WFC.Render`) only
-- ever consumes a `PatternCatalog`/`AdjacencyRules` pair — this is purely
-- an alternative way to construct those two values, the solving engine
-- itself needs no changes to support the tiled model.
buildTiledCatalog :: forall a. Ord a => Array (TileDef a) -> PatternCatalog a
buildTiledCatalog tileDefs =
  let indexed  = Array.mapWithIndex (\i t -> Tuple (PatternId i) t) tileDefs
      patterns = Map.fromFoldable (map (\(Tuple pid t) -> Tuple pid (Pattern [t.value])) indexed)
      weights  = Map.fromFoldable (map (\(Tuple pid t) -> Tuple pid t.weight) indexed)
      acc      = { nextId: Array.length tileDefs, byPixels: Map.empty, patterns, weights } :: Accum a
  in finalize acc 1

sidesMatch :: forall a. Direction -> TileDef a -> TileDef a -> Boolean
sidesMatch DirR a b = a.sockets.right == b.sockets.left
sidesMatch DirL a b = a.sockets.left  == b.sockets.right
sidesMatch DirD a b = a.sockets.down  == b.sockets.up
sidesMatch DirU a b = a.sockets.up    == b.sockets.down

-- Derive adjacency rules from socket compatibility instead of the
-- overlapping model's pixel-overlap agreement (`WFC.Pattern.agrees`).
buildTiledRules :: forall a. Array (TileDef a) -> AdjacencyRules
buildTiledRules tileDefs =
  let indexed = Array.mapWithIndex (\i t -> Tuple (PatternId i) t) tileDefs
      compatibleWith dir tileA =
        Array.mapMaybe
          (\(Tuple pidB tileB) -> if sidesMatch dir tileA tileB then Just pidB else Nothing)
          indexed
      forDir dir =
        Map.fromFoldable $ map
          (\(Tuple pid tileA) -> Tuple pid (compatibleWith dir tileA))
          indexed
  in AdjacencyRules $ Map.fromFoldable $ map (\dir -> Tuple dir (forDir dir)) allDirections
