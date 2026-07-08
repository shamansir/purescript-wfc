module WFC.TileSet.Symmetry
  ( Symmetry(..)
  , parseSymmetry
  , cardinality
  , distinctOrientations
  , rotateIndex
  , rotateIndexBy
  , reflectIndex
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))

-- The original Wave Function Collapse algorithm's tile symmetry classes:
-- how many of a tile's 4 rotations (or, for `F`, 4 rotations × mirrored)
-- are actually distinct pictures, given the tile's own reflectional/
-- rotational symmetry. `X`/`I`/`\`/`L`/`T`/`F` are exactly the classes the
-- original XML tileset format's `symmetry="..."` attribute uses.
data Symmetry
  = SymX     -- fully symmetric (e.g. blank, or a 4-way cross) — 1 distinct orientation
  | SymI     -- a straight line/bridge — 2 distinct (horizontal vs. vertical)
  | SymDiag  -- a diagonal connector ("\") — 2 distinct (the two diagonals)
  | SymL     -- a right-angle corner — 4 distinct (one per rotation)
  | SymT     -- a T-junction — 4 distinct (one per rotation)
  | SymF     -- no symmetry at all — 8 distinct (4 rotations × mirrored)

derive instance eqSymmetry :: Eq Symmetry
derive instance ordSymmetry :: Ord Symmetry

instance showSymmetry :: Show Symmetry where
  show SymX = "X"
  show SymI = "I"
  show SymDiag = "\\"
  show SymL = "L"
  show SymT = "T"
  show SymF = "F"

parseSymmetry :: String -> Either String Symmetry
parseSymmetry "X" = Right SymX
parseSymmetry "I" = Right SymI
parseSymmetry "\\" = Right SymDiag
parseSymmetry "L" = Right SymL
parseSymmetry "T" = Right SymT
parseSymmetry "F" = Right SymF
parseSymmetry other = Left ("unknown tile symmetry: " <> show other)

-- How many of a tile's rotations are visually distinct — the size of the
-- index space `rotateIndex`/`distinctOrientations` operate on for that
-- class. (The original algorithm also tracks a *reflection* permutation
-- per class, used when a sample author additionally wants mirrored
-- variants; the tileset XML format's `<neighbor>` rules only ever declare
-- a rotation, never a mirror, so reflection plays no part in adjacency
-- expansion and isn't needed here.)
cardinality :: Symmetry -> Int
cardinality SymX = 1
cardinality SymI = 2
cardinality SymDiag = 2
cardinality SymL = 4
cardinality SymT = 4
cardinality SymF = 8

-- A tile's own distinct-orientation indices, `0 .. cardinality - 1` — this
-- is exactly the index space the tileset XML's `"name N"` neighbor
-- references live in (a sample author only ever writes `N` up to that
-- tile's own cardinality, e.g. an `I`-symmetric tile only ever appears as
-- "name" or "name 1", never "name 2"/"name 3").
distinctOrientations :: Symmetry -> Array Int
distinctOrientations sym = Array.range 0 (cardinality sym - 1)

-- Rotate one of a tile's own distinct-orientation indices by 90° clockwise,
-- landing on another (or the same) index within `0 .. cardinality - 1`.
-- These are the original algorithm's per-class rotation permutations,
-- reproduced directly rather than re-derived: `SymI`/`SymDiag` are
-- order-2 (180° rotation is a no-op), `SymL`/`SymT` are a plain 4-cycle,
-- `SymF` cycles its unmirrored (0-3) and mirrored (4-7) halves separately —
-- and in *opposite* directions from each other (4→7→6→5→4, not
-- 4→5→6→7→4): a rotation can never cross the mirror boundary, but within
-- the mirrored half it still has to compose correctly with `reflectIndex`
-- below (`rotate >>> reflect == reflect >>> rotate⁻¹`, the standard
-- dihedral-group relation) — reproduced from `SimpleTiledModel.cs`'s own
-- `a = i => i < 4 ? (i + 1) % 4 : 4 + (i - 1) % 4`.
rotateIndex :: Symmetry -> Int -> Int
rotateIndex SymX _ = 0
rotateIndex SymI i = 1 - i
rotateIndex SymDiag i = 1 - i
rotateIndex SymL i = (i + 1) `mod` 4
rotateIndex SymT i = (i + 1) `mod` 4
rotateIndex SymF i = if i < 4 then (i + 1) `mod` 4 else 4 + ((i - 1) `mod` 4)

-- Rotate by N 90° steps (N taken mod 4, since a full grid rotation cycle
-- is always 4 steps regardless of a tile's own cardinality).
rotateIndexBy :: Symmetry -> Int -> Int -> Int
rotateIndexBy sym n i0 = Array.foldl (\i _ -> rotateIndex sym i) i0 (Array.replicate (n `mod` 4) unit)

-- Reflect one of a tile's own distinct-orientation indices across a
-- vertical axis (a horizontal flip), landing on another (or the same)
-- index within `0 .. cardinality - 1` — the original algorithm's `b`
-- (reflection) permutation per class, reproduced directly alongside `a`
-- (`rotateIndex`) above. A no-op for `X`/`I` (those classes' own picture is
-- already mirror-symmetric — flipping it lands back on the same index),
-- but a *real* permutation for `L`/`T`/`\`/`F`, e.g. `L`'s mirror image is
-- one of its own other rotations (`i even -> i+1, i odd -> i-1`), not
-- itself — this is what `WFC.TileSet.expandRule` uses to expand a declared
-- `<neighbor>` rule across reflection as well as rotation, matching
-- `SimpleTiledModel.cs`'s `densePropagator` construction (which uses this
-- same `b` table via `action[t][4..7]`).
reflectIndex :: Symmetry -> Int -> Int
reflectIndex SymX _ = 0
reflectIndex SymI i = i
reflectIndex SymDiag i = 1 - i
reflectIndex SymL i = if i `mod` 2 == 0 then i + 1 else i - 1
reflectIndex SymT i = if i `mod` 2 == 0 then i else 4 - i
reflectIndex SymF i = if i < 4 then i + 4 else i - 4
