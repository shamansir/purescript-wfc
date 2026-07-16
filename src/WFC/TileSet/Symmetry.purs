module WFC.TileSet.Symmetry
  ( Symmetry(..)
  , OrientationIndex(..)
  , OrientationCount(..)
  , RotationSteps(..)
  , SymmetryCode(..)
  , SymmetryParseError(..)
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

-- One of a tile's own `0 .. cardinality - 1` distinct-orientation indices
-- (see `cardinality`/`distinctOrientations`) — kept distinct from
-- `OrientationCount` (how many such indices exist for a class) and
-- `RotationSteps` (how many 90° grid-rotation steps to apply), even though
-- all three are "just an Int".
newtype OrientationIndex = OrientationIndex Int

derive newtype instance eqOrientationIndex :: Eq OrientationIndex
derive newtype instance ordOrientationIndex :: Ord OrientationIndex
derive newtype instance showOrientationIndex :: Show OrientationIndex

newtype OrientationCount = OrientationCount Int

derive newtype instance eqOrientationCount :: Eq OrientationCount
derive newtype instance showOrientationCount :: Show OrientationCount

-- How many 90° clockwise grid-rotation steps to apply — see `rotateIndexBy`.
-- Distinct from `OrientationIndex`: this counts *applications* of
-- `rotateIndex`, not a tile's own orientation slot.
newtype RotationSteps = RotationSteps Int

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

-- The raw `symmetry="..."` attribute text, before it's validated into a
-- `Symmetry` class — distinct from the plain `String` `TileDef.name`/etc.
-- fields (already labeled there).
newtype SymmetryCode = SymmetryCode String

newtype SymmetryParseError = SymmetryParseError String

derive newtype instance eqSymmetryParseError :: Eq SymmetryParseError
derive newtype instance showSymmetryParseError :: Show SymmetryParseError

parseSymmetry :: SymmetryCode -> Either SymmetryParseError Symmetry
parseSymmetry (SymmetryCode "X") = Right SymX
parseSymmetry (SymmetryCode "I") = Right SymI
parseSymmetry (SymmetryCode "\\") = Right SymDiag
parseSymmetry (SymmetryCode "L") = Right SymL
parseSymmetry (SymmetryCode "T") = Right SymT
parseSymmetry (SymmetryCode "F") = Right SymF
parseSymmetry (SymmetryCode other) = Left (SymmetryParseError ("unknown tile symmetry: " <> show other))

-- How many of a tile's rotations are visually distinct — the size of the
-- index space `rotateIndex`/`distinctOrientations` operate on for that
-- class. (The original algorithm also tracks a *reflection* permutation
-- per class, used when a sample author additionally wants mirrored
-- variants; the tileset XML format's `<neighbor>` rules only ever declare
-- a rotation, never a mirror, so reflection plays no part in adjacency
-- expansion and isn't needed here.)
cardinality :: Symmetry -> OrientationCount
cardinality SymX = OrientationCount 1
cardinality SymI = OrientationCount 2
cardinality SymDiag = OrientationCount 2
cardinality SymL = OrientationCount 4
cardinality SymT = OrientationCount 4
cardinality SymF = OrientationCount 8

-- A tile's own distinct-orientation indices, `0 .. cardinality - 1` — this
-- is exactly the index space the tileset XML's `"name N"` neighbor
-- references live in (a sample author only ever writes `N` up to that
-- tile's own cardinality, e.g. an `I`-symmetric tile only ever appears as
-- "name" or "name 1", never "name 2"/"name 3").
distinctOrientations :: Symmetry -> Array OrientationIndex
distinctOrientations sym =
  let OrientationCount n = cardinality sym
  in map OrientationIndex (Array.range 0 (n - 1))

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
rotateIndex :: Symmetry -> OrientationIndex -> OrientationIndex
rotateIndex SymX _ = OrientationIndex 0
rotateIndex SymI (OrientationIndex i) = OrientationIndex (1 - i)
rotateIndex SymDiag (OrientationIndex i) = OrientationIndex (1 - i)
rotateIndex SymL (OrientationIndex i) = OrientationIndex ((i + 1) `mod` 4)
rotateIndex SymT (OrientationIndex i) = OrientationIndex ((i + 1) `mod` 4)
rotateIndex SymF (OrientationIndex i) = OrientationIndex (if i < 4 then (i + 1) `mod` 4 else 4 + ((i - 1) `mod` 4))

-- Rotate by N 90° steps (N taken mod 4, since a full grid rotation cycle
-- is always 4 steps regardless of a tile's own cardinality).
rotateIndexBy :: Symmetry -> RotationSteps -> OrientationIndex -> OrientationIndex
rotateIndexBy sym (RotationSteps n) i0 = Array.foldl (\i _ -> rotateIndex sym i) i0 (Array.replicate (n `mod` 4) unit)

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
reflectIndex :: Symmetry -> OrientationIndex -> OrientationIndex
reflectIndex SymX _ = OrientationIndex 0
reflectIndex SymI (OrientationIndex i) = OrientationIndex i
reflectIndex SymDiag (OrientationIndex i) = OrientationIndex (1 - i)
reflectIndex SymL (OrientationIndex i) = OrientationIndex (if i `mod` 2 == 0 then i + 1 else i - 1)
reflectIndex SymT (OrientationIndex i) = OrientationIndex (if i `mod` 2 == 0 then i else 4 - i)
reflectIndex SymF (OrientationIndex i) = OrientationIndex (if i < 4 then i + 4 else i - 4)
