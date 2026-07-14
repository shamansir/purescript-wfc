module WFC.TileSet
  ( TileInstance(..)
  , TileDef
  , NeighborRule
  , Subset
  , TileSetDef
  , RuleFact
  , TileName(..)
  , SubsetName(..)
  , Unique(..)
  , rotateDirCW
  , expandRule
  , tileInstances
  , buildTileSet
  , selectSubset
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import WFC.Catalog (Accum, PatternCatalog, Weight(..), finalize)
import WFC.Direction (Direction(..), opposite)
import WFC.Pattern (Pattern(..), PatternId(..), PatternSize(..))
import WFC.PatternMap (PatternCount(..))
import WFC.Rules (AdjacencyRules, fromNestedMap)
import WFC.TileSet.Symmetry (OrientationIndex(..), RotationSteps(..), Symmetry(..), distinctOrientations, reflectIndex, rotateIndex, rotateIndexBy)

-- A tile's own name — distinct from a bare `String` at call sites that
-- look one up by name (`expandRule`'s `String -> Symmetry`), even though
-- `TileDef`/`NeighborRule`/`Subset`'s own `name`/`leftName`/etc. fields
-- stay plain `String` (already labeled, see those records).
newtype TileName = TileName String

derive newtype instance eqTileName :: Eq TileName
derive newtype instance ordTileName :: Ord TileName
derive newtype instance showTileName :: Show TileName

-- Whether each of a tileset's rotations has its own separate source image
-- vs. one image rotated programmatically — the parsed value of a `<set
-- unique="...">` attribute. `TileSetDef`'s own `unique` field stays plain
-- `Boolean` (already labeled there); this is for `WFC.TileSet.Xml.parseBool`,
-- which builds that field's value from raw XML text.
newtype Unique = Unique Boolean

-- One specific oriented tile — a base tile name plus which of its own
-- `cardinality`-many distinct orientations this is (see
-- `WFC.TileSet.Symmetry`). This is the `a` a tileset's `PatternCatalog`/
-- `Wave` gets instantiated with — analogous to `WFC.Tiles.TileDef`'s
-- `value :: a`, except here the catalog builds these itself (from a base
-- tile × its symmetry-derived orientation range) rather than a sample
-- author hand-listing one `TileDef` per orientation.
newtype TileInstance = TileInstance { name :: String, orientation :: Int }

derive instance eqTileInstance :: Eq TileInstance
derive instance ordTileInstance :: Ord TileInstance

instance showTileInstance :: Show TileInstance where
  show (TileInstance t) = t.name <> " " <> show t.orientation

-- One `<tile>` entry: its name, symmetry class, and frequency weight
-- (every one of its symmetry-derived orientations shares this same
-- weight — the original algorithm doesn't divide it across variants).
type TileDef =
  { name     :: String
  , symmetry :: Symmetry
  , weight   :: Number
  }

-- One declared `<neighbor left="A N" right="B M"/>` rule: tile A at its
-- own orientation N is compatible with tile B at orientation M, placed to
-- A's right (`DirR`) — see `expandRule` for how this one declaration
-- becomes up to 4 concrete directional adjacency facts.
type NeighborRule =
  { leftName  :: String
  , leftRot   :: Int
  , rightName :: String
  , rightRot  :: Int
  }

-- A named `<subset>` — a restricted tile list some samples group their
-- tiles into (e.g. "Turnless"), for building a smaller variant model.
type Subset =
  { name  :: String
  , tiles :: Array String
  }

-- The whole parsed `<set>...</set>` document.
type TileSetDef =
  { unique    :: Boolean         -- each rotation has its own separate source image, vs. one image rotated programmatically
  , tiles     :: Array TileDef
  , neighbors :: Array NeighborRule
  , subsets   :: Array Subset
  }

-- Rotate a cardinal direction by 90° clockwise — grid rotation, applied
-- uniformly regardless of any individual tile's own symmetry.
rotateDirCW :: Direction -> Direction
rotateDirCW DirR = DirD
rotateDirCW DirD = DirL
rotateDirCW DirL = DirU
rotateDirCW DirU = DirR

-- One concrete directional adjacency fact.
type RuleFact =
  { dir   :: Direction
  , left  :: TileInstance
  , right :: TileInstance
  }

-- Expand one declared (always `DirR`-based) neighbor rule into every
-- adjacency fact implied by the full 8-element symmetry group of the
-- square (4 rotations × mirrored-or-not) acting on the declared 1×2 patch.
--
-- This is a direct, faithful transliteration of `SimpleTiledModel.cs`'s
-- `densePropagator` construction — NOT a re-derivation from "rotate the
-- whole pair 0/90/180/270°, then again mirrored" (an earlier version of
-- this function tried that "cleaner" reformulation and got the axis/
-- ordering structure subtly wrong: rotating a horizontal pair 180° does
-- *not* leave the pair pointing the same way, it reverses which side is
-- "left" — {identity, reflect} alone is not the right pairing for a fixed
-- axis, {identity, rotate180, reflect, reflect+rotate180} is). Only 2 of
-- the 4 cardinal directions are ever built directly (`DirL` from the
-- declared left/right pair, `DirD` from the same pair pre-rotated 90°);
-- `DirR`/`DirU` come for free from `buildTileSet`'s existing
-- opposite-direction reciprocal insert on every fact, exactly mirroring
-- the original's `densePropagator[2]`/`[3]` transpose-fill.
--
-- Skipping the reflect-derived facts entirely (as an even earlier version
-- did) is a no-op for `X`/`I`/`\` — their `reflectIndex` is the identity or
-- coincides with a rotation they already have — but silently drops real,
-- valid adjacencies for `L`/`T`, whose mirror image is a genuinely
-- different orientation. Any tileset leaning on `L`/`T` tiles for
-- continuity (e.g. corner/junction pieces in a road or pipe network) ends
-- up with an incomplete rule set and visible discontinuities as a result.
expandRule :: (TileName -> Symmetry) -> NeighborRule -> Array RuleFact
expandRule symOf rule =
  [ { dir: DirL, left: ti rule.rightName r,  right: ti rule.leftName l   }
  , { dir: DirL, left: ti rule.rightName r6, right: ti rule.leftName l6  }
  , { dir: DirL, left: ti rule.leftName l4,  right: ti rule.rightName r4 }
  , { dir: DirL, left: ti rule.leftName l2,  right: ti rule.rightName r2 }
  , { dir: DirD, left: ti rule.rightName u,  right: ti rule.leftName d   }
  , { dir: DirD, left: ti rule.leftName d6,  right: ti rule.rightName u6 }
  , { dir: DirD, left: ti rule.rightName u4, right: ti rule.leftName d4  }
  , { dir: DirD, left: ti rule.leftName d2,  right: ti rule.rightName u2 }
  ]
  where
  leftSym = symOf (TileName rule.leftName)
  rightSym = symOf (TileName rule.rightName)
  ti name (OrientationIndex orientation) = TileInstance { name, orientation }

  l = OrientationIndex rule.leftRot
  r = OrientationIndex rule.rightRot
  d = rotateIndex leftSym l
  u = rotateIndex rightSym r

  l2 = rotateIndexBy leftSym (RotationSteps 2) l
  l4 = reflectIndex leftSym l
  l6 = reflectIndex leftSym l2

  r2 = rotateIndexBy rightSym (RotationSteps 2) r
  r4 = reflectIndex rightSym r
  r6 = reflectIndex rightSym r2

  d2 = rotateIndexBy leftSym (RotationSteps 2) d
  d4 = reflectIndex leftSym d
  d6 = reflectIndex leftSym d2

  u2 = rotateIndexBy rightSym (RotationSteps 2) u
  u4 = reflectIndex rightSym u
  u6 = reflectIndex rightSym u2

-- Every distinct oriented tile in the set, alongside its (shared, per-base-tile) weight.
tileInstances :: TileSetDef -> Array (Tuple TileInstance Weight)
tileInstances def =
  def.tiles >>= \t ->
    map (\(OrientationIndex o) -> Tuple (TileInstance { name: t.name, orientation: o }) (Weight t.weight))
      (distinctOrientations t.symmetry)

-- Build a `PatternCatalog`/`AdjacencyRules` pair directly from a parsed
-- tileset — same shape as `WFC.Tiles.buildTiledCatalog`/`buildTiledRules`
-- (every solving module downstream only ever consumes this pair, and needs
-- no changes to support it), but sourced from symmetry-expanded tiles and
-- rotation-expanded neighbor rules instead of hand-listed sockets.
buildTileSet
  :: TileSetDef
  -> { catalog :: PatternCatalog TileInstance
     , rules :: AdjacencyRules
     , index :: Map TileInstance PatternId
     }
buildTileSet def =
  let
    indexed = Array.mapWithIndex (\i (Tuple ti w) -> Tuple (PatternId i) (Tuple ti w)) (tileInstances def)
    patterns = Map.fromFoldable (map (\(Tuple pid (Tuple ti _)) -> Tuple pid (Pattern [ ti ])) indexed)
    weights = Map.fromFoldable (map (\(Tuple pid (Tuple _ (Weight w))) -> Tuple pid w) indexed)
    acc = { nextId: Array.length indexed, byPixels: Map.empty, patterns, weights, origins: Map.empty } :: Accum TileInstance
    catalog = finalize acc (PatternSize 1)

    index :: Map TileInstance PatternId
    index = Map.fromFoldable (map (\(Tuple pid (Tuple ti _)) -> Tuple ti pid) indexed)

    -- Every tile referenced by a `<neighbor>` rule is also declared in
    -- `<tiles>` in every real tileset this parses; `SymX` (cardinality 1)
    -- is a harmless fallback for a malformed file rather than a crash.
    symOf (TileName name) = fromMaybe SymX (Map.lookup name symbolsByName)
    symbolsByName = Map.fromFoldable (map (\t -> Tuple t.name t.symmetry) def.tiles)

    facts :: Array RuleFact
    facts = def.neighbors >>= expandRule symOf

    addFact acc0 fact = fromMaybe acc0 do
      lpid <- Map.lookup fact.left index
      rpid <- Map.lookup fact.right index
      pure
        $ insertNeighbor fact.dir lpid rpid
        $ insertNeighbor (opposite fact.dir) rpid lpid
        $ acc0

    insertNeighbor dir fromPid toPid m =
      Map.insertWith (Map.unionWith (\a b -> Array.nub (a <> b)))
        dir
        (Map.singleton fromPid [ toPid ])
        m

    rules = fromNestedMap (PatternCount (Array.length indexed)) (Array.foldl addFact Map.empty facts)
  in
    { catalog, rules, index }

-- Filter a `TileSetDef` down to a named `<subset>` (identity when
-- `Nothing`, or when the name doesn't match any declared subset) — keeps
-- only its tiles, and only the neighbor rules where both sides are in it.
newtype SubsetName = SubsetName String

selectSubset :: Maybe SubsetName -> TileSetDef -> TileSetDef
selectSubset Nothing def = def
selectSubset (Just (SubsetName name)) def =
  case Array.find (\s -> s.name == name) def.subsets of
    Nothing -> def
    Just subset ->
      let keep n = Array.elem n subset.tiles
      in def
        { tiles = Array.filter (\t -> keep t.name) def.tiles
        , neighbors = Array.filter (\n -> keep n.leftName && keep n.rightName) def.neighbors
        }
