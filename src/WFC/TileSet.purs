module WFC.TileSet
  ( TileInstance(..)
  , TileDef
  , NeighborRule
  , Subset
  , TileSetDef
  , RuleFact
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
import WFC.Catalog (Accum, PatternCatalog, finalize)
import WFC.Direction (Direction(..), opposite)
import WFC.Pattern (Pattern(..), PatternId(..))
import WFC.Rules (AdjacencyRules(..))
import WFC.TileSet.Symmetry (Symmetry(..), distinctOrientations, reflectIndex, rotateIndexBy)

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
-- square (4 rotations × mirrored-or-not) acting on the declared 1×2 patch
-- — not just the 4 pure rotations. Mirroring the patch horizontally
-- reverses which tile is "left"/"right" (a flip swaps left and right) and
-- reflects each tile's own picture (`reflectIndex`), giving a second base
-- fact (`left`/`right` swapped, each side's declared rotation reflected)
-- that then gets the same 4-way rotation treatment as the original —
-- `rotateAll` is that shared "rotate this (left, right) pair 0/90/180/270°"
-- step, called once per base fact.
--
-- Skipping the mirrored half (as an earlier version of this function did)
-- is a no-op for `X`/`I`/`\` — their `reflectIndex` is the identity or
-- coincides with a rotation they already have — but silently drops real,
-- valid adjacencies for `L`/`T`, whose mirror image is a genuinely
-- different orientation. Any tileset leaning on `L`/`T` tiles for
-- continuity (e.g. corner/junction pieces in a road or pipe network) ends
-- up with an incomplete rule set and visible discontinuities as a result.
-- This mirrors `SimpleTiledModel.cs`'s own `densePropagator` construction,
-- which builds exactly these 8 variants per declared rule via its `a`
-- (rotate)/`b` (reflect) tables.
expandRule :: (String -> Symmetry) -> NeighborRule -> Array RuleFact
expandRule symOf rule =
  rotateAll rule.leftName leftSym rule.leftRot rule.rightName rightSym rule.rightRot
  <> rotateAll rule.rightName rightSym (reflectIndex rightSym rule.rightRot)
       rule.leftName leftSym (reflectIndex leftSym rule.leftRot)
  where
  leftSym = symOf rule.leftName
  rightSym = symOf rule.rightName

  rotateAll lName lSym lRot rName rSym rRot =
    map toFact (Array.range 0 3)
    where
    toFact k =
      { dir: applyN k rotateDirCW DirR
      , left: TileInstance { name: lName, orientation: rotateIndexBy lSym k lRot }
      , right: TileInstance { name: rName, orientation: rotateIndexBy rSym k rRot }
      }

  applyN n f x = Array.foldl (\acc _ -> f acc) x (Array.replicate n unit)

-- Every distinct oriented tile in the set, alongside its (shared, per-base-tile) weight.
tileInstances :: TileSetDef -> Array (Tuple TileInstance Number)
tileInstances def =
  def.tiles >>= \t ->
    map (\o -> Tuple (TileInstance { name: t.name, orientation: o }) t.weight)
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
    weights = Map.fromFoldable (map (\(Tuple pid (Tuple _ w)) -> Tuple pid w) indexed)
    acc = { nextId: Array.length indexed, byPixels: Map.empty, patterns, weights, origins: Map.empty } :: Accum TileInstance
    catalog = finalize acc 1

    index :: Map TileInstance PatternId
    index = Map.fromFoldable (map (\(Tuple pid (Tuple ti _)) -> Tuple ti pid) indexed)

    -- Every tile referenced by a `<neighbor>` rule is also declared in
    -- `<tiles>` in every real tileset this parses; `SymX` (cardinality 1)
    -- is a harmless fallback for a malformed file rather than a crash.
    symOf name = fromMaybe SymX (Map.lookup name symbolsByName)
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

    rules = AdjacencyRules (Array.foldl addFact Map.empty facts)
  in
    { catalog, rules, index }

-- Filter a `TileSetDef` down to a named `<subset>` (identity when
-- `Nothing`, or when the name doesn't match any declared subset) — keeps
-- only its tiles, and only the neighbor rules where both sides are in it.
selectSubset :: Maybe String -> TileSetDef -> TileSetDef
selectSubset Nothing def = def
selectSubset (Just name) def =
  case Array.find (\s -> s.name == name) def.subsets of
    Nothing -> def
    Just subset ->
      let keep n = Array.elem n subset.tiles
      in def
        { tiles = Array.filter (\t -> keep t.name) def.tiles
        , neighbors = Array.filter (\n -> keep n.leftName && keep n.rightName) def.neighbors
        }
