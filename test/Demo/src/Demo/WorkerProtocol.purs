module Demo.WorkerProtocol where

import Prelude

import Data.Array as Array
import Data.Either (hush)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Demo.Samples (SampleDef)
import WFC.Catalog (PatternCatalog, patternIds, patternsWithIds)
import WFC.PatternMap as PatternMap
import WFC.Grid (Pos(..))
import WFC.Pattern (Pattern(..), PatternId(..))
import WFC.Render (topLeftPixel)
import WFC.Rules (AdjacencyRules)
import WFC.TileSet (NeighborRule, Subset, TileInstance(..), TileSetDef, buildTileSet)
import WFC.TileSet.Symmetry (Symmetry(..), parseSymmetry)
import WFC.Wave (Wave, getCellPossibilities)

-- One cell's worker-reported state, plain enough to satisfy `IsSendable`
-- (Boolean/Boolean/Array Int only) so it can cross the postMessage boundary.
type CellSnapshot =
  { collapsed     :: Boolean
  , contradiction :: Boolean
  , values        :: Array Int
  }

type Grid = Array (Array CellSnapshot)

-- Plain-data twin of `SampleDef` — `SampleDef.palette :: Int -> String` is a
-- function and can't cross postMessage, so an uploaded/custom sample is sent
-- as a flat color table instead, and both sides rebuild a `SampleDef` from it
-- via `customSampleDef`.
type CustomImage =
  { grid     :: Array (Array Int)
  , colors   :: Array String
  , n        :: Int
  , periodic :: Boolean
  , outW     :: Int
  , outH     :: Int
  , name     :: String
  , ground   :: Boolean -- see WFC.Propagate.applyGround; carried from the selected ImageSampleDef, false for uploads
  }

emptyCustomImage :: CustomImage
emptyCustomImage =
  { grid: [], colors: [], n: 1, periodic: false, outW: 1, outH: 1, name: "", ground: false }

paletteFromColors :: Array String -> Int -> String
paletteFromColors colors v = fromMaybe "#888888" (Array.index colors v)

customSampleDef :: CustomImage -> SampleDef
customSampleDef ci =
  { name: ci.name
  , grid: ci.grid
  , palette: paletteFromColors ci.colors
  , n: ci.n
  , periodic: ci.periodic
  , outW: ci.outW
  , outH: ci.outH
  , ground: ci.ground
  }

-- Plain-data twin of `WFC.TileSet.TileDef` — `symmetry` travels as its
-- `String` name instead of the `Symmetry` ADT, so the whole thing is
-- IsSendable-safe (`WFC.TileSet.NeighborRule`/`Subset` are already plain
-- records, no conversion needed there).
type WireTileDef =
  { name     :: String
  , symmetry :: String
  , weight   :: Number
  }

-- Plain-data twin of `WFC.TileSet.TileSetDef` — a parsed XML tileset, sent
-- from the main thread (which fetches/parses the file) to the worker
-- (which independently rebuilds the same catalog/rules from it), the same
-- way an uploaded/built-in image crosses as a `CustomImage`.
type CustomTileSet =
  { unique    :: Boolean
  , tiles     :: Array WireTileDef
  , neighbors :: Array NeighborRule
  , subsets   :: Array Subset
  , name      :: String
  }

emptyCustomTileSet :: CustomTileSet
emptyCustomTileSet = { unique: false, tiles: [], neighbors: [], subsets: [], name: "" }

toWireTileSet :: String -> TileSetDef -> CustomTileSet
toWireTileSet name def =
  { unique: def.unique
  , tiles: map (\t -> { name: t.name, symmetry: show t.symmetry, weight: t.weight }) def.tiles
  , neighbors: def.neighbors
  , subsets: def.subsets
  , name
  }

fromWireTileSet :: CustomTileSet -> TileSetDef
fromWireTileSet w =
  { unique: w.unique
  , tiles: map (\t -> { name: t.name, symmetry: fromMaybe SymX (hush (parseSymmetry t.symmetry)), weight: t.weight }) w.tiles
  , neighbors: w.neighbors
  , subsets: w.subsets
  }

-- Just the two `TileInstance` fields a renderer needs (which base tile,
-- which of its own orientations) — a plain record so it stays IsSendable
-- and doesn't require importing `WFC.TileSet`'s newtype everywhere.
type TileRef = { name :: String, orientation :: Int }

-- `WFC.TileSet.buildTileSet` produces a `PatternCatalog TileInstance` (each
-- pattern's one pixel is which oriented tile it is) — the rest of the demo
-- (canvas/table/pattern-thumb rendering, `CellSnapshot.values :: Array
-- Int`) is built around `PatternCatalog Int`, so this remaps each pattern
-- to a plain `Int` id (its own `PatternId`'s number — stable, unique),
-- derives an `Int -> String` palette by cycling a hue per distinct tile
-- name (used as a fallback before a tile's image has loaded, or if it
-- fails to), and exposes `tileOf :: Int -> Maybe TileRef` so a renderer
-- that *does* want the real picture (`Demo.App`'s pattern thumbnails/
-- canvas) can look up which tile+orientation a given `Int` stands for.
buildIntCatalogFromTileSet
  :: TileSetDef
  -> { catalog :: PatternCatalog Int, rules :: AdjacencyRules, palette :: Int -> String, tileOf :: Int -> Maybe TileRef }
buildIntCatalogFromTileSet def =
  let
    built = buildTileSet def
    entries = patternsWithIds built.catalog :: Array (Tuple PatternId (Pattern TileInstance))
    intOf (PatternId i) = i
    newPatterns = PatternMap.fromArray (map (\(Tuple pid _) -> Pattern [ intOf pid ]) entries)
    catalog = built.catalog { patterns = newPatterns }
    refsByInt :: Map Int TileRef
    refsByInt = Map.fromFoldable
      (Array.mapMaybe
        (\(Tuple pid (Pattern px)) -> Tuple (intOf pid) <$> (toRef <$> Array.head px))
        entries)
    toRef (TileInstance t) = { name: t.name, orientation: t.orientation }
    tileOf i = Map.lookup i refsByInt
    distinctNames = Array.nub (map _.name (Array.fromFoldable refsByInt))
    hueColor i = "hsl(" <> show ((i * 137) `mod` 360) <> ", 60%, 55%)"
    colorOfName name = fromMaybe "#888888" (hueColor <$> Array.elemIndex name distinctNames)
    palette i = colorOfName (fromMaybe "" (_.name <$> tileOf i))
  in
    { catalog, rules: built.rules, palette, tileOf }

-- main -> worker
type Command =
  { kind            :: String -- "run" | "step" | "stop" | "resetSession"
  , sourceKind      :: String -- "builtin" | "image" | "handTiled" | "xmlTileset" — which of the fields below is meaningful
  , sampleIdx       :: Int     -- index into the relevant compiled-in list ("builtin"/"handTiled"); ignored otherwise
  , mode            :: String -- "once" | "untilSolved" for "run"; ignored (ever ""/single-shot) for "step"
  , custom          :: CustomImage -- used when sourceKind == "image"
  , customTileSet   :: CustomTileSet -- used when sourceKind == "xmlTileset"
  , useBacktracking :: Boolean -- undo just the last guess instead of a full restart
  , patternSize     :: Int     -- overlapping model only (N×N patterns); ignored for tile-based sources
  , outW            :: Int     -- result grid width, overrides the sample's own default
  , outH            :: Int     -- result grid height, overrides the sample's own default
  , useRotations    :: Boolean -- overlapping model only; also extract 90°/180°/270° rotations
  , useMirror       :: Boolean -- overlapping model only; also extract the horizontal reflection
  , inputPeriodic   :: Boolean -- overlapping model only; wrap the source grid when extracting N×N windows
  , outputPeriodic  :: Boolean -- wrap the output wave's own edges during solving (all sources)
  }

-- worker -> main
type Progress =
  { kind           :: String -- "progress" | "done" | "contradiction" | "stopped"
  , token          :: Int    -- the worker's internal request counter at the time of this message;
                              -- mirrored by the main thread's own send-count so a message can be
                              -- recognized as stale (from a run/step superseded by a later one,
                              -- including a Stop) and ignored, without a separate stop/pause flag
  , step           :: Int
  , solvedDelta    :: Int
  , solvedTotal    :: Int
  , totalCells     :: Int
  , elapsedMs      :: Number
  , restarted      :: Boolean -- full restart from scratch: a new history row starting at column 0
  , rowBreak       :: Boolean -- this step starts a new history row (restart, or a backtracking pop)
  , rowStartColumn :: Int     -- column that new row starts at when rowBreak; meaningless otherwise
  , backedOut      :: Boolean -- this single step performed a backtrack pop (WFC.Backtrack.BackedOut)
  , continuous     :: Boolean -- true for a Run-driven message (loop keeps animating); false for a single "step" reply
  , contraX        :: Int -- -1 if none
  , contraY        :: Int -- -1 if none
  , grid           :: Grid
  }

emptyProgress :: Progress
emptyProgress =
  { kind: "progress"
  , token: 0
  , step: 0
  , solvedDelta: 0
  , solvedTotal: 0
  , totalCells: 0
  , elapsedMs: 0.0
  , restarted: false
  , rowBreak: false
  , rowStartColumn: 0
  , backedOut: false
  , continuous: false
  , contraX: -1
  , contraY: -1
  , grid: []
  }

contradictionSnapshot :: CellSnapshot
contradictionSnapshot = { collapsed: false, contradiction: true, values: [] }

cellSnapshot :: PatternCatalog Int -> Maybe (Set.Set PatternId) -> CellSnapshot
cellSnapshot _   Nothing     = contradictionSnapshot
cellSnapshot cat (Just pids) =
  { collapsed: Set.size pids == 1
  , contradiction: false
  , values: Array.mapMaybe (topLeftPixel cat) (Set.toUnfoldable pids :: Array PatternId)
  }

-- The canonical "fully uncollapsed" cell for a catalog — every pattern
-- still possible, same as any cell of a freshly `initWave`d wave's own
-- snapshot would be, computed directly instead of needing a whole `Wave`
-- just for this. Used to fill newly-exposed grid area the instant Result
-- W/H grows (see `resizeGrid`), without waiting on (or restarting) a solve.
blankCellSnapshot :: PatternCatalog Int -> CellSnapshot
blankCellSnapshot cat =
  cellSnapshot cat (Just (Set.fromFoldable (patternIds cat)))

-- Crop rows/columns beyond `newW`/`newH`, or pad the right/bottom edge with
-- `blank` — never touching a cell within the old bounds. Top-left anchored,
-- mirroring `WFC.Wave.resizeWave`'s same crop/extend semantics on the
-- engine side, so a displayed grid resized this way and a session's own
-- wave resized that way always agree pixel-for-pixel.
resizeGrid :: CellSnapshot -> Int -> Int -> Grid -> Grid
resizeGrid blank newW newH grid =
  let
    padRow row = Array.take newW row <> Array.replicate (max 0 (newW - Array.length row)) blank
    rows       = map padRow (Array.take newH grid)
    blankRow   = Array.replicate newW blank
  in
    rows <> Array.replicate (max 0 (newH - Array.length rows)) blankRow

-- Render a wave into the plain snapshot grid sent over postMessage.
waveToSnapshot :: PatternCatalog Int -> Wave Int -> Grid
waveToSnapshot cat wave =
  map (\y -> map (\x -> cellSnapshot cat (getCellPossibilities wave (Pos { x, y }))) xs) ys
  where
    xs = Array.range 0 (wave.size.width - 1)
    ys = Array.range 0 (wave.size.height - 1)

-- Force a specific cell to render as the contradiction marker, used when the
-- engine only reports the failing `Pos`, not the wave state at time of failure.
markContradiction :: Int -> Int -> Grid -> Grid
markContradiction x y = Array.mapWithIndex \yy row ->
  if yy /= y then row
  else Array.mapWithIndex (\xx c -> if xx == x then contradictionSnapshot else c) row

solvedCount :: Grid -> Int
solvedCount = foldl (\acc row -> acc + foldl (\a c -> if c.collapsed then a + 1 else a) 0 row) 0

totalCellCount :: Grid -> Int
totalCellCount = foldl (\acc row -> acc + Array.length row) 0

-- Map a cell snapshot to a fill color using a palette function — just the
-- palette, not a whole sample record, so this works the same whether the
-- active sample came from the overlapping model or the tiled model (or
-- anything else with a palette).
cellColor :: (Int -> String) -> CellSnapshot -> String
cellColor _ snap | snap.contradiction = "#ff4444"
cellColor palette snap
  | snap.collapsed = case Array.head snap.values of
      Just v  -> palette v
      Nothing -> "#888888"
  | Array.length snap.values == 0 = "#ff4444"
  | otherwise = "#c0c0d0"

snapshotToPalette :: (Int -> String) -> Grid -> Array (Array String)
snapshotToPalette palette = map (map (cellColor palette))
