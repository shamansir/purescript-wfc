module Demo.WorkerProtocol where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Demo.Samples (SampleDef)
import WFC.Catalog (PatternCatalog)
import WFC.Grid (Pos(..))
import WFC.Pattern (PatternId)
import WFC.Render (topLeftPixel)
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
  }

emptyCustomImage :: CustomImage
emptyCustomImage =
  { grid: [], colors: [], n: 1, periodic: false, outW: 1, outH: 1, name: "" }

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
  }

-- main -> worker
type Command =
  { kind            :: String -- "run" | "stop"
  , sampleIdx       :: Int     -- ignored for "stop"; -1 means "use `custom` below"
  , mode            :: String -- "once" | "untilSolved"
  , custom          :: CustomImage -- ignored unless sampleIdx == -1 and not tiledMode
  , useBacktracking :: Boolean -- undo just the last guess instead of a full restart
  , tiledMode       :: Boolean -- hand-authored tiles (WFC.Tiles) instead of the overlapping model
  }

-- worker -> main
type Progress =
  { kind        :: String -- "progress" | "done" | "contradiction" | "stopped"
  , step        :: Int
  , solvedDelta :: Int
  , solvedTotal :: Int
  , totalCells  :: Int
  , elapsedMs   :: Number
  , restarted   :: Boolean
  , contraX     :: Int -- -1 if none
  , contraY     :: Int -- -1 if none
  , grid        :: Grid
  }

emptyProgress :: Progress
emptyProgress =
  { kind: "progress"
  , step: 0
  , solvedDelta: 0
  , solvedTotal: 0
  , totalCells: 0
  , elapsedMs: 0.0
  , restarted: false
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
