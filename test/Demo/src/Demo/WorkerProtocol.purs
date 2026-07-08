module Demo.WorkerProtocol where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
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

-- main -> worker
type Command =
  { kind      :: String -- "run" | "stop"
  , sampleIdx :: Int     -- ignored for "stop"
  , mode      :: String -- "once" | "untilSolved"
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

-- Map a cell snapshot to a fill color using the sample's palette.
cellColor :: SampleDef -> CellSnapshot -> String
cellColor _ snap | snap.contradiction = "#ff4444"
cellColor sample snap
  | snap.collapsed = case Array.head snap.values of
      Just v  -> sample.palette v
      Nothing -> "#888888"
  | Array.length snap.values == 0 = "#ff4444"
  | otherwise = "#c0c0d0"

snapshotToPalette :: SampleDef -> Grid -> Array (Array String)
snapshotToPalette sample = map (map (cellColor sample))
