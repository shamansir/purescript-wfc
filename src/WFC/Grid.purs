module WFC.Grid where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import WFC.Direction (Direction, dirOffset)

newtype Pos = Pos { x :: Int, y :: Int }

derive instance eqPos  :: Eq Pos
derive instance ordPos :: Ord Pos

instance showPos :: Show Pos where
  show (Pos { x, y }) = "(" <> show x <> "," <> show y <> ")"

type GridSize = { width :: Int, height :: Int }

allPositions :: GridSize -> Array Pos
allPositions { width, height } = do
  y <- Array.range 0 (height - 1)
  x <- Array.range 0 (width - 1)
  pure (Pos { x, y })

-- Whether the *output* wave wraps at its edges during solving — distinct
-- from `WFC.Catalog`'s `InputPeriodic` (whether the *source sample* wraps
-- when extracting N×N windows): the two flags govern different phases and
-- mixing them up is a real bug, not just a naming slip (the demo's
-- "ground" heuristic once relied on getting exactly this distinction
-- right — see docs/ for the Flowers/MoreFlowers ground-row fix).
newtype OutputPeriodic = OutputPeriodic Boolean

derive newtype instance eqOutputPeriodic :: Eq OutputPeriodic
derive newtype instance showOutputPeriodic :: Show OutputPeriodic

-- Neighbour of pos in direction dir; Nothing when out-of-bounds (non-periodic)
neighborPos :: GridSize -> OutputPeriodic -> Pos -> Direction -> Maybe Pos
neighborPos { width, height } (OutputPeriodic periodic) (Pos { x, y }) dir =
  let { dx, dy } = dirOffset dir
      nx = x + dx
      ny = y + dy
  in if periodic
       then Just (Pos { x: nx `mod` width, y: ny `mod` height })
       else if nx >= 0 && nx < width && ny >= 0 && ny < height
              then Just (Pos { x: nx, y: ny })
              else Nothing

newtype GridWidth = GridWidth Int
newtype GridHeight = GridHeight Int

gridWidth :: forall a. Array (Array a) -> GridWidth
gridWidth grid = GridWidth (case Array.head grid of
  Nothing  -> 0
  Just row -> Array.length row)

gridHeight :: forall a. Array (Array a) -> GridHeight
gridHeight = GridHeight <<< Array.length
