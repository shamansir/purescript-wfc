module WFC.Direction where

import Prelude

data Direction = DirL | DirD | DirR | DirU

derive instance eqDirection  :: Eq Direction
derive instance ordDirection :: Ord Direction

instance showDirection :: Show Direction where
  show DirL = "L"
  show DirD = "D"
  show DirR = "R"
  show DirU = "U"

allDirections :: Array Direction
allDirections = [ DirL, DirD, DirR, DirU ]

-- Dense 0..3 ordinal matching `allDirections`'s order — used to fold a
-- Direction into a combined integer index (see WFC.Wave.compatibilityKey)
-- instead of comparing/hashing the constructor itself.
newtype DirectionIndex = DirectionIndex Int

derive newtype instance eqDirectionIndex :: Eq DirectionIndex
derive newtype instance showDirectionIndex :: Show DirectionIndex
derive newtype instance ordDirectionIndex :: Ord DirectionIndex

dirIndex :: Direction -> DirectionIndex
dirIndex DirL = DirectionIndex 0
dirIndex DirD = DirectionIndex 1
dirIndex DirR = DirectionIndex 2
dirIndex DirU = DirectionIndex 3

dirOffset :: Direction -> { dx :: Int, dy :: Int }
dirOffset DirL = { dx: -1, dy:  0 }
dirOffset DirD = { dx:  0, dy:  1 }
dirOffset DirR = { dx:  1, dy:  0 }
dirOffset DirU = { dx:  0, dy: -1 }

opposite :: Direction -> Direction
opposite DirL = DirR
opposite DirR = DirL
opposite DirU = DirD
opposite DirD = DirU
