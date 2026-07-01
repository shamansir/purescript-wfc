module Demo.Samples where

import Prelude

type SampleDef =
  { name     :: String
  , grid     :: Array (Array Int)
  , palette  :: Int -> String
  , outW     :: Int
  , outH     :: Int
  , n        :: Int
  , periodic :: Boolean
  }

checkerboard :: SampleDef
checkerboard =
  { name: "Checkerboard"
  , grid:
      [ [0, 1, 0, 1, 0, 1]
      , [1, 0, 1, 0, 1, 0]
      , [0, 1, 0, 1, 0, 1]
      , [1, 0, 1, 0, 1, 0]
      , [0, 1, 0, 1, 0, 1]
      , [1, 0, 1, 0, 1, 0]
      ]
  , palette: \v -> if v == 0 then "#f0f0f0" else "#222222"
  , outW: 16
  , outH: 16
  , n: 2
  , periodic: true
  }

rooms :: SampleDef
rooms =
  { name: "Rooms"
  , grid:
      [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [0, 1, 1, 1, 0, 1, 1, 1, 1, 0]
      , [0, 1, 1, 1, 0, 1, 1, 1, 1, 0]
      , [0, 1, 1, 1, 0, 1, 1, 1, 1, 0]
      , [0, 0, 0, 1, 0, 0, 0, 0, 0, 0]
      , [0, 1, 0, 1, 1, 1, 1, 1, 1, 0]
      , [0, 1, 0, 0, 0, 0, 0, 1, 1, 0]
      , [0, 1, 1, 1, 1, 1, 0, 1, 1, 0]
      , [0, 1, 1, 1, 1, 1, 0, 1, 1, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      ]
  , palette: \v -> if v == 0 then "#1a1a2e" else "#e0e0ff"
  , outW: 20
  , outH: 20
  , n: 3
  , periodic: false
  }

knot :: SampleDef
knot =
  { name: "Knot"
  , grid:
      [ [0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [0, 1, 1, 1, 0, 1, 1, 1, 0]
      , [0, 1, 0, 1, 0, 1, 0, 1, 0]
      , [0, 1, 0, 1, 1, 1, 0, 1, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [0, 1, 0, 1, 1, 1, 0, 1, 0]
      , [0, 1, 0, 1, 0, 1, 0, 1, 0]
      , [0, 1, 1, 1, 0, 1, 1, 1, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, 0]
      ]
  , palette: \v -> if v == 0 then "#f8f4e8" else "#5c3d11"
  , outW: 20
  , outH: 20
  , n: 3
  , periodic: true
  }

circuit :: SampleDef
circuit =
  { name: "Circuit"
  , grid:
      [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [0, 1, 1, 2, 0, 0, 2, 1, 1, 0]
      , [0, 1, 0, 2, 2, 2, 2, 0, 1, 0]
      , [0, 2, 2, 0, 0, 0, 0, 2, 2, 0]
      , [0, 0, 2, 0, 1, 1, 0, 2, 0, 0]
      , [0, 0, 2, 0, 1, 1, 0, 2, 0, 0]
      , [0, 2, 2, 0, 0, 0, 0, 2, 2, 0]
      , [0, 1, 0, 2, 2, 2, 2, 0, 1, 0]
      , [0, 1, 1, 2, 0, 0, 2, 1, 1, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      ]
  , palette: \v ->
      if v == 0 then "#0d1117"
      else if v == 1 then "#238636"
      else "#f0c000"
  , outW: 20
  , outH: 20
  , n: 3
  , periodic: false
  }

samples :: Array SampleDef
samples = [ checkerboard, rooms, knot, circuit ]
