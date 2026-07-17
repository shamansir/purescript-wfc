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
  , ground   :: Boolean
  }

checkerboard :: SampleDef
checkerboard =
  { name: "Checkerboard (inline)"
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
  , ground: false
  }

greekPattern :: SampleDef
greekPattern =
  { name: "Greek pattern (inline)"
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
  , ground: false
  }

azulejo :: SampleDef
azulejo =
  { name: "Azulejo (inline)"
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
  , ground: false
  }

-- Patterns below inspired by the overlapping-model samples in
-- https://github.com/mxgmn/WaveFunctionCollapse (Maze, Cave, Village), hand-
-- authored as small int grids since this repo has no bitmap loader.

maze :: SampleDef
maze =
  { name: "Maze (inline)"
  , grid:
      [ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
      , [1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1]
      , [1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1]
      , [1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1]
      , [1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1]
      , [1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1]
      , [1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1]
      , [1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1]
      , [1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1]
      , [1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1]
      , [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
      ]
  , palette: \v -> if v == 0 then "#f8f4e8" else "#111111"
  , outW: 22
  , outH: 22
  , n: 3
  , periodic: false
  , ground: false
  }

theEye :: SampleDef
theEye =
  { name: "The Eye (inline)"
  , grid:
      [ [1, 1, 1, 1, 0, 0, 1, 1, 1, 1]
      , [1, 1, 0, 0, 0, 0, 0, 0, 1, 1]
      , [1, 0, 0, 1, 1, 1, 0, 0, 0, 1]
      , [0, 0, 1, 1, 1, 1, 1, 0, 0, 0]
      , [0, 1, 1, 1, 0, 1, 1, 1, 0, 0]
      , [0, 1, 1, 0, 0, 0, 1, 1, 0, 0]
      , [0, 0, 1, 1, 0, 1, 1, 0, 0, 0]
      , [1, 0, 0, 1, 1, 1, 0, 0, 1, 1]
      , [1, 1, 0, 0, 0, 0, 0, 1, 1, 1]
      , [1, 1, 1, 0, 0, 1, 1, 1, 1, 1]
      ]
  , palette: \v -> if v == 0 then "#c9b28a" else "#2b1d0e"
  , outW: 20
  , outH: 20
  , n: 3
  , periodic: true
  , ground: false
  }

village :: SampleDef
village =
  { name: "Village (inline)"
  , grid:
      [ [1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1]
      , [1, 2, 2, 1, 2, 0, 0, 1, 2, 2, 1, 1]
      , [1, 2, 2, 1, 2, 0, 0, 1, 2, 2, 1, 1]
      , [1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1]
      , [1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1]
      , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
      , [1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1]
      , [1, 2, 2, 1, 2, 0, 0, 1, 2, 2, 1, 1]
      , [1, 2, 2, 1, 2, 0, 0, 1, 2, 2, 1, 1]
      , [1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1]
      , [1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1]
      ]
  , palette: \v ->
      if v == 0 then "#5c5248"
      else if v == 1 then "#4a7c3a"
      else "#8a5a2b"
  , outW: 24
  , outH: 24
  , n: 3
  , periodic: true
  , ground: false
  }

samples :: Array SampleDef
samples = [ checkerboard, greekPattern, azulejo, maze, theEye, village ]
