module Demo.TileSamples where

import Prelude

import WFC.Tiles (TileDef)

-- Hand-authored tile sets for the tiled model (WFC.Tiles) — the demo's
-- counterpart to Demo.Samples, which instead feeds a source image through
-- the overlapping model's extractPatterns/buildRules. There are no real
-- tile graphics in this project, so — like every other sample — each tile
-- just renders as one flat color; the solver still enforces the socket
-- adjacency correctly, it's just not pictured the way a real tile atlas
-- would show connecting road/pipe artwork.
type TileSampleDef =
  { name     :: String
  , tiles    :: Array (TileDef Int)
  , palette  :: Int -> String
  , outW     :: Int
  , outH     :: Int
  , periodic :: Boolean
  }

-- A classic road/pipe-style tile set: blank ground, straight segments, and
-- all four corner turns, using two socket labels ("0" = no connection,
-- "1" = connection) — the simplest possible Wang-tile adjacency.
roads :: TileSampleDef
roads =
  { name: "Roads (Tiled)"
  , tiles:
      [ { value: 0, weight: 6.0, sockets: { left: "0", right: "0", up: "0", down: "0" } } -- blank
      , { value: 1, weight: 3.0, sockets: { left: "1", right: "1", up: "0", down: "0" } } -- horizontal
      , { value: 2, weight: 3.0, sockets: { left: "0", right: "0", up: "1", down: "1" } } -- vertical
      , { value: 3, weight: 1.5, sockets: { left: "0", right: "1", up: "0", down: "1" } } -- corner: right+down
      , { value: 4, weight: 1.5, sockets: { left: "1", right: "0", up: "0", down: "1" } } -- corner: left+down
      , { value: 5, weight: 1.5, sockets: { left: "0", right: "1", up: "1", down: "0" } } -- corner: right+up
      , { value: 6, weight: 1.5, sockets: { left: "1", right: "0", up: "1", down: "0" } } -- corner: left+up
      ]
  , palette: \v ->
      if v == 0 then "#0d1117"
      else if v == 1 || v == 2 then "#3fb950"
      else "#f0c000"
  , outW: 24
  , outH: 24
  , periodic: true
  }

samples :: Array TileSampleDef
samples = [ roads ]
