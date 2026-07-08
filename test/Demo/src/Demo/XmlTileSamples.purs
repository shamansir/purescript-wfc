module Demo.XmlTileSamples where

-- The original WFC repo's XML tileset format (tiles with a symmetry class
-- + explicit neighbor rules, see `WFC.TileSet`/`WFC.TileSet.Xml`), dropped
-- into `test/Demo/tilesets/` and served as static assets alongside the
-- demo. Unlike `Demo.TileSamples`' hand-authored sockets, these are fetched
-- and parsed on demand (`Demo.Fetch.fetchText` + `WFC.TileSet.Xml.parseTileSetXml`)
-- the same way an "(image)" sample is loaded — see `Demo.App`'s
-- `SelectSample` handler.
type XmlTileSampleDef =
  { name    :: String -- always suffixed " (Tileset)" to mark it as this kind of sample in the dropdown
  , xmlPath :: String -- relative to test/Demo/index.html
  , tileDir :: String -- directory holding this set's per-tile PNGs, same base name as xmlPath
  }

xmlTileSamples :: Array XmlTileSampleDef
xmlTileSamples =
  [ { name: "Castle (Tileset)", xmlPath: "tilesets/Castle.xml", tileDir: "tilesets/Castle" }
  , { name: "Circles (Tileset)", xmlPath: "tilesets/Circles.xml", tileDir: "tilesets/Circles" }
  , { name: "Circuit (Tileset)", xmlPath: "tilesets/Circuit.xml", tileDir: "tilesets/Circuit" }
  , { name: "FloorPlan (Tileset)", xmlPath: "tilesets/FloorPlan.xml", tileDir: "tilesets/FloorPlan" }
  , { name: "Knots (Tileset)", xmlPath: "tilesets/Knots.xml", tileDir: "tilesets/Knots" }
  , { name: "Rooms (Tileset)", xmlPath: "tilesets/Rooms.xml", tileDir: "tilesets/Rooms" }
  , { name: "Summer (Tileset)", xmlPath: "tilesets/Summer.xml", tileDir: "tilesets/Summer" }
  ]
