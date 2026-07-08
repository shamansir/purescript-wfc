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
  }

xmlTileSamples :: Array XmlTileSampleDef
xmlTileSamples =
  [ { name: "Castle (Tileset)", xmlPath: "tilesets/Castle.xml" }
  , { name: "Circles (Tileset)", xmlPath: "tilesets/Circles.xml" }
  , { name: "Circuit (Tileset)", xmlPath: "tilesets/Circuit.xml" }
  , { name: "FloorPlan (Tileset)", xmlPath: "tilesets/FloorPlan.xml" }
  , { name: "Knots (Tileset)", xmlPath: "tilesets/Knots.xml" }
  , { name: "Rooms (Tileset)", xmlPath: "tilesets/Rooms.xml" }
  , { name: "Summer (Tileset)", xmlPath: "tilesets/Summer.xml" }
  ]
