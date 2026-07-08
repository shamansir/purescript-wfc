module Demo.ImageSamples where

-- Reference images from the original (C#) Wave Function Collapse repository,
-- dropped into `test/Demo/samples/` and served as static assets alongside
-- the demo. Unlike `Demo.Samples`' hand-authored grids, these are loaded
-- on demand (`Demo.ImageUpload.loadImageFromUrl`) the same way an uploaded
-- image is — decoded into a `CustomImage` and fed through the same
-- pattern-size/result-size/rotate/mirror controls, not a separate pipeline.
type ImageSampleDef =
  { name :: String -- always suffixed " (image)" to mark it as this kind of sample in the dropdown
  , path :: String -- relative to test/Demo/index.html
  }

imageSamples :: Array ImageSampleDef
imageSamples =
  [ { name: "3Bricks (image)", path: "samples/3Bricks.png" }
  , { name: "Angular (image)", path: "samples/Angular.png" }
  , { name: "BrownFox (image)", path: "samples/BrownFox.png" }
  , { name: "Cat (image)", path: "samples/Cat.png" }
  , { name: "Cats (image)", path: "samples/Cats.png" }
  , { name: "Cave (image)", path: "samples/Cave.png" }
  , { name: "Chess (image)", path: "samples/Chess.png" }
  , { name: "Circle (image)", path: "samples/Circle.png" }
  , { name: "City (image)", path: "samples/City.png" }
  , { name: "ColoredCity (image)", path: "samples/ColoredCity.png" }
  , { name: "Disk (image)", path: "samples/Disk.png" }
  , { name: "Dungeon (image)", path: "samples/Dungeon.png" }
  , { name: "Fabric (image)", path: "samples/Fabric.png" }
  , { name: "Flowers (image)", path: "samples/Flowers.png" }
  , { name: "Font (image)", path: "samples/Font.png" }
  , { name: "Forest (image)", path: "samples/Forest.png" }
  , { name: "Hogs (image)", path: "samples/Hogs.png" }
  , { name: "Knot (image)", path: "samples/Knot.png" }
  , { name: "Lake (image)", path: "samples/Lake.png" }
  , { name: "LessRooms (image)", path: "samples/LessRooms.png" }
  , { name: "Lines (image)", path: "samples/Lines.png" }
  , { name: "Link (image)", path: "samples/Link.png" }
  , { name: "Link2 (image)", path: "samples/Link2.png" }
  , { name: "MagicOffice (image)", path: "samples/MagicOffice.png" }
  , { name: "Maze (image)", path: "samples/Maze.png" }
  , { name: "Mazelike (image)", path: "samples/Mazelike.png" }
  , { name: "MoreFlowers (image)", path: "samples/MoreFlowers.png" }
  , { name: "Mountains (image)", path: "samples/Mountains.png" }
  , { name: "Nested (image)", path: "samples/Nested.png" }
  , { name: "NotKnot (image)", path: "samples/NotKnot.png" }
  , { name: "Office (image)", path: "samples/Office.png" }
  , { name: "Office2 (image)", path: "samples/Office2.png" }
  , { name: "Paths (image)", path: "samples/Paths.png" }
  , { name: "Platformer (image)", path: "samples/Platformer.png" }
  , { name: "Qud (image)", path: "samples/Qud.png" }
  , { name: "RedDot (image)", path: "samples/RedDot.png" }
  , { name: "RedMaze (image)", path: "samples/RedMaze.png" }
  , { name: "Rooms (image)", path: "samples/Rooms.png" }
  , { name: "Rule126 (image)", path: "samples/Rule126.png" }
  , { name: "Sand (image)", path: "samples/Sand.png" }
  , { name: "ScaledMaze (image)", path: "samples/ScaledMaze.png" }
  , { name: "Sewers (image)", path: "samples/Sewers.png" }
  , { name: "SimpleKnot (image)", path: "samples/SimpleKnot.png" }
  , { name: "SimpleMaze (image)", path: "samples/SimpleMaze.png" }
  , { name: "SimpleWall (image)", path: "samples/SimpleWall.png" }
  , { name: "Skew1 (image)", path: "samples/Skew1.png" }
  , { name: "Skew2 (image)", path: "samples/Skew2.png" }
  , { name: "Skyline (image)", path: "samples/Skyline.png" }
  , { name: "Skyline2 (image)", path: "samples/Skyline2.png" }
  , { name: "SmileCity (image)", path: "samples/SmileCity.png" }
  , { name: "Spirals (image)", path: "samples/Spirals.png" }
  , { name: "Town (image)", path: "samples/Town.png" }
  , { name: "TrickKnot (image)", path: "samples/TrickKnot.png" }
  , { name: "Village (image)", path: "samples/Village.png" }
  , { name: "Wall (image)", path: "samples/Wall.png" }
  , { name: "WalledDot (image)", path: "samples/WalledDot.png" }
  , { name: "Water (image)", path: "samples/Water.png" }
  , { name: "Wrinkles (image)", path: "samples/Wrinkles.png" }
  ]
