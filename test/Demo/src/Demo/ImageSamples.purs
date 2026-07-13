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
  , ground :: Boolean -- true for samples with a solid "ground" row along the bottom edge (see WFC.Propagate.applyGround)
  }

imageSamples :: Array ImageSampleDef
imageSamples =
  [ { name: "3Bricks (image)", path: "samples/3Bricks.png", ground: false }
  , { name: "Angular (image)", path: "samples/Angular.png", ground: false }
  , { name: "BrownFox (image)", path: "samples/BrownFox.png", ground: false }
  , { name: "Cat (image)", path: "samples/Cat.png", ground: false }
  , { name: "Cats (image)", path: "samples/Cats.png", ground: false }
  , { name: "Cave (image)", path: "samples/Cave.png", ground: false }
  , { name: "Chess (image)", path: "samples/Chess.png", ground: false }
  , { name: "Circle (image)", path: "samples/Circle.png", ground: false }
  , { name: "City (image)", path: "samples/City.png", ground: false }
  , { name: "ColoredCity (image)", path: "samples/ColoredCity.png", ground: false }
  , { name: "Disk (image)", path: "samples/Disk.png", ground: false }
  , { name: "Dungeon (image)", path: "samples/Dungeon.png", ground: false }
  , { name: "Fabric (image)", path: "samples/Fabric.png", ground: false }
  , { name: "Flowers (image)", path: "samples/Flowers.png", ground: true }
  , { name: "Font (image)", path: "samples/Font.png", ground: false }
  , { name: "Forest (image)", path: "samples/Forest.png", ground: false }
  , { name: "Hogs (image)", path: "samples/Hogs.png", ground: false }
  , { name: "Knot (image)", path: "samples/Knot.png", ground: false }
  , { name: "Lake (image)", path: "samples/Lake.png", ground: false }
  , { name: "LessRooms (image)", path: "samples/LessRooms.png", ground: false }
  , { name: "Lines (image)", path: "samples/Lines.png", ground: false }
  , { name: "Link (image)", path: "samples/Link.png", ground: false }
  , { name: "Link2 (image)", path: "samples/Link2.png", ground: false }
  , { name: "MagicOffice (image)", path: "samples/MagicOffice.png", ground: false }
  , { name: "Maze (image)", path: "samples/Maze.png", ground: false }
  , { name: "Mazelike (image)", path: "samples/Mazelike.png", ground: false }
  , { name: "MoreFlowers (image)", path: "samples/MoreFlowers.png", ground: true }
  , { name: "Mountains (image)", path: "samples/Mountains.png", ground: false }
  , { name: "Nested (image)", path: "samples/Nested.png", ground: false }
  , { name: "NotKnot (image)", path: "samples/NotKnot.png", ground: false }
  , { name: "Office (image)", path: "samples/Office.png", ground: false }
  , { name: "Office2 (image)", path: "samples/Office2.png", ground: false }
  , { name: "Paths (image)", path: "samples/Paths.png", ground: false }
  , { name: "Platformer (image)", path: "samples/Platformer.png", ground: true }
  , { name: "Qud (image)", path: "samples/Qud.png", ground: false }
  , { name: "RedDot (image)", path: "samples/RedDot.png", ground: false }
  , { name: "RedMaze (image)", path: "samples/RedMaze.png", ground: false }
  , { name: "Rooms (image)", path: "samples/Rooms.png", ground: false }
  , { name: "Rule126 (image)", path: "samples/Rule126.png", ground: false }
  , { name: "Sand (image)", path: "samples/Sand.png", ground: false }
  , { name: "ScaledMaze (image)", path: "samples/ScaledMaze.png", ground: false }
  , { name: "Sewers (image)", path: "samples/Sewers.png", ground: false }
  , { name: "SimpleKnot (image)", path: "samples/SimpleKnot.png", ground: false }
  , { name: "SimpleMaze (image)", path: "samples/SimpleMaze.png", ground: false }
  , { name: "SimpleWall (image)", path: "samples/SimpleWall.png", ground: false }
  , { name: "Skew1 (image)", path: "samples/Skew1.png", ground: false }
  , { name: "Skew2 (image)", path: "samples/Skew2.png", ground: false }
  , { name: "Skyline (image)", path: "samples/Skyline.png", ground: true }
  , { name: "Skyline2 (image)", path: "samples/Skyline2.png", ground: true }
  , { name: "SmileCity (image)", path: "samples/SmileCity.png", ground: false }
  , { name: "Spirals (image)", path: "samples/Spirals.png", ground: false }
  , { name: "Town (image)", path: "samples/Town.png", ground: false }
  , { name: "TrickKnot (image)", path: "samples/TrickKnot.png", ground: false }
  , { name: "Village (image)", path: "samples/Village.png", ground: false }
  , { name: "Wall (image)", path: "samples/Wall.png", ground: false }
  , { name: "WalledDot (image)", path: "samples/WalledDot.png", ground: false }
  , { name: "Water (image)", path: "samples/Water.png", ground: false }
  , { name: "Wrinkles (image)", path: "samples/Wrinkles.png", ground: false }
  ]
