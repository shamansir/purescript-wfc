module WFC.TileSet.Xml
  ( parseTileSetXml
  ) where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String as String
import Data.Traversable (traverse)
import WFC.TileSet (NeighborRule, Subset, TileDef, TileSetDef)
import WFC.TileSet.Symmetry (parseSymmetry)
import WFC.TileSet.Xml.Vendor (Element, XmlAttribute(..), XmlNode(..), parseXmlDocument)

attrValue :: String -> Element -> Maybe String
attrValue key el = Array.findMap matching (List.toUnfoldable el.attributes :: Array XmlAttribute)
  where
  matching (XmlAttribute k v) = if k == key then Just v else Nothing

-- Direct children of `el` that are `<tag>` elements, in document order.
childElements :: String -> Element -> Array Element
childElements tag el = Array.mapMaybe matching (List.toUnfoldable el.children :: Array XmlNode)
  where
  matching (XmlElement e) | e.name == tag = Just e
  matching _ = Nothing

-- "corner 1" -> { name: "corner", rotation: 1 }; "corner" (or anything
-- without a valid trailing integer) -> { name: "corner", rotation: 0 }.
splitNameRot :: String -> { name :: String, rotation :: Int }
splitNameRot s = case String.lastIndexOf (String.Pattern " ") s of
  Nothing -> { name: s, rotation: 0 }
  Just i ->
    let namePart = String.take i s
        rotPart = String.drop (i + 1) s
    in case Int.fromString rotPart of
      Just r -> { name: namePart, rotation: r }
      Nothing -> { name: s, rotation: 0 }

parseBool :: String -> Boolean
parseBool s = String.toLower s == "true"

tileFromElement :: Element -> Either String TileDef
tileFromElement el = do
  name <- note "<tile> missing name" (attrValue "name" el)
  symStr <- note ("<tile name=\"" <> name <> "\"> missing symmetry") (attrValue "symmetry" el)
  symmetry <- parseSymmetry symStr
  let weight = fromMaybe 1.0 (attrValue "weight" el >>= Number.fromString)
  pure { name, symmetry, weight }

neighborFromElement :: Element -> Either String NeighborRule
neighborFromElement el = do
  leftStr <- note "<neighbor> missing left" (attrValue "left" el)
  rightStr <- note "<neighbor> missing right" (attrValue "right" el)
  let l = splitNameRot leftStr
      r = splitNameRot rightStr
  pure { leftName: l.name, leftRot: l.rotation, rightName: r.name, rightRot: r.rotation }

subsetFromElement :: Element -> Either String Subset
subsetFromElement el = do
  name <- note "<subset> missing name" (attrValue "name" el)
  let tileNames = Array.mapMaybe (attrValue "name") (childElements "tile" el)
  pure { name, tiles: tileNames }

-- Parse a whole `<set>...</set>` tileset document (the original WFC
-- algorithm's XML tileset format) into a `TileSetDef`.
parseTileSetXml :: String -> Either String TileSetDef
parseTileSetXml input = do
  doc <- lmap show (parseXmlDocument input)
  case doc.root of
    XmlElement setEl | setEl.name == "set" -> do
      let unique = fromMaybe false (parseBool <$> attrValue "unique" setEl)
      tilesEl <- note "<set> missing <tiles>" (Array.head (childElements "tiles" setEl))
      neighborsEl <- note "<set> missing <neighbors>" (Array.head (childElements "neighbors" setEl))
      tiles <- traverse tileFromElement (childElements "tile" tilesEl)
      neighbors <- traverse neighborFromElement (childElements "neighbor" neighborsEl)
      subsets <- case Array.head (childElements "subsets" setEl) of
        Nothing -> pure []
        Just subsetsEl -> traverse subsetFromElement (childElements "subset" subsetsEl)
      pure { unique, tiles, neighbors, subsets }
    XmlElement other -> Left ("root element is <" <> other.name <> ">, expected <set>")
    _ -> Left "document has no root element"
