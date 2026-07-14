module WFC.TileSet.Xml
  ( AttrName(..)
  , AttrValue(..)
  , TagName(..)
  , XmlSource(..)
  , XmlParseError(..)
  , parseTileSetXml
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
import WFC.TileSet (NeighborRule, Subset, TileDef, TileSetDef, Unique(..))
import WFC.TileSet.Symmetry (parseSymmetry)
import WFC.TileSet.Xml.Vendor (Element, XmlAttribute(..), XmlNode(..), parseXmlDocument)

-- An XML attribute's name (the lookup key, e.g. "name"/"symmetry"/"weight")
-- — distinct from `AttrValue` (what's stored under that key), and from the
-- plain `String` fields `TileDef`/`NeighborRule`/`Subset` themselves store
-- once parsed (already labeled there, see those records).
newtype AttrName = AttrName String

-- An XML attribute's raw text value, before whatever it's parsed into
-- (a `Symmetry`, a `Number` weight, a `Unique` flag, ...).
newtype AttrValue = AttrValue String

-- An XML element's tag name (e.g. "tile"/"neighbor"/"subset"), for matching
-- child elements — distinct from `AttrName` (an attribute, not a tag).
newtype TagName = TagName String

-- The raw `<set>...</set>` document text handed to `parseTileSetXml`.
newtype XmlSource = XmlSource String

-- A human-readable parse failure — distinct from `WFC.TileSet.Xml.Vendor`'s
-- own `ParseError` (that one's the `string-parsers` library's own type, for
-- the underlying generic-XML parse; this one's for the *domain* validation
-- on top: missing attributes, unresolvable tile references, and so on).
newtype XmlParseError = XmlParseError String

attrValue :: AttrName -> Element -> Maybe AttrValue
attrValue (AttrName key) el = Array.findMap matching (List.toUnfoldable el.attributes :: Array XmlAttribute)
  where
  matching (XmlAttribute k v) = if k == key then Just (AttrValue v) else Nothing

-- Direct children of `el` that are `<tag>` elements, in document order.
childElements :: TagName -> Element -> Array Element
childElements (TagName tag) el = Array.mapMaybe matching (List.toUnfoldable el.children :: Array XmlNode)
  where
  matching (XmlElement e) | e.name == tag = Just e
  matching _ = Nothing

-- "corner 1" -> { name: "corner", rotation: 1 }; "corner" (or anything
-- without a valid trailing integer) -> { name: "corner", rotation: 0 }.
splitNameRot :: AttrValue -> { name :: String, rotation :: Int }
splitNameRot (AttrValue s) = case String.lastIndexOf (String.Pattern " ") s of
  Nothing -> { name: s, rotation: 0 }
  Just i ->
    let namePart = String.take i s
        rotPart = String.drop (i + 1) s
    in case Int.fromString rotPart of
      Just r -> { name: namePart, rotation: r }
      Nothing -> { name: s, rotation: 0 }

parseBool :: AttrValue -> Unique
parseBool (AttrValue s) = Unique (String.toLower s == "true")

tileFromElement :: Element -> Either XmlParseError TileDef
tileFromElement el = do
  AttrValue name <- note (XmlParseError "<tile> missing name") (attrValue (AttrName "name") el)
  AttrValue symStr <- note (XmlParseError ("<tile name=\"" <> name <> "\"> missing symmetry")) (attrValue (AttrName "symmetry") el)
  symmetry <- lmap XmlParseError (parseSymmetry symStr)
  let weight = fromMaybe 1.0 (attrValue (AttrName "weight") el >>= (\(AttrValue v) -> Number.fromString v))
  pure { name, symmetry, weight }

neighborFromElement :: Element -> Either XmlParseError NeighborRule
neighborFromElement el = do
  leftStr <- note (XmlParseError "<neighbor> missing left") (attrValue (AttrName "left") el)
  rightStr <- note (XmlParseError "<neighbor> missing right") (attrValue (AttrName "right") el)
  let l = splitNameRot leftStr
      r = splitNameRot rightStr
  pure { leftName: l.name, leftRot: l.rotation, rightName: r.name, rightRot: r.rotation }

subsetFromElement :: Element -> Either XmlParseError Subset
subsetFromElement el = do
  AttrValue name <- note (XmlParseError "<subset> missing name") (attrValue (AttrName "name") el)
  let tileNames = Array.mapMaybe (\e -> (\(AttrValue v) -> v) <$> attrValue (AttrName "name") e) (childElements (TagName "tile") el)
  pure { name, tiles: tileNames }

-- Parse a whole `<set>...</set>` tileset document (the original WFC
-- algorithm's XML tileset format) into a `TileSetDef`.
parseTileSetXml :: XmlSource -> Either XmlParseError TileSetDef
parseTileSetXml (XmlSource input) = do
  doc <- lmap (XmlParseError <<< show) (parseXmlDocument input)
  case doc.root of
    XmlElement setEl | setEl.name == "set" -> do
      let Unique unique = fromMaybe (Unique false) (parseBool <$> attrValue (AttrName "unique") setEl)
      tilesEl <- note (XmlParseError "<set> missing <tiles>") (Array.head (childElements (TagName "tiles") setEl))
      neighborsEl <- note (XmlParseError "<set> missing <neighbors>") (Array.head (childElements (TagName "neighbors") setEl))
      tiles <- traverse tileFromElement (childElements (TagName "tile") tilesEl)
      neighbors <- traverse neighborFromElement (childElements (TagName "neighbor") neighborsEl)
      subsets <- case Array.head (childElements (TagName "subsets") setEl) of
        Nothing -> pure []
        Just subsetsEl -> traverse subsetFromElement (childElements (TagName "subset") subsetsEl)
      pure { unique, tiles, neighbors, subsets }
    XmlElement other -> Left (XmlParseError ("root element is <" <> other.name <> ">, expected <set>"))
    _ -> Left (XmlParseError "document has no root element")
