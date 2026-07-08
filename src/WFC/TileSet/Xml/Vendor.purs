-- Vendored from https://github.com/m-bock/purescript-xml-parser
-- (commit 62e31fbf37c7dc9d064fdec79d73749dfb720b6b, `src/XmlParser.purs`,
-- MIT licensed), not published to Pursuit/the package registry so it can't
-- be added as a normal dependency. Copied in with attribution rather than
-- pinned as a `git` spago dependency: it has no `spago.yaml`/tagged release
-- of its own (only a legacy `spago.dhall`), and it's a single small module.
--
-- Trimmed from the original: the `Show`/`Generic` instances on `XmlNode`/
-- `XmlAttribute` (and their `generics-rep` dependency) are dropped since
-- nothing here needs to print the AST — everything else, including
-- identifiers and structure, is unchanged.
module WFC.TileSet.Xml.Vendor
  ( Element
  , XmlAttribute(..)
  , XmlDocument
  , XmlMeta
  , XmlNode(..)
  , parseXmlDocument
  , parseXmlNode
  , parseXmlNodes
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array as Array
import Data.Either (Either)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import StringParser (ParseError, Parser, anyChar, optionMaybe, regex, runParser, skipSpaces, string, try, whiteSpace)
import StringParser.Combinators (many, manyTill, option, optional, sepEndBy)

type XmlDocument =
  { meta :: XmlMeta
  , commentsBeforeRoot :: List String
  , root :: XmlNode
  , commentsAfterRoot :: List String
  }

type XmlMeta =
  { version :: Maybe String
  , encoding :: Maybe String
  , standalone :: Maybe String
  }

data XmlNode
  = XmlElement Element
  | XmlText String
  | XmlComment String

type Element =
  { name :: String
  , attributes :: List XmlAttribute
  , children :: List XmlNode
  }

data XmlAttribute = XmlAttribute String String

charListToString :: List Char -> String
charListToString = fromCharArray <<< Array.fromFoldable

attributeParser :: Parser XmlAttribute
attributeParser = do
  k :: String <- regex "[^=>/]+"
  v :: String <- option "" (string "=\"" *> regex "[^\"]*" <* string "\"")
  pure $ XmlAttribute k v

openingParser :: Parser Element
openingParser = do
  _ <- string "<"

  name :: String <- regex "[^/> ]+"

  attributes :: List XmlAttribute <-
    whiteSpace *> sepEndBy attributeParser whiteSpace

  pure
    { name
    , attributes
    , children: List.Nil
    }

closingOrChildrenParser :: Element -> Parser Element
closingOrChildrenParser element = defer \_ ->
  try childrenParser <|> try closingParser
  where
  closingParser :: Parser Element
  closingParser = whiteSpace *> optional (string "/") *> string ">" *> pure element

  childrenParser :: Parser Element
  childrenParser = do
    _ <- whiteSpace *> string ">"
    children :: List XmlNode <- manyTill nodeParser
      (string ("</" <> element.name <> ">"))
    pure $
      element { children = children }

elementParser :: Parser XmlNode
elementParser = defer \_ -> do
  skipSpaces
  openingParser
    >>= closingOrChildrenParser
    >>=
      pure <<< XmlElement

textParser :: Parser XmlNode
textParser = XmlText <$> regex "[^<]+"

commentParser :: Parser String
commentParser = do
  comment :: List Char <- string "<!--" *> manyTill anyChar (string "-->")
  skipSpaces
  pure $ charListToString comment

nodeParser :: Parser XmlNode
nodeParser = defer \_ ->
  try textParser
    <|> try (XmlComment <$> commentParser)
    <|>
      elementParser

metaParser :: Parser XmlMeta
metaParser = do
  _ <- string "<?xml"
  version :: Maybe String <- whiteSpace *> optionMaybe (string "version=\"" *> regex "[^\"]*" <* string "\"")
  encoding :: Maybe String <- whiteSpace *> optionMaybe (string "encoding=\"" *> regex "[^\"]*" <* string "\"")
  standalone :: Maybe String <- whiteSpace *> optionMaybe (whiteSpace *> string "standalone=\"" *> regex "[^\"]*" <* string "\"")
  _ <- whiteSpace *> string "?>"
  pure $ { version, encoding, standalone }

defaultMeta :: XmlMeta
defaultMeta =
  { version: Nothing
  , encoding: Nothing
  , standalone: Nothing
  }

documentParser :: Parser XmlDocument
documentParser = do
  skipSpaces
  meta :: Maybe XmlMeta <- optionMaybe metaParser

  commentsBeforeRoot :: List String <- whiteSpace *> many commentParser

  root :: XmlNode <- elementParser

  commentsAfterRoot :: List String <- whiteSpace *> many commentParser

  pure $
    { meta: fromMaybe defaultMeta meta
    , commentsBeforeRoot
    , root
    , commentsAfterRoot
    }

parseXmlNodes :: String -> Either ParseError (List XmlNode)
parseXmlNodes input =
  runParser (many nodeParser) input

parseXmlNode :: String -> Either ParseError XmlNode
parseXmlNode input =
  runParser nodeParser input

parseXmlDocument :: String -> Either ParseError XmlDocument
parseXmlDocument input =
  runParser documentParser input
