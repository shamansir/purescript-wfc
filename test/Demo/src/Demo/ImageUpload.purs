module Demo.ImageUpload
  ( LoadedImage
  , loadImageAsSample
  , gridFromPixels
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect.Aff (Aff, makeAff, nonCanceler)
import Graphics.Canvas (CanvasImageSource, ImageData)
import Graphics.Canvas as Canvas
import Web.File.File (File)
import Web.File.File as File
import Web.File.Url as Url

-- The installed `canvas` package's `CanvasImageSource`/`ImageData` are just
-- the underlying `Image`/`ImageData` DOM objects unsafe-coerced to opaque
-- types, so a couple of one-line FFI reads get at properties the PureScript
-- API doesn't expose (naturalWidth/Height, and the raw RGBA byte buffer).
foreign import naturalWidth :: CanvasImageSource -> Int
foreign import naturalHeight :: CanvasImageSource -> Int
foreign import imageDataToArray :: ImageData -> Array Int

type LoadedImage =
  { grid   :: Array (Array Int)
  , colors :: Array String
  , width  :: Int
  , height :: Int
  , name   :: String
  }

-- Two-digit lowercase hex, zero-padded.
hex2 :: Int -> String
hex2 n =
  let s = Int.toStringAs Int.hexadecimal (n `mod` 256)
  in if String.length s < 2 then "0" <> s else s

type Assign = { seen :: Map String Int, palette :: Array String, ids :: Array Int }

assignId :: Assign -> String -> Assign
assignId acc color = case Map.lookup color acc.seen of
  Just idx -> acc { ids = Array.snoc acc.ids idx }
  Nothing  ->
    let idx = Array.length acc.palette
    in acc
      { seen    = Map.insert color idx acc.seen
      , palette = Array.snoc acc.palette color
      , ids     = Array.snoc acc.ids idx
      }

-- Flat RGBA bytes (row-major, as laid out by ImageData) -> a palette-indexed
-- grid plus the first-seen-order hex palette backing it. Alpha is ignored;
-- fully transparent and fully opaque pixels of the same RGB collapse to one
-- color, which is the simplest sane behavior for a WFC pattern source.
gridFromPixels :: Int -> Int -> Array Int -> { grid :: Array (Array Int), colors :: Array String }
gridFromPixels w h bytes =
  let byteAt i = fromMaybe 0 (Array.index bytes i)
      pixelHex i =
        let base = i * 4
        in "#" <> hex2 (byteAt base) <> hex2 (byteAt (base + 1)) <> hex2 (byteAt (base + 2))
      hexList  = map pixelHex (Array.range 0 (w * h - 1))
      built    = Array.foldl assignId { seen: Map.empty, palette: [], ids: [] } hexList
      rowOf y  = Array.slice (y * w) (y * w + w) built.ids
      grid     = map rowOf (Array.range 0 (h - 1))
  in { grid, colors: built.palette }

-- Decode an uploaded file into a pattern-source grid. Always resolves with
-- an `Either` — a bad/oversized file is data (`Left`), not a thrown Aff
-- error, so callers just pattern-match instead of handling two failure
-- channels.
loadImageAsSample :: Int -> File -> Aff (Either String LoadedImage)
loadImageAsSample maxSide file = makeAff \respond -> do
  url <- Url.createObjectURL (File.toBlob file)
  Canvas.tryLoadImage url case _ of
    Nothing -> respond (Right (Left "Could not decode image file"))
    Just imgSrc -> do
      let w = naturalWidth imgSrc
          h = naturalHeight imgSrc
      if w <= 0 || h <= 0 then
        respond (Right (Left "Image appears to be empty"))
      else if w > maxSide || h > maxSide then
        respond (Right (Left
          ("Image is " <> show w <> "×" <> show h
            <> " — max allowed is " <> show maxSide <> "×" <> show maxSide)))
      else do
        mCanvas <- Canvas.getCanvasElementById "upload-canvas"
        case mCanvas of
          Nothing -> respond (Right (Left "Internal error: upload canvas not found"))
          Just canvasEl -> do
            Canvas.setCanvasDimensions canvasEl { width: Int.toNumber w, height: Int.toNumber h }
            ctx <- Canvas.getContext2D canvasEl
            Canvas.clearRect ctx { x: 0.0, y: 0.0, width: Int.toNumber w, height: Int.toNumber h }
            Canvas.drawImage ctx imgSrc 0.0 0.0
            imageData <- Canvas.getImageData ctx 0.0 0.0 (Int.toNumber w) (Int.toNumber h)
            let bytes = imageDataToArray imageData
                built = gridFromPixels w h bytes
            Url.revokeObjectURL url
            respond (Right (Right
              { grid: built.grid, colors: built.colors, width: w, height: h, name: File.name file }))
  pure nonCanceler
