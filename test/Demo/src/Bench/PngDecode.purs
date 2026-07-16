module Bench.PngDecode (decodePngFile) where

import Effect (Effect)

-- Reads a PNG straight off disk into flat RGBA bytes, the same shape
-- `Demo.ImageUpload.gridFromPixels` already turns into a pattern-source
-- grid. Uses the pure-JS `pngjs` package rather than `Demo.ImageUpload`'s
-- browser `<canvas>` decode path (`Graphics.Canvas`/DOM `Image`) — a CLI
-- benchmark has no DOM to decode through, and `pngjs` needs no native
-- (cairo/pango) build step, unlike a Node `canvas` polyfill would.
foreign import decodePngFile :: String -> Effect { width :: Int, height :: Int, bytes :: Array Int }
