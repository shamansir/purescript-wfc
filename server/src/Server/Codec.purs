module Server.Codec (decodeCreateRequest, errorJson) where

import Prelude

import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Combinators ((.!=), (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Foreign.Object (Object)
import Server.Engine (CreateRequest, InputSpec(..), MatrixSpec, TileSpec)

-- Every field type below (`Array (Array Int)`, `TileSpec`'s own record,
-- `Int`/`Number`/`Boolean`/`String`) already has a generic `DecodeJson`
-- instance via argonaut-codecs' `Record`/`Array` machinery — only the
-- `mode`-tagged `matrix` vs `tiles` dispatch needs to be hand-written.
decodeInputSpec :: Object Json -> Either JsonDecodeError InputSpec
decodeInputSpec o = do
  mode <- o .: "mode"
  case mode :: String of
    "matrix" -> do
      matrix <- o .: "matrix"
      patternSize <- o .:? "patternSize" .!= 3
      inputPeriodic <- o .:? "inputPeriodic" .!= false
      useRotations <- o .:? "useRotations" .!= false
      useMirror <- o .:? "useMirror" .!= false
      let spec = { matrix, patternSize, inputPeriodic, useRotations, useMirror }
      validateMatrixSpec spec
      pure (MatrixInput spec)
    "tiles" -> do
      tiles <- o .: "tiles"
      validateTiles tiles
      pure (TilesInput tiles)
    other -> Left (TypeMismatch ("mode: expected \"matrix\" or \"tiles\", got " <> show other))

-- Guards the one input shape that's been observed to crash deep inside the
-- engine instead of failing cleanly: a `patternSize` that doesn't fit the
-- matrix at all (e.g. a non-periodic 2x2 matrix with the default
-- `patternSize: 3` — there's no 3x3 window in it, so `extractPatterns`
-- yields an empty catalog, and something downstream partial-matches on
-- "there's at least one pattern"). Since this is now a public HTTP
-- boundary, that has to come back as a 400, not crash the request.
matrixHeight :: MatrixSpec -> Int
matrixHeight m = Array.length m.matrix

matrixWidth :: MatrixSpec -> Int
matrixWidth m = fromMaybe 0 (Array.length <$> Array.head m.matrix)

validateMatrixSpec :: MatrixSpec -> Either JsonDecodeError Unit
validateMatrixSpec m
  | Array.null m.matrix = Left (TypeMismatch "matrix: must not be empty")
  | m.patternSize < 1 = Left (TypeMismatch "patternSize: must be >= 1")
  | not m.inputPeriodic && (m.patternSize > matrixHeight m || m.patternSize > matrixWidth m) =
      Left
        ( TypeMismatch
            ( "patternSize (" <> show m.patternSize <> ") does not fit a "
                <> show (matrixWidth m) <> "x" <> show (matrixHeight m) <> " matrix with inputPeriodic: false"
            )
        )
  | otherwise = Right unit

validateTiles :: Array TileSpec -> Either JsonDecodeError Unit
validateTiles tiles
  | Array.null tiles = Left (TypeMismatch "tiles: must not be empty")
  | otherwise = Right unit

decodeCreateRequest :: Json -> Either JsonDecodeError CreateRequest
decodeCreateRequest json = do
  o <- decodeJson json
  input <- decodeInputSpec o
  outputWidth <- o .:? "outputWidth" .!= 20
  outputHeight <- o .:? "outputHeight" .!= 20
  outputPeriodic <- o .:? "outputPeriodic" .!= false
  backtracking <- o .:? "backtracking" .!= false
  maxAttempts <- o .:? "maxAttempts" .!= 50
  keepHistory <- o .:? "keepHistory" .!= true
  maxHistory <- o .:? "maxHistory" .!= 200
  if outputWidth < 1 || outputHeight < 1 then
    Left (TypeMismatch "outputWidth/outputHeight: must be >= 1")
  else
    pure { input, outputWidth, outputHeight, outputPeriodic, backtracking, maxAttempts, keepHistory, maxHistory }

errorJson :: String -> Json
errorJson msg = "error" := msg ~> jsonEmptyObject
