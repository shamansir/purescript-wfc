module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import WFC.Catalog (InputPeriodic(..), extractPatterns)
import WFC.Rules (buildRules)
import WFC.Wave (initWave)
import WFC.Algorithm (wfcWithRetry)
import WFC.Propagate (MaxAttempts(..))
import WFC.Grid (OutputPeriodic(..))
import WFC.Pattern (PatternSize(..), UseMirror(..), UseRotations(..))
import WFC.Render (renderWaveWith)
import Data.Maybe (Maybe(..), maybe)
import Data.Array as Array

-- 3×3 checkerboard input (0 = white, 1 = black)
sampleInput :: Array (Array Int)
sampleInput =
  [ [ 0, 1, 0, 1, 0 ]
  , [ 1, 0, 1, 0, 1 ]
  , [ 0, 1, 0, 1, 0 ]
  , [ 1, 0, 1, 0, 1 ]
  , [ 0, 1, 0, 1, 0 ]
  ]

main :: Effect Unit
main = do
  let catalog = extractPatterns (PatternSize 2) (InputPeriodic true) (UseRotations false) (UseMirror false) sampleInput
      rules   = buildRules catalog
      wave    = initWave catalog rules { width: 8, height: 8 } (OutputPeriodic true)
  log $ "Patterns found: " <> show (Array.length (Array.fromFoldable (map (\_ -> unit) [unit])))
  result <- wfcWithRetry (MaxAttempts 10) wave
  case result of
    Nothing -> log "Failed after 10 attempts"
    Just w  ->
      let grid = renderWaveWith (-1) w
      in log $ "Generated " <> show (Array.length grid) <> "×"
               <> show (maybe 0 Array.length (Array.head grid)) <> " grid"
