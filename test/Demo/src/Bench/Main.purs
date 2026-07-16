module Bench.Main (main) where

import Prelude

import Bench.Node (argv, exitProcess, onSigint)
import Bench.PngDecode (decodePngFile)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foldable (foldM, for_, maximum, minimum, sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Demo.ImageSamples (ImageSampleDef, imageSamples)
import Demo.ImageUpload (gridFromPixels)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import WFC.Algorithm (wfc)
import WFC.Backtrack (solveWithBacktracking)
import WFC.Catalog (InputPeriodic(..), PatternCatalog, extractPatterns)
import WFC.Grid (OutputPeriodic(..))
import WFC.Pattern (PatternSize(..), UseMirror(..), UseRotations(..))
import WFC.Propagate (Contradiction, MaxAttempts(..))
import WFC.Rules (AdjacencyRules, buildRules)
import WFC.Wave (Wave, initWave)

-- CLI benchmark over `Demo.ImageSamples`' real reference PNGs — the same
-- source the browser demo's "(image)" dropdown entries load, decoded here
-- via `Bench.PngDecode` (pure-JS, no DOM) instead of `Demo.ImageUpload`'s
-- `<canvas>` path. Two modes per example, matching the two ways the demo
-- itself can run a wave (`startRun "single"` vs `startRun "untilSolved"`,
-- see `Demo.App`):
--
--   one-shot   — a single, non-retrying pass (`WFC.Algorithm.wfc`): settles
--                once, either fully collapsed or stuck at a contradiction.
--   backtrack  — `WFC.Backtrack.solveWithBacktracking`: keeps retrying
--                (backing out and re-guessing) until solved or the attempt
--                budget runs out (reported as "timed out" here, though it's
--                an attempt-count budget rather than a wall-clock one).

-- ---------------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------------

type Config =
  { exampleNames :: Array String
  , runsPerMode  :: Int
  , outW         :: Int
  , outH         :: Int
  , patternSize  :: Int
  , maxAttempts  :: Int
  , samplesDir   :: String
  }

defaultConfig :: Config
defaultConfig =
  { exampleNames: [ "Skyline", "Maze", "City" ]
  , runsPerMode: 10
  , outW: 24
  , outH: 24
  , patternSize: 3
  , maxAttempts: 2000
  , samplesDir: "test/Demo/samples"
  }

-- "--key=value" -> Just { key: "key", value: "value" }; anything else Nothing.
parseFlag :: String -> Maybe { key :: String, value :: String }
parseFlag s = do
  rest <- String.stripPrefix (String.Pattern "--") s
  i <- String.indexOf (String.Pattern "=") rest
  pure { key: String.take i rest, value: String.drop (i + 1) rest }

applyFlag :: Config -> { key :: String, value :: String } -> Config
applyFlag cfg { key: "examples", value } = cfg { exampleNames = String.split (String.Pattern ",") value }
applyFlag cfg { key: "runs", value } = cfg { runsPerMode = fromMaybe cfg.runsPerMode (Int.fromString value) }
applyFlag cfg { key: "width", value } = cfg { outW = fromMaybe cfg.outW (Int.fromString value) }
applyFlag cfg { key: "height", value } = cfg { outH = fromMaybe cfg.outH (Int.fromString value) }
applyFlag cfg { key: "pattern", value } = cfg { patternSize = fromMaybe cfg.patternSize (Int.fromString value) }
applyFlag cfg { key: "attempts", value } = cfg { maxAttempts = fromMaybe cfg.maxAttempts (Int.fromString value) }
applyFlag cfg { key: "dir", value } = cfg { samplesDir = value }
applyFlag cfg _ = cfg

configFromArgv :: Array String -> Config
configFromArgv args = Array.foldl applyFlag defaultConfig (Array.mapMaybe parseFlag args)

-- ---------------------------------------------------------------------------
-- Timing
-- ---------------------------------------------------------------------------

timed :: forall a. Effect a -> Effect { result :: a, ms :: Number }
timed action = do
  Milliseconds t0 <- unInstant <$> now
  result <- action
  Milliseconds t1 <- unInstant <$> now
  pure { result, ms: t1 - t0 }

formatMs :: Number -> String
formatMs ms = show (Int.toNumber (Int.round (ms * 10.0)) / 10.0) <> "ms"

padRight :: Int -> String -> String
padRight n s = s <> String.joinWith "" (Array.replicate (max 0 (n - String.length s)) " ")

padLeft :: Int -> String -> String
padLeft n s = String.joinWith "" (Array.replicate (max 0 (n - String.length s)) " ") <> s

-- ---------------------------------------------------------------------------
-- Example lookup + preparation
-- ---------------------------------------------------------------------------

stripImageSuffix :: String -> String
stripImageSuffix name = fromMaybe name (String.stripSuffix (String.Pattern " (image)") name)

findExample :: String -> Maybe ImageSampleDef
findExample wanted =
  Array.find (\def -> String.toLower (stripImageSuffix def.name) == String.toLower wanted) imageSamples

fileNameOf :: String -> String
fileNameOf p = fromMaybe p (Array.last (String.split (String.Pattern "/") p))

type Prepared =
  { name    :: String
  , catalog :: PatternCatalog Int
  , rules   :: AdjacencyRules
  , setupMs :: Number
  }

prepareExample :: Config -> ImageSampleDef -> Effect Prepared
prepareExample cfg def = do
  { result, ms } <- timed do
    png <- decodePngFile (cfg.samplesDir <> "/" <> fileNameOf def.path)
    let
      decoded = gridFromPixels png.width png.height png.bytes
      catalog = extractPatterns (PatternSize cfg.patternSize) (InputPeriodic true) (UseRotations true) (UseMirror true) decoded.grid
      rules = buildRules catalog
    pure { catalog, rules }
  pure { name: stripImageSuffix def.name, catalog: result.catalog, rules: result.rules, setupMs: ms }

-- ---------------------------------------------------------------------------
-- Running one wave, one way or the other
-- ---------------------------------------------------------------------------

data Outcome = Solved | Contradiction | TimedOut

derive instance eqOutcome :: Eq Outcome

instance showOutcome :: Show Outcome where
  show Solved = "solved"
  show Contradiction = "contradiction"
  show TimedOut = "timed out"

type RunResult = { outcome :: Outcome, ms :: Number }

oneShotOutcome :: forall a. Either Contradiction (Wave a) -> Outcome
oneShotOutcome (Left _) = Contradiction
oneShotOutcome (Right _) = Solved

backtrackOutcome :: forall a. Either Contradiction (Wave a) -> Outcome
backtrackOutcome (Left _) = TimedOut
backtrackOutcome (Right _) = Solved

freshWave :: Prepared -> Config -> Wave Int
freshWave ex cfg = initWave ex.catalog ex.rules { width: cfg.outW, height: cfg.outH } (OutputPeriodic true)

runOneShot :: Prepared -> Config -> Effect RunResult
runOneShot ex cfg = do
  { result, ms } <- timed (wfc (freshWave ex cfg))
  pure { outcome: oneShotOutcome result, ms }

runBacktracked :: Prepared -> Config -> Effect RunResult
runBacktracked ex cfg = do
  { result, ms } <- timed (solveWithBacktracking (MaxAttempts cfg.maxAttempts) (freshWave ex cfg))
  pure { outcome: backtrackOutcome result, ms }

-- ---------------------------------------------------------------------------
-- Trial loop: prints progress as it goes, bails early once `stopRef` flips
-- (set by the SIGINT handler in `main`) instead of losing whatever already
-- ran. Runs in `Aff`, with a zero-length `delay` after each trial —
-- `wfc`/`solveWithBacktracking` are plain synchronous `Effect`, so without
-- an explicit yield back to the event loop between trials, a SIGINT's
-- callback (queued by Node, but never scheduled) would only ever get a
-- chance to run after the *entire* benchmark already finished on its own.
-- ---------------------------------------------------------------------------

runTrials :: Ref Boolean -> String -> Int -> Effect RunResult -> Aff (Array RunResult)
runTrials stopRef label total runOne = go 1 []
  where
  go i acc
    | i > total = pure acc
    | otherwise = do
        stop <- liftEffect (Ref.read stopRef)
        if stop then pure acc
        else do
          r <- liftEffect runOne
          liftEffect $ log
            ( "    " <> padRight 10 label
                <> "run " <> padLeft 2 (show i) <> "/" <> show total <> "  "
                <> padRight 13 (show r.outcome)
                <> formatMs r.ms
            )
          delay (Milliseconds 0.0)
          go (i + 1) (Array.snoc acc r)

-- ---------------------------------------------------------------------------
-- Summary
-- ---------------------------------------------------------------------------

type Summary = { count :: Int, solved :: Int, avgMs :: Number, minMs :: Number, maxMs :: Number }

summarize :: Array RunResult -> Summary
summarize rs =
  let
    times = map _.ms rs
    solved = Array.length (Array.filter (\r -> r.outcome == Solved) rs)
  in
    { count: Array.length rs
    , solved
    , avgMs: if Array.null rs then 0.0 else sum times / Int.toNumber (Array.length rs)
    , minMs: fromMaybe 0.0 (minimum times)
    , maxMs: fromMaybe 0.0 (maximum times)
    }

formatSummary :: Summary -> String
formatSummary s =
  show s.solved <> "/" <> show s.count <> " solved, avg " <> formatMs s.avgMs
    <> " (min " <> formatMs s.minMs <> ", max " <> formatMs s.maxMs <> ")"

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

type ExampleResults = { name :: String, oneShot :: Array RunResult, backtracked :: Array RunResult }

runExample :: Ref Boolean -> Config -> ImageSampleDef -> Aff ExampleResults
runExample stopRef cfg def = do
  liftEffect $ log ("=== " <> stripImageSuffix def.name <> " ===")
  ex <- liftEffect (prepareExample cfg def)
  liftEffect $ log ("    extract+rules: " <> formatMs ex.setupMs)

  liftEffect $ log "    -- one-shot (single pass, contradiction or solved) --"
  oneShot <- runTrials stopRef "one-shot  " cfg.runsPerMode (runOneShot ex cfg)

  stop <- liftEffect (Ref.read stopRef)
  backtracked <-
    if stop then pure []
    else do
      liftEffect $ log "    -- backtracked (retries until solved or attempts exhausted) --"
      runTrials stopRef "backtrack " cfg.runsPerMode (runBacktracked ex cfg)

  liftEffect $ log ""
  pure { name: ex.name, oneShot, backtracked }

main :: Effect Unit
main = do
  args <- argv
  let cfg = configFromArgv args
  stopRef <- Ref.new false
  onSigint do
    Ref.write true stopRef
    log ""
    log "  ^C — finishing the current run, then printing what we have..."

  log "WFC benchmark (Demo.ImageSamples)"
  log
    ( "  runs/mode: " <> show cfg.runsPerMode
        <> "   output: " <> show cfg.outW <> "x" <> show cfg.outH
        <> "   pattern size: " <> show cfg.patternSize
        <> "   max attempts: " <> show cfg.maxAttempts
    )
  log "  (--examples=A,B,C  --runs=N  --width=W  --height=H  --pattern=N  --attempts=N  --dir=path)"
  log ""

  let lookups = map (\n -> Tuple n (findExample n)) cfg.exampleNames
  for_ lookups \(Tuple n mDef) -> case mDef of
    Nothing -> log ("  ! unknown example \"" <> n <> "\" — skipping (see Demo.ImageSamples for valid names)")
    Just _ -> pure unit
  let defs = Array.mapMaybe (\(Tuple _ d) -> d) lookups

  launchAff_ do
    results <- runAllExamples stopRef cfg defs

    liftEffect $ log "=== summary ==="
    liftEffect $ for_ results \r -> do
      log (padRight 12 r.name <> "  one-shot:  " <> formatSummary (summarize r.oneShot))
      log (padRight 12 "" <> "  backtrack: " <> formatSummary (summarize r.backtracked))

    liftEffect (exitProcess 0)

runAllExamples :: Ref Boolean -> Config -> Array ImageSampleDef -> Aff (Array ExampleResults)
runAllExamples stopRef cfg = foldM
  ( \acc def -> do
      stop <- liftEffect (Ref.read stopRef)
      if stop then pure acc
      else do
        r <- runExample stopRef cfg def
        pure (Array.snoc acc r)
  )
  []
