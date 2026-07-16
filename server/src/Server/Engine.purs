module Server.Engine
  ( InputSpec(..)
  , MatrixSpec
  , TileSpec
  , CreateRequest
  , SolveResponse
  , Snapshot
  , SessionData
  , buildEngine
  , initialWave
  , freshSessionData
  , initialSnapshot
  , takeStep
  , statusOf
  , solveSync
  ) where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Now (now)
import WFC.Algorithm as Algorithm
import WFC.Backtrack (SearchState, StepResult(..))
import WFC.Backtrack as Backtrack
import WFC.Catalog (InputPeriodic(..), PatternCatalog, extractPatterns)
import WFC.Grid (OutputPeriodic(..))
import WFC.Pattern (PatternSize(..), UseMirror(..), UseRotations(..))
import WFC.Propagate (Contradiction(..), MaxAttempts(..))
import WFC.Render (renderWaveWith)
import WFC.Rules (AdjacencyRules, buildRules)
import WFC.Tiles (TileDef, buildTiledCatalog, buildTiledRules)
import WFC.Wave (Wave, initWave)

-- ---------------------------------------------------------------------------
-- Request shapes (decoded from JSON in Server.Codec)
-- ---------------------------------------------------------------------------

type MatrixSpec =
  { matrix        :: Array (Array Int)
  , patternSize   :: Int
  , inputPeriodic :: Boolean
  , useRotations  :: Boolean
  , useMirror     :: Boolean
  }

-- The hand-authored tile/socket model (`WFC.Tiles`), as an alternative to
-- `MatrixInput`'s pattern-extraction pipeline — a caller supplies named
-- tiles + adjacency-by-socket-label directly, bypassing `extractPatterns`
-- entirely, exactly like `WFC.Tiles.buildTiledCatalog`/`buildTiledRules`.
type TileSpec = TileDef Int

data InputSpec
  = MatrixInput MatrixSpec
  | TilesInput (Array TileSpec)

type CreateRequest =
  { input          :: InputSpec
  , outputWidth    :: Int
  , outputHeight   :: Int
  , outputPeriodic :: Boolean
  , backtracking   :: Boolean -- solve-until-solved (WFC.Backtrack) vs a single non-retrying pass
  , maxAttempts    :: Int     -- only used when backtracking; default 50, see Server.Codec
  , keepHistory    :: Boolean
  , maxHistory     :: Int
  }

buildEngine :: InputSpec -> { catalog :: PatternCatalog Int, rules :: AdjacencyRules }
buildEngine (MatrixInput m) =
  let catalog = extractPatterns (PatternSize m.patternSize) (InputPeriodic m.inputPeriodic) (UseRotations m.useRotations) (UseMirror m.useMirror) m.matrix
  in { catalog, rules: buildRules catalog }
buildEngine (TilesInput tiles) =
  { catalog: buildTiledCatalog tiles, rules: buildTiledRules tiles }

initialWave :: CreateRequest -> Wave Int
initialWave req =
  let built = buildEngine req.input
  in initWave built.catalog built.rules { width: req.outputWidth, height: req.outputHeight } (OutputPeriodic req.outputPeriodic)

-- ---------------------------------------------------------------------------
-- Stateless solve (POST /solve) — no session kept around afterwards.
-- ---------------------------------------------------------------------------

type SolveResponse = { status :: String, grid :: Maybe (Array (Array Int)) }

-- `wfc`/`solveWithBacktracking` only ever return the *final* wave (or
-- nothing, on a contradiction/exhausted-attempts) — there's no partial grid
-- to report on failure here, unlike the stepped session API below, which
-- drives the same primitives one unit at a time and keeps every step.
solveSync :: CreateRequest -> Effect SolveResponse
solveSync req = do
  let wave0 = initialWave req
  result <-
    if req.backtracking
      then Backtrack.solveWithBacktracking (MaxAttempts req.maxAttempts) wave0
      else Algorithm.wfc wave0
  pure $ case result of
    Left _ -> { status: "contradiction", grid: Nothing }
    Right wave -> { status: "solved", grid: Just (renderWaveWith (-1) wave) }

-- ---------------------------------------------------------------------------
-- Stepped sessions — one unit of work at a time, every stage kept around
-- (`history`), and resumable/stoppable from outside (Server.Session).
-- ---------------------------------------------------------------------------

type Snapshot =
  { step       :: Int
  , kind       :: String -- "ready" | "progress" | "backedOut" | "contradiction" | "timedOut" | "solved"
  , grid       :: Array (Array Int) -- uncollapsed cells are -1
  , solved     :: Int
  , totalCells :: Int
  , elapsedMs  :: Number
  }

type SessionData =
  { wave0           :: Wave Int
  , t0              :: Instant
  , stepIdx         :: Int
  , solvedSoFar     :: Int
  , plain           :: Maybe (Wave Int)         -- non-backtracking mode; Nothing once done
  , search          :: Maybe (SearchState Int)  -- backtracking mode; Nothing once done
  , useBacktracking :: Boolean
  , maxAttempts     :: Int
  , finished        :: Boolean
  , running         :: Boolean                  -- a background /run loop is currently stepping this session
  , keepHistory     :: Boolean
  , maxHistory      :: Int
  , history         :: Array Snapshot
  , lastSnapshot    :: Maybe Snapshot
  }

timeDiff :: Instant -> Instant -> Number
timeDiff t0 t1 =
  let
    Milliseconds ms0 = unInstant t0
    Milliseconds ms1 = unInstant t1
  in
    ms1 - ms0

solvedCountFromGrid :: Array (Array Int) -> Int
solvedCountFromGrid grid = Array.length (Array.filter (_ /= (-1)) (Array.concat grid))

totalCellsFromGrid :: Array (Array Int) -> Int
totalCellsFromGrid grid = Array.foldl (\acc row -> acc + Array.length row) 0 grid

freshSessionData :: CreateRequest -> Effect SessionData
freshSessionData req = do
  let wave0 = initialWave req
  t0 <- now
  Tuple plain search <-
    if req.backtracking then do
      initResult <- Backtrack.initSearch wave0
      pure $ case initResult of
        Continue st0 -> Tuple Nothing (Just st0)
        _            -> Tuple Nothing Nothing
    else
      pure (Tuple (Just wave0) Nothing)
  pure
    { wave0
    , t0
    , stepIdx: 0
    , solvedSoFar: 0
    , plain
    , search
    , useBacktracking: req.backtracking
    , maxAttempts: req.maxAttempts
    , finished: false
    , running: false
    , keepHistory: req.keepHistory
    , maxHistory: req.maxHistory
    , history: []
    , lastSnapshot: Nothing
    }

initialSnapshot :: SessionData -> Snapshot
initialSnapshot sd =
  let grid = renderWaveWith (-1) sd.wave0
  in { step: 0, kind: "ready", grid, solved: solvedCountFromGrid grid, totalCells: totalCellsFromGrid grid, elapsedMs: 0.0 }

capHistory :: Int -> Array Snapshot -> Array Snapshot
capHistory cap hs
  | Array.length hs > cap = Array.drop (Array.length hs - cap) hs
  | otherwise = hs

recordSnapshot :: SessionData -> Snapshot -> SessionData
recordSnapshot sd snap = sd
  { lastSnapshot = Just snap
  , history = if sd.keepHistory then capHistory sd.maxHistory (Array.snoc sd.history snap) else sd.history
  }

statusOf :: SessionData -> String
statusOf sd
  | sd.finished = fromMaybe "finished" (_.kind <$> sd.lastSnapshot)
  | sd.running = "running"
  | otherwise = "ready"

-- Advance exactly one unit of work (one plain `WFC.Algorithm.step`, or one
-- `WFC.Backtrack.stepSearch`, depending on `useBacktracking`), recording
-- the resulting snapshot into `history`/`lastSnapshot`. A no-op — replays
-- the last snapshot — once `finished`, so a caller never needs to guard
-- against calling this one step too many.
takeStep :: SessionData -> Effect (Tuple Snapshot SessionData)
takeStep sd
  | sd.finished = pure (Tuple (fromMaybe (initialSnapshot sd) sd.lastSnapshot) sd)
  | otherwise = do
      tNow <- now
      let elapsed = timeDiff sd.t0 tNow
      Tuple snap sd' <-
        if sd.useBacktracking then takeBacktrackStep elapsed sd
        else takePlainStep elapsed sd
      pure (Tuple snap (recordSnapshot sd' snap))

takeBacktrackStep :: Number -> SessionData -> Effect (Tuple Snapshot SessionData)
takeBacktrackStep elapsed sd = case sd.search of
  Nothing ->
    let
      grid = renderWaveWith (-1) sd.wave0
      snap = { step: sd.stepIdx, kind: "solved", grid, solved: solvedCountFromGrid grid, totalCells: totalCellsFromGrid grid, elapsedMs: elapsed }
    in
      pure (Tuple snap (sd { finished = true, running = false }))
  Just st | st.attempts >= sd.maxAttempts ->
    let
      wave = (NonEmpty.uncons st.stack).head.wave
      grid = renderWaveWith (-1) wave
      snap = { step: sd.stepIdx + 1, kind: "timedOut", grid, solved: sd.solvedSoFar, totalCells: totalCellsFromGrid grid, elapsedMs: elapsed }
    in
      pure (Tuple snap (sd { stepIdx = sd.stepIdx + 1, search = Nothing, finished = true, running = false }))
  Just st -> do
    result <- Backtrack.stepSearch st
    pure $ case result of
      Solved wave ->
        let
          grid = renderWaveWith (-1) wave
          solved = solvedCountFromGrid grid
          snap = { step: sd.stepIdx + 1, kind: "solved", grid, solved, totalCells: totalCellsFromGrid grid, elapsedMs: elapsed }
        in
          Tuple snap (sd { stepIdx = sd.stepIdx + 1, solvedSoFar = solved, search = Nothing, finished = true, running = false })
      Failed _ ->
        let
          grid = renderWaveWith (-1) sd.wave0
          snap = { step: sd.stepIdx + 1, kind: "contradiction", grid, solved: sd.solvedSoFar, totalCells: totalCellsFromGrid grid, elapsedMs: elapsed }
        in
          Tuple snap (sd { stepIdx = sd.stepIdx + 1, search = Nothing, finished = true, running = false })
      Continue st' ->
        let
          wave = (NonEmpty.uncons st'.stack).head.wave
          grid = renderWaveWith (-1) wave
          solved = solvedCountFromGrid grid
          snap = { step: sd.stepIdx + 1, kind: "progress", grid, solved, totalCells: totalCellsFromGrid grid, elapsedMs: elapsed }
        in
          Tuple snap (sd { stepIdx = sd.stepIdx + 1, solvedSoFar = solved, search = Just st' })
      BackedOut st' ->
        let
          wave = (NonEmpty.uncons st'.stack).head.wave
          grid = renderWaveWith (-1) wave
          solved = solvedCountFromGrid grid
          snap = { step: sd.stepIdx + 1, kind: "backedOut", grid, solved, totalCells: totalCellsFromGrid grid, elapsedMs: elapsed }
        in
          Tuple snap (sd { stepIdx = sd.stepIdx + 1, solvedSoFar = solved, search = Just st' })

takePlainStep :: Number -> SessionData -> Effect (Tuple Snapshot SessionData)
takePlainStep elapsed sd = case sd.plain of
  Nothing ->
    let
      grid = renderWaveWith (-1) sd.wave0
      snap = { step: sd.stepIdx, kind: "solved", grid, solved: solvedCountFromGrid grid, totalCells: totalCellsFromGrid grid, elapsedMs: elapsed }
    in
      pure (Tuple snap (sd { finished = true, running = false }))
  Just wave -> do
    result <- Algorithm.step wave
    pure $ case result of
      Left (Contradiction _) ->
        let
          grid = renderWaveWith (-1) wave
          snap = { step: sd.stepIdx + 1, kind: "contradiction", grid, solved: sd.solvedSoFar, totalCells: totalCellsFromGrid grid, elapsedMs: elapsed }
        in
          Tuple snap (sd { stepIdx = sd.stepIdx + 1, plain = Nothing, finished = true, running = false })
      Right Nothing ->
        let
          grid = renderWaveWith (-1) wave
          solved = solvedCountFromGrid grid
          snap = { step: sd.stepIdx + 1, kind: "solved", grid, solved, totalCells: totalCellsFromGrid grid, elapsedMs: elapsed }
        in
          Tuple snap (sd { stepIdx = sd.stepIdx + 1, solvedSoFar = solved, plain = Nothing, finished = true, running = false })
      Right (Just wave') ->
        let
          grid = renderWaveWith (-1) wave'
          solved = solvedCountFromGrid grid
          snap = { step: sd.stepIdx + 1, kind: "progress", grid, solved, totalCells: totalCellsFromGrid grid, elapsedMs: elapsed }
        in
          Tuple snap (sd { stepIdx = sd.stepIdx + 1, solvedSoFar = solved, plain = Just wave' })
