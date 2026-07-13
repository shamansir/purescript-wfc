module Demo.Worker where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Demo.Samples (checkerboard, samples)
import Demo.TileSamples (roads)
import Demo.TileSamples as TileSamples
import Demo.WorkerProtocol (Command, Progress, buildIntCatalogFromTileSet, customSampleDef, emptyProgress, fromWireTileSet, markContradiction, solvedCount, totalCellCount, waveToSnapshot)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)
import WFC.Algorithm (step)
import WFC.Backtrack (SearchState, StepResult(..))
import WFC.Backtrack as Backtrack
import WFC.Catalog (PatternCatalog, extractPatterns, lastPatternId)
import WFC.Grid (GridSize, Pos(..))
import WFC.Propagate (Contradiction(..), applyGround)
import WFC.Rules (AdjacencyRules, buildRules)
import WFC.Tiles (buildTiledCatalog, buildTiledRules)
import WFC.Wave (Wave, initWave, resizeWave)
import Demo.WorkerScope as Scope
import Web.Worker.MessageEvent (MessageEvent)
import Web.Worker.MessageEvent as MessageEvent

timeDiff :: Instant -> Instant -> Number
timeDiff t0 t1 =
  let Milliseconds ms0 = unInstant t0
      Milliseconds ms1 = unInstant t1
  in ms1 - ms0

-- The live, resumable solving attempt — kept alive across separate
-- "step"/"run"/"stop" messages so any of them can pick up exactly where
-- the last one left off, instead of each rebuilding its own wave from
-- scratch. Only `ExtractPatterns`/`ResetWave`/switching the sample
-- ("resetSession") invalidates it; `Stop` deliberately leaves it alone —
-- that's what lets Step (or a later Run) continue after a Stop.
type Session =
  { useBacktracking :: Boolean
  , wave0           :: Wave Int                    -- pristine start, for a fresh restart-on-contradiction
  , t0              :: Instant
  , stepIdx         :: Int
  , solvedSoFar     :: Int
  , plain           :: Maybe (Wave Int)             -- Nothing once done/failed
  , search          :: Maybe (SearchState Int)      -- Nothing once done/failed
  }

main :: Effect Unit
main = do
  tokenRef   <- Ref.new 0
  sessionRef <- Ref.new Nothing
  Scope.onMessage (handleMessage tokenRef sessionRef)

-- The run token doubles as the stop mechanism: "stop"/"resetSession" and
-- every new "run"/"step" bump it, and the loop below checks it before every
-- step — an in-flight loop simply notices it's been superseded and stops
-- posting.
handleMessage :: Ref Int -> Ref (Maybe Session) -> MessageEvent -> Effect Unit
handleMessage tokenRef sessionRef ev = do
  let cmd = unsafeCoerce (MessageEvent.data_ ev) :: Command
  case cmd.kind of
    "stop" -> void (Ref.modify (_ + 1) tokenRef)
    "resetSession" -> do
      void (Ref.modify (_ + 1) tokenRef)
      Ref.write Nothing sessionRef
    "run" -> do
      myToken <- Ref.modify (_ + 1) tokenRef
      launchAff_ (runFrom sessionRef tokenRef myToken cmd)
    "step" -> do
      myToken <- Ref.modify (_ + 1) tokenRef
      -- Force single-shot regardless of what `mode` the caller sent —
      -- `runFrom` only loops when `mode` is "once"/"untilSolved".
      launchAff_ (runFrom sessionRef tokenRef myToken (cmd { mode = "" }))
    "resize" -> do
      -- Deliberately doesn't touch `tokenRef` — unlike "stop"/"resetSession"/
      -- "run"/"step", a resize doesn't supersede or interrupt whatever's in
      -- flight, it just changes the live session's own grid size in place
      -- (or does nothing if there's no session yet — the next "run"/"step"
      -- will lazily build one at the new `cmd.outW`/`cmd.outH` anyway). No
      -- reply is sent either: the main thread already applied the same
      -- crop/fill resize to its own displayed grid the instant the size
      -- input changed, so the two stay in sync without a round trip —
      -- this message only needs to keep the *engine* state consistent with
      -- what's already on screen.
      mSession <- Ref.read sessionRef
      case mSession of
        Nothing -> pure unit
        Just s  -> Ref.write (Just (resizeSession { width: cmd.outW, height: cmd.outH } s)) sessionRef
    _ -> pure unit

-- Resize every wave a live session could currently be reporting from —
-- `wave0` (so a later restart-on-contradiction lands at the new size too),
-- `plain`, and every frame's own `wave` in a backtracking search's stack.
-- A frame's `pos`/`untried` never need adjustment: `untried` is just
-- catalog-level pattern ids (position-independent), and if a frame's `pos`
-- happens to fall outside the new bounds after shrinking, `resizeWave`
-- already dropped that position from `cells` — the next `attemptValue` at
-- that frame reads it as an ordinary contradiction and backtracks out
-- through the existing recovery path, no special-casing needed here.
resizeSession :: GridSize -> Session -> Session
resizeSession newSize session = session
  { wave0  = resizeWave newSize session.wave0
  , plain  = map (resizeWave newSize) session.plain
  , search = map (resizeSearchState newSize) session.search
  }

resizeSearchState :: GridSize -> SearchState Int -> SearchState Int
resizeSearchState newSize st = st { stack = map (\f -> f { wave = resizeWave newSize f.wave }) st.stack }

-- Build the (cat/rules/outW/outH/periodic) a fresh session needs from a
-- Command — the same construction "run" always did, now shared with
-- "step" so either can lazily initialize the session on first use.
-- `cmd.sourceKind` picks which of the demo's 4 sample sources this is;
-- "image"/"xmlTileset" carry their own already-fetched/parsed plain data
-- (`custom`/`customTileSet`) instead of indexing a compiled-in list.
buildFromCommand
  :: Command
  -> { cat :: PatternCatalog Int, rules :: AdjacencyRules, outW :: Int, outH :: Int, periodic :: Boolean, ground :: Boolean }
buildFromCommand cmd =
  case cmd.sourceKind of
    "handTiled" ->
      let ts = fromMaybe roads (Array.index TileSamples.samples cmd.sampleIdx)
      in { cat: buildTiledCatalog ts.tiles, rules: buildTiledRules ts.tiles
         , outW: cmd.outW, outH: cmd.outH, periodic: cmd.outputPeriodic, ground: false
         }
    "xmlTileset" ->
      let built = buildIntCatalogFromTileSet (fromWireTileSet cmd.customTileSet)
      in { cat: built.catalog, rules: built.rules, outW: cmd.outW, outH: cmd.outH, periodic: cmd.outputPeriodic, ground: false }
    _ ->
      let sample = if cmd.sourceKind == "image"
                     then customSampleDef cmd.custom
                     else fromMaybe checkerboard (Array.index samples cmd.sampleIdx)
          cat    = extractPatterns cmd.patternSize cmd.inputPeriodic cmd.useRotations cmd.useMirror sample.grid
      in { cat, rules: buildRules cat, outW: cmd.outW, outH: cmd.outH, periodic: cmd.outputPeriodic, ground: sample.ground }

-- Applies the "ground" heuristic (see WFC.Propagate.applyGround) to a
-- freshly-initialized wave, when the active sample declares `ground: true`.
-- A contradiction here would mean the sample's own bottom-row pattern is
-- already incompatible with the adjacency rules derived from itself, which
-- shouldn't happen in practice — falls back to the un-grounded wave rather
-- than crash the session.
applyGroundIfNeeded :: forall r. { cat :: PatternCatalog Int, ground :: Boolean | r } -> Wave Int -> Wave Int
applyGroundIfNeeded built wave0
  | not built.ground = wave0
  | otherwise = case lastPatternId built.cat of
      Nothing   -> wave0
      Just gpid -> case applyGround gpid wave0 of
        Left (Contradiction _) -> wave0
        Right wave1             -> wave1

-- Reuse the existing session if one's alive, otherwise build a brand-new
-- wave/search from `cmd` and stash it — this is what makes an idle worker
-- (just extracted, or freshly resumed after "resetSession") transparently
-- "just work" the first time either "step" or "run" is sent.
getOrInitSession :: Ref (Maybe Session) -> Command -> Effect Session
getOrInitSession sessionRef cmd = do
  mSession <- Ref.read sessionRef
  case mSession of
    Just s -> pure s
    Nothing -> do
      let built = buildFromCommand cmd
          wave0 = applyGroundIfNeeded built
                     (initWave built.cat built.rules { width: built.outW, height: built.outH } built.periodic)
      t0 <- now
      session <-
        if cmd.useBacktracking then do
          -- `initSearch` finds the first decision frame, wrapped in
          -- `StepResult` only to cover the (rare but real — a uniform
          -- catalog with 1 pattern) case where `wave0` is already fully
          -- collapsed with nothing to decide (`Solved`); `Failed`/
          -- `BackedOut` are never returned by `initSearch` itself.
          initResult <- Backtrack.initSearch wave0
          let search = case initResult of
                Continue st0 -> Just st0
                _            -> Nothing
          pure { useBacktracking: true, wave0, t0, stepIdx: 0, solvedSoFar: 0, plain: Nothing, search }
        else
          pure { useBacktracking: false, wave0, t0, stepIdx: 0, solvedSoFar: 0, plain: Just wave0, search: Nothing }
      Ref.write (Just session) sessionRef
      pure session

-- Perform exactly one engine step (plain `WFC.Algorithm.step`, or
-- `WFC.Backtrack.stepSearch` in backtracking mode) against a session,
-- returning the Progress to report and the session to continue from
-- (Nothing once done/failed — a later "step"/"run" will lazily start a
-- fresh attempt via `getOrInitSession`). Doesn't decide "restarted" —
-- that depends on the *caller's* mode (a manual "step" keeps a
-- plain-mode contradiction's frozen wave around for a retry; an
-- untilSolved "run" discards it and restarts instead), so `runFrom`
-- applies that policy after the fact.
takeOneStep :: Session -> Effect (Tuple Progress (Maybe Session))
takeOneStep session = do
  tNow <- now
  let elapsed = timeDiff session.t0 tNow
      base kind = emptyProgress { kind = kind, step = session.stepIdx + 1, elapsedMs = elapsed }
  if session.useBacktracking
    then case session.search of
      -- Only reachable right after `getOrInitSession` built a fresh
      -- session whose `wave0` was already fully collapsed (`initSearch`
      -- returned `Solved` immediately, nothing to step) — `wave0` itself
      -- *is* the solved wave in that case.
      Nothing -> do
        let snap  = waveToSnapshot session.wave0.catalog session.wave0
            total = solvedCount snap
        pure $ Tuple
          (base "done") { solvedDelta = total - session.solvedSoFar, solvedTotal = total, totalCells = totalCellCount snap, grid = snap }
          Nothing
      Just st -> do
        result <- Backtrack.stepSearch st
        case result of
          Solved wave -> do
            let snap  = waveToSnapshot wave.catalog wave
                total = solvedCount snap
            pure $ Tuple
              (base "done") { solvedDelta = total - session.solvedSoFar, solvedTotal = total, totalCells = totalCellCount snap, grid = snap }
              Nothing
          Failed (Contradiction (Pos p)) ->
            pure $ Tuple
              (base "contradiction") { solvedTotal = session.solvedSoFar, contraX = p.x, contraY = p.y }
              Nothing
          Continue st' -> do
            let wave  = (NonEmpty.uncons st'.stack).head.wave
                snap  = waveToSnapshot wave.catalog wave
                total = solvedCount snap
            pure $ Tuple
              (base "progress") { solvedDelta = total - session.solvedSoFar, solvedTotal = total, totalCells = totalCellCount snap, grid = snap, backedOut = false }
              (Just session { search = Just st', stepIdx = session.stepIdx + 1, solvedSoFar = total })
          BackedOut st' -> do
            let wave  = (NonEmpty.uncons st'.stack).head.wave
                snap  = waveToSnapshot wave.catalog wave
                total = solvedCount snap
                col   = NonEmpty.length st'.stack
            pure $ Tuple
              (base "progress") { solvedDelta = total - session.solvedSoFar, solvedTotal = total, totalCells = totalCellCount snap, grid = snap, backedOut = true, rowBreak = true, rowStartColumn = col }
              (Just session { search = Just st', stepIdx = session.stepIdx + 1, solvedSoFar = total })
    else case session.plain of
      Nothing -> pure (Tuple (base "done") Nothing)
      Just wave -> do
        result <- step wave
        case result of
          Left (Contradiction (Pos p)) -> do
            let snap = markContradiction p.x p.y (waveToSnapshot wave.catalog wave)
            -- Keep the frozen (pre-collapse) wave alive: a manual retry
            -- (another "step") re-tries the same decision with fresh
            -- randomness, matching the old local StepOnce's behavior. Still
            -- bump `stepIdx` so a subsequent retry reports its own step
            -- number instead of repeating this one's.
            pure $ Tuple
              (base "contradiction") { solvedTotal = session.solvedSoFar, totalCells = totalCellCount snap, contraX = p.x, contraY = p.y, grid = snap }
              (Just session { stepIdx = session.stepIdx + 1 })
          Right Nothing -> do
            let snap  = waveToSnapshot wave.catalog wave
                total = solvedCount snap
            pure $ Tuple
              (base "done") { solvedDelta = total - session.solvedSoFar, solvedTotal = total, totalCells = totalCellCount snap, grid = snap }
              Nothing
          Right (Just wave') -> do
            let snap  = waveToSnapshot wave'.catalog wave'
                total = solvedCount snap
            pure $ Tuple
              (base "progress") { solvedDelta = total - session.solvedSoFar, solvedTotal = total, totalCells = totalCellCount snap, grid = snap }
              (Just session { plain = Just wave', stepIdx = session.stepIdx + 1, solvedSoFar = total })

-- A fresh attempt from the *same* `wave0`/`t0`/`stepIdx` a session already
-- has — unlike `getOrInitSession`, which only fires when there's no
-- session at all, this is for the "untilSolved" restart-on-contradiction
-- path, where `wave0` doesn't change but the live attempt does. Keeping
-- `t0`/`stepIdx` (instead of rebuilding via `getOrInitSession`, which
-- would zero them and re-run `extractPatterns` for no reason) is what
-- keeps the Steps/elapsed-time counters monotonic across restarts.
restartSession :: Session -> Effect Session
restartSession session = do
  let bumped = session { stepIdx = session.stepIdx + 1, solvedSoFar = 0 }
  if session.useBacktracking then do
    initResult <- Backtrack.initSearch session.wave0
    let search = case initResult of
          Continue st0 -> Just st0
          _            -> Nothing
    pure bumped { plain = Nothing, search = search }
  else
    pure bumped { plain = Just session.wave0, search = Nothing }

-- Drives both "step" (cmd.mode == "", so the loop guards below never
-- fire — exactly one iteration) and "run" ("once"/"untilSolved", which
-- keep going): get-or-init the session, take one step, post it, and
-- decide whether to keep looping. An untilSolved contradiction restarts
-- the live attempt (`restartSession`) and loops again — everything else
-- (a manual step, or "once" hitting a contradiction) leaves whatever
-- `takeOneStep` returned in place so a later message can resume from it.
runFrom :: Ref (Maybe Session) -> Ref Int -> Int -> Command -> Aff Unit
runFrom sessionRef tokenRef myToken cmd = do
  current <- liftEffect (Ref.read tokenRef)
  if current /= myToken
    then pure unit -- stopped, or superseded by a newer run/step
    else do
      session <- liftEffect (getOrInitSession sessionRef cmd)
      Tuple progress0 nextSession <- liftEffect (takeOneStep session)
      let isRestart = progress0.kind == "contradiction" && cmd.mode == "untilSolved"
          continuous = cmd.mode == "once" || cmd.mode == "untilSolved"
          progress = progress0
            { token      = myToken
            , continuous = continuous
            , restarted  = progress0.restarted || isRestart
            , rowBreak   = progress0.rowBreak || isRestart
            , rowStartColumn = if isRestart then 0 else progress0.rowStartColumn
            }
      liftEffect (Scope.postMessage progress)
      finalSession <-
        if isRestart
          then Just <$> liftEffect (restartSession session)
          else pure nextSession
      liftEffect (Ref.write finalSession sessionRef)
      case progress.kind of
        "progress" ->
          when continuous do
            delay (Milliseconds 0.0)
            runFrom sessionRef tokenRef myToken cmd
        "contradiction" ->
          when isRestart do
            delay (Milliseconds 0.0)
            runFrom sessionRef tokenRef myToken cmd
        _ -> pure unit
