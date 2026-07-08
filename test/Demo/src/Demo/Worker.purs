module Demo.Worker where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Demo.Samples (checkerboard, samples)
import Demo.WorkerProtocol (Command, customSampleDef, emptyProgress, markContradiction, solvedCount, totalCellCount, waveToSnapshot)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)
import WFC.Algorithm (step)
import WFC.Backtrack (StepResult(..))
import WFC.Backtrack as Backtrack
import WFC.Catalog (extractPatterns)
import WFC.Grid (Pos(..))
import WFC.Propagate (Contradiction(..))
import WFC.Rules (buildRules)
import WFC.Wave (Wave, initWave)
import Demo.WorkerScope as Scope
import Web.Worker.MessageEvent (MessageEvent)
import Web.Worker.MessageEvent as MessageEvent

timeDiff :: Instant -> Instant -> Number
timeDiff t0 t1 =
  let Milliseconds ms0 = unInstant t0
      Milliseconds ms1 = unInstant t1
  in ms1 - ms0

main :: Effect Unit
main = do
  tokenRef <- Ref.new 0
  Scope.onMessage (handleMessage tokenRef)

-- The run token doubles as the stop mechanism: "stop" and every new "run"
-- bump it, and the loop below checks it before every step — an in-flight
-- loop simply notices it's been superseded and stops posting.
handleMessage :: Ref Int -> MessageEvent -> Effect Unit
handleMessage tokenRef ev = do
  let cmd = unsafeCoerce (MessageEvent.data_ ev) :: Command
  case cmd.kind of
    "stop" -> void (Ref.modify (_ + 1) tokenRef)
    "run"  -> do
      myToken <- Ref.modify (_ + 1) tokenRef
      let sample = if cmd.sampleIdx == -1
                     then customSampleDef cmd.custom
                     else fromMaybe checkerboard (Array.index samples cmd.sampleIdx)
          cat    = extractPatterns sample.n sample.periodic 1 sample.grid
          rules  = buildRules cat
          wave0  = initWave cat rules { width: sample.outW, height: sample.outH } sample.periodic
      t0 <- now
      if cmd.useBacktracking
        then do
          first <- Backtrack.initSearch wave0
          launchAff_ (runBacktrackLoop tokenRef myToken cmd.mode wave0 t0 0 0 first)
        else
          launchAff_ (runLoop tokenRef myToken cmd.mode t0 0 0 wave0)
    _ -> pure unit

runLoop :: Ref Int -> Int -> String -> Instant -> Int -> Int -> Wave Int -> Aff Unit
runLoop tokenRef myToken mode t0 stepIdx solvedSoFar wave = do
  result <- liftEffect (step wave)
  current <- liftEffect (Ref.read tokenRef)
  if current /= myToken
    then pure unit -- stopped, or superseded by a newer run
    else do
      tNow <- liftEffect now
      let elapsed = timeDiff t0 tNow
      case result of
        Left (Contradiction (Pos p)) -> do
          let snap = markContradiction p.x p.y (waveToSnapshot wave.catalog wave)
          if mode == "once" then
            liftEffect $ Scope.postMessage $ emptyProgress
              { kind        = "contradiction"
              , step        = stepIdx + 1
              , solvedTotal = solvedSoFar
              , totalCells  = totalCellCount snap
              , elapsedMs   = elapsed
              , contraX     = p.x
              , contraY     = p.y
              , grid        = snap
              }
          else do
            let fresh = initWave wave.catalog wave.rules wave.size wave.periodic
            liftEffect $ Scope.postMessage $ emptyProgress
              { kind        = "progress"
              , step        = stepIdx + 1
              , solvedTotal = solvedSoFar
              , totalCells  = totalCellCount snap
              , elapsedMs   = elapsed
              , restarted   = true
              , contraX     = p.x
              , contraY     = p.y
              , grid        = snap
              }
            delay (Milliseconds 0.0)
            runLoop tokenRef myToken mode t0 (stepIdx + 1) 0 fresh

        Right Nothing -> do
          let snap  = waveToSnapshot wave.catalog wave
              total = solvedCount snap
          liftEffect $ Scope.postMessage $ emptyProgress
            { kind        = "done"
            , step        = stepIdx + 1
            , solvedDelta = total - solvedSoFar
            , solvedTotal = total
            , totalCells  = totalCellCount snap
            , elapsedMs   = elapsed
            , grid        = snap
            }

        Right (Just wave') -> do
          let snap  = waveToSnapshot wave'.catalog wave'
              total = solvedCount snap
          liftEffect $ Scope.postMessage $ emptyProgress
            { kind        = "progress"
            , step        = stepIdx + 1
            , solvedDelta = total - solvedSoFar
            , solvedTotal = total
            , totalCells  = totalCellCount snap
            , elapsedMs   = elapsed
            , grid        = snap
            }
          delay (Milliseconds 0.0)
          runLoop tokenRef myToken mode t0 (stepIdx + 1) total wave'

-- Same outer shape as `runLoop` (token check, postMessage, yield, recurse),
-- driving `WFC.Backtrack.stepSearch` instead of `WFC.Algorithm.step`. Needs
-- `wave0` (the pristine starting wave) threaded through so the "untilSolved"
-- restart path can rebuild a fresh search — unlike `runLoop`, which always
-- has a full `Wave` in scope to rebuild from, `Failed` here only carries a
-- `Pos`.
runBacktrackLoop :: Ref Int -> Int -> String -> Wave Int -> Instant -> Int -> Int -> StepResult Int -> Aff Unit
runBacktrackLoop tokenRef myToken mode wave0 t0 stepIdx solvedSoFar result = do
  current <- liftEffect (Ref.read tokenRef)
  if current /= myToken
    then pure unit -- stopped, or superseded by a newer run
    else do
      tNow <- liftEffect now
      let elapsed = timeDiff t0 tNow
      case result of
        Failed (Contradiction (Pos p)) ->
          if mode == "once" then
            liftEffect $ Scope.postMessage $ emptyProgress
              { kind        = "contradiction"
              , step        = stepIdx + 1
              , solvedTotal = solvedSoFar
              , elapsedMs   = elapsed
              , contraX     = p.x
              , contraY     = p.y
              }
          else do
            liftEffect $ Scope.postMessage $ emptyProgress
              { kind        = "progress"
              , step        = stepIdx + 1
              , solvedTotal = solvedSoFar
              , elapsedMs   = elapsed
              , restarted   = true
              , contraX     = p.x
              , contraY     = p.y
              }
            delay (Milliseconds 0.0)
            fresh <- liftEffect (Backtrack.initSearch wave0)
            runBacktrackLoop tokenRef myToken mode wave0 t0 (stepIdx + 1) 0 fresh

        Solved wave -> do
          let snap  = waveToSnapshot wave.catalog wave
              total = solvedCount snap
          liftEffect $ Scope.postMessage $ emptyProgress
            { kind        = "done"
            , step        = stepIdx + 1
            , solvedDelta = total - solvedSoFar
            , solvedTotal = total
            , totalCells  = totalCellCount snap
            , elapsedMs   = elapsed
            , grid        = snap
            }

        Continue st -> do
          let wave  = (NonEmpty.uncons st.stack).head.wave
              snap  = waveToSnapshot wave.catalog wave
              total = solvedCount snap
          liftEffect $ Scope.postMessage $ emptyProgress
            { kind        = "progress"
            , step        = stepIdx + 1
            , solvedDelta = total - solvedSoFar
            , solvedTotal = total
            , totalCells  = totalCellCount snap
            , elapsedMs   = elapsed
            , grid        = snap
            }
          delay (Milliseconds 0.0)
          next <- liftEffect (Backtrack.stepSearch st)
          runBacktrackLoop tokenRef myToken mode wave0 t0 (stepIdx + 1) total next
