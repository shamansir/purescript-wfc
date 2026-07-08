module Demo.Worker where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Demo.Samples (checkerboard, samples)
import Demo.WorkerProtocol (Command, emptyProgress, markContradiction, solvedCount, totalCellCount, waveToSnapshot)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Unsafe.Coerce (unsafeCoerce)
import WFC.Algorithm (step)
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
      let sample = fromMaybe checkerboard (Array.index samples cmd.sampleIdx)
          cat    = extractPatterns sample.n sample.periodic 1 sample.grid
          rules  = buildRules cat
          wave0  = initWave cat rules { width: sample.outW, height: sample.outH } sample.periodic
      t0 <- now
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
