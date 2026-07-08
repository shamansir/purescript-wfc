module WFC.Backtrack where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmpty
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import WFC.Collapse (weightedSample)
import WFC.Entropy (minEntropyPos)
import WFC.Grid (Pos)
import WFC.Pattern (PatternId)
import WFC.Propagate (BanEvent, Contradiction(..), propagate)
import WFC.Wave (Wave, getCellPossibilities)

-- One decision point in the search: the wave as it was *before* this cell
-- was guessed, plus whichever of its original possibilities haven't been
-- ruled out yet at this point in the search. Snapshotting the whole `Wave`
-- is cheap — it's an immutable record built from persistent Map/Set, so
-- unrelated branches are shared rather than copied.
type Frame a =
  { wave    :: Wave a
  , pos     :: Pos
  , untried :: Set PatternId
  }

type SearchState a =
  { stack    :: NonEmptyList (Frame a)
  , attempts :: Int
  }

-- One unit of backtracking search progress. Mirrors `WFC.Algorithm.step`'s
-- role, but with more outcomes: a bad guess doesn't necessarily end the
-- search — it either tries another value at the same cell, or backtracks
-- to the parent's decision (`BackedOut`, distinguished from a forward
-- `Continue` so a caller can tell "still going" apart from "just returned
-- to an earlier point in the search" — e.g. to draw a search-tree-shaped
-- history instead of one flat sequence). Only exhausting every alternative
-- all the way back to the first cell ends the search (`Failed`).
data StepResult a
  = Continue (SearchState a)
  | BackedOut (SearchState a)
  | Solved (Wave a)
  | Failed Contradiction

-- Ban every possibility at `pos` in `wave` except `value`, then propagate —
-- the same "commit to one value" step `WFC.Collapse.collapseAt` does, but
-- parameterized over an explicit choice instead of drawing one itself, so
-- backtracking can retry the same cell with a different value without
-- redrawing from its full, unreduced possibility set each time.
attemptValue :: forall a. Wave a -> Pos -> PatternId -> Either Contradiction (Wave a)
attemptValue wave pos value =
  case getCellPossibilities wave pos of
    Nothing -> Left (Contradiction pos)
    Just s  ->
      let toBan   = Array.filter (_ /= value) (Set.toUnfoldable s :: Array PatternId)
          banEvts = map (Tuple pos) toBan :: Array BanEvent
      in propagate wave banEvts

-- A fresh frame for the wave's current lowest-entropy cell, if any cell is
-- still undecided; Nothing means the wave is already fully collapsed.
nextFrame :: forall a. Wave a -> Effect (Maybe (Frame a))
nextFrame wave = do
  mPos <- minEntropyPos wave
  pure $ mPos >>= \pos ->
    getCellPossibilities wave pos <#> \s -> { wave, pos, untried: s }

-- Start a new search from `wave0` — `Solved wave0` immediately if it's
-- already fully collapsed, otherwise the first cell's decision frame.
initSearch :: forall a. Wave a -> Effect (StepResult a)
initSearch wave0 = do
  mFrame0 <- nextFrame wave0
  pure $ case mFrame0 of
    Nothing     -> Solved wave0
    Just frame0 -> Continue { stack: NonEmpty.singleton frame0, attempts: 0 }

-- Advance the search by exactly one unit of work: either backtrack out of
-- an exhausted frame, or try the next untried value at the current one.
-- This granularity — one pop, or one value-attempt — is what lets a caller
-- (e.g. the demo's worker) interleave the search with progress reporting
-- and cancellation checks, the same way it already does for plain
-- step-by-step solving via `WFC.Algorithm.step`.
stepSearch :: forall a. SearchState a -> Effect (StepResult a)
stepSearch st =
  let { head: frame, tail: rest } = NonEmpty.uncons st.stack in
  if Set.isEmpty frame.untried then
    -- exhausted every value at this cell; backtrack to the parent's
    -- decision, or fail outright if there's no parent left to try
    case NonEmpty.fromList rest of
      Nothing     -> pure (Failed (Contradiction frame.pos))
      Just parent -> pure (BackedOut (st { stack = parent }))
  else do
    mChosen <- weightedSample frame.wave frame.untried
    case mChosen of
      Nothing ->
        -- no weight left to draw from a non-empty `untried`; treat the
        -- same as exhausted rather than looping on it forever
        case NonEmpty.fromList rest of
          Nothing     -> pure (Failed (Contradiction frame.pos))
          Just parent -> pure (BackedOut (st { stack = parent }))
      Just value -> do
        let frame'           = frame { untried = Set.delete value frame.untried }
            stackWithRetried = NonEmpty.cons' frame' rest
            attempts'        = st.attempts + 1
        case attemptValue frame.wave frame.pos value of
          Left _ ->
            pure (Continue { stack: stackWithRetried, attempts: attempts' })
          Right wave' -> do
            mNext <- nextFrame wave'
            case mNext of
              Nothing   -> pure (Solved wave')
              Just next -> pure (Continue { stack: NonEmpty.cons next stackWithRetried, attempts: attempts' })

-- Solve by incremental backtracking: on a bad guess, undo just that guess
-- (ban the value, try another at the same cell) instead of restarting the
-- whole wave from scratch like `WFC.Algorithm.wfcWithRetry` does. Only once
-- every value at a cell has failed does the search actually unwind to the
-- previous cell's decision and try one of *its* remaining alternatives.
--
-- `maxAttempts` bounds the total number of individual value-attempts tried
-- across the whole search (not full restarts, unlike `wfcWithRetry`'s
-- budget of the same name) — a safety valve against a pathological or
-- genuinely-unsatisfiable ruleset searching forever.
solveWithBacktracking :: forall a. Int -> Wave a -> Effect (Either Contradiction (Wave a))
solveWithBacktracking maxAttempts wave0 = do
  first <- initSearch wave0
  tailRecM go first
  where
    go (Solved wave)   = pure (Done (Right wave))
    go (Failed err)    = pure (Done (Left err))
    go (BackedOut st)  = go (Continue st)
    go (Continue st)
      | st.attempts >= maxAttempts =
          pure (Done (Left (Contradiction (NonEmpty.uncons st.stack).head.pos)))
      | otherwise = do
          next <- stepSearch st
          pure (Loop next)
