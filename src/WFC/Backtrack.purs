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

type SearchState a =
  { stack    :: NonEmptyList (Frame a)
  , attempts :: Int
  }

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
  mFrame0 <- nextFrame wave0
  case mFrame0 of
    Nothing     -> pure (Right wave0) -- already fully collapsed
    Just frame0 -> tailRecM go { stack: NonEmpty.singleton frame0, attempts: 0 }
  where
    go st =
      let { head: frame, tail: rest } = NonEmpty.uncons st.stack in
      if st.attempts >= maxAttempts then
        pure (Done (Left (Contradiction frame.pos)))
      else if Set.isEmpty frame.untried then
        -- exhausted every value at this cell; backtrack to the parent's
        -- decision, or fail outright if there's no parent left to try
        case NonEmpty.fromList rest of
          Nothing     -> pure (Done (Left (Contradiction frame.pos)))
          Just parent -> pure (Loop (st { stack = parent }))
      else do
        mChosen <- weightedSample frame.wave frame.untried
        case mChosen of
          Nothing ->
            -- no weight left to draw from a non-empty `untried`; treat the
            -- same as exhausted rather than looping on it forever
            case NonEmpty.fromList rest of
              Nothing     -> pure (Done (Left (Contradiction frame.pos)))
              Just parent -> pure (Loop (st { stack = parent }))
          Just value -> do
            let frame'          = frame { untried = Set.delete value frame.untried }
                stackWithRetried = NonEmpty.cons' frame' rest
            case attemptValue frame.wave frame.pos value of
              Left _ ->
                pure (Loop { stack: stackWithRetried, attempts: st.attempts + 1 })
              Right wave' -> do
                mNext <- nextFrame wave'
                case mNext of
                  Nothing   -> pure (Done (Right wave'))
                  Just next -> pure (Loop { stack: NonEmpty.cons next stackWithRetried, attempts: st.attempts + 1 })
