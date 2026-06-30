module WFC.Algorithm where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import WFC.Collapse (collapseAt)
import WFC.Entropy (minEntropyPos)
import WFC.Propagate (Contradiction)
import WFC.Wave (Wave)

-- One WFC step: find the min-entropy cell, collapse it, propagate.
-- Returns:
--   Right (Just wave') — step succeeded, more cells remain
--   Right Nothing      — all cells collapsed (algorithm complete)
--   Left contradiction — constraint violation
step :: forall a. Wave a -> Effect (Either Contradiction (Maybe (Wave a)))
step wave = do
  mPos <- minEntropyPos wave
  case mPos of
    Nothing  -> pure (Right Nothing)   -- no superposed cells → done
    Just pos -> map (map Just) (collapseAt wave pos)

-- Full WFC algorithm: iterate steps until done or contradiction.
-- Stack-safe via tailRecM (MonadRec Effect instance).
wfc :: forall a. Wave a -> Effect (Either Contradiction (Wave a))
wfc initialWave = tailRecM go initialWave
  where
    go wave = do
      result <- step wave
      pure $ case result of
        Left err           -> Done (Left err)
        Right Nothing      -> Done (Right wave)
        Right (Just wave') -> Loop wave'

-- Restart on contradiction up to maxAttempts times.
wfcWithRetry :: forall a. Int -> Wave a -> Effect (Maybe (Wave a))
wfcWithRetry maxAttempts initialWave = tailRecM go maxAttempts
  where
    go 0 = pure (Done Nothing)
    go n = do
      result <- wfc initialWave
      pure $ case result of
        Right wave -> Done (Just wave)
        Left _     -> Loop (n - 1)
