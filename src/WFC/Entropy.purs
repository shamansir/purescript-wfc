module WFC.Entropy where

import Prelude

import Data.Array as Array
import Data.Foldable (minimumBy)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (random)
import WFC.Grid (Pos)
import WFC.Pattern (PatternId)
import WFC.Wave (Entropy(..), Wave, entropyFromStats, statsForSet)

-- Shannon entropy of an arbitrary possibility set, given the catalog's
-- weight table — H = ln(Σw) - (Σ w*ln(w)) / Σw, summed from `possible`
-- itself every call (this is the general-purpose entrypoint for a
-- hypothetical/explicit set; `cellsWithEntropy` below uses the wave's
-- incrementally-maintained cache instead, since it always wants a cell's
-- *actual current* possibilities).
cellEntropy :: forall a. Wave a -> Set.Set PatternId -> Entropy
cellEntropy wave possible = entropyFromStats (statsForSet wave.catalog possible)

-- All non-collapsed, non-contradiction cells with their entropy. Reads each
-- cell's entropy from `wave.entropy` (kept up to date one ban at a time by
-- WFC.Propagate.processBan) instead of re-summing that cell's whole
-- possibility set here — this is what makes calling `minEntropyPos` once
-- per solving step cheap regardless of how many patterns remain possible.
cellsWithEntropy :: forall a. Wave a -> Array (Tuple Pos Entropy)
cellsWithEntropy wave =
  Array.mapMaybe go (Map.toUnfoldable wave.cells :: Array (Tuple Pos _))
  where
    go (Tuple _   Nothing)  = Nothing
    go (Tuple pos (Just s)) =
      if Set.size s <= 1
        then Nothing  -- collapsed or empty; skip
        else Just (Tuple pos (entropyOf pos s))

    -- Falls back to a direct recompute if a position is somehow missing
    -- from the cache (shouldn't happen — every `cells` mutation site keeps
    -- `entropy` in lockstep — but this keeps `cellsWithEntropy` correct
    -- rather than silently wrong if that invariant is ever broken).
    entropyOf pos s = case Map.lookup pos wave.entropy of
      Just stats -> entropyFromStats stats
      Nothing    -> cellEntropy wave s

-- Position with minimum entropy, with small random noise for tie-breaking.
-- Returns Nothing when all cells are collapsed (algorithm complete).
minEntropyPos :: forall a. Wave a -> Effect (Maybe Pos)
minEntropyPos wave = do
  let candidates = cellsWithEntropy wave
  case Array.head candidates of
    Nothing -> pure Nothing
    Just _  -> do
      noisied <- traverse addNoise candidates
      pure $ map (\(Tuple pos _) -> pos) (minimumBy cmpSnd noisied)
  where
    addNoise (Tuple pos (Entropy e)) = do
      noise <- random
      pure (Tuple pos (Entropy (e + noise * 1.0e-6)))
    cmpSnd (Tuple _ a) (Tuple _ b) = compare a b
