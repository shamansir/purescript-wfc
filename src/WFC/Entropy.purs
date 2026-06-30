module WFC.Entropy where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, minimumBy)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (random)
import Data.Number (log)
import WFC.Grid (Pos)
import WFC.Pattern (PatternId)
import WFC.Wave (Wave)

-- Shannon entropy of a cell given the catalog's weight table.
-- H = ln(Σw) - (Σ w*ln(w)) / Σw, computed from scratch each time.
cellEntropy :: forall a. Wave a -> Set.Set PatternId -> Number
cellEntropy wave possible =
  let ws     = map (\pid -> case Map.lookup pid wave.catalog.weights of
                               Just w  -> w
                               Nothing -> 0.0)
                   (Set.toUnfoldable possible :: Array PatternId)
      totalW = foldl (+) 0.0 ws
      sumWLW = foldl (\acc w -> acc + w * log w) 0.0 ws
  in if totalW > 0.0
       then log totalW - sumWLW / totalW
       else 0.0

-- All non-collapsed, non-contradiction cells with their entropy.
cellsWithEntropy :: forall a. Wave a -> Array (Tuple Pos Number)
cellsWithEntropy wave =
  Array.mapMaybe go (Map.toUnfoldable wave.cells :: Array (Tuple Pos _))
  where
    go (Tuple _   Nothing)  = Nothing
    go (Tuple pos (Just s)) =
      if Set.size s <= 1
        then Nothing  -- collapsed or empty; skip
        else Just (Tuple pos (cellEntropy wave s))

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
    addNoise (Tuple pos e) = do
      noise <- random
      pure (Tuple pos (e + noise * 1.0e-6))
    cmpSnd (Tuple _ a) (Tuple _ b) = compare a b
