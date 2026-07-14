module WFC.Collapse where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Random (random)
import WFC.Catalog (Weight(..), weightOf)
import WFC.Grid (Pos)
import WFC.Pattern (PatternId)
import WFC.Propagate (BanEvent, Contradiction(..), propagate)
import WFC.Wave (Wave)

-- Weighted random pick from (pid, weight) pairs.
-- threshold should be in [0, totalWeight).
pickWeighted :: Weight -> Array (Tuple PatternId Weight) -> Maybe PatternId
pickWeighted (Weight threshold) ws = go 0.0 ws
  where
    go acc arr = case Array.uncons arr of
      Nothing -> map fst (Array.last ws)  -- fallback
      Just { head: Tuple pid (Weight w), tail: rest } ->
        if acc + w >= threshold then Just pid else go (acc + w) rest

-- Sample one pattern ID from the possibility set using catalog weights.
weightedSample :: forall a. Wave a -> Set.Set PatternId -> Effect (Maybe PatternId)
weightedSample wave possible = do
  let pids   = Set.toUnfoldable possible :: Array PatternId
      ws     = map (\pid -> Tuple pid (weightOf wave.catalog pid)) pids
      totalW = foldl (\acc (Tuple _ (Weight w)) -> acc + w) 0.0 ws
  if totalW <= 0.0
    then pure Nothing
    else do
      r <- map (_ * totalW) random
      pure (pickWeighted (Weight r) ws)

-- Collapse the cell at pos: pick one pattern, ban all others, propagate.
collapseAt :: forall a. Wave a -> Pos -> Effect (Either Contradiction (Wave a))
collapseAt wave pos =
  case Map.lookup pos wave.cells of
    Nothing       -> pure (Left (Contradiction pos))
    Just Nothing  -> pure (Left (Contradiction pos))
    Just (Just s) -> do
      mChosen <- weightedSample wave s
      case mChosen of
        Nothing     -> pure (Left (Contradiction pos))
        Just chosen ->
          -- let wave'   = setCell pos (Just (Set.singleton chosen)) wave
          --     toBan   = Array.filter (_ /= chosen) (Set.toUnfoldable s :: Array PatternId)
          --     banEvts = map (Tuple pos) toBan :: Array BanEvent
          -- in pure (propagate wave' banEvts)
          let toBan   = Array.filter (_ /= chosen) (Set.toUnfoldable s :: Array PatternId)
              banEvts = map (Tuple pos) toBan :: Array BanEvent
          in pure (propagate wave banEvts)

