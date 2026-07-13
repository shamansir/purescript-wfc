module WFC.Propagate where

import Prelude

import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import WFC.Direction (Direction, allDirections, opposite)
import WFC.Grid (Pos(..), neighborPos, allPositions)
import WFC.Pattern (PatternId)
import WFC.Rules (lookupNeighbors)
import WFC.Wave (Cell, Wave)

newtype Contradiction = Contradiction Pos

instance showContradiction :: Show Contradiction where
  show (Contradiction pos) = "Contradiction at " <> show pos

-- A pending removal: position + tile to ban.
type BanEvent = Tuple Pos PatternId

type PropState a =
  { wave  :: Wave a
  , queue :: List BanEvent
  }

-- Read the compat count for (pos, pid, dir).
getCompat :: forall a. Wave a -> Pos -> PatternId -> Direction -> Int
getCompat wave pos pid dir =
  fromMaybe 0 $ do
    byPid <- Map.lookup pos wave.compat
    byDir <- Map.lookup pid byPid
    Map.lookup dir byDir

-- Decrement the compat count for (pos, pid, dir); return new count + updated wave.
decrementCompat
  :: forall a
  .  Pos -> PatternId -> Direction -> Wave a -> Tuple Int (Wave a)
decrementCompat pos pid dir wave =
  let newCount = getCompat wave pos pid dir - 1
      newCompat = Map.alter
        (map (Map.alter (map (Map.insert dir newCount)) pid))
        pos
        wave.compat
  in Tuple newCount (wave { compat = newCompat })

-- For each direction from pos, find neighbours that relied on pid
-- and decrement their compat counts, enqueuing new bans where count hits 0.
processNeighbours
  :: forall a
  .  PatternId
  -> Pos
  -> Wave a
  -> List BanEvent
  -> PropState a
processNeighbours pid pos wave0 queue0 =
  Array.foldl stepDir { wave: wave0, queue: queue0 } allDirections
  where
    stepDir st dir =
      case neighborPos wave0.size wave0.periodic pos dir of
        Nothing   -> st
        Just nPos ->
          let supported = lookupNeighbors wave0.rules dir pid
          in Array.foldl (stepTile nPos dir) st supported

    stepTile nPos dir st nPid =
      let Tuple newCount wave' = decrementCompat nPos nPid (opposite dir) st.wave
      in if newCount == 0
           then st { wave = wave', queue = (Tuple nPos nPid) : st.queue }
           else st { wave = wave' }

-- Remove pid from the cell at pos; propagate consequences.
processBan :: forall a. BanEvent -> PropState a -> Either Contradiction (PropState a)
processBan (Tuple pos pid) st =
  case Map.lookup pos st.wave.cells of
    Nothing        -> Left (Contradiction pos)
    Just Nothing   -> Left (Contradiction pos)
    Just (Just pids) ->
      if not (Set.member pid pids)
        then Right st  -- already removed; skip
        else
          let newPids = Set.delete pid pids
              newCell :: Cell
              newCell = if Set.isEmpty newPids then Nothing else Just newPids
              wave'   = st.wave { cells = Map.insert pos newCell st.wave.cells }
          in case newCell of
               Nothing -> Left (Contradiction pos)
               Just _  -> Right (processNeighbours pid pos wave' st.queue)

-- Drain the worklist until empty or a contradiction is found.
drainQueue :: forall a. PropState a -> Either Contradiction (Wave a)
drainQueue st =
  case st.queue of
    Nil      -> Right st.wave
    ev : rest ->
      case processBan ev (st { queue = rest }) of
        Left err  -> Left err
        Right st' -> drainQueue st'

-- Public entry: propagate starting from a list of initial ban events.
propagate :: forall a. Wave a -> Array BanEvent -> Either Contradiction (Wave a)
propagate wave initialBans =
  drainQueue { wave, queue: List.fromFoldable initialBans }

-- "Ground" heuristic (mirrors the original C# WFC's per-sample `ground`
-- flag): forces `groundPid` onto every cell of the bottom row, and bans it
-- from every other row, then propagates the consequences. Matches
-- OverlappingModel's Clear(): ground = T-1, the last pattern id assigned by
-- extraction (see WFC.Catalog.extractPatterns's scan order).
applyGround :: forall a. PatternId -> Wave a -> Either Contradiction (Wave a)
applyGround groundPid wave =
  propagate wave (allPositions wave.size >>= bansFor)
  where
    bottomY = wave.size.height - 1

    bansFor pos@(Pos { y }) =
      case Map.lookup pos wave.cells of
        Just (Just pids) ->
          if y == bottomY
            then map (Tuple pos) (Array.filter (_ /= groundPid) (Set.toUnfoldable pids))
            else if Set.member groundPid pids
                   then [ Tuple pos groundPid ]
                   else []
        _ -> []
