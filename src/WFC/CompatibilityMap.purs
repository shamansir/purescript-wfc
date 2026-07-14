module WFC.CompatibilityMap
  ( CompatibilityMap
  , CompatibilityKey(..)
  , CompatibilityCount(..)
  , empty
  , lookup
  , insert
  , fromFoldable
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

-- | A key into a `CompatibilityMap` — a combined `PatternId × Direction`
-- | index folded into one `Int` (see `WFC.Wave.compatibilityKey`), wrapped
-- | so it can't be confused with an unrelated `Int` at a call site, the
-- | same job `PatternId` does for a raw pattern id.
newtype CompatibilityKey = CompatibilityKey Int

derive newtype instance eqCompatibilityKey :: Eq CompatibilityKey
derive newtype instance ordCompatibilityKey :: Ord CompatibilityKey
derive newtype instance showCompatibilityKey :: Show CompatibilityKey

-- | compat[pid][dir] = how many tiles in the direction-dir neighbour of a
-- | position still support pid being there — see `WFC.Wave`'s
-- | `compatibility` field, the one and only place this type is used (not
-- | generic — an earlier version was parameterized over the map's key
-- | *and* value type "for reuse", but nothing else ever needed either
-- | parameter, so it was just ceremony around a `Map CompatibilityKey Int`).
-- |
-- | Backed by `Data.Map`, not a raw `Array`: mutated on every single
-- | constraint-propagation ban while `WFC.Backtrack`'s search keeps old
-- | `Wave` values around indefinitely for cheap snapshotting. `Data.Map` is
-- | a persistent, path-copying tree — updating it shares every untouched
-- | branch with whatever older value still points at it. A plain `Array`
-- | has no such sharing in PureScript (`Data.Array.updateAt` copies the
-- | entire array via `Array.prototype.slice`), so it would turn every
-- | single update into a full copy.
newtype CompatibilityMap = CompatibilityMap (Map CompatibilityKey CompatibilityCount)

-- | How many tiles in some direction-dir neighbour still support a given
-- | pattern being at the current cell — the value `CompatibilityMap`
-- | stores, wrapped so it can't be confused with an unrelated `Int` (a
-- | pattern id, a direction index, ...) at a call site.
newtype CompatibilityCount = CompatibilityCount Int

derive newtype instance eqCompatibilityCount :: Eq CompatibilityCount
derive newtype instance ordCompatibilityCount :: Ord CompatibilityCount
derive newtype instance showCompatibilityCount :: Show CompatibilityCount

empty :: CompatibilityMap
empty = CompatibilityMap Map.empty

lookup :: CompatibilityKey -> CompatibilityMap -> Maybe CompatibilityCount
lookup k (CompatibilityMap m) = Map.lookup k m

insert :: CompatibilityKey -> CompatibilityCount -> CompatibilityMap -> CompatibilityMap
insert k v (CompatibilityMap m) = CompatibilityMap (Map.insert k v m)

fromFoldable :: forall f. Foldable f => f (Tuple CompatibilityKey CompatibilityCount) -> CompatibilityMap
fromFoldable = CompatibilityMap <<< Map.fromFoldable
