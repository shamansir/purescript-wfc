module WFC.CompatibilityMap
  ( CompatibilityMap
  , empty
  , lookup
  , insert
  , update
  , fromFoldable
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

-- | A `Map Int x`, wrapped with a phantom `a` naming which logical key
-- | space the `Int`s come from (e.g. `WFC.Wave`'s combined
-- | position+pattern+direction index). The phantom carries no runtime
-- | value — it's erased right along with the newtype itself — it just
-- | stops two different "which Int means what" index spaces from
-- | type-checking as interchangeable by accident, the same job `PatternId`
-- | does for a plain `Int` pattern id.
-- |
-- | Backed by `Data.Map`, not a raw `Array`: this is meant for maps that
-- | get mutated over and over (e.g. once per constraint-propagation ban)
-- | while older versions must stay around and cheap to hold onto (see
-- | `WFC.Backtrack`'s `Frame`, which keeps old `Wave` values on its search
-- | stack indefinitely). `Data.Map` is a persistent, path-copying tree —
-- | updating it shares every untouched branch with whatever older value
-- | still points at it. A plain `Array` has no such sharing in PureScript
-- | (`Data.Array.updateAt` copies the entire array via
-- | `Array.prototype.slice`), so it would turn every single update into a
-- | full copy. For read-only-after-construction data (e.g. a pattern
-- | catalog's per-pattern tables), reach for a plain `Array` instead —
-- | there's nothing to share against once nothing ever mutates it again.
newtype CompatibilityMap :: forall k. k -> Type -> Type
newtype CompatibilityMap a x = CompatibilityMap (Map Int x)

empty :: forall a x. CompatibilityMap a x
empty = CompatibilityMap Map.empty

lookup :: forall a x. Int -> CompatibilityMap a x -> Maybe x
lookup k (CompatibilityMap m) = Map.lookup k m

insert :: forall a x. Int -> x -> CompatibilityMap a x -> CompatibilityMap a x
insert k v (CompatibilityMap m) = CompatibilityMap (Map.insert k v m)

update :: forall a x. (x -> Maybe x) -> Int -> CompatibilityMap a x -> CompatibilityMap a x
update f k (CompatibilityMap m) = CompatibilityMap (Map.update f k m)

fromFoldable :: forall a x f. Foldable f => f (Tuple Int x) -> CompatibilityMap a x
fromFoldable = CompatibilityMap <<< Map.fromFoldable
