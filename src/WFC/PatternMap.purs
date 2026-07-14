module WFC.PatternMap
  ( PatternMap
  , PatternCount(..)
  , empty
  , length
  , index
  , fromArray
  , toArray
  , ids
  , withIds
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import WFC.Pattern (PatternId(..))

-- | An `Array x` with exactly one entry per `PatternId` in a catalog,
-- | indexed directly by that `PatternId`'s own underlying `Int` — the
-- | natural representation since `PatternId`s are contiguous `0..T-1` by
-- | construction (`WFC.Catalog.accumulatePattern` only ever assigns the
-- | next sequential id). A function that takes/returns a `PatternMap x`
-- | says "one `x` per pattern" directly in its type, instead of a bare
-- | `Array x` (indistinguishable from any other array) plus a comment
-- | explaining the indexing convention.
-- |
-- | Erases to the plain `Array` at runtime, same as `PatternId` erasing to
-- | `Int` — a `newtype`, not a `data` type, so wrapping/unwrapping costs
-- | nothing. Read-only by design (no `insert`/`update`): built once via
-- | `fromArray` (see `WFC.Catalog.finalize`) and never mutated again. A
-- | table that *is* mutated over and over (`WFC.Wave`'s per-cell
-- | compatibility/entropy state) wants `WFC.CompatibilityMap` instead —
-- | see that module's docs for why the two shouldn't be conflated.
newtype PatternMap x = PatternMap (Array x)

derive instance functorPatternMap :: Functor PatternMap
derive newtype instance foldablePatternMap :: Foldable PatternMap

-- | T, the total number of patterns in a catalog — distinct from
-- | `WFC.Pattern.PatternSize` ("N", a single pattern's own width/height),
-- | a pair of concepts the original WFC literature itself distinguishes
-- | (N vs. T) but that are easy to mix up as bare `Int`s.
newtype PatternCount = PatternCount Int

derive newtype instance eqPatternCount :: Eq PatternCount
derive newtype instance ordPatternCount :: Ord PatternCount
derive newtype instance showPatternCount :: Show PatternCount

empty :: forall x. PatternMap x
empty = PatternMap []

length :: forall x. PatternMap x -> PatternCount
length (PatternMap arr) = PatternCount (Array.length arr)

index :: forall x. PatternMap x -> PatternId -> Maybe x
index (PatternMap arr) (PatternId i) = Array.index arr i

-- | Build a `PatternMap` from an `Array` already in `PatternId` order (id 0
-- | first, then 1, 2, ...) — the shape `Data.Map.toUnfoldable` on a
-- | `PatternId`-keyed `Map` hands back, since it's ordered ascending by key
-- | and `PatternId`s are contiguous, so dropping the keys is enough (see
-- | `WFC.Catalog.finalize`).
fromArray :: forall x. Array x -> PatternMap x
fromArray = PatternMap

toArray :: forall x. PatternMap x -> Array x
toArray (PatternMap arr) = arr

-- | Every `PatternId` in the map, in order (id 0, 1, 2, ...).
ids :: forall x. PatternMap x -> Array PatternId
ids (PatternMap arr) =
  let t = Array.length arr
  in if t <= 0 then [] else map PatternId (Array.range 0 (t - 1))

-- | Every entry tagged with its own id — a drop-in replacement for the
-- | `Array (Tuple PatternId x)` shape `Data.Map.toUnfoldable` used to hand
-- | back, at call sites that want every pattern alongside its id (rendering
-- | a pattern list, etc.), not just looking one up.
withIds :: forall x. PatternMap x -> Array (Tuple PatternId x)
withIds (PatternMap arr) = Array.mapWithIndex (\i x -> Tuple (PatternId i) x) arr
