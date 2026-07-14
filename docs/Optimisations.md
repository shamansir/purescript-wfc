# Performance optimization notes (src/WFC engine)

Investigation into why the WFC solver is slower than it needs to be, scoped to
`src/WFC` only (Demo excluded). Grounded in the actual compiled JS output
(`output/`), not just PureScript source reading — key facts confirmed there:
newtypes (`Pos`, `PatternId`, `AdjacencyRules`) erase to their underlying
representation with zero wrapper cost; `Data.Map`/`Data.Set` are size-balanced
trees with path-copying inserts (`Data.Map.Internal`'s `insert`/`alter` allocate
a new `Node` per level touched); `Data.Array.updateAt` is **not** persistent —
its FFI does `l.slice()`, a full O(n) copy, on every call.

**Constraint that shapes every fix below**: `WFC.Backtrack`'s `Frame` holds a
full `Wave` per stack entry and relies on it being O(1) to snapshot (persistent
Map/Set structural sharing). Any fix must preserve that — no raw mutable JS
structures for anything reachable from `Wave`.

## Findings, ranked by impact

1. **`minEntropyPos` recomputes entropy for every open cell from scratch, every
   step** (`WFC.Entropy.cellsWithEntropy`/`WFC.Algorithm.step`). O(cells ×
   avg possibilities) per step. Original C# WFC keeps running sums
   (`sumsOfWeights`/`sumsOfWeightLogWeights`) updated incrementally in `Ban()`.
   **Status: implemented** (entropy cache added to `Wave`, updated in
   `WFC.Propagate.processBan`; `cellEntropy`'s public signature/behavior
   unchanged for arbitrary sets). **Measured**: 10–20% faster on a
   322-pattern/up-to-3600-cell benchmark, growing with grid size — real but
   smaller than the static analysis alone predicted, since compat-map churn
   (#2) turned out to dominate more at these scales.

2. **`CompatMap = Map Pos (Map PatternId (Map Direction Int))`** — three
   nested trees, so every `decrementCompat` call pays three separate
   tree-rebalance allocations, keyed by three different comparators (`Pos`'s
   2-field record compare, `PatternId`'s already-free int compare, and
   `Direction`'s `instanceof` chain). **Status: in progress** — flattening
   pos/pid/dir into one combined `Int` key, one `Map Int x` instead of three,
   wrapped in a phantom-typed newtype (renamed `CompatibilityMap` per
   discussion) for a coherent `Map a x -> CompatibilityMap a x` feel at call
   sites. Kept `Map`-backed (not a raw `Array`) specifically because `Array`
   isn't persistent — confirmed via `Data.Array`'s FFI, `_updateAt` does
   `l.slice()` (full copy) every call, which would regress backtracking's
   many-small-attempts pattern.

3. **Cell possibilities are `Set PatternId`** (`Data.Set a = Map a Unit`,
   same tree cost as above). Since `PatternId` is contiguous `0..T-1` and real
   catalogs run 80–150+ patterns, a bitset (`Array Int`, one word per 32
   patterns) would make delete/member/size O(1)/O(words) instead of
   O(log T), while the *outer* `Map Pos Cell` stays exactly as persistent as
   today (backtracking-safe). More finicky than #2 (multi-word bit
   arithmetic, bit-scan iteration for weighted sampling) — do after #2, only
   if still needed.

4. **`PatternCatalog`'s `patterns`/`weights`/`wLogW`/`origins` are all
   `Map PatternId X`** despite being built once in `finalize` and never
   mutated again. Since `PatternId` is dense `0..T-1`, these should just be
   `Array X` indexed directly — zero tradeoff, since there's no mutation to
   preserve sharing for. Turns every weight/wLogW lookup in
   `cellEntropy`/`weightedSample` (hot path) from O(log T) to O(1).

5. **`AdjacencyRules = Map Direction (Map PatternId (Array PatternId))`** —
   also built once (`buildRules`/`buildTiledRules`/`WFC.TileSet.buildTileSet`)
   and read very hot (`lookupNeighbors`, 4×/ban in `processNeighbours`). Same
   fix as #4: `Array (Array (Array PatternId))` indexed by `[dirTag][pidIdx]`.
   `AdjacencyRules` is already an opaque newtype, so this is invisible to
   callers.

**Lower priority**: `extractPatterns`'s dedup (`Map (Pattern a) PatternId`)
compares whole N×N pattern arrays as a map key — real cost, but paid once at
Extract, not per solving step, so it matters far less than 1–5.

## Notes on the Array-vs-Map decision

- For #4/#5 (read-only after construction): plain `Array`, wrapped in a small
  phantom-typed newtype for `Map`-like ergonomics, is both simpler *and*
  faster than any general-purpose IntMap package — for a dense, contiguous,
  never-mutated key range, "array indexed by int" already *is* the optimal
  int-map. No dependency needed.
- For #2/#3 (mutated thousands of times per solve, must stay cheap to
  snapshot): a real `Array` would need `Effect`/`ST`-based thaw-mutate-freeze
  per `propagate` call to avoid the O(n)-copy-per-update problem — viable in
  principle (nothing outside a single `propagate` call observes the
  intermediate mutable state, so `runST` could keep it pure at the type
  level), but its win depends on how many cascading bans happen per
  `propagate` call vs. the grid's total compat-array size; untested, so
  parked in favor of the safer `Map Int x` flattening for now.
- Searched Pursuit for an existing "`Map Int` backed by `Array`" package —
  found `purescript-intmap`/`purescript-intmaps` (Patricia-tree-backed, not
  array-backed) but nothing that matches the dense/array-backed shape; none
  needed for #4/#5 since that's just `Array` itself, and #2/#3 use `Map Int x`
  from the already-installed `ordered-collections`.