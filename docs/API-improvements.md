# API improvement: stop passing bare `Boolean`/`Int` around

Not an optimization — a readability/type-safety change. Parked separately from
`docs/Optimisations.md` because it's bigger (touches call sites in
`Demo.App`/`Demo.Worker`/`Test.Main`, not just `src/WFC` internals) and
shouldn't ride along with a perf-only merge to `main`.

## The rule

Every function argument or return value that's a bare `Boolean`, `Int`, or
other generic primitive should be a `newtype` instead, *if* it's part of the
API (which in this codebase means: everything, since modules use `module Foo
where` with no export list). Costs nothing at runtime (newtypes erase, see
`docs/Optimisations.md`'s compiled-JS notes) and turns call sites like
`extractPatterns 3 true false true grid` — four positional
`Int`/`Boolean`/`Boolean`/`Boolean`, no way to tell which is which without
opening the source — into something self-describing.

## Proof of concept: `CompatibilityMapKey`

Already done, on the `optimisations` branch. `WFC.CompatibilityMap`'s
`lookup`/`insert`/`update` used to take a bare `Int` key — nothing stopped a
key encoded for one index space from being handed to a *different*
`CompatibilityMap`. Now:

```purescript
newtype CompatibilityMapKey a = CompatibilityMapKey Int
lookup :: forall a x. CompatibilityMapKey a -> CompatibilityMap a x -> Maybe x
```

tagged with the same phantom `a` as the map — the compiler rejects a
mismatched key instead of silently reading/writing the wrong slot. This is
the pattern to replicate below: not just "wrap it," but wrap it so the
wrapper actually buys back a guarantee the bare primitive couldn't express.

## Inventory: where bare primitives show up in `src/WFC`

Grouped by the primitive, since a handful of repeating parameters
(`periodic`, `n`/pattern size, `useRotations`/`useMirror`, `maxAttempts`)
account for most of the noise — one newtype each covers many signatures at
once, more coherent than one-off wrapping per function.

### `periodic :: Boolean`

- `WFC.Wave.initWave :: PatternCatalog a -> AdjacencyRules -> GridSize -> Boolean -> Wave a`
- `WFC.Wave`'s `Wave a` record field `periodic :: Boolean`
- `WFC.Grid.neighborPos :: GridSize -> Boolean -> Pos -> Direction -> Maybe Pos`
- `WFC.Catalog.extractPatterns`/`sampleAt`/`patternAt` (input-side periodic, see below)

Candidate: `newtype Periodic = Periodic Boolean` (or `IsPeriodic`). Note
`extractPatterns` and `initWave` both take "periodic" but mean *input-wrap*
vs *output-wrap* respectively (the demo layer keeps these as two separately
tracked booleans, `inputPeriodic`/`outputPeriodic`) — if wrapped, consider
two distinct newtypes (`InputPeriodic`/`OutputPeriodic`) rather than one
`Periodic` reused for both meanings, so a mix-up between the two is also a
type error, not just a naming convention.

### `n :: Int` (pattern size, N×N)

- `WFC.Catalog.extractPatterns`/`sampleAt`/`patternAt`
- `WFC.Pattern.rotate`/`reflect`/`patternGet`/`taggedVariantsFor`/`variantsFor`/`agrees`
- `WFC.Catalog.finalize`'s second arg, `PatternCatalog.size` field

Candidate: `newtype PatternSize = PatternSize Int`. Highest fan-out of any
single wrap here — appears in nearly every `Pattern`/`Catalog` function.

### `useRotations`/`useMirror :: Boolean`

- `WFC.Catalog.extractPatterns`
- `WFC.Pattern.taggedVariantsFor`/`variantsFor`

Candidates: `newtype UseRotations = UseRotations Boolean`,
`newtype UseMirror = UseMirror Boolean`. Two separate newtypes, not one
shared "options" boolean pair — `extractPatterns 3 true true false` reads no
better than the un-wrapped version if both flags share a type and can still
be transposed.

### `maxAttempts :: Int`

- `WFC.Algorithm.wfcWithRetry :: Int -> Wave a -> Effect (Maybe (Wave a))`
- `WFC.Backtrack.solveWithBacktracking :: Int -> Wave a -> Effect (Either Contradiction (Wave a))`

Candidate: `newtype MaxAttempts = MaxAttempts Int`. Low fan-out (2 call
sites in `src/WFC`, but also used from `Demo.Worker`), easy first target.

### One-off `Int`/`Boolean` (lower priority, single call site each)

- `WFC.TileSet.Symmetry`'s `Int` orientation indices (`rotateIndex`,
  `rotateIndexBy`, `reflectIndex`, `distinctOrientations`, `cardinality`) —
  candidate `newtype OrientationIndex = OrientationIndex Int`, would also
  clean up `WFC.TileSet.TileInstance`'s `orientation :: Int` field.
- `WFC.TileSet.Xml.splitNameRot`'s returned `{ rotation :: Int }` — same
  `OrientationIndex` candidate.
- `WFC.Direction.dirIndex :: Direction -> Int` — return value, feeds
  directly into `WFC.Wave.compatibilityKey`'s arithmetic; low priority since
  it's already fully internal (never crosses a public call site un-wrapped).

## Not in scope for this pass

- `PatternCatalog`/`AdjacencyRules`'s internal `Map`/`Array` value types —
  covered by `docs/Optimisations.md` findings #4/#5 instead (a
  representation change, not a primitive-wrapping one).
- Record fields that already have a self-documenting label (e.g.
  `EntropyStats`'s `sumW`/`sumWLogW :: Number`, `GridSize`'s
  `width`/`height :: Int`) — the label already carries the meaning a
  newtype would add for a bare positional argument; wrapping every labeled
  record field too would be scope creep without a matching readability win.

## Why this is separate from the `optimisations` branch merge

Every wrap above needs its call sites updated too — `extractPatterns`,
`initWave`, and `wfcWithRetry`/`solveWithBacktracking` alone are called from
`Demo.App`, `Demo.Worker`, and `Test.Main` in dozens of places. Bundling that
into the same merge as the entropy-cache/`CompatibilityMap` perf work would
make it hard to `git bisect`/revert one without the other if either turns out
to need adjustment later. Do it as its own branch/PR, after deciding (see
"Inventory" above) which primitives are worth it — probably not the
single-call-site ones.
