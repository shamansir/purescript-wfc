# WFC in this repo — implementation vs. the algorithm

Maps [`context/WFC.md`](WFC.md)'s language-independent description of Wave
Function Collapse onto this repo's actual PureScript modules (`src/WFC/*`,
plus the demo's `test/Demo/src/Demo/Worker.purs`), with real excerpts. Ends
with a list of places where the current code diverges from — or simply
doesn't yet implement — something `WFC.md` describes, to plan follow-up
work from.

## Module map

| `WFC.md` concept | This repo |
|---|---|
| Wave (whole grid of domains) | `WFC.Wave.Wave` |
| Cell / domain / superposition | `WFC.Wave.Cell = Maybe (Set PatternId)` |
| Value / pattern | `WFC.Pattern.Pattern`, identified by `PatternId` |
| Adjacency constraint | `WFC.Rules.AdjacencyRules` |
| Support count | `WFC.Wave.CompatMap` |
| Observation / Collapse | `WFC.Collapse.collapseAt` |
| Propagation | `WFC.Propagate.propagate` / `processBan` / `processNeighbours` |
| Contradiction | `WFC.Propagate.Contradiction` |
| Entropy heuristic | `WFC.Entropy.cellEntropy` / `minEntropyPos` |
| Main loop | `WFC.Algorithm.step` / `wfc` / `wfcWithRetry` |
| Incremental backtracking | `WFC.Backtrack.solveWithBacktracking` |
| Overlapping-model extraction | `WFC.Catalog.extractPatterns` |
| Automatic rule derivation | `WFC.Rules.buildRules` / `WFC.Pattern.agrees` |
| Reconstructing the output | `WFC.Render.renderWave` |

Only the **overlapping model** is implemented — there is no hand-authored
tiled/Wang-tile mode; every `SampleDef` supplies a source grid, not a
tileset+socket table (see [Differences](#differences-from-the-wfcmd-description) below).

## Walking the main loop

### 1–2. Define input & initialize (`WFC.Catalog`, `WFC.Rules`, `WFC.Wave`)

`WFC.md`'s "define values + adjacency rules, then initialize every cell to
full superposition" is three separate calls in this codebase, each producing
one immutable value fed into the next:

```purescript
cat   = extractPatterns sample.n sample.periodic 1 sample.grid  -- values + weights
rules = buildRules cat                                          -- adjacency
wave  = initWave cat rules { width: sample.outW, height: sample.outH } sample.periodic
```

`initWave` is the literal "every cell starts in full superposition" step —
every cell gets the full set of pattern IDs, and every `compat` entry gets
its starting support count (`initialCompatCount`, see
[Propagation](#4-propagate-wfcpropagate) below):

```purescript
-- WFC.Wave
initWave catalog rules size periodic =
  let ids      = ...
      allPids  = Set.fromFoldable ids
      initCell = Just allPids
      cellComp = initialCellCompat rules ids
      ...
  in { cells, compat, catalog, rules, size, periodic }
```

### 3. Observe / Collapse (`WFC.Entropy`, `WFC.Collapse`)

`WFC.md`'s entropy formula is implemented essentially verbatim:

```purescript
-- WFC.Entropy — H = ln(Σw) - (Σ w·ln w) / Σw
cellEntropy wave possible =
  let ws     = <weights of patterns still possible in this cell>
      totalW = foldl (+) 0.0 ws
      sumWLW = foldl (\acc w -> acc + w * log w) 0.0 ws
  in if totalW > 0.0 then log totalW - sumWLW / totalW else 0.0
```

`minEntropyPos` restricts candidates to genuinely undecided cells
(`Set.size s > 1` — collapsed and contradiction cells are excluded via
`cellsWithEntropy`'s filter), then adds the small random jitter `WFC.md`
calls out for tie-breaking, before picking the minimum:

```purescript
addNoise (Tuple pos e) = do
  noise <- random
  pure (Tuple pos (e + noise * 1.0e-6))
```

Collapse (`WFC.Collapse.collapseAt`) is "randomly pick one value, weighted
by frequency" (`weightedSample`/`pickWeighted`, a standard weighted-draw:
roll a threshold in `[0, totalWeight)`, walk the list accumulating weight
until it's exceeded), followed immediately by banning every other value
that was in the cell — which is what actually drives propagation (see
next section):

```purescript
collapseAt wave pos = case Map.lookup pos wave.cells of
  Just (Just s) -> do
    mChosen <- weightedSample wave s
    case mChosen of
      Just chosen ->
        let toBan   = Array.filter (_ /= chosen) (Set.toUnfoldable s)
            banEvts = map (Tuple pos) toBan
        in pure (propagate wave banEvts)
      ...
```

### 4. Propagate (`WFC.Propagate`)

This is the one place the implementation goes noticeably further than the
plain description in `WFC.md` — it implements the **support-count caching**
optimization from `WFC.md`'s [Propagation mechanics and
efficiency](WFC.md#propagation-mechanics-and-efficiency) section directly,
rather than the naive "re-check all constraints" approach:

```purescript
-- WFC.Wave — support counts, not just possibility sets
-- compat[pos][pid][dir] = number of tiles in the direction-dir neighbour of pos
-- that still support pid being at pos.  Reaches 0 → pid must be banned.
type CompatMap = Map Pos (Map PatternId (Map Direction Int))
```

`processBan`/`processNeighbours`/`drainQueue` are exactly `WFC.md`'s
worklist/flood-fill shape: a queue of pending bans, drained one at a time,
each one decrementing neighbours' support counts and enqueuing a new ban
only when a count hits zero — never a full grid re-scan:

```purescript
drainQueue st = case st.queue of
  Nil       -> Right st.wave
  ev : rest -> case processBan ev (st { queue = rest }) of
    Left err  -> Left err
    Right st' -> drainQueue st'
```

`WFC.Rules.buildRules`/`initialCompatCount` populate the initial support
counts from the automatically-derived adjacency (see next section) — this
is the concrete realization of `WFC.md`'s "maintain a running count per
(cell, value, direction) that only ever decreases."

### 5–6. Contradiction & retry (`WFC.Propagate`, `WFC.Algorithm`)

A `Contradiction Pos` is returned the moment any cell's possibility set is
banned down to empty (`processBan`'s `Set.isEmpty newPids` check). Recovery
is the **restart** strategy `WFC.md` documents as an accepted simpler
alternative to true backtracking — throw the whole wave away and try again
from a fresh `initWave`, up to a bounded number of attempts:

```purescript
-- WFC.Algorithm
wfcWithRetry maxAttempts initialWave = tailRecM go maxAttempts
  where
    go 0 = pure (Done Nothing)
    go n = do
      result <- wfc initialWave
      pure $ case result of
        Right wave -> Done (Just wave)
        Left _     -> Loop (n - 1)
```

Note `wfc initialWave` is called fresh each retry with the *same*
`initialWave` value — since it's immutable, "restart" is just "run the pure
algorithm again from the untouched starting state," no explicit reset step
needed.

`step` (one collapse+propagate cycle) is the atomic unit both `wfc` (run to
completion or failure) and the demo's own worker loop are built from — see
[Where the demo's loop differs](#where-the-demos-loop-differs) below.

## Incremental backtracking (`WFC.Backtrack`)

Added 2026-07-08 alongside `wfc`/`wfcWithRetry`, not replacing them (the
demo's worker still uses restart-based solving). `WFC.md` documents
restart-on-failure and true backtracking — "undo just the last guess, ban
that value, retry" — as two accepted contradiction-recovery strategies;
this module is the second one.

The key move that made this cheap: `Wave a` is a plain immutable record
over `Data.Map`/`Data.Set` (persistent, structurally-shared trees), so
snapshotting the *whole* wave before every guess costs nothing like a deep
copy — unrelated branches are shared. That sidesteps needing `Contradiction`
to carry any more state than it already does (still just the failing
`Pos`): a search frame just pairs the pre-guess `Wave a` snapshot with the
possibilities not yet tried at that cell —

```purescript
type Frame a = { wave :: Wave a, pos :: Pos, untried :: Set PatternId }
```

— and the loop is an explicit `NonEmptyList (Frame a)` stack, driven by
`tailRecM` for stack-safety exactly like `WFC.Algorithm` already does: try
a weighted-random pick from `untried` (reusing `WFC.Collapse.weightedSample`
against a shrinking subset, not the full cell domain) via `propagate`; on
`Left`, loop again on the *same* frame with that value removed from
`untried`; on `Right`, push a new frame for the next lowest-entropy cell
and recurse forward; once a frame's `untried` is empty, pop it — the parent
frame's own `untried` is already correctly reduced from when it pushed the
now-abandoned child, so the loop naturally tries the parent's next
alternative. A `maxAttempts` budget (same shape as `wfcWithRetry`'s, but
counting individual value-attempts, not full restarts) caps runaway search
on a genuinely-unsatisfiable ruleset.

Verified in `test/Test/Main.purs` ("Stage 9") against an 11×11 maze sample
tight enough that plain single-shot `wfc` fails on it ~95–100% of the time
(measured across repeated runs) — `solveWithBacktracking` solves the same
wave reliably, and the solved result is checked against the same
pairwise-`agrees` structural-correctness test used earlier this session,
not just "returned `Right`."

## The overlapping model: extraction and rule derivation

`WFC.Catalog.extractPatterns` is `WFC.md`'s three-part "derive tileset +
rules from an example" recipe, steps 1–2 of the [Overlapping
model](WFC.md#2-overlapping-model) section:

```purescript
extractPatterns n periodic symmetry grid =
  let yMax = if periodic then h else h - n + 1     -- slide an NxN window...
      xMax = if periodic then w else w - n + 1     -- ...optionally wrapping
      positions = <every top-left corner in range>
      allVariants = positions >>= \{x,y} ->
        symmetryVariants n symmetry (patternAt n periodic w h grid x y)  -- + rotations/reflections
      acc = foldl accumulatePattern emptyAccum allVariants               -- dedup + count
  in finalize acc n
```

`accumulatePattern` is the "duplicate patterns are merged, occurrence count
becomes frequency weight" step, using an `Ord`-based `Map (Pattern a)
PatternId` for dedup (equivalent to the hash-based dedup some of the
`WFC.md` sources describe — same effect, different mechanism) rather than
literal hashing.

`WFC.Rules.buildRules`/`WFC.Pattern.agrees` are the automatic rule
derivation: two patterns are compatible in a direction exactly when their
pixels agree on the region their windows would overlap — no hand-written
adjacency table:

```purescript
agrees n dir p1 p2 =
  let { dx, dy } = dirOffset dir
      xMin = max 0 dx
      xMax = min n (n + dx)
      ...
  in all identity $ do
       y <- Array.range yMin (yMax - 1)
       x <- Array.range xMin (xMax - 1)
       pure $ patternGet n p1 x y == patternGet n p2 (x - dx) (y - dy)
```

Finally, `WFC.Render.renderWave`/`topLeftPixel` is exactly `WFC.md`'s
closing point about why the overlapping model reconstructs a coherent
image at all: once every cell is a singleton, reading back each cell's
pattern's *top-left pixel* is sufficient, because overlap-agreement was
enforced everywhere during propagation — nothing more needs to be
recomputed or blended:

```purescript
topLeftPixel catalog pid = do
  Pattern px <- Map.lookup pid catalog.patterns
  Array.head px
```

## Where the demo's loop differs

`test/Demo/src/Demo/Worker.purs`'s `runLoop` does not call `wfc`/
`wfcWithRetry` — it drives `WFC.Algorithm.step` itself in a loop, because it
needs to `postMessage` progress after *every* individual step (for the
demo's live progress bar) and to check a cancellation token between steps,
neither of which `wfc`/`wfcWithRetry` expose. The collapse/propagate
mechanics invoked are identical either way (`step` is the shared atomic
primitive); this is a UI-driven wrapper, not a second algorithm
implementation.

## Differences from the `WFC.md` description

Noted while doing this comparison; not fixed yet. Ordered roughly by
how much they'd affect correctness/capability if left alone.

1. **Only the overlapping model exists.** `WFC.md` documents the tiled
   model (hand-authored tileset + per-edge adjacency/"sockets") as an
   equally-valid, simpler-to-reason-about way to supply values and rules.
   This repo has no representation for a hand-authored tileset at all —
   every sample is a source grid run through `extractPatterns`. Not a bug,
   but a real capability gap if a future sample ever wants exact,
   hand-specified adjacency rather than ones mined from an example image.

2. ~~**`periodic` conflates two independent choices.**~~ `WFC.md`'s
   overlapping-model section treats "does the *source* wrap when patterns
   are extracted from it" and "does the *output* grid wrap for neighbor
   lookups" as two separate decisions, and `Demo.Samples.SampleDef` only
   has one `periodic :: Boolean` fed to both. **Decided (2026-07-08): not
   needed** — left as-is, not planned.

3. ~~**Only full-restart contradiction recovery, no incremental
   backtracking.**~~ `WFC.md` presents restart-on-failure and true
   backtracking (undo just the last guess, ban that value, retry) as two
   accepted strategies; this repo only implemented the former
   (`wfcWithRetry`). **Fixed 2026-07-08** — see [Incremental
   backtracking](#incremental-backtracking-wfcbacktrack) above
   (`WFC.Backtrack.solveWithBacktracking`), added alongside `wfcWithRetry`
   rather than replacing it (the demo still uses restart-based solving).

4. ~~**`Contradiction` only carries the failing `Pos`, not the wave state at
   the moment of failure.**~~ True, and still the case — but turned out not
   to matter for #3: backtracking only ever needs the wave state *before*
   a guess, which a cheap per-guess snapshot provides directly, without
   widening `Contradiction`'s shape at all. **Resolved 2026-07-08** as a
   non-issue in practice, alongside #3.

5. **No support for `WFC.md`'s "Extensions beyond the basic grid"** —
   non-Euclidean/hex/3D grids, multi-cell modules (a value spanning more
   than one cell), or weighted (rather than strictly allow/disallow)
   adjacency. Expected, since these are explicitly framed as extensions in
   `WFC.md` rather than core algorithm, but noted for completeness.

6. ~~**Stale comment in `WFC.Rules.initialCompatCount`.**~~ Claimed the
   formula was `|propagator[opposite(dir)][pid]|`; code actually computes
   `|propagator[dir][pid]|` (the correct value). **Fixed 2026-07-08** —
   comment corrected in `src/WFC/Rules.purs`.

7. **Dead commented-out code in `WFC.Collapse.collapseAt`.** Lines 55–58
   are the pre-fix version of `collapseAt` (the one that caused the
   noise-output bug fixed earlier — see the `wfc-collapse-propagate-bug`
   memory) left in as a comment. **Decided (2026-07-08): leave it** —
   intentionally kept, not planned for removal.

## Open plan — come back to this later

Live to-do list distilled from the differences above. Trim an item once
it's actually done (or decided against, like #2/#7 above); don't leave
resolved items in this section.

1. **Tiled-model support (#1)** — a hand-authored tileset + per-edge
   adjacency ("sockets") mode, as an alternative to always mining patterns
   from a source image via `extractPatterns`. Larger, separable feature;
   no timeline.
2. **Grid/adjacency extensions (#5)** — non-Euclidean/hex/3D grids,
   multi-cell modules, weighted (not just allow/disallow) adjacency. Larger,
   separable feature; no timeline.

(#3/#4 — incremental backtracking — done 2026-07-08, see `WFC.Backtrack`.)
