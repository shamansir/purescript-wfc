# Wave Function Collapse — Algorithm Summary

A language-independent explanation of the Wave Function Collapse (WFC)
algorithm, synthesized from six external write-ups (listed in
[Sources](#sources)). It describes the algorithm conceptually — every
example is pseudocode, not tied to any particular programming language or
codebase.

## What it actually is

WFC borrows its name and vocabulary from quantum mechanics (*superposition*,
*collapse*, *observation*, *wave function*), but every source agrees the
physics analogy is shallow and mostly a naming convenience. Structurally,
WFC is a **constraint satisfaction problem (CSP)** solver with a specific,
cheap heuristic for variable/value ordering:

- a set of **variables** — one per output cell/position
- each variable has a **domain** — the set of values (tiles/colors/patterns)
  it could still take
- a set of **constraints** — rules about which values adjacent variables may
  simultaneously hold
- a solving loop that interleaves **constraint propagation** (deduction,
  no guessing) with **guessing** (random choice, weighted by frequency)
  whenever propagation alone stalls

It differs from a general CSP solver mainly in *how* it picks the next
variable to guess (lowest entropy first) and in how it usually reacts to
failure (often a full restart rather than deep backtracking) — both
pragmatic choices that make it fast enough for real-time and interactive
procedural generation, at the cost of completeness guarantees.

## Core concepts

| Term | Meaning |
|---|---|
| **Wave** | The whole grid of cells, each still holding a set ("superposition") of possible values. The name for the overall mutable state the algorithm operates on. |
| **Cell / position / variable** | One slot in the output grid. Starts containing *every* value as a possibility. |
| **Tile / pattern / value** | One concrete thing a cell could ultimately become — a tile graphic, a color, a terrain type, or (in the overlapping model) a small NxN pixel block. |
| **Superposition** | A cell's current state before it's fully decided: it still holds more than one possible value. |
| **Domain** | Synonym for "the set of values still possible for a cell" — CSP terminology for the same thing as "superposition." |
| **Observation / Collapse** | Picking one concrete value for a chosen cell and discarding all its other possibilities — collapsing its domain down to size 1. |
| **Propagation** | After a collapse (or any reduction of a cell's domain), removing now-impossible values from *neighboring* cells according to the adjacency constraints, and cascading that outward — "constraint solving can work like a chain reaction: once a color is picked for one tile, it limits the options for the tiles placed next to it." |
| **Support** | For a given cell and a candidate value, the set (or count) of neighboring values that would still make that candidate valid. When a value's support in some direction drops to zero, it can no longer survive there. |
| **Contradiction** | A cell's domain has been propagated down to *zero* remaining values — the current partial solution is unsatisfiable and something earlier must have been a wrong guess. |
| **Entropy** | A numeric measure of how *undecided* a cell still is — used purely as a heuristic for picking which cell to collapse next, not a physical quantity. |
| **Adjacency constraint / rule** | The compatibility relation between values in neighboring cells (e.g. "edges must match," "this tile can only be to the right of that tile"). |

## The main loop

Every source converges on essentially the same cycle, varying mostly in
where they draw the step boundaries:

1. **Define the input** — the set of possible values and the adjacency
   rules between them (see [Where the rules come from](#where-the-rules-come-from-two-source-models)),
   plus, ideally, a *frequency weight* per value.
2. **Initialize** — every cell in the output starts in full superposition:
   every value is still possible everywhere.
3. **Observe** — among all cells that aren't yet fully collapsed, pick the
   one with the **lowest entropy** (fewest remaining possibilities; ties
   broken randomly, or with a small amount of noise added to the entropy
   score so ties don't always resolve the same way). Randomly pick one
   value from that cell's remaining possibilities, **weighted by that
   value's frequency** in the source data, and collapse the cell to it.
4. **Propagate** — for each neighbor of the just-changed cell, remove any
   value that is no longer supported given the neighbor's new, smaller
   domain. Whenever a neighbor's domain itself shrinks, repeat this for
   *its* neighbors too — a flood-fill / chain reaction that keeps spreading
   until no cell changes anymore in a pass (a fixed point).
5. **Check for contradiction** — if any cell's domain has been reduced to
   empty, the current attempt has failed. Recover by backtracking to an
   earlier saved state and trying a different value, or — the simpler and
   very commonly used approach — throw the whole attempt away and restart
   generation from scratch (small/medium outputs make full restarts cheap
   enough that this is often preferred over bookkeeping an undo history).
6. **Repeat** from step 3 until either every cell is collapsed to exactly
   one value (success) or the algorithm gives up (persistent contradiction
   after retries).

Pseudocode form:

```text
function WaveFunctionCollapse(values, rules, weights, grid_size):
    wave = new_grid(grid_size, cell => all(values))

    loop:
        if any cell in wave has zero possibilities:
            return Contradiction

        if every cell in wave has exactly one possibility:
            return Solved(wave)

        cell = pick_cell_with_lowest_entropy(wave)
        value = pick_weighted_random(cell.possibilities, weights)
        wave[cell] = { value }                      # collapse

        queue = [cell]
        while queue is not empty:                   # propagate
            current = queue.pop()
            for neighbor, direction in neighbors_of(current):
                changed = remove_unsupported_values(
                    wave[neighbor], wave[current], direction, rules)
                if changed:
                    if wave[neighbor] is empty:
                        return Contradiction
                    queue.push(neighbor)
```

### Entropy, precisely

Plain "number of remaining possibilities" is a fine entropy stand-in when
every value is equally likely, but once values carry frequency weights,
the standard **Shannon entropy** of the weighted possibility set is used
instead, so that a cell with one very-likely and one very-unlikely option
is treated as "more decided" than a cell with two equally-likely options:

```text
entropy(cell) = log(sum(weights)) - sum(weight * log(weight)) / sum(weights)
```

summed/looped over the weights of the values still possible in that cell.
Lower entropy is prioritized: it both mirrors how a person would solve the
same puzzle by hand (fill in the most-constrained square first, exactly
the same intuition used to solve Sudoku) and empirically produces fewer
contradictions than picking cells randomly or in raster order.

A small amount of random noise is commonly added to the computed entropy
value before comparing cells, purely to break ties between multiple
cells that happen to have identical entropy, so the algorithm doesn't
always resolve ties in the same fixed order.

## Where the rules come from: two source models

Sources agree the *loop above* is identical either way; what differs is
how the values and the adjacency rules are obtained in the first place.

### 1. Tiled model

You supply the tileset and the rules directly: a fixed set of
tiles/graphics, each with metadata about which of its edges can join to
which edges of which other tiles (sometimes called **sockets**). A
"simple tiled model" additionally accounts for rotations/reflections of
tiles as effectively-new tiles. **Wang tiles** are a classic concrete
example: each tile's four edges are labeled, and two tiles may sit next
to each other only if the touching edges' labels match — a pipe segment
that exits to the right can only be followed by a tile whose left edge is
also a pipe. This model is simple to reason about but requires an artist
or designer to hand-author a consistent tileset and its adjacency rules.

### 2. Overlapping model

You instead supply a single example bitmap and a window size **N**. The
algorithm itself derives the tileset and the rules by:

1. Sliding an N×N window over every position of the example image
   (optionally wrapping at the edges for a tileable/periodic source, and
   optionally also including each window's rotations/reflections to
   enrich the effective sample) and recording each distinct N×N block as
   a **pattern**. Duplicate patterns (found via direct comparison or a
   hash) are merged into one, and the number of times a pattern occurred
   becomes its **frequency weight** for later weighted selection.
2. Deriving adjacency rules automatically: pattern A is allowed directly
   next to pattern B (in a given direction) exactly when their pixels
   *agree* on the region where their two N×N windows would overlap once
   placed one step apart. No rules are hand-written; they fall directly
   out of what pixel arrangements actually occurred in the example.
3. Running the same observe/propagate loop as above, except each output
   "cell" corresponds to the position of one pattern's top-left corner,
   and because neighboring patterns overlap by N−1 rows/columns, once the
   whole wave is fully collapsed with no contradictions, simply reading
   each cell's pattern's top-left pixel back out reconstructs one coherent
   final image — the overlap-agreement constraint is exactly what
   guarantees adjacent cells' pixels line up correctly.

The overlapping model captures visual structure (recurring shapes,
textures, local motifs) directly from an example image with zero manual
rule-authoring, at the cost of being significantly more expensive per
step than the tiled model (many more distinct patterns, and richer
per-direction compatibility sets to check).

A further variant mentioned across sources: **multi-pass generation**,
where a first coarse WFC pass decides large regions (e.g. biomes: ocean,
plains, mountains), and a second pass runs WFC again *within* each region
at finer detail, using per-region tilesets/rules — useful for content
that has both large-scale and small-scale structure.

## Propagation mechanics and efficiency

Naively, propagation could re-check every constraint after every single
change, which is wasteful. Practical implementations converge on the same
handful of optimizations:

- **Bitmask representation** — when the number of distinct values is
  small enough, a cell's possibility set is stored as one integer, one bit
  per value. Set operations (which possibilities survive an intersection
  with a neighbor's allowed set) become plain bitwise AND/OR/XOR, which is
  both compact and fast.
- **Queued / worklist propagation** — instead of re-scanning the whole
  grid, keep an explicit queue (or stack) of "cells that changed and
  haven't had their neighbors re-checked yet." Pop one, re-check just its
  immediate neighbors, push any neighbor that itself changed, and stop
  when the queue empties — this is a standard fixed-point / flood-fill
  shape, and it naturally only ever does work proportional to how far the
  consequences of one collapse actually spread, not the whole grid.
- **Support counts / caching** — rather than re-deriving from scratch
  "how many of my neighbor's current possibilities would still support
  value V being here," maintain a running count per (cell, value,
  direction) that only ever decreases, and only bother re-examining V
  at all once its support count hits zero. This turns a repeated
  set-intersection into an O(1) decrement-and-check.
- **Undo / snapshot for backtracking** — the simplest possible recovery
  strategy is to snapshot the entire wave state before a risky guess and
  restore it wholesale on contradiction; perfectly fine for small-to-medium
  grids, though more sophisticated implementations may only log the
  individual removals so they can be undone incrementally instead of
  copying the whole state.

## Contradiction handling

All sources agree WFC, as commonly implemented, is **not guaranteed to
find a solution** on a given attempt — an early unlucky random guess can
box later cells into an impossible corner. Two responses show up:

- **Backtrack** — undo the most recent guess (and everything propagation
  did because of it), ban the value that was just tried at that cell, and
  try again with what remains. Done recursively/repeatedly this becomes a
  full backtracking search, at the cost of needing an undo mechanism.
- **Restart** — throw away the whole in-progress wave and start over from
  a blank grid, optionally with a new random seed. Much simpler to
  implement, and often cheap enough in practice (especially combined with
  the lowest-entropy heuristic, which empirically keeps contradictions
  fairly rare) that many practical implementations use restart-on-failure
  as their *only* recovery mechanism rather than true backtracking.

## Illustrative analogies used across the sources

Different write-ups reach for different everyday analogies to build
intuition before introducing the formalism — worth keeping in mind since
they're a good way to explain the algorithm to someone new to it:

- **Wedding seating chart** — every seat can hold any guest (superposition);
  seating one guest (collapse) rules out incompatible relatives from
  neighboring seats (propagation); an unseatable remaining guest is a
  contradiction.
- **Mini Sudoku / Sudoku** — cells are variables, digits are values, the
  "all distinct in this row/column/box" rule is the constraint; a 4×4
  puzzle is often small enough that pure propagation (no guessing) already
  solves it completely, while full 9×9 Sudoku usually needs at least one
  guess-and-backtrack step, making it a clean bridge from "plain constraint
  propagation" to "WFC's guess-when-stuck" behavior.
- **Terrain/biome map generation** — water, grass, mountain, snow, cliff
  as the values, with common-sense adjacency ("snow near mountains, not
  oceans"; "cliffs border mountains"; "banks separate grass from water")
  as the constraints — a natural fit for the multi-pass biome-then-detail
  extension mentioned above.
- **Pipe / Wang tileset** — tiles are pipe segments that must connect
  edge-to-edge, directly illustrating the tiled model and why "sockets"/
  edge-labels are a natural way to encode adjacency.

## Extensions beyond the basic grid

- **Non-Euclidean / non-square domains** — hexagonal grids, 3D voxel
  grids, spherical or otherwise curved surfaces; the algorithm itself
  doesn't actually require a square 2D raster, only a well-defined notion
  of "neighbor" and "direction" for propagation to traverse.
- **Multi-cell modules** — allowing a single placed value to occupy more
  than one grid cell at once (e.g. a large building spanning several
  tiles), rather than assuming every value is exactly one cell in size.
- **Weighted / biased adjacency** — beyond a strict "allowed or not"
  adjacency table, giving certain valid neighbor combinations higher
  probability than others (e.g. grass prefers to border more grass over
  bordering desert, even though both are technically legal), for output
  that leans toward particular local compositions.

## Summary

WFC = constraint satisfaction (variables with domains, adjacency
constraints, propagation-to-fixed-point) plus two pragmatic choices that
make it fast and simple enough for procedural content: always guess the
*lowest-entropy* undecided variable next, weighted by observed/example
frequency, and on failure prefer a cheap *restart* over an expensive
general-purpose backtracking search. The "tiled" vs "overlapping" split is
purely about *where the values and adjacency rules come from* — hand
authored, or automatically mined from a single example image — the
solving loop itself doesn't change between them.

## Sources

1. Kavin Bharathi — [*The Fascinating Wave Function Collapse Algorithm*](https://dev.to/kavinbharathi/the-fascinating-wave-function-collapse-algorithm-4nc3)
2. Nathan Coleman — [*Wave Collapse*](https://nathanmcoleman.com/projects/wavecollapse/)
3. vectrx — [*Wave Function Collapse*](https://vectrx.substack.com/p/wave-function-collapse)
4. Robert Heaton — [*Wavefunction Collapse Algorithm*](https://robertheaton.com/2018/12/17/wavefunction-collapse-algorithm/)
5. Boris the Brave — [*Wave Function Collapse Explained*](https://www.boristhebrave.com/2020/04/13/wave-function-collapse-explained/)
6. pyprogramming.org — [*The Wave Function Collapse Algorithm*](https://pyprogramming.org/the-wave-function-collapse-algorithm/)
