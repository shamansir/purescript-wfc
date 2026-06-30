# WFC API

## Core types

| Type | Module | Description |
|------|--------|-------------|
| `Direction` | `WFC.Direction` | `DirL \| DirD \| DirR \| DirU` — 4-connected grid directions |
| `Pos` | `WFC.Grid` | `Pos { x :: Int, y :: Int }` — grid coordinate |
| `GridSize` | `WFC.Grid` | `{ width :: Int, height :: Int }` |
| `PatternId` | `WFC.Pattern` | Opaque `Int` wrapper assigned during extraction |
| `Pattern a` | `WFC.Pattern` | Flat row-major `Array a` of length `n*n`; `Functor` / `Foldable` / `Traversable` |
| `PatternCatalog a` | `WFC.Catalog` | Unique patterns, frequency weights, entropy constants extracted from a sample |
| `AdjacencyRules` | `WFC.Rules` | `propagator[dir][pid]` — which patterns may neighbour `pid` in each direction |
| `Cell` | `WFC.Wave` | `Maybe (Set PatternId)` — `Nothing` = contradiction |
| `CompatMap` | `WFC.Wave` | Per-`(pos, pid, dir)` support counters; drives `propagate` |
| `Wave a` | `WFC.Wave` | Full algorithm state: cells, compat, catalog, rules, size, periodic flag |
| `BanEvent` | `WFC.Propagate` | `Tuple Pos PatternId` — a pending tile removal |
| `Contradiction` | `WFC.Propagate` | Wraps the `Pos` where a cell reached zero possibilities |

---

## Stage 1 — Extract patterns

```
extractPatterns
  :: forall a. Ord a
  => Int          -- n: pattern size (n×n tiles)
  -> Boolean      -- periodic: wrap edges when sampling
  -> Int          -- symmetry: 1–8 variants per tile (rotate/reflect)
  -> Array (Array a)
  -> PatternCatalog a
```

Slides an `n×n` window over the input grid. Counts frequencies, computes
`wLogW` and `startEntropy` so later entropy queries are O(1) per cell.

---

## Stage 2 — Build adjacency rules

```
buildRules      :: forall a. Eq a => PatternCatalog a -> AdjacencyRules
lookupNeighbors :: AdjacencyRules -> Direction -> PatternId -> Array PatternId
```

Two patterns are compatible in direction `d` when their `(n−1)`-wide overlap
strip matches (`agrees`). `lookupNeighbors rules d pid` returns every pattern
that may be placed one step in direction `d` from `pid`.

---

## Stage 3 — Initialise the wave

```
initWave :: forall a. PatternCatalog a -> AdjacencyRules -> GridSize -> Boolean -> Wave a
```

Every cell starts as `Just (Set.fromFoldable allPatternIds)` — full
superposition. `CompatMap` is initialised to `|propagator[dir][pid]|` at every
position so `propagate` can track support counts.

```
getCellPossibilities :: forall a. Wave a -> Pos -> Maybe (Set PatternId)
isFullyCollapsed     :: forall a. Wave a -> Boolean
setCell              :: forall a. Pos -> Cell -> Wave a -> Wave a
```

---

## Stage 4 — Entropy

```
cellEntropy      :: forall a. Wave a -> Set PatternId -> Number
cellsWithEntropy :: forall a. Wave a -> Array (Tuple Pos Number)
minEntropyPos    :: forall a. Wave a -> Effect (Maybe Pos)
```

`cellEntropy` computes Shannon entropy `H = ln(Σw) − Σ(w ln w)/Σw`.
`minEntropyPos` adds tiny random noise before finding the minimum so that
ties are broken uniformly at random. Returns `Nothing` when all cells are
collapsed.

---

## Stage 5 — Collapse

```
collapseAt    :: forall a. Wave a -> Pos -> Effect (Either Contradiction (Wave a))
weightedSample :: forall a. Wave a -> Set PatternId -> Effect (Maybe PatternId)
pickWeighted   :: Number -> Array (Tuple PatternId Number) -> Maybe PatternId
```

`collapseAt` picks one pattern by weighted sampling, sets the cell to that
singleton, bans all rejected patterns, and calls `propagate` immediately.

---

## Stage 6 — Propagate

```
propagate :: forall a. Wave a -> Array BanEvent -> Either Contradiction (Wave a)
```

Drains a worklist of ban events. For each banned `(pos, pid)`:

1. Remove `pid` from `wave.cells[pos]`; if the set becomes empty → `Contradiction pos`
2. For each direction `dir`, decrement `compat[nPos][nPid][opposite(dir)]` for
   every `nPid ∈ propagator[dir][pid]`
3. If a count reaches 0, enqueue `(nPos, nPid)` as a new ban

```
getCompat        :: forall a. Wave a -> Pos -> PatternId -> Direction -> Int
decrementCompat  :: forall a. Pos -> PatternId -> Direction -> Wave a -> Tuple Int (Wave a)
processNeighbours :: forall a. PatternId -> Pos -> Wave a -> List BanEvent -> PropState a
processBan       :: forall a. BanEvent -> PropState a -> Either Contradiction (PropState a)
drainQueue       :: forall a. PropState a -> Either Contradiction (Wave a)
```

---

## Stage 7 — Main loop

```
step         :: forall a. Wave a -> Effect (Either Contradiction (Maybe (Wave a)))
wfc          :: forall a. Wave a -> Effect (Either Contradiction (Wave a))
wfcWithRetry :: forall a. Int -> Wave a -> Effect (Maybe (Wave a))
```

`step` = find min-entropy cell → `collapseAt` → `propagate`.  
`wfc` loops via `tailRecM` (stack-safe). Returns `Right wave` when
`minEntropyPos` returns `Nothing` (all cells collapsed).  
`wfcWithRetry` retries from the original wave up to `maxAttempts` times on
contradiction.

---

## Stage 8 — Render

```
renderWave     :: forall a. Wave a -> Maybe (Array (Array a))
renderWaveWith :: forall a. a -> Wave a -> Array (Array a)
topLeftPixel   :: forall a. PatternCatalog a -> PatternId -> Maybe a
collapsedId    :: Maybe (Set PatternId) -> Maybe PatternId
```

`renderWave` extracts the top-left pixel of each collapsed cell. Returns
`Nothing` if any cell is still in superposition or is a contradiction.
`renderWaveWith fallback` always returns a complete grid, substituting
`fallback` for non-collapsed cells.

---

## Pattern utilities

```
rotate           :: forall a. Int -> Pattern a -> Pattern a   -- 90° CW: new(x,y) = old(y, n-1-x)
reflect          :: forall a. Int -> Pattern a -> Pattern a   -- horizontal: new(x,y) = old(n-1-x, y)
symmetryVariants :: forall a. Int -> Int -> Pattern a -> Array (Pattern a)  -- up to 8
agrees           :: forall a. Eq a => Int -> Direction -> Pattern a -> Pattern a -> Boolean
patternGet       :: forall a. Int -> Pattern a -> Int -> Int -> Maybe a
```

---

## Direction utilities

```
allDirections :: Array Direction                       -- [DirL, DirD, DirR, DirU]
dirOffset     :: Direction -> { dx :: Int, dy :: Int }
opposite      :: Direction -> Direction
```

## Grid utilities

```
allPositions :: GridSize -> Array Pos
neighborPos  :: GridSize -> Boolean -> Pos -> Direction -> Maybe Pos
gridWidth    :: forall a. Array (Array a) -> Int
gridHeight   :: forall a. Array (Array a) -> Int
```
