# Wave Function Collapse — Functional PureScript Rewrite Plan

## Goals & Philosophy

- Every algorithm stage is a named, composable function with explicit types
- Illegal states unrepresentable: phantom types, `NonEmptySet`, `Maybe` encode invariants
- Effectful parts (randomness, IO) isolated via `MonadGen` / `Effect`; core is pure
- No direct translation from C# — rethink each stage as a fold, unfold, or algebra
- Pattern, Wave, Grid are parametric over pixel type `a`; pixel logic never leaks into algorithm

---

## Type Hierarchy

### Primitives

```purescript
newtype Pos        = Pos { x :: Int, y :: Int }
newtype PatternId  = PatternId Int
newtype PatternSize = PatternSize Int
newtype Weight     = Weight Number
newtype Entropy    = Entropy Number

type GridSize = { width :: Int, height :: Int }

data Direction = L | D | R | U   -- Left Down Right Up
                                  -- offsets: (-1,0) (0,1) (1,0) (0,-1)

opposite :: Direction -> Direction
dirOffset :: Direction -> Int /\ Int

derive instance Eq     PatternId; derive instance Ord     PatternId
derive instance Eq     Direction;  derive instance Ord     Direction
derive instance Eq     Entropy;    derive instance Ord     Entropy
```

### Pattern

```purescript
-- Flat row-major array of length N², parametric in pixel type
newtype Pattern a = Pattern (Array a)

-- Derived instances follow from pixel type
derive instance Functor     Pattern    -- map f (Pattern px) = Pattern (map f px)
derive instance Foldable    Pattern    -- fold over pixels
derive instance Traversable Pattern    -- traverse for effectful pixel transforms

-- Symmetry variants: rotate / reflect as pure functions on Pattern
rotate  :: forall a. PatternSize -> Pattern a -> Pattern a
reflect :: forall a. PatternSize -> Pattern a -> Pattern a

-- Generate up to 8 geometric variants for symmetry augmentation
symmetryVariants :: forall a. PatternSize -> Int -> Pattern a -> Array (Pattern a)
```

**Note:** `Functor` on `Pattern` lets you map color transformations (e.g. palette remapping,
grayscale, rendering) without touching algorithm logic.

### PatternCatalog — opaque, only built via `extractPatterns`

```purescript
newtype PatternCatalog a = PatternCatalog
  { patterns      :: Map PatternId (Pattern a)  -- id → pattern pixels
  , weights       :: Map PatternId Weight        -- id → frequency
  , wLogW         :: Map PatternId Number        -- id → weight * ln(weight), precomputed
  , size          :: PatternSize
  , totalW        :: Number                      -- Σ weights, precomputed
  , totalWLogW    :: Number                      -- Σ (w * ln w), precomputed
  , startEntropy  :: Number                      -- ln(totalW) - totalWLogW/totalW
  }
```

`totalW`, `totalWLogW`, `startEntropy` are computed once at construction time.
All cells in the initial wave share the same `startEntropy`.

### AdjacencyRules — opaque, only built via `buildRules`

```purescript
-- propagator[dir][patternId] = Array of valid neighbor IDs in that direction
newtype AdjacencyRules = AdjacencyRules (Map Direction (Map PatternId (Array PatternId)))
```

### CompatibilityMap — counts per cell per tile per direction

```purescript
-- compatible[pos][tile][dir] = how many tiles in direction `dir` are still compatible
-- When any value drops to 0, that tile must be banned from `pos`
type CompatMap = Map Pos (Map PatternId (Map Direction Int))
```

This mirrors the original's `compatible[position][tile][direction]` int array.
Initial value: `propagator[opposite(dir)][tile].length` — how many tiles *could*
have arrived from that direction. When a neighbor loses a tile, decrement; at 0, ban.

### CellState

```purescript
-- Running entropy sums, mirroring the C# incremental approach
type CellState =
  { possible  :: NonEmptySet PatternId   -- at least one option (empty = use Nothing)
  , sumW      :: Number                  -- Σ weight(p) for p in possible
  , sumWLogW  :: Number                  -- Σ weight(p)*ln(weight(p)) for p in possible
  }

-- Nothing encodes contradiction directly; no third constructor needed
-- Just (singleton s) means collapsed
-- Just (multiple s) means superposed
type Cell = Maybe CellState
```

`NonEmptySet` is the key invariant: if possibilities would become empty, we return
`Nothing` (contradiction) instead. No `Contradiction` constructor needed — `Maybe`'s
monad propagates failure naturally.

```purescript
-- Smart constructor: returns Nothing on empty, otherwise wraps in Just
mkCell :: PatternCatalog a -> Set PatternId -> Cell
mkCell catalog pids = map (buildCellState catalog) (NonEmptySet.fromSet pids)

-- Remove one pattern from a cell; returns Nothing if this causes contradiction
banFromCell :: PatternCatalog a -> PatternId -> CellState -> Cell
banFromCell (PatternCatalog { weights, wLogW }) pid cs =
  let w    = fromMaybe 0.0 (Map.lookup pid weights)
      wlw  = fromMaybe 0.0 (Map.lookup pid wLogW)
      poss = NonEmptySet.delete pid cs.possible
  in map (\s -> cs { possible  = s
                   , sumW      = cs.sumW    - w
                   , sumWLogW  = cs.sumWLogW - wlw
                   }) poss      -- NonEmptySet.delete returns Maybe (NonEmptySet _)
```

### Wave — the central mutable-ish state

```purescript
newtype Wave = Wave
  { cells    :: Map Pos Cell       -- Nothing = contradiction at that pos
  , compat   :: CompatMap          -- per-pos, per-tile, per-direction counts
  , rules    :: AdjacencyRules
  , catalog  :: PatternCatalog Void  -- erase pixel type; only IDs needed during algorithm
  , size     :: GridSize
  , periodic :: Boolean
  }
```

We erase the pixel type inside `Wave` using `Void` (or an existential) since the
algorithm only needs pattern *identities*. Pixel lookup only happens at output time.

### Contradiction

```purescript
newtype Contradiction = Contradiction Pos  -- where it occurred
```

---

## Algorithm Stages

### Stage 1 — Extract Patterns

```purescript
extractPatterns
  :: forall a. Ord a
  => PatternSize
  -> Boolean        -- periodic input?
  -> Int            -- symmetry count 1..8
  -> Array (Array a)
  -> PatternCatalog a
```

**Implementation:** fold over all valid positions, extract each N×N patch, generate
`symmetry` variants, deduplicate via `Map (Pattern a) PatternId`, accumulate weights.

```purescript
-- Extract single N×N patch at position (with optional wrapping)
patternAt :: forall a. PatternSize -> Boolean -> Array (Array a) -> Pos -> Pattern a
patternAt (PatternSize n) periodic grid (Pos { x, y }) = Pattern do
  dy <- Array.range 0 (n - 1)
  dx <- Array.range 0 (n - 1)
  let x' = if periodic then (x + dx) `mod` w else x + dx
      y' = if periodic then (y + dy) `mod` h else y + dy
  pure (unsafeGet grid x' y')

-- Fold: accumulate patterns into a Map, counting duplicates as weights
type Accum a =
  { byPixels :: Map (Pattern a) PatternId  -- dedup by pixel content
  , catalog  :: PatternCatalog a
  }

accumPattern :: forall a. Ord a => Accum a -> Pattern a -> Accum a
```

**FP note:** The entire extraction is a `foldl` — no mutation, no global state.
`Pattern a` gets `Eq`/`Ord` instances from the `Ord a` constraint (lexicographic).

### Stage 2 — Build Adjacency Rules

```purescript
buildRules :: forall a. Eq a => PatternCatalog a -> AdjacencyRules
```

Two patterns `p1` and `p2` are compatible in direction `d` if their overlap region matches.
For direction `L` (dx = -1, dy = 0), the overlap is the leftmost N-1 columns of `p1`
vs the rightmost N-1 columns of `p2`.

```purescript
-- Pure predicate on Pattern overlap
agrees :: forall a. Eq a => PatternSize -> Direction -> Pattern a -> Pattern a -> Boolean
agrees (PatternSize n) dir (Pattern p1) (Pattern p2) =
  let (dx /\ dy) = dirOffset dir
      xRange = Array.range (max 0 dx) (min n (n + dx) - 1)
      yRange = Array.range (max 0 dy) (min n (n + dy) - 1)
  in all (\(x /\ y) ->
       safeIdx p1 (x + y * n) == safeIdx p2 ((x - dx) + (y - dy) * n)
     ) (xRange `cartesian` yRange)

-- Build: all-pairs check per direction (O(T² * N² * 4))
buildRules catalog =
  AdjacencyRules $ Map.fromFoldable $ allDirections <#> \dir ->
    dir /\ Map.fromFoldable (ids <#> \pid ->
      pid /\ Array.filter (agrees size dir (lookupPattern pid)) ids
    )
  where ids = Map.keys catalog.patterns
```

### Stage 3 — Initialize Wave

```purescript
initWave
  :: forall a
  => PatternCatalog a
  -> AdjacencyRules
  -> GridSize
  -> Boolean        -- periodic output?
  -> Wave
```

Every cell starts as `Just (fullSuperposition catalog)`.
The `compat` map is initialized to `propagator[opposite(d)][tile].length` for each
`(pos, tile, dir)` — this is the count of neighbor tiles that *can* supply compatibility
from direction `d`. When it hits 0, tile must be banned.

```purescript
-- Initial compat count for tile `pid` from direction `dir`
initialCompat :: AdjacencyRules -> PatternId -> Direction -> Int
initialCompat (AdjacencyRules rules) pid dir =
  fromMaybe 0 $
    Map.lookup (opposite dir) rules >>= Map.lookup pid >>= pure <<< Array.length

initCompatMap :: AdjacencyRules -> NonEmptySet PatternId -> GridSize -> CompatMap
initCompatMap rules allIds size =
  Map.fromFoldable (allPositions size <#> \pos ->
    pos /\ Map.fromFoldable (NonEmptySet.toUnfoldable allIds <#> \pid ->
      pid /\ Map.fromFoldable (allDirections <#> \dir ->
        dir /\ initialCompat rules pid dir
      )
    )
  )
```

### Stage 4 — Entropy

```purescript
-- Cell entropy from running sums (O(1), no recomputation)
cellEntropy :: CellState -> Entropy
cellEntropy { sumW, sumWLogW } = Entropy (Math.log sumW - sumWLogW / sumW)

-- All non-collapsed positions and their current entropy
waveEntropies :: Wave -> Map Pos Entropy
waveEntropies (Wave { cells }) =
  Map.mapMaybe (map cellEntropy <=< flip bind notSingleton) cells
  where
    notSingleton cs
      | NonEmptySet.size cs.possible > 1 = Just cs
      | otherwise                         = Nothing

-- Minimum entropy position with small random noise (breaks ties stochastically)
minEntropyPos :: forall m. MonadGen m => Wave -> m (Maybe Pos)
minEntropyPos wave = do
  let es = waveEntropies wave
  noisyEs <- traverse addNoise es
  pure (map fst (minimumBy (comparing snd) (Map.toUnfoldable noisyEs)))

addNoise :: forall m. MonadGen m => Entropy -> m Entropy
addNoise (Entropy e) = (\n -> Entropy (e + n * 1.0e-6)) <$> Gen.uniform
```

**FP note:** Entropy is maintained incrementally in `CellState` (not recomputed),
exactly as in the C# code, but here it's a field in a pure record updated via `banFromCell`.

### Stage 5 — Observe (Weighted Collapse)

```purescript
-- Weighted random sample: pick PatternId proportional to weight
weightedSample
  :: forall m. MonadGen m
  => Map PatternId Weight
  -> NonEmptySet PatternId
  -> m PatternId
weightedSample weights possible = do
  let ws = NonEmptySet.toUnfoldable possible
              <#> \pid -> pid /\ fromMaybe 0.0 (map unwrap (Map.lookup pid weights))
      total = sum (map snd ws)
  r <- (_ * total) <$> Gen.uniform
  pure (pickWeighted r ws)
  where
    pickWeighted threshold = go 0.0
      where go acc ((pid /\ w) : rest)
              | acc + w >= threshold = pid
              | otherwise            = go (acc + w) rest
            go _ [] = fst (NonEmptySet.max possible)  -- fallback (shouldn't happen)

-- Collapse: pick a pattern, ban everything else at that position
-- Returns the updated wave BEFORE propagation
collapseCell
  :: forall m a. MonadGen m
  => PatternCatalog a
  -> Wave
  -> Pos
  -> m (Either Contradiction Wave)
collapseCell catalog wave pos = do
  case Map.lookup pos wave.cells >>= identity of
    Nothing -> pure (Left (Contradiction pos))
    Just cs -> do
      chosen <- weightedSample catalog.weights cs.possible
      let toBan = NonEmptySet.toSet cs.possible `Set.difference` Set.singleton chosen
      pure (Right (foldl (banAt catalog pos) wave toBan))
```

### Stage 6 — Propagate

This is the heart of the algorithm. In the C# code it uses an imperative stack and
mutation; here we use `StateT` + `tailRecM` for stack-safety and purity.

```purescript
-- A pending ban: (position, tile to remove)
type BanEvent = Pos /\ PatternId

-- Propagation monad: stateful wave + queue of pending bans, failing on contradiction
type PropState = { wave :: Wave, queue :: List BanEvent }
type PropM     = StateT PropState (Either Contradiction)

-- Main entry: propagate starting from a set of initial bans
propagate :: Wave -> Array BanEvent -> Either Contradiction Wave
propagate wave initial =
  map _.wave (execStateT (tailRecM drainQueue unit) { wave, queue: List.fromFoldable initial })
  where
    drainQueue _ = gets _.queue >>= case _ of
      Nil        -> pure (Done unit)
      ev : rest  -> do
        modify_ _ { queue = rest }
        processBan ev
        pure (Loop unit)

-- Process one ban: remove tile, update compat counts, push new bans
processBan :: BanEvent -> PropM Unit
processBan (pos /\ pid) = do
  { wave } <- get
  case Map.lookup pos wave.cells >>= identity of
    Nothing -> throwError (Contradiction pos)  -- already a contradiction
    Just cs ->
      case banFromCell wave.catalog pid cs of
        Nothing  -> throwError (Contradiction pos)  -- removal caused contradiction
        Just cs' -> do
          modify_ \s -> s { wave = setCell pos (Just cs') s.wave }
          -- For each direction, decrement compat count of neighbor; ban if reaches 0
          for_ allDirections \dir -> do
            { wave: w } <- get
            case neighborPos w.size w.periodic pos dir of
              Nothing   -> pure unit
              Just nPos -> updateCompat nPos pid dir

-- Decrement compatible count of `pid` at `nPos` from direction `dir`
-- If count reaches 0, add new ban event
updateCompat :: Pos -> PatternId -> Direction -> PropM Unit
updateCompat nPos pid dir = do
  { wave } <- get
  let compat = getCompat wave nPos pid dir
      compat' = compat - 1
  modify_ \s -> s { wave = setCompat nPos pid dir compat' s.wave }
  when (compat' == 0) do
    modify_ \s -> s { queue = (nPos /\ pid) : s.queue }
```

**Why `StateT` + `tailRecM`:** `tailRecM` gives us stack safety for arbitrarily deep
propagation chains (large grids). `StateT` lets `processBan` read and update both wave
and queue atomically. `Either Contradiction` as the base monad means any ban that would
empty a cell short-circuits immediately.

**Alternative — Comonad propagation (elegant but less efficient):**

```purescript
-- Grid as a focused comonad; extend applies neighborhood function everywhere at once
-- Propagation = iterate extend to fixed point
-- Beautiful mathematically; worse performance than worklist approach
propagateComonad :: AdjacencyRules -> FocusedGrid Cell -> FocusedGrid Cell
propagateComonad rules = fix \loop fg ->
  let fg' = extend (constrainByNeighbors rules) fg
  in if fg == fg' then fg else loop fg'
```

Include as an alternative module for didactic purposes.

### Stage 7 — Main Loop

```purescript
-- One iteration: find min-entropy cell, collapse it, propagate
step
  :: forall m a. MonadGen m
  => PatternCatalog a
  -> Wave
  -> m (Either Contradiction (Maybe Wave))   -- Nothing = fully collapsed (done)
step catalog wave = do
  mPos <- minEntropyPos wave
  case mPos of
    Nothing  -> pure (Right Nothing)          -- no superposed cells remain → done
    Just pos -> do
      collapsed <- collapseCell catalog wave pos
      case collapsed of
        Left err   -> pure (Left err)
        Right wave' ->
          let toBan = bannedEvents wave wave' pos  -- diff: find newly banned tiles
          in pure (map Just (propagate wave' toBan))

-- Full algorithm: stack-safe loop via tailRecM
wfc
  :: forall m a. MonadGen m
  => PatternCatalog a
  -> Wave
  -> m (Either Contradiction Wave)
wfc catalog = runExceptT <<< tailRecM go
  where
    go wave = do
      mPos <- lift (minEntropyPos wave)
      case mPos of
        Nothing  -> pure (Done wave)
        Just pos -> do
          wave' <- ExceptT (collapseCell catalog wave pos)
          let bans = bannedEvents wave wave' pos
          wave'' <- ExceptT (pure (propagate wave' bans))
          pure (Loop wave'')
```

**Key composition:** `collapseCell` (Gen effect) → `propagate` (pure, Either) → `tailRecM`
(stack safety). The three concerns are cleanly separated.

### Stage 8 — Output Rendering

```purescript
-- Extract collapsed pixel grid; Nothing if any cell is not fully collapsed
renderWave :: forall a. PatternCatalog a -> Wave -> Maybe (Array (Array a))
renderWave catalog (Wave { cells, size }) = do
  let positions = rowMajorPositions size
  pids <- traverse (collapsePos <=< flip Map.lookup cells) positions
  pure (reshape size (map (topLeftPixel catalog) pids))
  where
    collapsePos (Just { possible })
      | Just pid <- NonEmptySet.toMaybe (NonEmptySet.filter isOnly possible) = Just pid
    collapsePos _ = Nothing

    isOnly pid = NonEmptySet.size possible == 1  -- only valid if singleton

-- Top-left pixel of a pattern = canonical color for that cell in output
topLeftPixel :: forall a. PatternCatalog a -> PatternId -> Maybe a
topLeftPixel (PatternCatalog { patterns }) pid =
  Map.lookup pid patterns >>= \(Pattern px) -> Array.head px
```

---

## Functional Patterns Applied

| Concept | Where |
|---------|-------|
| `Functor` on `Pattern` | Pixel-agnostic transformations, palette mapping |
| `Foldable`/`Traversable` on `Pattern` | Equality check, rendering, fold over pixels |
| `NonEmptySet` | Superposition: empty set *is* `Nothing`, no extra constructor |
| `Maybe` as failure | `Cell = Maybe CellState`; contradiction = `Nothing`; monadic chain |
| Phantom type erasure | Wave uses `PatternId` only; pixel type erased to `Void` inside |
| `StateT (Either Contradiction)` | Propagation: atomic wave + queue update, early exit |
| `tailRecM` | Main loop and propagation queue drain; stack-safe recursion |
| `MonadGen` constraint | Collapse and noise are generic over any random monad |
| Newtype opacity | `PatternCatalog` and `AdjacencyRules` only constructible via their stages |
| Incremental entropy | `CellState` carries running sums; `cellEntropy` is O(1) |
| Comonad `extend` | Alternative propagation via `FocusedGrid` (didactic module) |
| `fix` | Comonad propagation loop (iterate to fixed point) |
| Algebra over `PatternCatalog` | Could be `Semigroup` (merge two input samples) |

### On `void` / superposition analogy

In the algorithm, a cell in full superposition holds *all* patterns simultaneously —
the opposite of `Void`. `NonEmptySet PatternId` is the faithful encoding: non-empty means
"not yet decided", singleton means "collapsed". `Maybe (NonEmptySet _)` maps directly:

```
Just (NonEmptySet size>1) ≡ superposed
Just (NonEmptySet size=1) ≡ collapsed
Nothing                   ≡ contradiction
```

No sentinel values, no magic integers, no `observed[i] = -1`.

---

## Module Structure

```
src/
  WFC/
    Grid.purs          -- Pos, GridSize, 2D array helpers, row-major indexing
    Direction.purs     -- Direction, dirOffset, opposite, allDirections
    Pattern.purs       -- Pattern a, rotate, reflect, symmetryVariants, agrees
    Catalog.purs       -- PatternCatalog, extractPatterns (Stage 1)
    Rules.purs         -- AdjacencyRules, buildRules (Stage 2)
    Wave.purs          -- CellState, Cell, Wave, initWave (Stage 3)
    Entropy.purs       -- cellEntropy, waveEntropies, minEntropyPos (Stage 4)
    Collapse.purs      -- weightedSample, collapseCell (Stage 5)
    Propagate.purs     -- PropM, propagate, processBan (Stage 6)
    Algorithm.purs     -- step, wfc — composes stages 4-6 (Stage 7)
    Render.purs        -- renderWave, topLeftPixel (Stage 8)
    Comonad.purs       -- FocusedGrid, propagateComonad (alternative, didactic)
  WFC.purs             -- Re-exports public API: extractPatterns, buildRules, wfc, renderWave
```

---

## Backtracking

Original WFC has no backtracking — contradictions restart from a new seed.
Three options to consider for our implementation:

**Option A — Restart (matches original):** `wfc` returns `Left Contradiction`;
caller retries with a new `Gen` seed. Simplest; sufficient for most tilesets.

```purescript
runUntilSuccess :: forall m a. MonadGen m => Int -> PatternCatalog a -> Wave -> m (Maybe Wave)
runUntilSuccess maxAttempts catalog initialWave = go maxAttempts
  where
    go 0 = pure Nothing
    go n = wfc catalog initialWave >>= case _ of
      Right wave -> pure (Just wave)
      Left _     -> go (n - 1)
```

**Option B — Snapshot stack:** Before each collapse, push a `Wave` snapshot.
On contradiction, pop and try a different collapse. Purely functional; `Wave` snapshots
are cheap with persistent `Map` (structural sharing).

```purescript
type SearchState = { current :: Wave, snapshots :: List (Wave /\ Pos) }
```

**Option C — Lazy search tree:** Model all possible collapses as a lazy `Tree Wave`
(rose tree of continuations), prune contradictions, take first `Done` leaf.
Elegant but high memory for large grids.

Recommendation: **implement A first**, add **B as an option** — it's natural with
persistent maps and enables reliable generation for difficult tilesets.

---

## Performance Notes

- `Map Pos Cell` is functional but slower than a 2D array for large grids.
  Consider `Data.Array` with `pos.x + pos.y * width` indexing, wrapped in a newtype,
  for the hot path. Swap representation behind the `Wave` newtype without changing API.
- `purescript-st` allows in-place `STArray` updates inside `ST` for propagation inner
  loop if profiling shows Map overhead is a bottleneck.
- `tailRecM` in propagation ensures no stack overflow on grids where a single collapse
  triggers a long chain of bans (e.g. large uniform areas).
- Pattern extraction is one-time cost; `PatternCatalog` construction is O(H*W*T²*N²).
  This is offline — not latency-sensitive.
- Entropy is O(1) per ban update (incremental sums), not O(T) per cell.
