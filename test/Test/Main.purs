module Test.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), isLeft)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (log)
import Data.List.NonEmpty as NonEmpty
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import WFC.Catalog (PatternCatalog, extractPatterns)
import WFC.Direction (Direction(..), allDirections, opposite)
import WFC.Grid (Pos(..), allPositions, neighborPos)
import WFC.Pattern (Pattern(..), PatternId(..), agrees, reflect, rotate)
import WFC.Rules (AdjacencyRules, buildRules, lookupNeighbors)
import WFC.Wave (Wave, getCellPossibilities, initWave, isFullyCollapsed)
import WFC.Entropy (cellEntropy, cellsWithEntropy, minEntropyPos)
import WFC.Collapse (collapseAt)
import WFC.Propagate (propagate)
import WFC.Algorithm (wfc, wfcWithRetry)
import WFC.Backtrack (StepResult(..), solveWithBacktracking, stepSearch)
import WFC.Render (renderWave, renderWaveWith)
import WFC.Tiles (TileDef, buildTiledCatalog, buildTiledRules, sidesMatch)

-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

-- 3×3 grid where every cell is the same pixel.
-- Produces exactly 1 unique 2×2 pattern.
uniform3x3 :: Array (Array Int)
uniform3x3 =
  [ [0, 0, 0]
  , [0, 0, 0]
  , [0, 0, 0]
  ]

-- 3×3 checkerboard (alternating 0/1).
-- Produces exactly 2 unique 2×2 patterns (each appearing twice):
--   P0 = [0,1,1,0]  (top-left corner)
--   P1 = [1,0,0,1]  (top-right corner)
checker3x3 :: Array (Array Int)
checker3x3 =
  [ [0, 1, 0]
  , [1, 0, 1]
  , [0, 1, 0]
  ]

-- Horizontal stripes: two alternating solid rows.
-- Produces 2 unique 2×2 patterns: top-of-stripe and bottom-of-stripe.
stripes3x3 :: Array (Array Int)
stripes3x3 =
  [ [0, 0, 0]
  , [1, 1, 1]
  , [0, 0, 0]
  ]

-- Pre-built catalog and rules for the checkerboard fixture.
-- P0 = PatternId 0, P1 = PatternId 1 (extraction order is deterministic).
checkerCatalog :: PatternCatalog Int
checkerCatalog = extractPatterns 2 false 1 checker3x3

checkerRules :: AdjacencyRules
checkerRules = buildRules checkerCatalog

-- 2×2 wave seeded from the checkerboard: every cell starts as {P0, P1}.
checker2x2Wave :: Wave Int
checker2x2Wave = initWave checkerCatalog checkerRules { width: 2, height: 2 } false

-- Convenience aliases
p0 :: PatternId
p0 = PatternId 0

p1 :: PatternId
p1 = PatternId 1

pos :: Int -> Int -> Pos
pos x y = Pos { x, y }

-- A small hand-authored road/pipe-style tile set for WFC.Tiles: blank plus
-- one tile per connection shape, using "0"/"1" socket labels (0 = no
-- connection, 1 = connection) — the classic 2-label Wang-tile mechanism.
tileBlank :: TileDef Int
tileBlank = { value: 0, weight: 6.0, sockets: { left: "0", right: "0", up: "0", down: "0" } }

tileHoriz :: TileDef Int
tileHoriz = { value: 1, weight: 2.0, sockets: { left: "1", right: "1", up: "0", down: "0" } }

tileVert :: TileDef Int
tileVert = { value: 2, weight: 2.0, sockets: { left: "0", right: "0", up: "1", down: "1" } }

-- Connects to the right and downward (an "L" turn).
tileCorner :: TileDef Int
tileCorner = { value: 3, weight: 1.0, sockets: { left: "0", right: "1", up: "0", down: "1" } }

tileSet :: Array (TileDef Int)
tileSet = [ tileBlank, tileHoriz, tileVert, tileCorner ]

tileCatalog :: PatternCatalog Int
tileCatalog = buildTiledCatalog tileSet

tileRules :: AdjacencyRules
tileRules = buildTiledRules tileSet

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do

  -- =========================================================================
  describe "Pattern — parametric N×N tile (Functor / Foldable / Traversable)" do
  -- =========================================================================

    describe "rotate 90° clockwise — new(x,y) = old(y, n-1-x)" do

      it "rotates a 2×2 tile: [[1,2],[3,4]] → [[3,1],[4,2]]" do
        let Pattern px = rotate 2 (Pattern [1, 2, 3, 4])
        px `shouldEqual` [3, 1, 4, 2]

      it "four rotations return to the original" do
        let p  = Pattern [1, 2, 3, 4]
            r4 = rotate 2 (rotate 2 (rotate 2 (rotate 2 p)))
        r4 `shouldEqual` p

    describe "reflect horizontally — new(x,y) = old(n-1-x, y)" do

      it "reflects a 2×2 tile: [[1,2],[3,4]] → [[2,1],[4,3]]" do
        let Pattern px = reflect 2 (Pattern [1, 2, 3, 4])
        px `shouldEqual` [2, 1, 4, 3]

      it "two reflections return to the original" do
        let p = Pattern [1, 2, 3, 4]
        reflect 2 (reflect 2 p) `shouldEqual` p

    describe "agrees — overlap-region compatibility check" do
      -- Checkerboard patterns:
      --   P0 = [[0,1],[1,0]]   P1 = [[1,0],[0,1]]
      -- Their right/left columns interlock perfectly: P0's right col = P1's left col.

      it "P0 right of P0 is incompatible (same column, values differ)" do
        agrees 2 DirR (Pattern [0,1,1,0]) (Pattern [0,1,1,0]) `shouldEqual` false

      it "P0 right of P1 is compatible (columns match)" do
        agrees 2 DirR (Pattern [0,1,1,0]) (Pattern [1,0,0,1]) `shouldEqual` true

      it "compatibility is symmetric across direction pairs: DirR ↔ DirL" do
        let p0' = Pattern [0,1,1,0]
            p1' = Pattern [1,0,0,1]
        agrees 2 DirR p0' p1' `shouldEqual` agrees 2 DirL p1' p0'

    it "Pattern is a Functor — map transforms every pixel independently" do
      map (_ * 2) (Pattern [1, 2, 3, 4]) `shouldEqual` Pattern [2, 4, 6, 8]

  -- =========================================================================
  describe "Direction — 4-connected grid navigation" do
  -- =========================================================================

    it "opposite is involutive: opposite (opposite d) = d" do
      map (\d -> opposite (opposite d)) allDirections `shouldEqual` allDirections

    it "L↔R and U↔D are the two opposite pairs" do
      opposite DirL `shouldEqual` DirR
      opposite DirR `shouldEqual` DirL
      opposite DirU `shouldEqual` DirD
      opposite DirD `shouldEqual` DirU

    it "allDirections has exactly 4 elements" do
      Array.length allDirections `shouldEqual` 4

  -- =========================================================================
  describe "Grid — coordinates and neighbourhood" do
  -- =========================================================================

    it "neighborPos at a left edge returns Nothing (non-periodic)" do
      neighborPos { width: 3, height: 3 } false (pos 0 0) DirL `shouldEqual` Nothing

    it "neighborPos at a top edge returns Nothing (non-periodic)" do
      neighborPos { width: 3, height: 3 } false (pos 0 0) DirU `shouldEqual` Nothing

    it "neighborPos in bounds returns Just the adjacent cell" do
      neighborPos { width: 3, height: 3 } false (pos 1 1) DirR `shouldEqual` Just (pos 2 1)
      neighborPos { width: 3, height: 3 } false (pos 1 1) DirD `shouldEqual` Just (pos 1 2)

    it "neighborPos wraps around left edge when periodic" do
      neighborPos { width: 3, height: 3 } true (pos 0 0) DirL `shouldEqual` Just (pos 2 0)

    it "neighborPos wraps around top edge when periodic" do
      neighborPos { width: 3, height: 3 } true (pos 0 0) DirU `shouldEqual` Just (pos 0 2)

    it "allPositions covers exactly width × height cells" do
      Array.length (allPositions { width: 4, height: 5 }) `shouldEqual` 20

  -- =========================================================================
  describe "Stage 1 · extractPatterns — harvest N×N tiles from the input sample" do
  -- =========================================================================

    it "a uniform grid yields exactly 1 unique pattern" do
      let cat = extractPatterns 2 false 1 uniform3x3
      Map.size cat.patterns `shouldEqual` 1

    it "a checkerboard yields exactly 2 unique patterns" do
      Map.size checkerCatalog.patterns `shouldEqual` 2

    it "horizontal stripes yield exactly 2 unique patterns" do
      let cat = extractPatterns 2 false 1 stripes3x3
      Map.size cat.patterns `shouldEqual` 2

    it "totalW equals the number of tiles extracted (frequency accounting)" do
      -- 3×3 grid, n=2, non-periodic → 4 tiles; uniform: all same → weight 4
      let cat = extractPatterns 2 false 1 uniform3x3
      cat.totalW `shouldEqual` 4.0

    it "checkerboard: each pattern appears exactly twice (weight = 2)" do
      -- 4 tiles total, 2 patterns → each has weight 2 → totalW = 4
      checkerCatalog.totalW `shouldEqual` 4.0

    it "pattern size (n) is stored in the catalog" do
      checkerCatalog.size `shouldEqual` 2

    it "periodic mode wraps the sample and finds more tiles" do
      -- 2×2 periodic checkerboard: all 4 wrapping positions give the 2 patterns
      let cat = extractPatterns 2 true 1 [[0,1],[1,0]]
      Map.size cat.patterns `shouldEqual` 2

  -- =========================================================================
  describe "Stage 2 · buildRules — derive adjacency constraints between patterns" do
  -- =========================================================================

    it "in a checkerboard, P0 can only neighbor P1 (not itself) to the right" do
      let neighbors = Array.sort $ lookupNeighbors checkerRules DirR p0
      neighbors `shouldEqual` [p1]

    it "in a checkerboard, all four directions enforce the alternation constraint" do
      let nbrs dir = Array.sort $ lookupNeighbors checkerRules dir p0
      nbrs DirL `shouldEqual` [p1]
      nbrs DirR `shouldEqual` [p1]
      nbrs DirU `shouldEqual` [p1]
      nbrs DirD `shouldEqual` [p1]

    it "the sole pattern in a uniform grid is compatible with itself in every direction" do
      let cat   = extractPatterns 2 false 1 uniform3x3
          rules = buildRules cat
          nbrs dir = Array.sort $ lookupNeighbors rules dir (PatternId 0)
      nbrs DirL `shouldEqual` [PatternId 0]
      nbrs DirR `shouldEqual` [PatternId 0]
      nbrs DirU `shouldEqual` [PatternId 0]
      nbrs DirD `shouldEqual` [PatternId 0]

    it "adjacency is mutually consistent: P0 right-of P1 ↔ P1 left-of P0" do
      -- If P1 is a valid right-neighbor of P0, then P0 is a valid left-neighbor of P1.
      let p0InP1Left  = Array.elem p0 (lookupNeighbors checkerRules DirL p1)
          p1InP0Right = Array.elem p1 (lookupNeighbors checkerRules DirR p0)
      p0InP1Left `shouldEqual` p1InP0Right

  -- =========================================================================
  describe "Stage 3 · initWave — place all patterns in superposition at every cell" do
  -- =========================================================================

    it "every cell starts with all patterns as possibilities" do
      let cell = getCellPossibilities checker2x2Wave (pos 0 0)
      cell `shouldEqual` Just (Set.fromFoldable [p0, p1])

    it "the wave is not fully collapsed initially" do
      isFullyCollapsed checker2x2Wave `shouldEqual` false

    it "cell count equals width × height" do
      Map.size checker2x2Wave.cells `shouldEqual` 4

    it "a uniform wave (1 pattern) is considered fully collapsed from the start" do
      -- With only one possible pattern, every cell is already a singleton set.
      let cat   = extractPatterns 2 false 1 uniform3x3
          rules = buildRules cat
          wave  = initWave cat rules { width: 3, height: 3 } false
      isFullyCollapsed wave `shouldEqual` true

  -- =========================================================================
  describe "Stage 4 · entropy — measure uncertainty; pick the cell to observe next" do
  -- =========================================================================

    it "a singleton possibility set has entropy 0 — no uncertainty" do
      -- H = ln(w) - (w*ln(w))/w = 0 regardless of weight
      cellEntropy checker2x2Wave (Set.singleton p0) `shouldEqual` 0.0

    it "two equal-weight patterns give entropy ln 2 ≈ 0.693" do
      -- H = ln(2w) - (2 * w*ln(w)) / (2w) = ln(2w) - ln(w) = ln(2)
      let bothPids = Set.fromFoldable [p0, p1]
      cellEntropy checker2x2Wave bothPids `shouldEqual` log 2.0

    it "cellsWithEntropy skips collapsed (singleton) and contradiction (Nothing) cells" do
      -- checker2x2Wave has all cells with 2 options → 4 cells reported
      Array.length (cellsWithEntropy checker2x2Wave) `shouldEqual` 4

    it "minEntropyPos returns Nothing when all cells are already collapsed" do
      -- uniform wave has every cell as a 1-element set → no entropy to minimise
      let cat   = extractPatterns 2 false 1 uniform3x3
          rules = buildRules cat
          wave  = initWave cat rules { width: 3, height: 3 } false
      mPos <- liftEffect $ minEntropyPos wave
      mPos `shouldEqual` Nothing

    it "minEntropyPos returns Just pos when uncollapsed cells exist" do
      mPos <- liftEffect $ minEntropyPos checker2x2Wave
      mPos `shouldSatisfy` isJust

  -- =========================================================================
  describe "Stage 5 · collapseAt — weighted-random single-pattern selection" do
  -- =========================================================================

    it "the target cell becomes a singleton after collapse" do
      let isSingleton c = case c of
            Just s  -> Set.size s == 1
            Nothing -> false
      result <- liftEffect $ collapseAt checker2x2Wave (pos 0 0)
      case result of
        Left  _  -> fail "unexpected contradiction collapsing a valid checkerboard cell"
        Right w' ->
          getCellPossibilities w' (pos 0 0) `shouldSatisfy` isSingleton

    it "the chosen pattern was among the original possibilities" do
      result <- liftEffect $ collapseAt checker2x2Wave (pos 0 0)
      case result of
        Left  _  -> fail "unexpected contradiction"
        Right w' ->
          getCellPossibilities w' (pos 0 0)
            `shouldSatisfy` (\c -> case c of
              Just s -> Set.size s == 1 &&
                        Array.all (\pid -> pid == p0 || pid == p1)
                                  (Set.toUnfoldable s :: Array PatternId)
              Nothing -> false)

  -- =========================================================================
  describe "Stage 6 · propagate — remove impossible tiles; spread constraints" do
  -- =========================================================================

    it "banning a tile removes it from the target cell" do
      case propagate checker2x2Wave [Tuple (pos 0 0) p1] of
        Left  _  -> fail "unexpected contradiction banning one of two tiles"
        Right w' ->
          getCellPossibilities w' (pos 0 0) `shouldEqual` Just (Set.singleton p0)

    it "banning the only tile causes a Contradiction" do
      -- One-pattern catalog → ban that pattern → the cell has no options left.
      let cat   = extractPatterns 2 false 1 uniform3x3
          rules = buildRules cat
          wave  = initWave cat rules { width: 2, height: 2 } false
      isLeft (propagate wave [Tuple (pos 0 0) (PatternId 0)]) `shouldEqual` true

    it "constraint propagation fully determines a 2×2 checkerboard wave from one ban" do
      -- Banning P1 from (0,0) forces P0 there.
      -- P0 can only neighbor P1 → (1,0) and (0,1) become {P1}.
      -- Those P1s can only neighbor P0 → (1,1) becomes {P0}.
      -- One ban event cascades to full determination.
      case propagate checker2x2Wave [Tuple (pos 0 0) p1] of
        Left  _  -> fail "unexpected contradiction"
        Right w' -> do
          getCellPossibilities w' (pos 0 0) `shouldEqual` Just (Set.singleton p0)
          getCellPossibilities w' (pos 1 0) `shouldEqual` Just (Set.singleton p1)
          getCellPossibilities w' (pos 0 1) `shouldEqual` Just (Set.singleton p1)
          getCellPossibilities w' (pos 1 1) `shouldEqual` Just (Set.singleton p0)

    it "banning an already-absent tile is a no-op (idempotent)" do
      -- Propagate twice with the same ban; result should be the same wave.
      let ban = [Tuple (pos 0 0) p1]
      case propagate checker2x2Wave ban of
        Left  _   -> fail "first propagate failed unexpectedly"
        Right w1  ->
          case propagate w1 ban of
            Left  _   -> fail "second propagate failed — but P1 is already absent"
            Right w2  ->
              getCellPossibilities w2 (pos 0 0) `shouldEqual` Just (Set.singleton p0)

  -- =========================================================================
  describe "Stage 7 · wfc — iterate step-by-step until fully collapsed or contradiction" do
  -- =========================================================================

    it "a uniform wave (already collapsed) returns Right immediately" do
      let cat   = extractPatterns 2 false 1 uniform3x3
          rules = buildRules cat
          wave  = initWave cat rules { width: 3, height: 3 } false
      result <- liftEffect $ wfc wave
      case result of
        Left  _  -> fail "contradiction in a trivially collapsed uniform wave"
        Right w' -> isFullyCollapsed w' `shouldEqual` true

    it "a 2×2 checkerboard wave always collapses fully (no contradictions possible)" do
      -- The 2×2 grid is so constrained that any first collapse fully determines all cells.
      result <- liftEffect $ wfc checker2x2Wave
      case result of
        Left  _  -> fail "unexpected contradiction in 2×2 checkerboard"
        Right w' -> isFullyCollapsed w' `shouldEqual` true

    it "wfcWithRetry returns Just a fully-collapsed wave" do
      mWave <- liftEffect $ wfcWithRetry 10 checker2x2Wave
      case mWave of
        Nothing -> fail "wfcWithRetry failed 10 times on a 2×2 checkerboard"
        Just w' -> isFullyCollapsed w' `shouldEqual` true

  -- =========================================================================
  describe "Stage 8 · render — extract a pixel grid from the collapsed wave" do
  -- =========================================================================

    it "renderWave returns Nothing for an uncollapsed wave" do
      renderWave checker2x2Wave `shouldEqual` (Nothing :: Maybe (Array (Array Int)))

    it "renderWaveWith substitutes the fallback for every uncollapsed cell" do
      -- checker2x2Wave has no singleton cells → every pixel is the fallback.
      renderWaveWith (-1) checker2x2Wave `shouldEqual` [[-1, -1], [-1, -1]]

    it "renderWave returns Just grid after full WFC collapse" do
      result <- liftEffect $ wfc checker2x2Wave
      case result of
        Left  _  -> fail "unexpected contradiction"
        Right w' -> renderWave w' `shouldSatisfy` isJust

    it "the rendered grid has the correct dimensions" do
      result <- liftEffect $ wfc checker2x2Wave
      case result of
        Left  _  -> fail "unexpected contradiction"
        Right w' ->
          case renderWave w' of
            Nothing   -> fail "wave not fully collapsed after wfc"
            Just grid -> do
              Array.length grid `shouldEqual` 2
              map Array.length grid `shouldEqual` [2, 2]

    it "all pixels in the rendered grid come from the original palette (0 or 1)" do
      result <- liftEffect $ wfc checker2x2Wave
      case result of
        Left  _  -> fail "unexpected contradiction"
        Right w' ->
          case renderWave w' of
            Nothing   -> fail "wave not fully collapsed"
            Just grid ->
              Array.all (Array.all (\px -> px == 0 || px == 1)) grid `shouldEqual` true

    it "renderWaveWith on a collapsed wave produces no fallback pixels" do
      result <- liftEffect $ wfc checker2x2Wave
      case result of
        Left  _  -> fail "unexpected contradiction"
        Right w' ->
          Array.all (Array.all (_ /= -1)) (renderWaveWith (-1) w') `shouldEqual` true

  -- =========================================================================
  describe "Stage 9 · WFC.Backtrack — incremental backtracking recovery" do
  -- =========================================================================
  -- Backtracking undoes just the last guess (ban that value, try another at
  -- the same cell) instead of restarting the whole wave like
  -- `WFC.Algorithm.wfcWithRetry` does on contradiction.

    it "stepSearch backtracks (BackedOut) to the parent frame once untried is exhausted" do
      -- Deterministic, rather than relying on a puzzle hard enough to
      -- actually need a full pop by chance (most real catalogs resolve
      -- local dead-ends via same-cell retries alone — see the maze
      -- diagnostic above, 0 pops across many solves): construct a 2-frame
      -- stack directly, with the top frame's `untried` already empty.
      let childFrame  = { wave: checker2x2Wave, pos: pos 1 0, untried: (Set.empty :: Set.Set PatternId) }
          parentFrame = { wave: checker2x2Wave, pos: pos 0 0, untried: Set.singleton p0 }
          st = { stack: NonEmpty.cons childFrame (NonEmpty.singleton parentFrame), attempts: 0 }
      result <- liftEffect (stepSearch st)
      case result of
        BackedOut st' -> NonEmpty.length st'.stack `shouldEqual` 1
        Continue _    -> fail "expected BackedOut, not a forward Continue"
        Solved _      -> fail "expected BackedOut, not Solved"
        Failed _      -> fail "expected BackedOut, not Failed"

    it "solves a trivial 2×2 checkerboard, same as plain wfc" do
      result <- liftEffect $ solveWithBacktracking 100 checker2x2Wave
      case result of
        Left  _  -> fail "unexpected contradiction in 2×2 checkerboard"
        Right w' -> isFullyCollapsed w' `shouldEqual` true

    it "maxAttempts = 0 fails immediately without collapsing anything" do
      result <- liftEffect $ solveWithBacktracking 0 checker2x2Wave
      isLeft result `shouldEqual` true

    it "reliably solves a maze that plain single-shot wfc usually can't" do
      -- This 11×11 maze at n=3, non-periodic, is tightly constrained enough
      -- that a single un-retried `wfc` run fails almost every time (measured
      -- ~19/20 across repeated manual runs) — demonstrating that undoing one
      -- bad guess and trying a different value at the same cell recovers
      -- where plain restart-free collapse can't, without throwing away the
      -- whole wave.
      let grid =
            [ [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
            , [1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1]
            , [1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1]
            , [1, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1]
            , [1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1]
            , [1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1]
            , [1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1]
            , [1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1]
            , [1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1]
            , [1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1]
            , [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
            ]
          cat   = extractPatterns 3 false 1 grid
          rules = buildRules cat
          wave  = initWave cat rules { width: 22, height: 22 } false

          cellPidAt w x y = do
            s <- getCellPossibilities w (Pos { x, y })
            if Set.size s == 1
              then Array.head (Set.toUnfoldable s :: Array PatternId)
              else Nothing
          patternOf pid = fromMaybe (Pattern []) (Map.lookup pid cat.patterns)

      result <- liftEffect $ solveWithBacktracking 5000 wave
      case result of
        Left  _  -> fail "backtracking failed to solve a maze it should reliably handle"
        Right w' -> do
          isFullyCollapsed w' `shouldEqual` true
          -- Structural correctness, not just "no cell ended up empty": every
          -- adjacent pair of collapsed cells must genuinely satisfy the
          -- overlap-agreement rule (same check used to validate the engine
          -- against noise-output regressions earlier).
          let violations = do
                y <- Array.range 0 20
                x <- Array.range 0 20
                Tuple dir (Tuple ox oy) <- [ Tuple DirR (Tuple 1 0), Tuple DirD (Tuple 0 1) ]
                let nx = x + ox
                    ny = y + oy
                case Tuple (cellPidAt w' x y) (cellPidAt w' nx ny) of
                  Tuple (Just pidA) (Just pidB) ->
                    if agrees 3 dir (patternOf pidA) (patternOf pidB)
                      then []
                      else [ Tuple (Tuple x y) (Tuple nx ny) ]
                  _ -> []
          violations `shouldEqual` []

  -- =========================================================================
  describe "Stage 10 · WFC.Tiles — hand-authored tiles, socket adjacency" do
  -- =========================================================================
  -- Same solving engine as the overlapping model — WFC.Tiles only builds a
  -- PatternCatalog/AdjacencyRules pair a different way (sockets, not pixel
  -- overlap), everything downstream is identical.

    it "one PatternId per tile, size 1 (a tile is a single value, not an NxN block)" do
      Map.size tileCatalog.patterns `shouldEqual` 4
      tileCatalog.size `shouldEqual` 1

    it "weights come directly from the tile def, not an occurrence count" do
      tileCatalog.totalW `shouldEqual` 11.0 -- 6 + 2 + 2 + 1
      Map.lookup (PatternId 0) tileCatalog.weights `shouldEqual` Just 6.0
      Map.lookup (PatternId 3) tileCatalog.weights `shouldEqual` Just 1.0

    it "blank (right=0) can neighbor any tile whose left socket is 0, not horiz (left=1)" do
      let neighbors = Array.sort $ lookupNeighbors tileRules DirR (PatternId 0)
      neighbors `shouldEqual` [ PatternId 0, PatternId 2, PatternId 3 ]

    it "horiz (right=1) can only neighbor tiles whose left socket is 1" do
      let neighbors = Array.sort $ lookupNeighbors tileRules DirR (PatternId 1)
      neighbors `shouldEqual` [ PatternId 1 ]

    it "adjacency is mutually consistent: corner right-of blank ↔ blank left-of corner" do
      let cornerInBlankRight = Array.elem (PatternId 3) (lookupNeighbors tileRules DirR (PatternId 0))
          blankInCornerLeft  = Array.elem (PatternId 0) (lookupNeighbors tileRules DirL (PatternId 3))
      cornerInBlankRight `shouldEqual` blankInCornerLeft

    it "solves a periodic tiled wave into a socket-consistent result" do
      let wave = initWave tileCatalog tileRules { width: 10, height: 10 } true

          cellPidAt w x y = do
            s <- getCellPossibilities w (Pos { x, y })
            if Set.size s == 1
              then Array.head (Set.toUnfoldable s :: Array PatternId)
              else Nothing
          tileOf (PatternId i) = fromMaybe tileBlank (Array.index tileSet i)

      result <- liftEffect $ solveWithBacktracking 2000 wave
      case result of
        Left  _  -> fail "backtracking failed to solve a small tiled wave"
        Right w' -> do
          isFullyCollapsed w' `shouldEqual` true
          let violations = do
                y <- Array.range 0 9
                x <- Array.range 0 9
                Tuple dir (Tuple ox oy) <- [ Tuple DirR (Tuple 1 0), Tuple DirD (Tuple 0 1) ]
                let nx = (x + ox) `mod` 10
                    ny = (y + oy) `mod` 10
                case Tuple (cellPidAt w' x y) (cellPidAt w' nx ny) of
                  Tuple (Just pidA) (Just pidB) ->
                    if sidesMatch dir (tileOf pidA) (tileOf pidB)
                      then []
                      else [ Tuple (Tuple x y) (Tuple nx ny) ]
                  _ -> []
          violations `shouldEqual` []
