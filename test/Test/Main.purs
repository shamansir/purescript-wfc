module Test.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..), isLeft)
import Data.Foldable (all, for_)
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

import WFC.Catalog (PatternCatalog, extractPatterns, lastPatternId)
import WFC.Direction (Direction(..), allDirections, opposite)
import WFC.Grid (Pos(..), allPositions, neighborPos)
import WFC.Pattern (Pattern(..), PatternId(..), agrees, reflect, rotate)
import WFC.Rules (AdjacencyRules, buildRules, lookupNeighbors)
import WFC.Wave (Wave, getCellPossibilities, initWave, isFullyCollapsed)
import WFC.Entropy (cellEntropy, cellsWithEntropy, minEntropyPos)
import WFC.Collapse (collapseAt)
import WFC.Propagate (applyGround, propagate)
import WFC.Algorithm (wfc, wfcWithRetry)
import WFC.Backtrack (StepResult(..), solveWithBacktracking, stepSearch)
import WFC.Render (renderWave, renderWaveWith)
import WFC.Tiles (TileDef, buildTiledCatalog, buildTiledRules, sidesMatch)
import WFC.TileSet as TS
import WFC.TileSet.Symmetry (Symmetry(..), cardinality, distinctOrientations, parseSymmetry, rotateIndex, rotateIndexBy)
import WFC.TileSet.Xml (parseTileSetXml)

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

-- A single 2×2 window, all 4 pixel values distinct — no rotation or
-- reflection of it ever equals another, making rotate/mirror variant
-- counts exact (see the extractPatterns rotation/mirror tests below).
asymGrid :: Array (Array Int)
asymGrid =
  [ [0, 1]
  , [2, 3]
  ]

-- Pre-built catalog and rules for the checkerboard fixture.
-- P0 = PatternId 0, P1 = PatternId 1 (extraction order is deterministic).
checkerCatalog :: PatternCatalog Int
checkerCatalog = extractPatterns 2 false false false checker3x3

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

-- A 2-tile fixture for the applyGround tests below: `groundTile` is
-- self-compatible horizontally (like a real sample's solid ground row) and
-- sits directly beneath `skyTile`, which is self-compatible in every
-- direction (so any number of non-ground rows can stack on each other).
-- `groundTile` is last in the array, matching `lastPatternId`'s "highest
-- PatternId = T-1" assumption.
skyTile :: TileDef Int
skyTile = { value: 0, weight: 1.0, sockets: { left: "s", right: "s", up: "u", down: "u" } }

groundTile :: TileDef Int
groundTile = { value: 1, weight: 1.0, sockets: { left: "g", right: "g", up: "u", down: "d" } }

groundSkyCatalog :: PatternCatalog Int
groundSkyCatalog = buildTiledCatalog [ skyTile, groundTile ]

groundSkyRules :: AdjacencyRules
groundSkyRules = buildTiledRules [ skyTile, groundTile ]

pSky :: PatternId
pSky = PatternId 0

pGround :: PatternId
pGround = PatternId 1

-- test/Demo/tilesets/Knots.xml, embedded verbatim — the smallest of the 7
-- real original-WFC tileset XML files (5 tiles, 35 neighbor rules, 10
-- subsets), used to cross-check WFC.TileSet.Xml/WFC.TileSet end-to-end
-- against real data rather than only synthetic fixtures.
knotsXml :: String
knotsXml =
  """<set>
  <tiles>
    <tile name="corner" symmetry="L"/>
    <tile name="cross" symmetry="I"/>
    <tile name="empty" symmetry="X"/>
    <tile name="line" symmetry="I"/>
    <tile name="t" symmetry="T"/>
  </tiles>
  <neighbors>
    <neighbor left="corner 1" right="empty"/>
    <neighbor left="corner" right="cross"/>
    <neighbor left="corner" right="cross 1"/>
    <neighbor left="corner" right="line"/>
    <neighbor left="corner 1" right="line 1"/>
    <neighbor left="corner" right="t 2"/>
    <neighbor left="corner" right="t 3"/>
    <neighbor left="corner" right="t"/>
    <neighbor left="corner 1" right="t 1"/>
    <neighbor left="corner 1" right="corner 3"/>
    <neighbor left="corner 1" right="corner"/>
    <neighbor left="corner" right="corner 1"/>
    <neighbor left="corner" right="corner 2"/>
    <neighbor left="cross" right="cross"/>
    <neighbor left="cross" right="cross 1"/>
    <neighbor left="cross 1" right="cross 1"/>
    <neighbor left="cross" right="line"/>
    <neighbor left="cross 1" right="line"/>
    <neighbor left="cross" right="t"/>
    <neighbor left="cross" right="t 3"/>
    <neighbor left="cross 1" right="t"/>
    <neighbor left="cross 1" right="t 3"/>
    <neighbor left="empty" right="empty"/>
    <neighbor left="empty" right="line 1"/>
    <neighbor left="empty" right="t 1"/>
    <neighbor left="line" right="line"/>
    <neighbor left="line 1" right="line 1"/>
    <neighbor left="line" right="t"/>
    <neighbor left="line 1" right="t 1"/>
    <neighbor left="line" right="t 3"/>
    <neighbor left="t 1" right="t 3"/>
    <neighbor left="t" right="t"/>
    <neighbor left="t 2" right="t"/>
    <neighbor left="t 1" right="t"/>
    <neighbor left="t 3" right="t 1"/>
  </neighbors>
  <subsets>
    <subset name="Standard">
      <tile name="corner"/>
      <tile name="cross"/>
      <tile name="empty"/>
      <tile name="line"/>
    </subset>
    <subset name="Dense">
      <tile name="corner"/>
      <tile name="cross"/>
      <tile name="line"/>
    </subset>
    <subset name="Crossless">
      <tile name="corner"/>
      <tile name="empty"/>
      <tile name="line"/>
    </subset>
    <subset name="TE">
      <tile name="t"/>
      <tile name="empty"/>
    </subset>
    <subset name="T">
      <tile name="t"/>
    </subset>
    <subset name="CL">
      <tile name="corner"/>
      <tile name="line"/>
    </subset>
    <subset name="CE">
      <tile name="corner"/>
      <tile name="empty"/>
    </subset>
    <subset name="C">
      <tile name="corner"/>
    </subset>
    <subset name="Fabric">
      <tile name="cross"/>
      <tile name="line"/>
    </subset>
    <subset name="Dense Fabric">
      <tile name="cross"/>
    </subset>
  </subsets>
</set>
"""

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
      let cat = extractPatterns 2 false false false uniform3x3
      Map.size cat.patterns `shouldEqual` 1

    it "a checkerboard yields exactly 2 unique patterns" do
      Map.size checkerCatalog.patterns `shouldEqual` 2

    it "horizontal stripes yield exactly 2 unique patterns" do
      let cat = extractPatterns 2 false false false stripes3x3
      Map.size cat.patterns `shouldEqual` 2

    it "totalW equals the number of tiles extracted (frequency accounting)" do
      -- 3×3 grid, n=2, non-periodic → 4 tiles; uniform: all same → weight 4
      let cat = extractPatterns 2 false false false uniform3x3
      cat.totalW `shouldEqual` 4.0

    it "checkerboard: each pattern appears exactly twice (weight = 2)" do
      -- 4 tiles total, 2 patterns → each has weight 2 → totalW = 4
      checkerCatalog.totalW `shouldEqual` 4.0

    it "pattern size (n) is stored in the catalog" do
      checkerCatalog.size `shouldEqual` 2

    it "periodic mode wraps the sample and finds more tiles" do
      -- 2×2 periodic checkerboard: all 4 wrapping positions give the 2 patterns
      let cat = extractPatterns 2 true false false [[0,1],[1,0]]
      Map.size cat.patterns `shouldEqual` 2

    -- A single 2×2 window with 4 distinct pixel values: no rotation or
    -- reflection of it ever coincides with another, so each toggle's
    -- output count is exact — a clean way to check `variantsFor`'s wiring
    -- through `extractPatterns` without depending on rotate/reflect's
    -- internals being separately correct.
    it "with rotations off and mirror off, only the original window is extracted" do
      let cat = extractPatterns 2 false false false asymGrid
      Map.size cat.patterns `shouldEqual` 1

    it "with rotations on, all 4 rotations of an asymmetric window are extracted" do
      let cat = extractPatterns 2 false true false asymGrid
      Map.size cat.patterns `shouldEqual` 4

    it "with mirror on, the original and its reflection are extracted" do
      let cat = extractPatterns 2 false false true asymGrid
      Map.size cat.patterns `shouldEqual` 2

    it "with both rotations and mirror on, all 8 dihedral variants are extracted" do
      let cat = extractPatterns 2 false true true asymGrid
      Map.size cat.patterns `shouldEqual` 8

    it "with neither toggle, no pattern is marked as a rotation/mirror-only origin" do
      let cat = extractPatterns 2 false false false asymGrid
      Map.size cat.origins `shouldEqual` 0

    it "with rotations on, exactly the 3 non-base rotations are marked rotated-only" do
      let cat     = extractPatterns 2 false true false asymGrid
          origins = Array.fromFoldable (Map.values cat.origins)
      Map.size cat.origins `shouldEqual` 3
      origins `shouldSatisfy` all (\o -> o.rotated && not o.mirrored)

    it "with mirror on, exactly the 1 non-base reflection is marked mirrored-only" do
      let cat     = extractPatterns 2 false false true asymGrid
          origins = Array.fromFoldable (Map.values cat.origins)
      Map.size cat.origins `shouldEqual` 1
      origins `shouldSatisfy` all (\o -> o.mirrored && not o.rotated)

    it "with both on, the 7 non-base variants split 3 rotated / 1 mirrored / 3 both" do
      let cat        = extractPatterns 2 false true true asymGrid
          origins    = Array.fromFoldable (Map.values cat.origins)
          rotOnly    = Array.filter (\o -> o.rotated && not o.mirrored) origins
          mirOnly    = Array.filter (\o -> o.mirrored && not o.rotated) origins
          both       = Array.filter (\o -> o.rotated && o.mirrored) origins
      Map.size cat.origins `shouldEqual` 7
      Array.length rotOnly `shouldEqual` 3
      Array.length mirOnly `shouldEqual` 1
      Array.length both `shouldEqual` 3

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
      let cat   = extractPatterns 2 false false false uniform3x3
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
      let cat   = extractPatterns 2 false false false uniform3x3
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
      let cat   = extractPatterns 2 false false false uniform3x3
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
      let cat   = extractPatterns 2 false false false uniform3x3
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
      let cat   = extractPatterns 2 false false false uniform3x3
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
          cat   = extractPatterns 3 false false false grid
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

  -- =========================================================================
  describe "Stage 11 · WFC.TileSet — original-WFC tileset system (symmetry + neighbor rules, XML)" do
  -- =========================================================================
  -- Same solving engine, same PatternCatalog/AdjacencyRules pair as
  -- WFC.Tiles's socket-based model — this module is a different way to
  -- *author* one: symmetry-expanded tiles + an explicit, rotation-expanded
  -- neighbor-rule list (parsed from the original algorithm's XML format),
  -- instead of hand-listed per-tile sockets.

    describe "WFC.TileSet.Symmetry — per-class rotation cardinality" do

      it "cardinality matches each symmetry class' distinct-orientation count" do
        cardinality SymX `shouldEqual` 1
        cardinality SymI `shouldEqual` 2
        cardinality SymDiag `shouldEqual` 2
        cardinality SymL `shouldEqual` 4
        cardinality SymT `shouldEqual` 4
        cardinality SymF `shouldEqual` 8

      it "distinctOrientations is exactly 0..cardinality-1" do
        distinctOrientations SymX `shouldEqual` [ 0 ]
        distinctOrientations SymI `shouldEqual` [ 0, 1 ]
        distinctOrientations SymL `shouldEqual` [ 0, 1, 2, 3 ]
        distinctOrientations SymF `shouldEqual` [ 0, 1, 2, 3, 4, 5, 6, 7 ]

      it "rotating 4 times returns to the start, for every symmetry class" do
        let allSyms = [ SymX, SymI, SymDiag, SymL, SymT, SymF ]
        for_ allSyms \sym ->
          for_ (distinctOrientations sym) \i ->
            rotateIndexBy sym 4 i `shouldEqual` i

      it "SymI/SymDiag rotation is order-2 (180° rotation is a no-op)" do
        rotateIndex SymI 0 `shouldEqual` 1
        rotateIndex SymI 1 `shouldEqual` 0
        rotateIndexBy SymI 2 0 `shouldEqual` 0
        rotateIndexBy SymDiag 2 1 `shouldEqual` 1

      it "SymL/SymT rotation is a plain 4-cycle" do
        map (rotateIndex SymL) [ 0, 1, 2, 3 ] `shouldEqual` [ 1, 2, 3, 0 ]
        map (rotateIndex SymT) [ 0, 1, 2, 3 ] `shouldEqual` [ 1, 2, 3, 0 ]

      it "parseSymmetry accepts all 6 class codes and rejects unknown ones" do
        parseSymmetry "X" `shouldEqual` Right SymX
        parseSymmetry "I" `shouldEqual` Right SymI
        parseSymmetry "\\" `shouldEqual` Right SymDiag
        parseSymmetry "L" `shouldEqual` Right SymL
        parseSymmetry "T" `shouldEqual` Right SymT
        parseSymmetry "F" `shouldEqual` Right SymF
        parseSymmetry "?" `shouldSatisfy` isLeft

    describe "expandRule — one declared rule expanded across rotation AND reflection" do

      -- `expandRule` itself only ever builds `DirL`/`DirD` facts directly
      -- (a faithful port of `SimpleTiledModel.cs`'s `densePropagator[0]`/
      -- `[1]`) — `DirR`/`DirU` are never built here, they fall out of
      -- `buildTileSet`'s existing opposite-direction reciprocal insert on
      -- every fact (mirroring the original's `densePropagator[2]`/`[3]`
      -- transpose-fill). So these tests check `expandRule`'s raw output
      -- for `DirL`/`DirD` only; the "buildTileSet" tests below check the
      -- full picture (including the emergent `DirR`/`DirU` coverage) via
      -- `lookupNeighbors`.

      it "a fully asymmetric (SymF) pair produces 8 facts, 4 DirL + 4 DirD" do
        let symOf _ = SymF
            rule = { leftName: "a", leftRot: 0, rightName: "b", rightRot: 0 }
            facts = TS.expandRule symOf rule
        Array.length facts `shouldEqual` 8
        Array.sort (map _.dir facts) `shouldEqual` [ DirL, DirL, DirL, DirL, DirD, DirD, DirD, DirD ]

      it "the reflect-derived facts are real, distinct pairs (SymF has no symmetry to collapse them)" do
        let symOf _ = SymF
            rule = { leftName: "a", leftRot: 0, rightName: "b", rightRot: 0 }
            facts = TS.expandRule symOf rule
            atDirL = Array.filter (\f -> f.dir == DirL) facts
            ti n o = TS.TileInstance { name: n, orientation: o }
        atDirL `shouldEqual`
          [ { dir: DirL, left: ti "b" 0, right: ti "a" 0 }
          , { dir: DirL, left: ti "b" 6, right: ti "a" 6 }
          , { dir: DirL, left: ti "a" 4, right: ti "b" 4 }
          , { dir: DirL, left: ti "a" 2, right: ti "b" 2 }
          ]

      it "SymL's mirror image is a different rotation of itself, not a no-op" do
        -- A straight ("I") track next to a corner ("L") turn — same shape
        -- as Circuit.xml's actual `<neighbor left="track" right="turn"/>`.
        -- The corner's reflection lands on one of its own *other* rotations
        -- (`reflectIndex SymL 0 == 1`), so the reflect-derived facts touch
        -- `turn`'s orientations 1 and 2 here too (not just the 0/1/2/3 a
        -- plain rotation sweep alone would already reach) — this richer,
        -- per-direction coverage is the exact gap that caused Circuit's
        -- roads to break.
        let symOf "track" = SymI
            symOf _         = SymL
            rule = { leftName: "track", leftRot: 0, rightName: "turn", rightRot: 0 }
            facts = TS.expandRule symOf rule
            atDirL = Array.filter (\f -> f.dir == DirL) facts
            ti n o = TS.TileInstance { name: n, orientation: o }
        atDirL `shouldEqual`
          [ { dir: DirL, left: ti "turn" 0, right: ti "track" 0 }
          , { dir: DirL, left: ti "turn" 3, right: ti "track" 0 }
          , { dir: DirL, left: ti "track" 0, right: ti "turn" 1 }
          , { dir: DirL, left: ti "track" 0, right: ti "turn" 2 }
          ]

      it "SymX never changes orientation, in any of the facts" do
        let symOf _ = SymX
            rule = { leftName: "empty", leftRot: 0, rightName: "empty", rightRot: 0 }
            facts = TS.expandRule symOf rule
        for_ facts \f -> do
          f.left `shouldEqual` TS.TileInstance { name: "empty", orientation: 0 }
          f.right `shouldEqual` TS.TileInstance { name: "empty", orientation: 0 }

      it "SymI (reflection-symmetric) still only ever touches orientations 0/1" do
        let symOf _ = SymI
            rule = { leftName: "track", leftRot: 0, rightName: "wire", rightRot: 0 }
            facts = TS.expandRule symOf rule
        for_ facts \f -> do
          let TS.TileInstance l = f.left
              TS.TileInstance r = f.right
          l.orientation `shouldSatisfy` (\o -> o == 0 || o == 1)
          r.orientation `shouldSatisfy` (\o -> o == 0 || o == 1)

    describe "buildTileSet — PatternCatalog/AdjacencyRules from a TileSetDef" do

      let
        synthDef :: TS.TileSetDef
        synthDef =
          { unique: false
          , tiles:
              [ { name: "blank", symmetry: SymX, weight: 5.0 }
              , { name: "corner", symmetry: SymL, weight: 2.0 }
              ]
          , neighbors:
              [ { leftName: "blank", leftRot: 0, rightName: "blank", rightRot: 0 }
              , { leftName: "corner", leftRot: 0, rightName: "blank", rightRot: 0 }
              ]
          , subsets: []
          }
        built = TS.buildTileSet synthDef
        tileName (TS.TileInstance t) = t.name

      it "catalog has one PatternId per (tile, distinct orientation)" do
        Map.size built.catalog.patterns `shouldEqual` 5 -- 1 (blank, X) + 4 (corner, L)
        built.catalog.size `shouldEqual` 1

      it "every orientation of a tile keeps that tile's own declared weight" do
        let entries = Map.toUnfoldable built.index :: Array (Tuple TS.TileInstance PatternId)
            weightOf ti = Array.findMap (\(Tuple ti' pid) -> if ti' == ti then Map.lookup pid built.catalog.weights else Nothing) entries
            cornerWeights = Array.mapMaybe (\(Tuple ti _) -> if tileName ti == "corner" then weightOf ti else Nothing) entries
        Array.length cornerWeights `shouldEqual` 4
        all (_ == 2.0) cornerWeights `shouldEqual` true

      it "a declared rule is inserted in both directions independently" do
        let blankPid = Map.lookup (TS.TileInstance { name: "blank", orientation: 0 }) built.index
            cornerPid = Map.lookup (TS.TileInstance { name: "corner", orientation: 0 }) built.index
        case Tuple blankPid cornerPid of
          Tuple (Just bp) (Just cp) -> do
            Array.elem bp (lookupNeighbors built.rules DirR cp) `shouldEqual` true
            Array.elem cp (lookupNeighbors built.rules DirL bp) `shouldEqual` true
          _ -> fail "expected both blank@0 and corner@0 to be in the built index"

      it "a single declared corner rule reaches a pairing a plain rotation sweep alone would miss" do
        -- The regression this whole Circuit.xml investigation was about.
        -- The old (rotation-only) expansion of `<neighbor left="track"
        -- right="turn"/>` put `turn@2` (not `turn@1`) on `track@0`'s DirL
        -- side (reached via rotating the whole declared pair 180°). The
        -- reflection-derived facts add `turn@1` there too — real
        -- information a corner tile needs that rotation alone can't
        -- supply, since a corner's mirror image is a genuinely different
        -- orientation of itself (`reflectIndex SymL 0 == 1`, not 0).
        let roadDef :: TS.TileSetDef
            roadDef =
              { unique: false
              , tiles:
                  [ { name: "track", symmetry: SymI, weight: 1.0 }
                  , { name: "turn", symmetry: SymL, weight: 1.0 }
                  ]
              , neighbors: [ { leftName: "track", leftRot: 0, rightName: "turn", rightRot: 0 } ]
              , subsets: []
              }
            roadBuilt = TS.buildTileSet roadDef
            pidOf name o = Map.lookup (TS.TileInstance { name, orientation: o }) roadBuilt.index
        case Tuple (pidOf "track" 0) (pidOf "turn" 1) of
          Tuple (Just t0) (Just c1) ->
            Array.elem c1 (lookupNeighbors roadBuilt.rules DirL t0) `shouldEqual` true
          _ -> fail "expected track@0 and turn@1 to both be in the built index"

    describe "WFC.TileSet.Xml — parsing the real Knots.xml tileset" do

      it "parses all 5 tiles with their declared symmetry" do
        case parseTileSetXml knotsXml of
          Left err -> fail ("failed to parse knotsXml: " <> err)
          Right def -> do
            Array.length def.tiles `shouldEqual` 5
            map _.symmetry (Array.sortWith _.name def.tiles)
              `shouldEqual` [ SymL, SymI, SymX, SymI, SymT ] -- corner, cross, empty, line, t

      it "parses all 35 neighbor rules and 10 subsets" do
        case parseTileSetXml knotsXml of
          Left err -> fail ("failed to parse knotsXml: " <> err)
          Right def -> do
            Array.length def.neighbors `shouldEqual` 35
            Array.length def.subsets `shouldEqual` 10
            def.unique `shouldEqual` false

      it "buildTileSet on the parsed Knots.xml reproduces a hand-checked fact from the file" do
        -- <neighbor left="empty" right="line 1"/>: empty (SymX) is always
        -- orientation 0; line (SymI) rotates 0/1 every 90°, alternating
        -- back every 180° — so the DirR fact is (empty@0, line@1), and its
        -- 90°-rotation (DirD) uses `rotateIndexBy SymI 1 1 = 0`.
        case parseTileSetXml knotsXml of
          Left err -> fail ("failed to parse knotsXml: " <> err)
          Right def -> do
            let built = TS.buildTileSet def
                emptyPid = Map.lookup (TS.TileInstance { name: "empty", orientation: 0 }) built.index
                lineAt0Pid = Map.lookup (TS.TileInstance { name: "line", orientation: 0 }) built.index
                lineAt1Pid = Map.lookup (TS.TileInstance { name: "line", orientation: 1 }) built.index
            case Tuple emptyPid (Tuple lineAt0Pid lineAt1Pid) of
              Tuple (Just ep) (Tuple (Just l0) (Just l1)) -> do
                -- DirR: empty@0 -> line@1 (as declared)
                Array.elem l1 (lookupNeighbors built.rules DirR ep) `shouldEqual` true
                -- and the reverse entry: line@1 has empty@0 to its DirL
                Array.elem ep (lookupNeighbors built.rules DirL l1) `shouldEqual` true
                -- DirD (one grid-rotation step later): empty stays @0 (SymX),
                -- line's declared orientation 1 rotates to 0
                Array.elem l0 (lookupNeighbors built.rules DirD ep) `shouldEqual` true
              _ -> fail "expected empty@0, line@0 and line@1 to all be in the built index"

      it "solves a Knots wave built end-to-end from the parsed XML" do
        case parseTileSetXml knotsXml of
          Left err -> fail ("failed to parse knotsXml: " <> err)
          Right def -> do
            let built = TS.buildTileSet def
                wave = initWave built.catalog built.rules { width: 6, height: 6 } true
            result <- liftEffect $ solveWithBacktracking 2000 wave
            case result of
              Left _ -> fail "backtracking failed to solve a small Knots wave"
              Right w' -> isFullyCollapsed w' `shouldEqual` true

  -- =========================================================================
  describe "Stage 12 · WFC.Propagate.applyGround — original-WFC 'ground' heuristic" do
  -- =========================================================================
  -- Regression coverage for the Flowers/MoreFlowers bug: a sample's bottom
  -- row should consistently collapse to its own ground-truth pattern, not
  -- scatter like every other row. `groundSkyCatalog`'s last-extracted
  -- pattern (`pGround` = PatternId 1, matching `lastPatternId
  -- groundSkyCatalog`) stands in for the original C# WFC's `T-1`.

    it "pins the ground pattern onto the bottom row and bans it from every other row" do
      let wave0 = initWave groundSkyCatalog groundSkyRules { width: 2, height: 3 } false
      lastPatternId groundSkyCatalog `shouldEqual` Just pGround
      case applyGround pGround wave0 of
        Left _   -> fail "unexpected contradiction grounding a ground/sky wave"
        Right w' -> do
          getCellPossibilities w' (pos 0 2) `shouldEqual` Just (Set.singleton pGround)
          getCellPossibilities w' (pos 1 2) `shouldEqual` Just (Set.singleton pGround)
          for_ [ pos 0 0, pos 1 0, pos 0 1, pos 1 1 ] \p ->
            case getCellPossibilities w' p of
              Nothing   -> fail ("cell " <> show p <> " became a contradiction")
              Just pids -> Set.member pGround pids `shouldEqual` false

    it "height 1: the single row is entirely the bottom row (no other rows to ban from)" do
      let wave0 = initWave groundSkyCatalog groundSkyRules { width: 2, height: 1 } false
      case applyGround pGround wave0 of
        Left _   -> fail "unexpected contradiction grounding a 1-row wave"
        Right w' -> do
          getCellPossibilities w' (pos 0 0) `shouldEqual` Just (Set.singleton pGround)
          getCellPossibilities w' (pos 1 0) `shouldEqual` Just (Set.singleton pGround)
