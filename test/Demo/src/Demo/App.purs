module Demo.App where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (pi)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)

import Demo.Fetch as Fetch
import Demo.ImageSamples (imageSamples)
import Demo.ImageUpload as ImageUpload
import Demo.Samples (SampleDef, checkerboard, samples)
import Demo.TileSamples (TileSampleDef)
import Demo.TileSamples as TileSamples
import Demo.WorkerProtocol (Grid, CellSnapshot)
import Demo.WorkerProtocol as WP
import Demo.XmlTileSamples (xmlTileSamples)
import WFC.Catalog (PatternCatalog, extractPatterns)
import WFC.Pattern (Pattern(..), PatternId)
import WFC.Rules (buildRules)
import WFC.Tiles (buildTiledCatalog, buildTiledRules)
import WFC.TileSet.Xml (parseTileSetXml)
import WFC.TileSet as TS
import WFC.TileSet.Symmetry (distinctOrientations)
import WFC.Wave (Wave, initWave)
import Web.Event.Event as Event
import Web.File.FileList as FileList
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.Worker.MessageEvent (MessageEvent)
import Web.Worker.MessageEvent as MessageEvent
import Web.Worker.Worker (Worker)
import Web.Worker.Worker as Worker

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data WFCStatus = Idle | Ready | Stepped | Done | Contradiction | Stopped

derive instance eqWFCStatus :: Eq WFCStatus

type State =
  { sampleIdx   :: Int
  , status      :: WFCStatus
  , catalog     :: Maybe (PatternCatalog Int)
  , initWave_   :: Maybe (Wave Int)
  , stepCount   :: Int
  , stepTimes   :: Array Number
  , totalTime   :: Number
  , showPats    :: Boolean
  , showRules   :: Boolean
  , worker        :: Maybe Worker
  , running       :: Boolean
  , untilSolvedRunning :: Boolean -- true only while a "Run until solved" is in flight; keeps Reset clickable through it
  , cmdSeq        :: Int -- mirrors the worker's internal request counter; see `sendToWorker`
  , displayGrid   :: Maybe Grid
  , progressLog   :: Array WP.Progress
  , customImage   :: Maybe WP.CustomImage
  , uploadError   :: Maybe String
  , useBacktracking :: Boolean
  , viewedStep      :: Maybe Int
  , patternSize     :: Int -- overlapping model only; ignored (fixed at 1) for tile-based sources
  , outW            :: Int -- result grid width; defaults to the active sample's own outW
  , outH            :: Int -- result grid height; defaults to the active sample's own outH
  , useRotations    :: Boolean -- overlapping model only; also extract 90/180/270 rotations
  , useMirror       :: Boolean -- overlapping model only; also extract the horizontal reflection
  , inputPeriodic   :: Boolean -- overlapping model only; wrap the source grid when extracting N×N windows
  , outputPeriodic  :: Boolean -- wrap the output wave's own edges during solving (all sources)
  , extractedWith   :: Maybe ExtractSettings -- settings `catalog` was actually extracted with
  , stepBackedOut   :: Boolean -- the last manual Step performed a backtrack pop (WFC.Backtrack.BackedOut)
  , customTileSet   :: Maybe TS.TileSetDef -- fetched+parsed XML tileset, for SrcXmlTileset (mirrors customImage)
  , xmlTilesetDir   :: Maybe String -- directory holding the active tileset's PNGs, set alongside customTileSet
  , tilesetPalette  :: Maybe (Int -> String) -- built alongside `catalog` at Extract time, for SrcXmlTileset only
  , tilesetTileOf   :: Maybe (Int -> Maybe WP.TileRef) -- ditto — which tile+orientation a pattern's Int stands for
  , tilesetImageCache :: Map String Canvas.CanvasImageSource -- preloaded per-tile PNGs, keyed by src URL
  , blankCellSnapshot :: Maybe WP.CellSnapshot -- the fully-uncollapsed cell for the active catalog; built alongside it at Extract time, used to fill new area on a live resize
  , fixOutputSize     :: Boolean -- when true, switching samples keeps Result W/H instead of resetting to the new sample's own defaults
  }

-- Which of the demo's 4 sample sources is currently selected, derived
-- purely from `sampleIdx`'s position across the 4 concatenated lists that
-- make up the single flat dropdown (`renderSidebar`'s `sampleNames`) — see
-- `sourceKindOf`/the offset helpers just below it.
data SourceKind = SrcBuiltin | SrcImage | SrcHandTiled | SrcXmlTileset

derive instance eqSourceKind :: Eq SourceKind

-- Just the settings that affect what `ExtractPatterns` produces — captured
-- at extraction time so a later change to any of them (before the next
-- Extract) can be detected and flagged as stale, without re-deriving it
-- from unrelated state (result size/output-periodicity don't affect pattern
-- extraction, so they're deliberately not tracked here).
type ExtractSettings =
  { patternSize   :: Int
  , useRotations  :: Boolean
  , useMirror     :: Boolean
  , inputPeriodic :: Boolean
  }

data Action
  = Init
  | Finalize
  | SelectSample Int
  | UploadImage Event.Event
  | ExtractPatterns
  | StepOnce
  | ResetWave
  | RunOnce
  | RunUntilSolved
  | Stop
  | WorkerMsg MessageEvent
  | TogglePatterns
  | ToggleRules
  | ToggleBacktracking
  | ViewStep Int
  | SetPatternSize Int
  | SetOutW Int
  | SetOutH Int
  | ToggleFixOutputSize
  | ToggleRotations
  | ToggleMirror
  | ToggleInputPeriodic
  | ToggleOutputPeriodic

type Slots :: forall k. Row k
type Slots = ()

-- ---------------------------------------------------------------------------
-- Initial state
-- ---------------------------------------------------------------------------

initialState :: State
initialState =
  { sampleIdx:   0
  , status:      Idle
  , catalog:     Nothing
  , initWave_:   Nothing
  , stepCount:   0
  , stepTimes:   []
  , totalTime:   0.0
  , showPats:    false
  , showRules:   false
  , worker:        Nothing
  , running:       false
  , untilSolvedRunning: false
  , cmdSeq:        0
  , displayGrid:   Nothing
  , progressLog:   []
  , customImage:   Nothing
  , uploadError:   Nothing
  , useBacktracking: false
  , viewedStep:      Nothing
  , patternSize:     checkerboard.n
  , outW:            checkerboard.outW
  , outH:            checkerboard.outH
  , useRotations:    false
  , useMirror:       false
  , inputPeriodic:   checkerboard.periodic
  , outputPeriodic:  checkerboard.periodic
  , extractedWith:   Nothing
  , stepBackedOut:   false
  , customTileSet:   Nothing
  , xmlTilesetDir:   Nothing
  , tilesetPalette:  Nothing
  , tilesetTileOf:   Nothing
  , tilesetImageCache: Map.empty
  , blankCellSnapshot: Nothing
  , fixOutputSize:     false
  }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

fmtMs :: Number -> String
fmtMs ms =
  show (Int.toNumber (Int.floor (ms * 10.0)) / 10.0) <> "ms"

-- Shared metadata every renderer needs (name/palette/output size/periodic),
-- regardless of whether the active sample came from the overlapping model
-- or the tiled model — every canvas/cell/pattern-thumb renderer only ever
-- needs this, never the model-specific fields (grid/n vs. tiles).
type SampleMeta =
  { name     :: String
  , palette  :: Int -> String
  , outW     :: Int
  , outH     :: Int
  }

sampleMetaOf :: SampleDef -> SampleMeta
sampleMetaOf s = { name: s.name, palette: s.palette, outW: s.outW, outH: s.outH }

tileSampleMetaOf :: TileSampleDef -> SampleMeta
tileSampleMetaOf s = { name: s.name, palette: s.palette, outW: s.outW, outH: s.outH }

-- Output size an XML tileset defaults to — unlike SampleDef/TileSampleDef,
-- the XML format itself doesn't declare one (the original repo pairs it
-- with separate app config this project doesn't have), so all 7 just get
-- the same reasonable constant; still fully overridable via Result W/H.
defaultXmlTilesetSize :: Int
defaultXmlTilesetSize = 24

-- Where each of the 4 concatenated sample lists starts within the single
-- flat dropdown (`renderSidebar`'s `sampleNames`): built-in overlapping,
-- then "(image)", then "(Tiled)" hand-authored, then "(Tileset)" XML.
handTiledOffset :: Int
handTiledOffset = Array.length samples + Array.length imageSamples

xmlTilesetOffset :: Int
xmlTilesetOffset = handTiledOffset + Array.length TileSamples.samples

sourceKindOfIdx :: Int -> SourceKind
sourceKindOfIdx idx
  | idx < Array.length samples = SrcBuiltin
  | idx < handTiledOffset      = SrcImage
  | idx < xmlTilesetOffset     = SrcHandTiled
  | otherwise                  = SrcXmlTileset

sourceKindOf :: State -> SourceKind
sourceKindOf st = sourceKindOfIdx st.sampleIdx

isOverlappingSource :: SourceKind -> Boolean
isOverlappingSource SrcBuiltin = true
isOverlappingSource SrcImage   = true
isOverlappingSource _          = false

-- Resolves the active *overlapping-model* sample (built-in, "(image)", or
-- a raw upload — `customImage` is set the same way for the latter two, so
-- this doesn't need to know which); meaningless for the tile-based sources.
currentSampleDef :: State -> SampleDef
currentSampleDef st = case st.customImage of
  Just ci -> WP.customSampleDef ci
  Nothing -> fromMaybe (unsafeHead samples) (Array.index samples st.sampleIdx)

currentTileSample :: State -> TileSampleDef
currentTileSample st = fromMaybe (unsafeHead TileSamples.samples) (Array.index TileSamples.samples (st.sampleIdx - handTiledOffset))

currentXmlTileSampleName :: State -> String
currentXmlTileSampleName st =
  fromMaybe "Tileset" (_.name <$> Array.index xmlTileSamples (st.sampleIdx - xmlTilesetOffset))

currentSample :: State -> SampleMeta
currentSample st = case sourceKindOf st of
  SrcHandTiled  -> tileSampleMetaOf (currentTileSample st)
  SrcXmlTileset ->
    { name: currentXmlTileSampleName st
    , palette: fromMaybe (const "#888888") st.tilesetPalette
    , outW: defaultXmlTilesetSize
    , outH: defaultXmlTilesetSize
    }
  _ -> sampleMetaOf (currentSampleDef st)

unsafeHead :: forall a. Array a -> a
unsafeHead arr = case Array.head arr of
  Just x  -> x
  Nothing -> unsafeHead arr  -- should never happen for non-empty arrays

-- The active sample's own N/output-size defaults — recomputed whenever the
-- sample source changes (SelectSample/UploadImage) so the pattern-size/
-- result-size controls start at a sensible value instead of carrying over
-- the previous sample's numbers.
sampleDefaults :: State -> { n :: Int, outW :: Int, outH :: Int, periodic :: Boolean }
sampleDefaults st = case sourceKindOf st of
  SrcHandTiled  -> let ts = currentTileSample st in { n: 1, outW: ts.outW, outH: ts.outH, periodic: ts.periodic }
  SrcXmlTileset -> { n: 1, outW: defaultXmlTilesetSize, outH: defaultXmlTilesetSize, periodic: true }
  _             -> let s = currentSampleDef st in { n: s.n, outW: s.outW, outH: s.outH, periodic: s.periodic }

-- What to actually render: the live latest grid, unless the user clicked a
-- past progress-bar square to review it (`viewedStep`), in which case that
-- step's own snapshot wins instead — a live-updating run keeps appending to
-- `progressLog`/`displayGrid` underneath regardless, so clicking back to
-- "now" is just clicking the newest square again.
activeGrid :: State -> Maybe Grid
activeGrid st = case st.viewedStep >>= Array.index st.progressLog of
  Just p  -> Just p.grid
  Nothing -> st.displayGrid

-- True when there's no catalog yet, or the pattern-size/rotate/mirror
-- settings have changed since the catalog currently shown was extracted
-- (`st.extractedWith` is only set by `ExtractPatterns`, and cleared
-- whenever `resetRunState` runs) — drives both the Extract button's
-- highlight and the "~" staleness marker on the Patterns panel title.
patternsStale :: State -> Boolean
patternsStale st = case st.extractedWith of
  Nothing -> true
  Just e  ->
    e.patternSize /= st.patternSize || e.useRotations /= st.useRotations || e.useMirror /= st.useMirror
      || e.inputPeriodic /= st.inputPeriodic

stopCommand :: WP.Command
stopCommand =
  { kind: "stop", sourceKind: "builtin", sampleIdx: 0, mode: ""
  , custom: WP.emptyCustomImage, customTileSet: WP.emptyCustomTileSet
  , useBacktracking: false
  , patternSize: 1, outW: 1, outH: 1
  , useRotations: false, useMirror: false
  , inputPeriodic: false, outputPeriodic: false
  }

-- Cancels any in-flight run *and* discards the worker's persistent solving
-- session — unlike `stopCommand`, which only cancels (Stop deliberately
-- keeps the session alive so Step/Run can resume from it). Sent whenever
-- the sample/settings actually change underneath it: Extract, Reset,
-- switching samples.
resetSessionCommand :: WP.Command
resetSessionCommand = stopCommand { kind = "resetSession" }

-- Shared by "run" (Run once/Run until solved) and "step" (Step) — both
-- just tell the worker what sample/settings to (lazily) build a fresh
-- session from if it doesn't have a live one already; `mode` only matters
-- to "run" ("once"/"untilSolved" — "step" ignores it, always single-shot).
-- `sourceKind` tells the worker which of `sampleIdx`/`custom`/
-- `customTileSet` is meaningful — the other two are left at their empty
-- defaults, same idea as `-1 means use custom` before, just explicit.
buildCommand :: State -> String -> String -> WP.Command
buildCommand st kind mode =
  let base =
        { kind, mode, sampleIdx: 0
        , custom: WP.emptyCustomImage, customTileSet: WP.emptyCustomTileSet
        , useBacktracking: st.useBacktracking
        , patternSize: st.patternSize, outW: st.outW, outH: st.outH
        , useRotations: st.useRotations, useMirror: st.useMirror
        , inputPeriodic: st.inputPeriodic, outputPeriodic: st.outputPeriodic
        , sourceKind: "builtin"
        }
  in case sourceKindOf st of
    SrcBuiltin    -> base { sampleIdx = st.sampleIdx }
    SrcImage      -> base { sourceKind = "image", custom = fromMaybe WP.emptyCustomImage st.customImage }
    SrcHandTiled  -> base { sourceKind = "handTiled", sampleIdx = st.sampleIdx - handTiledOffset }
    SrcXmlTileset -> base
      { sourceKind = "xmlTileset"
      , customTileSet = maybe WP.emptyCustomTileSet (WP.toWireTileSet (currentXmlTileSampleName st)) st.customTileSet
      }

runCommand :: State -> String -> WP.Command
runCommand st mode = buildCommand st "run" mode

stepCommand :: State -> WP.Command
stepCommand st = buildCommand st "step" ""

-- Only `outW`/`outH` actually matter to the worker's "resize" handler; the
-- rest of `buildCommand`'s fields ride along unused, same as `stepCommand`.
resizeCommand :: State -> WP.Command
resizeCommand st = buildCommand st "resize" ""

-- Fields shared by "switch to a different sample" (built-in or uploaded):
-- drop whatever local wave/run/progress state referred to the old sample.
resetRunState :: State -> State
resetRunState s = s
  { status        = Idle
  , catalog       = Nothing
  , initWave_     = Nothing
  , stepCount     = 0
  , stepTimes     = []
  , totalTime     = 0.0
  , running       = false
  , untilSolvedRunning = false
  , displayGrid   = Nothing
  , progressLog   = []
  , viewedStep    = Nothing
  , extractedWith = Nothing
  , stepBackedOut = false
  , tilesetPalette = Nothing
  , tilesetTileOf = Nothing
  , blankCellSnapshot = Nothing
  }

-- Reasonable defaults for a freshly-decoded upload: N=3 when the image is at
-- least 3px on both axes (falling back to 2 or 1 for tinier uploads),
-- non-periodic, output scaled up 3x within a sane runtime-cost range.
clampInt :: Int -> Int -> Int -> Int
clampInt lo hi n = max lo (min hi n)

-- The bundled reference images (Demo.ImageSamples) are known-good and
-- larger than the 32×32 cap enforced on user uploads (Font.png is 267×15)
-- — generous enough to cover all of them without being a meaningful attack
-- surface, unlike an arbitrary user-supplied file.
builtinImageMaxSide :: Int
builtinImageMaxSide = 300

customImageFrom :: ImageUpload.LoadedImage -> WP.CustomImage
customImageFrom loaded =
  { grid: loaded.grid
  , colors: loaded.colors
  , n: clampInt 1 3 (min loaded.width loaded.height)
  , periodic: false
  , outW: clampInt 16 64 (loaded.width * 3)
  , outH: clampInt 16 64 (loaded.height * 3)
  , name: loaded.name
  }

-- ---------------------------------------------------------------------------
-- XML tileset image rendering
-- ---------------------------------------------------------------------------

-- A `TileRef.orientation` is an index into a tile's own `0 .. cardinality-1`
-- distinct pictures (see `WFC.TileSet.Symmetry`) — for `L`/`T`/`I`/`\`/`X`
-- classes that's always a plain rotation (`(idx mod 4) * 90°`, no mirror);
-- `F` (cardinality 8) additionally uses indices 4-7 for the mirrored half.
-- This holds uniformly across all 6 classes given how `distinctOrientations`/
-- `rotateIndex` are built (index *is* "how many 90° steps from the base
-- picture, wrapping into the mirrored half past 3") — no per-class casing
-- needed here.
orientationTransform :: Int -> { rotationDeg :: Int, mirrored :: Boolean }
orientationTransform idx = { rotationDeg: (idx `mod` 4) * 90, mirrored: idx >= 4 }

-- Where a tile+orientation's PNG lives: `unique` tilesets (e.g. Summer.xml)
-- have a separate image per orientation ("cliff 0.png".."cliff 3.png");
-- everything else has one base image per tile name, rotated/mirrored at
-- render time instead.
tileImageSrc :: String -> Boolean -> WP.TileRef -> String
tileImageSrc dir unique ref =
  if unique
    then dir <> "/" <> ref.name <> " " <> show ref.orientation <> ".png"
    else dir <> "/" <> ref.name <> ".png"

-- Every distinct image URL a tileset's patterns could need, so they can
-- all be preloaded together right after Extract (well before a run
-- reaches "Done" and actually wants to draw them).
allTileImageSrcs :: String -> TS.TileSetDef -> Array String
allTileImageSrcs dir def =
  Array.nub $ def.tiles >>= \t ->
    map (\o -> tileImageSrc dir def.unique { name: t.name, orientation: o })
      (distinctOrientations t.symmetry)

loadTilesetImage :: String -> Aff (Maybe Canvas.CanvasImageSource)
loadTilesetImage url = makeAff \respond -> do
  Canvas.tryLoadImage url (respond <<< Right)
  pure nonCanceler

loadAllTilesetImages :: String -> TS.TileSetDef -> Aff (Map String Canvas.CanvasImageSource)
loadAllTilesetImages dir def = do
  let srcs = allTileImageSrcs dir def
  loaded <- traverse (\src -> Tuple src <$> loadTilesetImage src) srcs
  pure (Map.fromFoldable (Array.mapMaybe (\(Tuple src m) -> Tuple src <$> m) loaded))

-- For the Patterns panel (plain Halogen HTML, not a `<canvas>`): a real
-- `<img>` + CSS `transform` is enough, no need to preload/cache a
-- `CanvasImageSource` the way the main canvas draw does — the browser
-- handles fetching/caching `<img src>` itself.
-- Shared by the Patterns panel (`tileImageFor`, going through a pattern's
-- catalog `Int` first) and anything that already has a `TileRef` directly
-- (the Source-tiles preview and the Rules panel, both reading straight off
-- `st.customTileSet`, before/without any catalog at all) — just needs
-- `xmlTilesetDir`/`customTileSet` to know where the picture lives and
-- whether it's a `unique` (per-orientation-file) tileset.
tileImageForTile :: State -> WP.TileRef -> Maybe (Tuple String String)
tileImageForTile st ref = do
  dir <- st.xmlTilesetDir
  def <- st.customTileSet
  let t         = orientationTransform ref.orientation
      src       = tileImageSrc dir def.unique ref
      transform = "rotate(" <> show t.rotationDeg <> "deg)" <> (if t.mirrored then " scaleX(-1)" else "")
  pure (Tuple src transform)

tileImageFor :: State -> Maybe Int -> Maybe (Tuple String String)
tileImageFor st mv = do
  v      <- mv
  tileOf <- st.tilesetTileOf
  ref    <- tileOf v
  tileImageForTile st ref

-- ---------------------------------------------------------------------------
-- Canvas drawing
-- ---------------------------------------------------------------------------

-- Everything needed to draw a collapsed cell as its real tile picture
-- instead of a flat color — each already-collapsed cell draws as its tile
-- image as soon as one is available in `tilesetImageCache`, live during a
-- run too; still-uncollapsed cells and any cache miss fall back to flat
-- colors, so this never blocks on a not-yet-loaded image.
type ImageDrawMode = { tileOf :: Int -> Maybe WP.TileRef, dir :: String, unique :: Boolean }

imageDrawMode :: State -> Maybe ImageDrawMode
imageDrawMode st =
  case Tuple st.tilesetTileOf (Tuple st.xmlTilesetDir st.customTileSet) of
    Tuple (Just tileOf) (Tuple (Just dir) (Just def)) -> Just { tileOf, dir, unique: def.unique }
    _ -> Nothing

-- Draw one tile image into a cell's square, rotated/mirrored per its
-- orientation — `orientationTransform` (see above) already reduces any
-- symmetry class down to "rotate N*90°, optionally mirrored first".
drawTileImage :: Canvas.Context2D -> Canvas.CanvasImageSource -> Number -> Number -> Number -> Number -> Int -> Effect Unit
drawTileImage ctx img x y w h orientation = do
  let t = orientationTransform orientation
  Canvas.save ctx
  Canvas.translate ctx { translateX: x + w / 2.0, translateY: y + h / 2.0 }
  when t.mirrored (Canvas.scale ctx { scaleX: -1.0, scaleY: 1.0 })
  Canvas.rotate ctx (Int.toNumber t.rotationDeg * pi / 180.0)
  Canvas.drawImageScale ctx img (-(w / 2.0)) (-(h / 2.0)) w h
  Canvas.restore ctx

drawCanvasEffect :: State -> Effect Unit
drawCanvasEffect st = do
  mCanvas <- Canvas.getCanvasElementById "wfc-canvas"
  case mCanvas of
    Nothing     -> pure unit
    Just canvas -> do
      ctx <- Canvas.getContext2D canvas
      let cw = 320.0
          ch = 320.0
      Canvas.clearRect ctx { x: 0.0, y: 0.0, width: cw, height: ch }
      Canvas.setFillStyle ctx "#1a1a2e"
      Canvas.fillRect ctx { x: 0.0, y: 0.0, width: cw, height: ch }
      case activeGrid st of
        Nothing   -> pure unit
        Just grid -> do
          let height = Array.length grid
              width  = fromMaybe 0 (map Array.length (Array.head grid))
          when (width > 0 && height > 0) do
            let cellW = cw / Int.toNumber width
                cellH = ch / Int.toNumber height
                sampleDef = currentSample st
                mImg = imageDrawMode st
                cells = Array.concat
                  (Array.mapWithIndex
                    (\y row -> Array.mapWithIndex (\x cell -> Tuple (Tuple x y) cell) row)
                    grid)
            Canvas.setFont ctx (show (Int.floor (min cellW cellH * 0.7)) <> "px sans-serif")
            Canvas.setTextAlign ctx Canvas.AlignCenter
            for_ cells \(Tuple (Tuple x y) cell) -> do
              let px = Int.toNumber x * cellW
                  py = Int.toNumber y * cellH
                  tileImage = do
                    im  <- mImg
                    guard (cell.collapsed && not cell.contradiction)
                    ref <- Array.head cell.values >>= im.tileOf
                    img <- Map.lookup (tileImageSrc im.dir im.unique ref) st.tilesetImageCache
                    pure (Tuple img ref.orientation)
              case tileImage of
                Just (Tuple img orientation) ->
                  drawTileImage ctx img px py cellW cellH orientation
                Nothing -> do
                  let color = WP.cellColor sampleDef.palette cell
                  Canvas.setFillStyle ctx color
                  Canvas.fillRect ctx
                    { x:      px
                    , y:      py
                    , width:  cellW - 0.5
                    , height: cellH - 0.5
                    }
                  when cell.contradiction do
                    Canvas.setFillStyle ctx "#ffffff"
                    Canvas.fillText ctx "?" (px + cellW / 2.0) (py + cellH * 0.72)

drawCanvas :: H.HalogenM State Action Slots Void Aff Unit
drawCanvas = do
  st <- H.get
  H.liftEffect $ drawCanvasEffect st

-- Applied on every Result W/H change: crops or pads the *currently
-- displayed* grid to the new size right away (top-left anchored — new area
-- fills with the fully-uncollapsed cell, same as a freshly-extracted
-- catalog's own initial wave would show there), then quietly tells the
-- worker to resize its own live session's wave(s) to match, so the next
-- Step/Run continues from the new size instead of overwriting this back to
-- the old one. A no-op before the first Extract (`blankCellSnapshot`
-- isn't built yet, and there's nothing running to resize either).
resizeLive :: H.HalogenM State Action Slots Void Aff Unit
resizeLive = do
  st <- H.get
  case st.blankCellSnapshot of
    Nothing -> pure unit
    Just blank -> do
      H.modify_ \s -> s
        { displayGrid = WP.resizeGrid blank s.outW s.outH <$> s.displayGrid
        , viewedStep  = Nothing
        }
      drawCanvas
      sendToWorkerQuiet (resizeCommand st)

-- ---------------------------------------------------------------------------
-- Worker plumbing
-- ---------------------------------------------------------------------------

-- Lazily spawn the worker and subscribe to its messages; reused across runs.
ensureWorker :: H.HalogenM State Action Slots Void Aff Worker
ensureWorker = do
  st <- H.get
  case st.worker of
    Just w  -> pure w
    Nothing -> do
      w <- H.liftEffect (Worker.new "worker.js" Worker.defaultWorkerOptions)
      let emitter = HS.makeEmitter \emit -> do
            Worker.onMessage (\ev -> emit (WorkerMsg ev)) w
            pure (pure unit)
      _ <- H.subscribe emitter
      H.modify_ _ { worker = Just w }
      pure w

-- Every message sent to the worker bumps its internal request counter by
-- exactly one (`Demo.Worker.handleMessage`), and the single ordered
-- postMessage channel guarantees the Nth message sent is the Nth one the
-- worker processes — so mirroring that count locally in `cmdSeq` lets
-- `WorkerMsg` recognize a stale reply (from a run/step superseded by a
-- later one, including a Stop) just by comparing `msg.token`, with no
-- separate stop/pause flag to keep in sync by hand.
sendToWorker :: WP.Command -> H.HalogenM State Action Slots Void Aff Unit
sendToWorker cmd = do
  st <- H.get
  for_ st.worker \w -> do
    H.modify_ _ { cmdSeq = st.cmdSeq + 1 }
    H.liftEffect (Worker.postMessage cmd w)

-- Like `sendToWorker`, but doesn't bump `cmdSeq` and doesn't spawn a worker
-- if there isn't one yet — for commands that intentionally don't supersede
-- whatever's in flight (only "resize" today; see `Demo.Worker`'s own
-- "resize" case, which likewise leaves its `tokenRef` alone) and that don't
-- need a worker at all when nothing's ever been extracted.
sendToWorkerQuiet :: WP.Command -> H.HalogenM State Action Slots Void Aff Unit
sendToWorkerQuiet cmd = do
  st <- H.get
  for_ st.worker \w -> H.liftEffect (Worker.postMessage cmd w)

-- Same, but spawns the worker first if this is the very first command sent
-- to it ("run"/"step" need to reach a worker that may not exist yet;
-- "stop"/"resetSession" are no-ops when there's nothing to stop/reset, so
-- they use `sendToWorker` instead and skip spawning one just to tell it
-- nothing).
sendToWorkerEnsuring :: WP.Command -> H.HalogenM State Action Slots Void Aff Unit
sendToWorkerEnsuring cmd = do
  _  <- ensureWorker
  sendToWorker cmd

applySampleDefaults :: H.HalogenM State Action Slots Void Aff Unit
applySampleDefaults = do
  st <- H.get
  let d = sampleDefaults st
  H.modify_ \s -> s
    { patternSize = d.n
    -- "Fix" keeps Result W/H exactly as they were across a sample switch,
    -- instead of snapping to the newly-selected sample's own defaults.
    , outW = if s.fixOutputSize then s.outW else d.outW
    , outH = if s.fixOutputSize then s.outH else d.outH
    , inputPeriodic = d.periodic, outputPeriodic = d.periodic
    }

-- Continues the worker's session if it has one (from earlier Steps, or a
-- Run that was Stopped) rather than forcing a fresh start — matches Step's
-- continuation behavior, so switching between the two controls mid-solve
-- doesn't lose progress either way.
startRun :: String -> H.HalogenM State Action Slots Void Aff Unit
startRun mode = do
  st <- H.get
  H.modify_ _
    { running = true, status = Stepped, viewedStep = Nothing
    , untilSolvedRunning = mode == "untilSolved"
    }
  sendToWorkerEnsuring (runCommand st mode)

-- ---------------------------------------------------------------------------
-- Component
-- ---------------------------------------------------------------------------

component :: forall q i. H.Component q i Void Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , finalize     = Just Finalize
      }
  }

-- ---------------------------------------------------------------------------
-- Render
-- ---------------------------------------------------------------------------

render :: State -> H.ComponentHTML Action Slots Aff
render st =
  HH.div
    [ HP.class_ (H.ClassName "demo") ]
    [ renderSidebar st
    , renderMain st
    , renderRunPanel st
    ]

-- Sample source: picking/building/inspecting the pattern source, not
-- running it — select, upload, size/rotate/mirror/periodic controls,
-- Extract/Patterns, pattern list. One flat dropdown across all 4 sample
-- kinds (built-in overlapping, "(image)", "(Tiled)" hand-authored,
-- "(Tileset)" XML-parsed) — see `sourceKindOf`/the offset helpers above.
renderSidebar :: State -> H.ComponentHTML Action Slots Aff
renderSidebar st =
  let sampleNames =
        map _.name samples <> map _.name imageSamples <> map _.name TileSamples.samples <> map _.name xmlTileSamples
  in
  HH.div
    [ HP.class_ (H.ClassName "sidebar") ]
    [ HH.select
        [ HE.onSelectedIndexChange SelectSample ]
        (Array.mapWithIndex renderOption sampleNames)
    , if isOverlappingSource (sourceKindOf st) then renderUpload st else HH.text ""
    , renderSizeControls st
    , renderSourcePreview st
    , renderSourceControls st
    , renderPatterns st
    , renderRules st
    ]

-- Running/observing an already-extracted wave — Step/Reset/Run/Stop, the
-- backtracking toggle, status info, and the step history. To the right of
-- the canvas/table on desktop widths (see index.html's media query);
-- stacks below on mobile.
renderRunPanel :: State -> H.ComponentHTML Action Slots Aff
renderRunPanel st =
  HH.div
    [ HP.class_ (H.ClassName "run-panel") ]
    [ renderRunControls st
    , renderStats st
    , renderProgress st
    ]

-- File upload: pick a small (<=32x32) image to use as the pattern source
-- instead of a built-in sample. `currentSample` prefers `customImage` when
-- set, so nothing else in the app needs to know whether the active sample
-- came from here or from the dropdown.
renderUpload :: State -> H.ComponentHTML Action Slots Aff
renderUpload st =
  HH.div
    [ HP.class_ (H.ClassName "upload") ]
    [ HH.label
        [ HP.class_ (H.ClassName "upload-label") ]
        [ HH.text "Or upload image (max 32×32): " ]
    , HH.input
        [ HP.type_ HP.InputFile
        , HP.attr (H.AttrName "accept") "image/*"
        , HE.onChange UploadImage
        ]
    , case st.customImage of
        Just ci -> HH.div [ HP.class_ (H.ClassName "upload-status") ]
          [ HH.text ("Active: " <> ci.name <> " ("
              <> show (fromMaybe 0 (map Array.length (Array.head ci.grid))) <> "×"
              <> show (Array.length ci.grid) <> ")") ]
        Nothing -> HH.text ""
    , case st.uploadError of
        Just err -> HH.div [ HP.class_ (H.ClassName "upload-error") ] [ HH.text err ]
        Nothing  -> HH.text ""
    ]

renderOption :: Int -> String -> H.ComponentHTML Action Slots Aff
renderOption i name =
  HH.option
    [ HP.value (show i) ]
    [ HH.text name ]

-- Pattern size (N×N, overlapping model only — tiles are always size 1) and
-- result grid size, both editable before Extract; default to the active
-- sample's own numbers (kept in sync by `sampleDefaults` whenever the
-- source changes) rather than some fixed constant.
renderSizeControls :: State -> H.ComponentHTML Action Slots Aff
renderSizeControls st =
  HH.div
    [ HP.class_ (H.ClassName "size-controls") ]
    ( ( if not (isOverlappingSource (sourceKindOf st)) then []
        else
          [ HH.label
              [ HP.class_ (H.ClassName "size-label") ]
              [ HH.text "Pattern size: "
              , HH.select
                  [ HE.onSelectedIndexChange (\i -> SetPatternSize (i + 1)) ]
                  (map (renderSizeOption st.patternSize) [ 1, 2, 3, 4 ])
              ]
          , HH.label
              [ HP.class_ (H.ClassName "size-label") ]
              [ HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.checked st.useRotations
                  , HE.onChecked \_ -> ToggleRotations
                  ]
              , HH.text " Rotate"
              ]
          , HH.label
              [ HP.class_ (H.ClassName "size-label") ]
              [ HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.checked st.useMirror
                  , HE.onChecked \_ -> ToggleMirror
                  ]
              , HH.text " Mirror"
              ]
          , HH.label
              [ HP.class_ (H.ClassName "size-label") ]
              [ HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.checked st.inputPeriodic
                  , HE.onChecked \_ -> ToggleInputPeriodic
                  ]
              , HH.text " Periodic input"
              ]
          ]
      )
      <>
      [ HH.label
          [ HP.class_ (H.ClassName "size-label") ]
          [ HH.input
              [ HP.type_ HP.InputCheckbox
              , HP.checked st.outputPeriodic
              , HE.onChecked \_ -> ToggleOutputPeriodic
              ]
          , HH.text " Periodic output"
          ]
      , HH.label
          [ HP.class_ (H.ClassName "size-label") ]
          [ HH.text "Result W: "
          , HH.input
              [ HP.type_ HP.InputNumber
              , HP.value (show st.outW)
              , HP.min 1.0
              , HP.max 64.0
              , HE.onValueChange (\v -> SetOutW (clampInt 1 64 (fromMaybe st.outW (Int.fromString v))))
              ]
          ]
      , HH.label
          [ HP.class_ (H.ClassName "size-label") ]
          [ HH.text "Result H: "
          , HH.input
              [ HP.type_ HP.InputNumber
              , HP.value (show st.outH)
              , HP.min 1.0
              , HP.max 64.0
              , HE.onValueChange (\v -> SetOutH (clampInt 1 64 (fromMaybe st.outH (Int.fromString v))))
              ]
          ]
      , HH.label
          [ HP.class_ (H.ClassName "size-label") ]
          [ HH.input
              [ HP.type_ HP.InputCheckbox
              , HP.checked st.fixOutputSize
              , HE.onChecked \_ -> ToggleFixOutputSize
              ]
          , HH.text " Fix"
          ]
      ]
    )

renderSizeOption :: Int -> Int -> H.ComponentHTML Action Slots Aff
renderSizeOption current n =
  HH.option
    [ HP.value (show n), HP.selected (n == current) ]
    [ HH.text (show n <> "×" <> show n) ]

-- What Extract actually reads from: the raw source grid (built-in sample
-- or uploaded image) for the overlapping model, the raw tile set for the
-- hand-authored tiled model, or the parsed tile list for an XML tileset —
-- shown as-is, before any patterns get extracted from it, so the dropdown
-- choice is visible up front instead of only showing up once you've
-- already clicked Extract.
renderSourcePreview :: State -> H.ComponentHTML Action Slots Aff
renderSourcePreview st = case sourceKindOf st of
  SrcHandTiled  -> renderTilePreview (currentTileSample st)
  SrcXmlTileset -> renderXmlTilesetPreview st
  _             -> renderGridPreview (currentSampleDef st)

renderGridPreview :: SampleDef -> H.ComponentHTML Action Slots Aff
renderGridPreview sample =
  HH.div
    [ HP.class_ (H.ClassName "source-preview") ]
    [ HH.div [ HP.class_ (H.ClassName "source-label") ] [ HH.text ("Source: " <> sample.name) ]
    , HH.table
        [ HP.class_ (H.ClassName "source-grid") ]
        (map (\row -> HH.tr_ (map renderSourceCell row)) sample.grid)
    ]
  where
  renderSourceCell v = HH.td [ HP.style ("background:" <> sample.palette v <> ";") ] []

renderTilePreview :: TileSampleDef -> H.ComponentHTML Action Slots Aff
renderTilePreview ts =
  HH.div
    [ HP.class_ (H.ClassName "source-preview") ]
    [ HH.div [ HP.class_ (H.ClassName "source-label") ] [ HH.text ("Source: " <> ts.name) ]
    , HH.div
        [ HP.class_ (H.ClassName "tile-list") ]
        (map (\t -> HH.div
                [ HP.class_ (H.ClassName "tile-swatch")
                , HP.style ("background:" <> ts.palette t.value <> ";")
                , HP.title (show t.value)
                ]
                []
             ) ts.tiles)
    ]

-- Each tile's own base picture (orientation 0 — the picture as declared,
-- before any of its symmetry-derived rotations/mirrors) — `xmlTilesetDir`
-- is set alongside `customTileSet` at selection time (see `SelectSample`),
-- well before Extract, so this can show real tiles immediately rather than
-- waiting on a catalog to exist.
renderXmlTilesetPreview :: State -> H.ComponentHTML Action Slots Aff
renderXmlTilesetPreview st =
  HH.div
    [ HP.class_ (H.ClassName "source-preview") ]
    [ HH.div [ HP.class_ (H.ClassName "source-label") ] [ HH.text ("Source: " <> currentXmlTileSampleName st) ]
    , case st.customTileSet of
        Nothing -> HH.text "Loading…"
        Just def ->
          HH.div
            [ HP.class_ (H.ClassName "tile-list") ]
            (map (renderSourceTile st) def.tiles)
    ]

renderSourceTile :: State -> TS.TileDef -> H.ComponentHTML Action Slots Aff
renderSourceTile st t =
  renderTileSwatch st { name: t.name, orientation: 0 } (t.name <> " (" <> show t.symmetry <> ")")

-- One small square tile picture, correctly rotated/mirrored per `ref`'s
-- orientation — shared by the source-tiles list (always orientation 0) and
-- the Rules panel (whatever orientation the rule declares on each side).
-- Falls back to a plain swatch if the image isn't resolvable yet (should
-- only happen in the instant between selecting a tileset and its
-- `xmlTilesetDir`/`customTileSet` both landing).
renderTileSwatch :: State -> WP.TileRef -> String -> H.ComponentHTML Action Slots Aff
renderTileSwatch st ref tip =
  case tileImageForTile st ref of
    Just (Tuple src transform) ->
      HH.div
        [ HP.class_ (H.ClassName "tile-swatch")
        , HP.title tip
        ]
        [ HH.img
            [ HP.src src
            , HP.style ("width:100%;height:100%;image-rendering:pixelated;transform:" <> transform <> ";")
            ]
        ]
    Nothing ->
      HH.div
        [ HP.class_ (H.ClassName "tile-swatch")
        , HP.style "background:#888888;"
        , HP.title tip
        ]
        []

-- Stays in the left sidebar: controls that pick/build the sample source.
renderSourceControls :: State -> H.ComponentHTML Action Slots Aff
renderSourceControls st =
  HH.div
    [ HP.class_ (H.ClassName "controls") ]
    [ HH.button
        [ HE.onClick \_ -> ExtractPatterns
        , HP.class_ (H.ClassName (if patternsStale st then "needs-extract" else ""))
        ]
        [ HH.text "◫ Extract" ]
    ]

-- Moves to the right-hand run panel (desktop) / below main view (mobile):
-- controls that drive/observe an already-extracted wave.
renderRunControls :: State -> H.ComponentHTML Action Slots Aff
renderRunControls st =
  let notReady   = st.status == Idle
      noStepsTaken = st.status == Idle || st.status == Ready
  in
  HH.div
    [ HP.class_ (H.ClassName "controls") ]
    [ HH.button
        [ HE.onClick \_ -> StepOnce
        , HP.disabled (notReady || st.running)
        , HP.title (if st.stepBackedOut then "Last step backtracked to an earlier decision" else "")
        ]
        [ HH.text (if st.stepBackedOut then "⟲ Step" else "▶ Step") ]
    , HH.button
        [ HE.onClick \_ -> ResetWave
        -- Stays clickable through a whole "Run until solved" (it's safe —
        -- ResetWave tells the worker to discard its session either way,
        -- which also cancels an in-flight run); "Run once"/Step still
        -- disable it while running, same as before.
        , HP.disabled (noStepsTaken || (st.running && not st.untilSolvedRunning))
        ]
        [ HH.text "■ Reset" ]
    , HH.button
        [ HE.onClick \_ -> RunOnce
        , HP.disabled (notReady || st.running)
        ]
        [ HH.text "▶ Run once" ]
    , HH.button
        [ HE.onClick \_ -> RunUntilSolved
        , HP.disabled (notReady || st.running)
        ]
        [ HH.text "⏩ Run until solved" ]
    , HH.button
        [ HE.onClick \_ -> Stop
        , HP.disabled (not st.running)
        , HP.title "Pause — the current solving session stays alive; Step/Run resume it"
        ]
        [ HH.text "⏸ Pause" ]
    , HH.label
        [ HP.class_ (H.ClassName "toggle-row") ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked st.useBacktracking
            , HP.disabled st.running
            , HE.onChecked \_ -> ToggleBacktracking
            ]
        , HH.text " Use backtracking"
        ]
    ]

-- Per-iteration step counts — one number per history row (see
-- `buildHistoryRows`): a full restart or, in backtracking mode, an
-- individual backtrack pop both start a new row/iteration. Single-row runs
-- collapse back to the plain total, same as `st.stepCount`.
iterationStepCounts :: Array WP.Progress -> Array Int
iterationStepCounts entries = map (\r -> Array.length r.cells) (buildHistoryRows entries)

renderStats :: State -> H.ComponentHTML Action Slots Aff
renderStats st =
  let catInfo = case st.catalog of
        Nothing  -> "No patterns"
        Just cat ->
          show (Map.size cat.patterns) <> " patterns, size "
          <> show cat.size <> "×" <> show cat.size
      lastMs = fromMaybe 0.0 (Array.last st.stepTimes)
      segments = iterationStepCounts st.progressLog
      stepsStr =
        if st.useBacktracking && Array.length segments > 1
          then Array.intercalate "|" (map show segments)
          else show st.stepCount
      statsStr =
        catInfo
        <> " | Steps: " <> stepsStr
        <> " | Last: " <> fmtMs lastMs
        <> " | Total: " <> fmtMs st.totalTime
      statusStr = case st.status of
        Idle          -> "Idle"
        Ready         -> "Ready"
        Stepped       -> "Stepping..."
        Done          -> "Done"
        Contradiction -> "Contradiction!"
        Stopped       -> "Paused"
  in
  HH.div
    [ HP.class_ (H.ClassName "stats") ]
    [ HH.div_ [ HH.text ("Status: " <> statusStr) ]
    , HH.div_ [ HH.text statsStr ]
    ]

-- One history row per "cycle": the run so far, until either a full restart
-- (`restarted`, always starts the next row at column 0) or — in
-- backtracking mode — an individual backtrack pop (`rowBreak` with
-- `rowStartColumn` set to the search depth resumed at), which starts the
-- next row partway across instead, so the history reads like a search
-- tree: rightward = forward progress, a row starting mid-way = "returned
-- to here and tried a different branch."
type HistoryRow = { startColumn :: Int, cells :: Array (Tuple Int WP.Progress) }

buildHistoryRows :: Array WP.Progress -> Array HistoryRow
buildHistoryRows entries =
  case Array.uncons (Array.mapWithIndex Tuple entries) of
    Nothing -> []
    Just { head: first, tail: rest } ->
      let initAcc = { rows: ([] :: Array HistoryRow), current: { startColumn: 0, cells: [ first ] } }
          step acc cell@(Tuple _ p) =
            if p.rowBreak
              then acc { rows = Array.snoc acc.rows acc.current
                       , current = { startColumn: p.rowStartColumn, cells: [ cell ] }
                       }
              else acc { current = acc.current { cells = Array.snoc acc.current.cells cell } }
          final = Array.foldl step initAcc rest
      in Array.snoc final.rows final.current

-- One square per step reported so far, green-shaded by percent of cells
-- solved at that step (red for a contradiction); click one to freeze the
-- canvas/table on that step's grid (`activeGrid`) — a live run keeps
-- appending squares underneath regardless, so clicking the newest one
-- again jumps back to "now". Persists until Reset, a new run, or Extract
-- clears it.
renderProgress :: State -> H.ComponentHTML Action Slots Aff
renderProgress st =
  let rows = buildHistoryRows st.progressLog
  in
  if Array.null rows
    then HH.text ""
    else
      HH.div
        [ HP.class_ (H.ClassName "history-block") ]
        (map (renderHistoryRow st.viewedStep) rows)

renderHistoryRow :: Maybe Int -> HistoryRow -> H.ComponentHTML Action Slots Aff
renderHistoryRow viewedStep row =
  HH.div
    [ HP.class_ (H.ClassName "history-row") ]
    ( Array.replicate row.startColumn (HH.div [ HP.class_ (H.ClassName "progress-spacer") ] [])
      <> map (\(Tuple i p) -> renderProgressCell viewedStep i p) row.cells
    )

renderProgressCell :: Maybe Int -> Int -> WP.Progress -> H.ComponentHTML Action Slots Aff
renderProgressCell viewedStep i p =
  let percent    = if p.totalCells > 0 then Int.toNumber p.solvedTotal / Int.toNumber p.totalCells else 0.0
      lightness  = 15 + Int.floor (percent * 45.0)
      -- A full restart (`restarted`) is always triggered by a contradiction,
      -- even when the worker reports it as an ordinary "progress" step (so
      -- `status`/`running` keep tracking the run that continues past it) —
      -- so the cell still needs to read as red, not green, at that step.
      isContra   = p.kind == "contradiction" || p.restarted
      isViewed   = viewedStep == Just i
      bg         = if isContra then "#ff4444" else "hsl(130, 55%, " <> show lightness <> "%)"
      extraClass =
        (if p.restarted then " restarted" else "")
        <> (if isContra then " contradiction" else "")
        <> (if isViewed then " viewed" else "")
      tip =
        "step " <> show p.step
        <> " — solved +" <> show p.solvedDelta
        <> " (" <> show p.solvedTotal <> "/" <> show p.totalCells <> ")"
        <> " — " <> fmtMs p.elapsedMs
        <> (if p.restarted then " — attempt restarted here" else "")
        <> (if isContra then " — contradiction" else "")
  in
  HH.div
    [ HP.class_ (H.ClassName ("progress-cell" <> extraClass))
    , HP.style ("background:" <> bg <> ";")
    , HP.title tip
    , HE.onClick \_ -> ViewStep i
    ]
    []

renderPatterns :: State -> H.ComponentHTML Action Slots Aff
renderPatterns st =
  case st.catalog of
    Nothing  -> HH.text ""
    Just cat ->
      HH.div
        [ HP.class_ (H.ClassName "pattern-section") ]
        [ HH.button
            [ HE.onClick \_ -> TogglePatterns
            , HP.class_ (H.ClassName "toggle-btn")
            ]
            [ HH.text
                ( (if st.showPats then "▲ " else "▼ ")
                  <> (if patternsStale st then "~" else "")
                  <> "Patterns (" <> show (Map.size cat.patterns) <> ")"
                )
            ]
        , if st.showPats
            then
              HH.div
                [ HP.class_ (H.ClassName "pattern-list") ]
                (map (renderPatThumb st cat) (Map.toUnfoldable cat.patterns :: Array _))
            else
              HH.text ""
        ]

renderPatThumb
  :: State
  -> PatternCatalog Int
  -> Tuple PatternId (Pattern Int)
  -> H.ComponentHTML Action Slots Aff
renderPatThumb st cat (Tuple pid (Pattern px)) =
  HH.div
    [ HP.class_ (H.ClassName "pattern-thumb") ]
    [ case tileImageFor st (Array.head px) of
        Just (Tuple src transform) ->
          HH.div
            [ HP.class_ (H.ClassName "pattern-grid") ]
            [ HH.img
                [ HP.src src
                , HP.style ("width:100%;height:100%;image-rendering:pixelated;transform:" <> transform <> ";")
                ]
            ]
        Nothing ->
          let n      = cat.size
              sample = currentSample st
              cells  = Array.mapWithIndex (\_ v ->
                HH.div
                  [ HP.class_ (H.ClassName "mini-cell")
                  , HP.style ("background:" <> sample.palette v)
                  ]
                  []
                ) px
          in
          HH.div
            [ HP.class_ (H.ClassName "pattern-grid")
            , HP.style ("grid-template-columns: repeat(" <> show n <> ", 1fr);")
            ]
            cells
    , renderSymmetryBadges cat pid
    , HH.div
        [ HP.class_ (H.ClassName "pat-label") ]
        [ HH.text (show pid) ]
    ]

-- Only meaningful for an XML tileset (`WFC.TileSet.NeighborRule` is that
-- format's own adjacency declaration — the overlapping model and the
-- hand-tiled sockets model each derive adjacency their own way, with
-- nothing analogous to list here); `def.neighbors` is available as soon as
-- the tileset's XML is fetched, so — like the source-tiles preview — this
-- doesn't need to wait on Extract either.
renderRules :: State -> H.ComponentHTML Action Slots Aff
renderRules st = case Tuple (sourceKindOf st) st.customTileSet of
  Tuple SrcXmlTileset (Just def) ->
    HH.div
      [ HP.class_ (H.ClassName "rules-section") ]
      [ HH.button
          [ HE.onClick \_ -> ToggleRules
          , HP.class_ (H.ClassName "toggle-btn")
          ]
          [ HH.text
              ( (if st.showRules then "▲ " else "▼ ")
                <> "Rules (" <> show (Array.length def.neighbors) <> ")"
              )
          ]
      , if st.showRules
          then
            HH.div
              [ HP.class_ (H.ClassName "rule-list") ]
              (map (renderRuleRow st) def.neighbors)
          else
            HH.text ""
      ]
  _ -> HH.text ""

-- One declared `<neighbor left="A N" right="B M"/>` row, shown as its two
-- actual tile pictures (each at the orientation *that rule* declares, not
-- necessarily each tile's own base orientation) either side of an arrow —
-- this is always a left-of/right-of pair (`WFC.TileSet.expandRule` is what
-- turns it into all 4 cardinal directions at catalog-build time; the raw
-- declaration itself is only ever the one `DirR` relation).
renderRuleRow :: State -> TS.NeighborRule -> H.ComponentHTML Action Slots Aff
renderRuleRow st rule =
  HH.div
    [ HP.class_ (H.ClassName "rule-row") ]
    [ renderTileSwatch st { name: rule.leftName, orientation: rule.leftRot } (rule.leftName <> " " <> show rule.leftRot)
    , HH.div [ HP.class_ (H.ClassName "rule-arrow") ] [ HH.text "→" ]
    , renderTileSwatch st { name: rule.rightName, orientation: rule.rightRot } (rule.rightName <> " " <> show rule.rightRot)
    ]

-- Top-right badges on a pattern thumbnail — only for patterns that only
-- exist in the catalog *because* of the rotation/mirror options
-- (`cat.origins`; see `WFC.Catalog`'s origin tracking). A pattern that also
-- occurs as a genuine unmodified window elsewhere in the sample is treated
-- as original and gets no badge, even if rotations/mirroring are on.
renderSymmetryBadges :: PatternCatalog Int -> PatternId -> H.ComponentHTML Action Slots Aff
renderSymmetryBadges cat pid =
  case Map.lookup pid cat.origins of
    Nothing -> HH.text ""
    Just o ->
      HH.div
        [ HP.class_ (H.ClassName "symmetry-badges") ]
        ( (if o.rotated then [ badge "rotate" "↻" "Rotated variant" ] else [])
          <> (if o.mirrored then [ badge "mirror" "⇋" "Mirrored variant" ] else [])
        )
  where
  badge cls glyph tip =
    HH.span
      [ HP.class_ (H.ClassName ("symmetry-badge " <> cls))
      , HP.title tip
      ]
      [ HH.text glyph ]

renderMain :: State -> H.ComponentHTML Action Slots Aff
renderMain st =
  HH.div
    [ HP.class_ (H.ClassName "main-view") ]
    [ HH.canvas
        [ HP.id "wfc-canvas"
        , HP.width 320
        , HP.height 320
        ]
    , HH.canvas
        [ HP.id "upload-canvas"
        , HP.width 32
        , HP.height 32
        , HP.style "display:none;"
        ]
    , renderMatrix st
    ]

renderMatrix :: State -> H.ComponentHTML Action Slots Aff
renderMatrix st =
  case activeGrid st of
    Nothing   -> HH.text ""
    Just grid ->
      let sample = currentSample st
      in
      HH.table
        [ HP.class_ (H.ClassName "matrix") ]
        (map (\row ->
          HH.tr_ (map (renderCell sample) row)
        ) grid)

renderCell :: SampleMeta -> CellSnapshot -> H.ComponentHTML Action Slots Aff
renderCell sample cell
  | cell.contradiction =
      HH.td
        [ HP.style "background:#ff4444;color:white;text-align:center;" ]
        [ HH.text "?" ]
  | cell.collapsed =
      let bg   = WP.cellColor sample.palette cell
          text = fromMaybe "" (map show (Array.head cell.values))
      in
      HH.td
        [ HP.style ("background:" <> bg <> ";font-size:8px;text-align:center;") ]
        [ HH.text text ]
  | otherwise =
      let visible = Array.take 9 cell.values
          spans   = map (\v ->
            HH.span
              [ HP.style ("background:" <> sample.palette v <> ";") ]
              []
            ) visible
      in
      HH.td
        [ HP.class_ (H.ClassName "uncollapsed") ]
        [ HH.div [ HP.class_ (H.ClassName "sudoku") ] spans ]

-- ---------------------------------------------------------------------------
-- handleAction
-- ---------------------------------------------------------------------------

handleAction :: Action -> H.HalogenM State Action Slots Void Aff Unit
handleAction = case _ of

  Init -> pure unit

  Finalize -> do
    st <- H.get
    for_ st.worker (H.liftEffect <<< Worker.terminate)

  SelectSample idx -> do
    sendToWorker resetSessionCommand
    H.modify_ \s -> (resetRunState s)
      { sampleIdx     = idx
      , customImage   = Nothing
      , uploadError   = Nothing
      , customTileSet = Nothing
      }
    case sourceKindOfIdx idx of
      SrcImage ->
        -- Indices in the "(image)" range are the bundled reference images
        -- (Demo.ImageSamples) — decode the same way an uploaded file
        -- would be, just fetched from a static path instead of a File/Blob.
        case Array.index imageSamples (idx - Array.length samples) of
          Nothing  -> applySampleDefaults
          Just def -> do
            result <- H.liftAff (ImageUpload.loadImageFromUrl builtinImageMaxSide def.path def.name)
            case result of
              Left err     -> H.modify_ _ { uploadError = Just err }
              Right loaded -> H.modify_ _ { customImage = Just (customImageFrom loaded) }
            applySampleDefaults
      SrcXmlTileset ->
        -- Indices in the "(Tileset)" range are the original-WFC XML
        -- tileset files — fetch the XML text, then parse it into a
        -- `TileSetDef` (the actual catalog/rules only get built at
        -- Extract time, same as every other source).
        case Array.index xmlTileSamples (idx - xmlTilesetOffset) of
          Nothing  -> applySampleDefaults
          Just def -> do
            H.modify_ _ { xmlTilesetDir = Just def.tileDir }
            result <- H.liftAff (Fetch.fetchText def.xmlPath)
            case result of
              Left err -> H.modify_ _ { uploadError = Just err }
              Right xmlText -> case parseTileSetXml xmlText of
                Left err   -> H.modify_ _ { uploadError = Just ("XML parse error: " <> err) }
                Right def' -> H.modify_ _ { customTileSet = Just def' }
            applySampleDefaults
      _ ->
        applySampleDefaults
    drawCanvas

  UploadImage ev -> do
    let mInputEl = Event.target ev >>= HTMLInputElement.fromEventTarget
    case mInputEl of
      Nothing -> pure unit
      Just inputEl -> do
        mFiles <- H.liftEffect (HTMLInputElement.files inputEl)
        case mFiles >>= FileList.item 0 of
          Nothing -> pure unit
          Just file -> do
            sendToWorker resetSessionCommand
            result <- H.liftAff (ImageUpload.loadImageAsSample 32 file)
            case result of
              Left err ->
                H.modify_ _ { uploadError = Just err }
              Right loaded -> do
                H.modify_ \s -> (resetRunState s)
                  { customImage = Just (customImageFrom loaded)
                  , uploadError = Nothing
                  }
                applySampleDefaults
    drawCanvas

  ExtractPatterns -> do
    st <- H.get
    sendToWorker resetSessionCommand
    let built =
          case sourceKindOf st of
            SrcHandTiled ->
              let ts = currentTileSample st
              in { cat: buildTiledCatalog ts.tiles, rules: buildTiledRules ts.tiles, palette: Nothing, tileOf: Nothing }
            SrcXmlTileset ->
              -- `st.customTileSet` should already be loaded by the time
              -- Extract is reachable (the button/its highlight don't wait
              -- on the fetch), but fall back to an empty tileset rather
              -- than crash if it's clicked mid-load.
              let def = fromMaybe { unique: false, tiles: [], neighbors: [], subsets: [] } st.customTileSet
                  b   = WP.buildIntCatalogFromTileSet def
              in { cat: b.catalog, rules: b.rules, palette: Just b.palette, tileOf: Just b.tileOf }
            _ ->
              let sample = currentSampleDef st
                  c      = extractPatterns st.patternSize st.inputPeriodic st.useRotations st.useMirror sample.grid
              in { cat: c, rules: buildRules c, palette: Nothing, tileOf: Nothing }
        cat   = built.cat
        rules = built.rules
        wave  = initWave cat rules { width: st.outW, height: st.outH } st.outputPeriodic
    H.modify_ \s -> s
      { status      = Ready
      , catalog     = Just cat
      , initWave_   = Just wave
      , stepCount   = 0
      , stepTimes   = ([] :: Array Number)
      , totalTime   = 0.0
      , displayGrid = Just (WP.waveToSnapshot cat wave)
      , progressLog = []
      , viewedStep  = Nothing
      , extractedWith = Just
          { patternSize: st.patternSize, useRotations: st.useRotations, useMirror: st.useMirror
          , inputPeriodic: st.inputPeriodic
          }
      , stepBackedOut  = false
      , tilesetPalette = built.palette
      , tilesetTileOf  = built.tileOf
      , blankCellSnapshot = Just (WP.blankCellSnapshot cat)
      }
    drawCanvas
    -- Preload the tileset's tile images in the background, if this is an
    -- XML tileset — flat colors (the palette fallback) keep being used
    -- until an image lands in the cache, and every redraw re-checks the
    -- cache, so nothing else needs to react to this finishing.
    case Tuple (sourceKindOf st) (Tuple st.customTileSet st.xmlTilesetDir) of
      Tuple SrcXmlTileset (Tuple (Just def) (Just dir)) -> do
        cache <- H.liftAff (loadAllTilesetImages dir def)
        H.modify_ _ { tilesetImageCache = cache }
        drawCanvas
      _ -> pure unit

  -- Single step, driven through the worker's persistent session (same one
  -- Run once/Run until solved/Stop share) so it continues from wherever
  -- that session currently is — whether it got there via earlier Steps, a
  -- completed Run, or a Run that was Stopped mid-way — instead of
  -- restarting. The reply lands in `WorkerMsg` like any other worker
  -- message (`msg.continuous = false` keeps it from flipping `running`).
  StepOnce -> do
    st <- H.get
    case st.catalog of
      Nothing -> pure unit
      Just _  -> sendToWorkerEnsuring (stepCommand st)

  ResetWave -> do
    st <- H.get
    sendToWorker resetSessionCommand
    H.modify_ \s -> s
      { status      = Ready
      -- Reset always wins over an in-flight run: `resetSessionCommand`
      -- (above) discards the worker's session and cancels whatever it was
      -- doing, so the local "still running" flags need to be cleared right
      -- along with it — otherwise Pause/Reset/Step/Run's `disabled`
      -- conditions (all driven by `running`/`untilSolvedRunning`) keep
      -- reflecting a run that no longer exists.
      , running     = false
      , untilSolvedRunning = false
      , stepCount   = 0
      , stepTimes   = ([] :: Array Number)
      , totalTime   = 0.0
      , viewedStep  = Nothing
      , progressLog = []
      , stepBackedOut = false
      , displayGrid = case Tuple st.catalog st.initWave_ of
          Tuple (Just cat) (Just w) -> Just (WP.waveToSnapshot cat w)
          _                         -> st.displayGrid
      }
    drawCanvas

  RunOnce        -> startRun "once"
  RunUntilSolved -> startRun "untilSolved"

  Stop -> do
    sendToWorker stopCommand
    H.modify_ _ { running = false, untilSolvedRunning = false, status = Stopped }

  WorkerMsg ev -> do
    st <- H.get
    let msg = unsafeCoerce (MessageEvent.data_ ev) :: WP.Progress
    -- A step already in flight when Stop (or a later Step/Run) superseded
    -- it can still land after the fact — `msg.token` is the worker's
    -- request counter at the time it was sent, and `st.cmdSeq` mirrors it
    -- on this side (`sendToWorker`), so a mismatch means this reply is
    -- stale and gets dropped instead of clobbering newer state.
    when (msg.token == st.cmdSeq) do
      -- The worker reports `elapsedMs` as cumulative time since the run
      -- started (it never resets `t0`, even across restarts), so the
      -- per-step duration is the delta from the previous cumulative value,
      -- not `msg.elapsedMs` itself.
      H.modify_ \s -> s
        { progressLog = Array.snoc s.progressLog msg
        , displayGrid = Just msg.grid
        -- A "contradiction" in "Run until solved" always auto-restarts
        -- (the worker sets `restarted` on that very message when it will),
        -- so it's still "running" through it — only a genuinely terminal
        -- outcome ("done", or a "Run once" contradiction, which never
        -- restarts) actually stops. Without this, Stop/Reset/Step/etc.
        -- would flicker enabled/disabled on every restart cycle.
        , running     = msg.continuous && (msg.kind == "progress" || (msg.kind == "contradiction" && msg.restarted))
        , untilSolvedRunning = s.untilSolvedRunning && msg.kind /= "done"
        , status      = case msg.kind of
            "done"          -> Done
            "contradiction" -> Contradiction
            "progress"      -> Stepped
            _               -> s.status
        , stepCount   = msg.step
        , stepTimes   = Array.snoc s.stepTimes (msg.elapsedMs - s.totalTime)
        , totalTime   = msg.elapsedMs
        -- Only a manual Step's own reply (`continuous = false`) drives this
        -- — an automatic Run's messages leave it alone, so the Step
        -- button's rewind mark only ever reflects the last time *it* was
        -- clicked, not incidental backtrack pops during a Run.
        , stepBackedOut = if msg.continuous then s.stepBackedOut else msg.backedOut
        }
      drawCanvas

  TogglePatterns ->
    H.modify_ \st -> st { showPats = not st.showPats }

  ToggleRules ->
    H.modify_ \st -> st { showRules = not st.showRules }

  ToggleBacktracking ->
    H.modify_ \st -> st { useBacktracking = not st.useBacktracking }

  ViewStep i -> do
    H.modify_ _ { viewedStep = Just i }
    drawCanvas

  SetPatternSize n ->
    H.modify_ _ { patternSize = n }

  SetOutW w -> do
    H.modify_ _ { outW = w }
    resizeLive

  SetOutH h -> do
    H.modify_ _ { outH = h }
    resizeLive

  ToggleFixOutputSize ->
    H.modify_ \st -> st { fixOutputSize = not st.fixOutputSize }

  ToggleRotations ->
    H.modify_ \st -> st { useRotations = not st.useRotations }

  ToggleMirror ->
    H.modify_ \st -> st { useMirror = not st.useMirror }

  ToggleInputPeriodic ->
    H.modify_ \st -> st { inputPeriodic = not st.inputPeriodic }

  ToggleOutputPeriodic ->
    H.modify_ \st -> st { outputPeriodic = not st.outputPeriodic }
