module Demo.App where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Now (now)
import Graphics.Canvas as Canvas
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)

import Demo.ImageUpload as ImageUpload
import Demo.Samples (SampleDef, samples)
import Demo.TileSamples (TileSampleDef)
import Demo.TileSamples as TileSamples
import Demo.WorkerProtocol (Grid, CellSnapshot)
import Demo.WorkerProtocol as WP
import WFC.Algorithm (step)
import WFC.Catalog (PatternCatalog, extractPatterns)
import WFC.Grid (Pos(..))
import WFC.Pattern (Pattern(..), PatternId)
import WFC.Propagate (Contradiction(..)) as Propagate
import WFC.Rules (AdjacencyRules, buildRules)
import WFC.Tiles (buildTiledCatalog, buildTiledRules)
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
  , rules       :: Maybe AdjacencyRules
  , wave        :: Maybe (Wave Int)
  , initWave_   :: Maybe (Wave Int)
  , stepCount   :: Int
  , stepTimes   :: Array Number
  , totalTime   :: Number
  , showPats    :: Boolean
  , worker        :: Maybe Worker
  , running       :: Boolean
  , stopRequested :: Boolean
  , displayGrid   :: Maybe Grid
  , progressLog   :: Array WP.Progress
  , customImage   :: Maybe WP.CustomImage
  , uploadError   :: Maybe String
  , useBacktracking :: Boolean
  , tiledMode       :: Boolean
  , viewedStep      :: Maybe Int
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
  | ToggleBacktracking
  | ToggleTiledMode
  | ViewStep Int

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
  , rules:       Nothing
  , wave:        Nothing
  , initWave_:   Nothing
  , stepCount:   0
  , stepTimes:   []
  , totalTime:   0.0
  , showPats:    false
  , worker:        Nothing
  , running:       false
  , stopRequested: false
  , displayGrid:   Nothing
  , progressLog:   []
  , customImage:   Nothing
  , uploadError:   Nothing
  , useBacktracking: false
  , tiledMode:       false
  , viewedStep:      Nothing
  }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

timeDiff :: Instant -> Instant -> Number
timeDiff t0 t1 =
  let Milliseconds ms0 = unInstant t0
      Milliseconds ms1 = unInstant t1
  in ms1 - ms0

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
  , periodic :: Boolean
  }

sampleMetaOf :: SampleDef -> SampleMeta
sampleMetaOf s = { name: s.name, palette: s.palette, outW: s.outW, outH: s.outH, periodic: s.periodic }

tileSampleMetaOf :: TileSampleDef -> SampleMeta
tileSampleMetaOf s = { name: s.name, palette: s.palette, outW: s.outW, outH: s.outH, periodic: s.periodic }

-- Resolves the active *overlapping-model* sample (built-in or uploaded);
-- meaningless while `tiledMode` is on (image upload only applies there).
currentSampleDef :: State -> SampleDef
currentSampleDef st = case st.customImage of
  Just ci -> WP.customSampleDef ci
  Nothing -> fromMaybe (unsafeHead samples) (Array.index samples st.sampleIdx)

currentTileSample :: State -> TileSampleDef
currentTileSample st = fromMaybe (unsafeHead TileSamples.samples) (Array.index TileSamples.samples st.sampleIdx)

currentSample :: State -> SampleMeta
currentSample st
  | st.tiledMode = tileSampleMetaOf (currentTileSample st)
  | otherwise    = sampleMetaOf (currentSampleDef st)

unsafeHead :: forall a. Array a -> a
unsafeHead arr = case Array.head arr of
  Just x  -> x
  Nothing -> unsafeHead arr  -- should never happen for non-empty arrays

-- What to actually render: the live latest grid, unless the user clicked a
-- past progress-bar square to review it (`viewedStep`), in which case that
-- step's own snapshot wins instead — a live-updating run keeps appending to
-- `progressLog`/`displayGrid` underneath regardless, so clicking back to
-- "now" is just clicking the newest square again.
activeGrid :: State -> Maybe Grid
activeGrid st = case st.viewedStep >>= Array.index st.progressLog of
  Just p  -> Just p.grid
  Nothing -> st.displayGrid

stopCommand :: WP.Command
stopCommand =
  { kind: "stop", sampleIdx: 0, mode: "", custom: WP.emptyCustomImage
  , useBacktracking: false, tiledMode: false
  }

runCommand :: State -> String -> WP.Command
runCommand st mode = case st.customImage of
  Just ci | not st.tiledMode ->
    { kind: "run", sampleIdx: -1, mode, custom: ci
    , useBacktracking: st.useBacktracking, tiledMode: false
    }
  _ ->
    { kind: "run", sampleIdx: st.sampleIdx, mode, custom: WP.emptyCustomImage
    , useBacktracking: st.useBacktracking, tiledMode: st.tiledMode
    }

-- Fields shared by "switch to a different sample" (built-in or uploaded):
-- drop whatever local wave/run/progress state referred to the old sample.
resetRunState :: State -> State
resetRunState s = s
  { status        = Idle
  , catalog       = Nothing
  , rules         = Nothing
  , wave          = Nothing
  , initWave_     = Nothing
  , stepCount     = 0
  , stepTimes     = []
  , totalTime     = 0.0
  , running       = false
  , stopRequested = true
  , displayGrid   = Nothing
  , progressLog   = []
  , viewedStep    = Nothing
  }

-- Reasonable defaults for a freshly-decoded upload: N=3 when the image is at
-- least 3px on both axes (falling back to 2 or 1 for tinier uploads),
-- non-periodic, output scaled up 3x within a sane runtime-cost range.
clampInt :: Int -> Int -> Int -> Int
clampInt lo hi n = max lo (min hi n)

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
-- Canvas drawing
-- ---------------------------------------------------------------------------

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
                cells = Array.concat
                  (Array.mapWithIndex
                    (\y row -> Array.mapWithIndex (\x cell -> Tuple (Tuple x y) cell) row)
                    grid)
            Canvas.setFont ctx (show (Int.floor (min cellW cellH * 0.7)) <> "px sans-serif")
            Canvas.setTextAlign ctx Canvas.AlignCenter
            for_ cells \(Tuple (Tuple x y) cell) -> do
              let color = WP.cellColor sampleDef.palette cell
                  px    = Int.toNumber x * cellW
                  py    = Int.toNumber y * cellH
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

startRun :: String -> H.HalogenM State Action Slots Void Aff Unit
startRun mode = do
  st <- H.get
  w  <- ensureWorker
  H.modify_ _
    { running       = true
    , stopRequested = false
    , status        = Stepped
    , progressLog   = []
    , displayGrid   = Nothing
    , viewedStep    = Nothing
    }
  H.liftEffect $ Worker.postMessage (runCommand st mode) w

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
-- running it — select, mode toggles, upload, Extract/Patterns, pattern list.
renderSidebar :: State -> H.ComponentHTML Action Slots Aff
renderSidebar st =
  let sampleNames =
        if st.tiledMode
          then map _.name TileSamples.samples
          else map _.name samples
  in
  HH.div
    [ HP.class_ (H.ClassName "sidebar") ]
    [ HH.select
        [ HE.onSelectedIndexChange SelectSample ]
        (Array.mapWithIndex renderOption sampleNames)
    , renderModeToggle st
    , if st.tiledMode then HH.text "" else renderUpload st
    , renderSourcePreview st
    , renderSourceControls st
    , renderPatterns st
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

-- Switches the sample source between the overlapping model (image-derived
-- patterns) and the tiled model (hand-authored tiles + socket adjacency,
-- WFC.Tiles) — both build the same PatternCatalog/AdjacencyRules pair the
-- solving engine consumes, this only changes which one supplies them.
-- Image upload only applies to the overlapping model, so its UI is hidden
-- while tiled mode is active.
renderModeToggle :: State -> H.ComponentHTML Action Slots Aff
renderModeToggle st =
  HH.label
    [ HP.class_ (H.ClassName "toggle-row") ]
    [ HH.input
        [ HP.type_ HP.InputCheckbox
        , HP.checked st.tiledMode
        , HP.disabled st.running
        , HE.onChecked \_ -> ToggleTiledMode
        ]
    , HH.text " Tiled mode (hand-authored tiles)"
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

-- What Extract actually reads from: the raw source grid (built-in sample
-- or uploaded image) for the overlapping model, or the raw tile set for
-- the tiled model — shown as-is, before any patterns get extracted from
-- it, so the dropdown/upload/tiled-mode choice is visible up front instead
-- of only showing up once you've already clicked Extract.
renderSourcePreview :: State -> H.ComponentHTML Action Slots Aff
renderSourcePreview st
  | st.tiledMode = renderTilePreview (currentTileSample st)
  | otherwise    = renderGridPreview (currentSampleDef st)

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

-- Stays in the left sidebar: controls that pick/build the sample source.
renderSourceControls :: State -> H.ComponentHTML Action Slots Aff
renderSourceControls _ =
  HH.div
    [ HP.class_ (H.ClassName "controls") ]
    [ HH.button
        [ HE.onClick \_ -> ExtractPatterns ]
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
        ]
        [ HH.text "▶ Step" ]
    , HH.button
        [ HE.onClick \_ -> ResetWave
        , HP.disabled (noStepsTaken || st.running)
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
        ]
        [ HH.text "■ Stop" ]
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

renderStats :: State -> H.ComponentHTML Action Slots Aff
renderStats st =
  let catInfo = case st.catalog of
        Nothing  -> "No patterns"
        Just cat ->
          show (Map.size cat.patterns) <> " patterns, size "
          <> show cat.size <> "×" <> show cat.size
      lastMs = fromMaybe 0.0 (Array.last st.stepTimes)
      statsStr =
        catInfo
        <> " | Steps: " <> show st.stepCount
        <> " | Last: " <> fmtMs lastMs
        <> " | Total: " <> fmtMs st.totalTime
      statusStr = case st.status of
        Idle          -> "Idle"
        Ready         -> "Ready"
        Stepped       -> "Stepping..."
        Done          -> "Done"
        Contradiction -> "Contradiction!"
        Stopped       -> "Stopped"
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
            [ HH.text (if st.showPats then "▲ Patterns" else "▼ Patterns") ]
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
  let n       = cat.size
      sample  = currentSample st
      cells   = Array.mapWithIndex (\_ v ->
        HH.div
          [ HP.class_ (H.ClassName "mini-cell")
          , HP.style ("background:" <> sample.palette v)
          ]
          []
        ) px
  in
  HH.div
    [ HP.class_ (H.ClassName "pattern-thumb") ]
    [ HH.div
        [ HP.class_ (H.ClassName "pattern-grid")
        , HP.style ("grid-template-columns: repeat(" <> show n <> ", 1fr);")
        ]
        cells
    , HH.div
        [ HP.class_ (H.ClassName "pat-label") ]
        [ HH.text (show pid) ]
    ]

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
    st <- H.get
    for_ st.worker (H.liftEffect <<< Worker.postMessage stopCommand)
    H.modify_ \s -> (resetRunState s)
      { sampleIdx   = idx
      , customImage = Nothing
      , uploadError = Nothing
      }
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
            st <- H.get
            for_ st.worker (H.liftEffect <<< Worker.postMessage stopCommand)
            result <- H.liftAff (ImageUpload.loadImageAsSample 32 file)
            case result of
              Left err ->
                H.modify_ _ { uploadError = Just err }
              Right loaded ->
                H.modify_ \s -> (resetRunState s)
                  { customImage = Just (customImageFrom loaded)
                  , uploadError = Nothing
                  }
    drawCanvas

  ExtractPatterns -> do
    st <- H.get
    let Tuple cat rules =
          if st.tiledMode
            then
              let ts = currentTileSample st
                  c  = buildTiledCatalog ts.tiles
              in Tuple c (buildTiledRules ts.tiles)
            else
              let sample = currentSampleDef st
                  c      = extractPatterns sample.n sample.periodic 1 sample.grid
              in Tuple c (buildRules c)
        meta = currentSample st
        wave = initWave cat rules { width: meta.outW, height: meta.outH } meta.periodic
    H.modify_ \s -> s
      { status      = Ready
      , catalog     = Just cat
      , rules       = Just rules
      , wave        = Just wave
      , initWave_   = Just wave
      , stepCount   = 0
      , stepTimes   = ([] :: Array Number)
      , totalTime   = 0.0
      , displayGrid = Just (WP.waveToSnapshot cat wave)
      , progressLog = []
      , viewedStep  = Nothing
      }
    drawCanvas

  StepOnce -> do
    st <- H.get
    case Tuple st.catalog st.wave of
      Tuple (Just cat) (Just wave) -> do
        t0 <- H.liftEffect now
        result <- H.liftEffect (step wave)
        t1 <- H.liftEffect now
        let elapsed   = timeDiff t0 t1
            prevTotal = fromMaybe 0 (map _.solvedTotal (Array.last st.progressLog))
            logEntry snap kind contra =
              WP.emptyProgress
                { kind        = kind
                , step        = st.stepCount + 1
                , solvedDelta = WP.solvedCount snap - prevTotal
                , solvedTotal = WP.solvedCount snap
                , totalCells  = WP.totalCellCount snap
                , elapsedMs   = elapsed
                , contraX     = fromMaybe (-1) (map _.x contra)
                , contraY     = fromMaybe (-1) (map _.y contra)
                , grid        = snap
                }
        case result of
          Left (Propagate.Contradiction (Pos p)) -> do
            let snap = WP.markContradiction p.x p.y (WP.waveToSnapshot cat wave)
            H.modify_ \s -> s
              { status      = Contradiction
              , stepCount   = s.stepCount + 1
              , stepTimes   = Array.snoc s.stepTimes elapsed
              , totalTime   = s.totalTime + elapsed
              , displayGrid = Just snap
              , viewedStep  = Nothing
              , progressLog = Array.snoc s.progressLog (logEntry snap "contradiction" (Just p))
              }
          Right Nothing -> do
            let snap = WP.waveToSnapshot cat wave
            H.modify_ \s -> s
              { status      = Done
              , stepCount   = s.stepCount + 1
              , stepTimes   = Array.snoc s.stepTimes elapsed
              , totalTime   = s.totalTime + elapsed
              , displayGrid = Just snap
              , viewedStep  = Nothing
              , progressLog = Array.snoc s.progressLog (logEntry snap "done" Nothing)
              }
          Right (Just wave') -> do
            let snap = WP.waveToSnapshot cat wave'
            H.modify_ \s -> s
              { wave        = Just wave'
              , status      = Stepped
              , stepCount   = s.stepCount + 1
              , stepTimes   = Array.snoc s.stepTimes elapsed
              , totalTime   = s.totalTime + elapsed
              , displayGrid = Just snap
              , viewedStep  = Nothing
              , progressLog = Array.snoc s.progressLog (logEntry snap "progress" Nothing)
              }
        drawCanvas
      _ -> pure unit

  ResetWave -> do
    st <- H.get
    H.modify_ \s -> s
      { wave        = s.initWave_
      , status      = Ready
      , stepCount   = 0
      , stepTimes   = ([] :: Array Number)
      , totalTime   = 0.0
      , viewedStep  = Nothing
      , progressLog = []
      , displayGrid = case Tuple st.catalog st.initWave_ of
          Tuple (Just cat) (Just w) -> Just (WP.waveToSnapshot cat w)
          _                         -> st.displayGrid
      }
    drawCanvas

  RunOnce        -> startRun "once"
  RunUntilSolved -> startRun "untilSolved"

  Stop -> do
    st <- H.get
    for_ st.worker (H.liftEffect <<< Worker.postMessage stopCommand)
    H.modify_ _ { running = false, stopRequested = true, status = Stopped }

  WorkerMsg ev -> do
    st <- H.get
    -- A step already in flight when Stop was clicked can still land after
    -- it; once the user has asked to stop, ignore further worker chatter
    -- for this run instead of letting a straggler flip `running` back on.
    unless st.stopRequested do
      let msg = unsafeCoerce (MessageEvent.data_ ev) :: WP.Progress
      H.modify_ \s -> s
        { progressLog = Array.snoc s.progressLog msg
        , displayGrid = Just msg.grid
        , running     = msg.kind == "progress"
        , status      = case msg.kind of
            "done"          -> Done
            "contradiction" -> Contradiction
            _               -> s.status
        }
      drawCanvas

  TogglePatterns ->
    H.modify_ \st -> st { showPats = not st.showPats }

  ToggleBacktracking ->
    H.modify_ \st -> st { useBacktracking = not st.useBacktracking }

  ToggleTiledMode -> do
    st <- H.get
    for_ st.worker (H.liftEffect <<< Worker.postMessage stopCommand)
    H.modify_ \s -> (resetRunState s)
      { tiledMode   = not s.tiledMode
      , sampleIdx   = 0
      , customImage = Nothing
      , uploadError = Nothing
      }
    drawCanvas

  ViewStep i -> do
    H.modify_ _ { viewedStep = Just i }
    drawCanvas
