module Demo.App where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
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

import Demo.Samples (SampleDef, samples)
import WFC.Algorithm (step)
import WFC.Catalog (PatternCatalog, extractPatterns)
import WFC.Grid (Pos(..), allPositions)
import WFC.Pattern (Pattern(..), PatternId(..))
import WFC.Rules (AdjacencyRules, buildRules)
import WFC.Wave (Wave, getCellPossibilities, initWave)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

data WFCStatus = Idle | Ready | Stepped | Done | Contradiction

derive instance eqWFCStatus :: Eq WFCStatus

type State =
  { sampleIdx :: Int
  , status    :: WFCStatus
  , catalog   :: Maybe (PatternCatalog Int)
  , rules     :: Maybe AdjacencyRules
  , wave      :: Maybe (Wave Int)
  , initWave_ :: Maybe (Wave Int)
  , stepCount :: Int
  , stepTimes :: Array Number
  , totalTime :: Number
  , showPats  :: Boolean
  }

data Action
  = Init
  | SelectSample Int
  | ExtractPatterns
  | StepOnce
  | ResetWave
  | RunAll
  | TogglePatterns

type Slots :: forall k. Row k
type Slots = ()

-- ---------------------------------------------------------------------------
-- Initial state
-- ---------------------------------------------------------------------------

initialState :: State
initialState =
  { sampleIdx: 0
  , status:    Idle
  , catalog:   Nothing
  , rules:     Nothing
  , wave:      Nothing
  , initWave_: Nothing
  , stepCount: 0
  , stepTimes: []
  , totalTime: 0.0
  , showPats:  false
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

currentSample :: State -> SampleDef
currentSample st =
  fromMaybe (unsafeHead samples) (Array.index samples st.sampleIdx)

unsafeHead :: forall a. Array a -> a
unsafeHead arr = case Array.head arr of
  Just x  -> x
  Nothing -> unsafeHead arr  -- should never happen for non-empty arrays

-- Top-left pixel color for a given pattern in a catalog
patColor :: SampleDef -> PatternCatalog Int -> PatternId -> String
patColor sample cat pid = fromMaybe "#888888" do
  Pattern px <- Map.lookup pid cat.patterns
  v <- Array.head px
  pure (sample.palette v)

cellColor :: SampleDef -> Maybe (PatternCatalog Int) -> Maybe (Set PatternId) -> String
cellColor _ _ Nothing = "#ff4444"
cellColor _ Nothing (Just _) = "#888888"
cellColor sample (Just cat) (Just pids)
  | Set.isEmpty pids = "#ff4444"
  | Set.size pids == 1 =
      fromMaybe "#888888" do
        pid <- Array.head (Set.toUnfoldable pids :: Array PatternId)
        Pattern px <- Map.lookup pid cat.patterns
        v <- Array.head px
        pure (sample.palette v)
  | otherwise = "#c0c0d0"

-- Run all steps in Effect, counting them
runAllEffect :: Wave Int -> Effect { wave :: Maybe (Wave Int), steps :: Int }
runAllEffect w0 = go w0 0
  where
    go w n = do
      r <- step w
      case r of
        Left  _          -> pure { wave: Nothing, steps: n }
        Right Nothing    -> pure { wave: Just w, steps: n }
        Right (Just w')  -> go w' (n + 1)

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
      case st.wave of
        Nothing   -> pure unit
        Just wave -> do
          let size = wave.size
              cellW = cw / Int.toNumber size.width
              cellH = ch / Int.toNumber size.height
              sampleDef = currentSample st
          void $ Array.foldM
            (\_ pos -> do
              let Pos { x, y } = pos
                  cell  = getCellPossibilities wave pos
                  color = cellColor sampleDef st.catalog cell
              Canvas.setFillStyle ctx color
              Canvas.fillRect ctx
                { x:      Int.toNumber x * cellW
                , y:      Int.toNumber y * cellH
                , width:  cellW - 0.5
                , height: cellH - 0.5
                }
            )
            unit
            (allPositions size)

drawCanvas :: H.HalogenM State Action Slots Void Aff Unit
drawCanvas = do
  st <- H.get
  H.liftEffect $ drawCanvasEffect st

-- ---------------------------------------------------------------------------
-- Component
-- ---------------------------------------------------------------------------

component :: forall q i. H.Component q i Void Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
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
    ]

renderSidebar :: State -> H.ComponentHTML Action Slots Aff
renderSidebar st =
  HH.div
    [ HP.class_ (H.ClassName "sidebar") ]
    [ HH.select
        [ HE.onSelectedIndexChange SelectSample ]
        (Array.mapWithIndex renderOption samples)
    , renderButtons st
    , renderStats st
    , renderPatterns st
    ]

renderOption :: Int -> SampleDef -> H.ComponentHTML Action Slots Aff
renderOption i s =
  HH.option
    [ HP.value (show i) ]
    [ HH.text s.name ]

renderButtons :: State -> H.ComponentHTML Action Slots Aff
renderButtons st =
  let notReady   = st.status == Idle
      noStepsTaken = st.status == Idle || st.status == Ready
  in
  HH.div
    [ HP.class_ (H.ClassName "controls") ]
    [ HH.button
        [ HE.onClick \_ -> ExtractPatterns ]
        [ HH.text "◫ Extract" ]
    , HH.button
        [ HE.onClick \_ -> TogglePatterns
        , HP.disabled notReady
        ]
        [ HH.text "⊞ Patterns" ]
    , HH.button
        [ HE.onClick \_ -> StepOnce
        , HP.disabled notReady
        ]
        [ HH.text "▶ Step" ]
    , HH.button
        [ HE.onClick \_ -> ResetWave
        , HP.disabled noStepsTaken
        ]
        [ HH.text "■ Reset" ]
    , HH.button
        [ HE.onClick \_ -> RunAll
        , HP.disabled notReady
        ]
        [ HH.text "⏩ Run All" ]
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
        Idle         -> "Idle"
        Ready        -> "Ready"
        Stepped      -> "Stepping..."
        Done         -> "Done"
        Contradiction -> "Contradiction!"
  in
  HH.div
    [ HP.class_ (H.ClassName "stats") ]
    [ HH.div_ [ HH.text ("Status: " <> statusStr) ]
    , HH.div_ [ HH.text statsStr ]
    ]

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
    , renderMatrix st
    ]

renderMatrix :: State -> H.ComponentHTML Action Slots Aff
renderMatrix st =
  case st.wave of
    Nothing   -> HH.text ""
    Just wave ->
      let sample = currentSample st
          size   = wave.size
          rows   = Array.range 0 (size.height - 1)
      in
      HH.table
        [ HP.class_ (H.ClassName "matrix") ]
        (map (\y ->
          HH.tr_
            (map (\x ->
              renderCell st sample wave (Pos { x, y })
            ) (Array.range 0 (size.width - 1)))
        ) rows)

renderCell
  :: State
  -> SampleDef
  -> Wave Int
  -> Pos
  -> H.ComponentHTML Action Slots Aff
renderCell st sample wave pos =
  case getCellPossibilities wave pos of
    Nothing    ->
      HH.td
        [ HP.style "background:#ff4444;color:white;text-align:center;" ]
        [ HH.text "×" ]
    Just pids
      | Set.size pids == 1 ->
          let pid = fromMaybe (PatternId 0) (Array.head (Set.toUnfoldable pids :: Array PatternId))
              bg  = case st.catalog of
                      Nothing  -> "#888888"
                      Just cat -> patColor sample cat pid
          in
          HH.td
            [ HP.style ("background:" <> bg <> ";font-size:8px;text-align:center;") ]
            [ HH.text (show pid) ]
      | otherwise ->
          let visible = Array.take 9 (Set.toUnfoldable pids :: Array PatternId)
              spans   = map (\pid ->
                let c = case st.catalog of
                          Nothing  -> "#888888"
                          Just cat -> patColor sample cat pid
                in HH.span
                     [ HP.style ("background:" <> c <> ";") ]
                     [ HH.text "" ]
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

  SelectSample idx ->
    H.modify_ \st ->
      { sampleIdx: idx
      , status:    Idle
      , catalog:   (Nothing :: Maybe (PatternCatalog Int))
      , rules:     (Nothing :: Maybe AdjacencyRules)
      , wave:      (Nothing :: Maybe (Wave Int))
      , initWave_: (Nothing :: Maybe (Wave Int))
      , stepCount: 0
      , stepTimes: ([] :: Array Number)
      , totalTime: 0.0
      , showPats:  st.showPats
      }

  ExtractPatterns -> do
    st <- H.get
    let sample = currentSample st
        cat    = extractPatterns sample.n sample.periodic 1 sample.grid
        rules  = buildRules cat
        wave   = initWave cat rules { width: sample.outW, height: sample.outH } sample.periodic
    H.modify_ \s -> s
      { status    = Ready
      , catalog   = Just cat
      , rules     = Just rules
      , wave      = Just wave
      , initWave_ = Just wave
      , stepCount = 0
      , stepTimes = ([] :: Array Number)
      , totalTime = 0.0
      }
    drawCanvas

  StepOnce -> do
    st <- H.get
    case st.wave of
      Nothing   -> pure unit
      Just wave -> do
        t0 <- H.liftEffect now
        result <- H.liftEffect (step wave)
        t1 <- H.liftEffect now
        let elapsed = timeDiff t0 t1
        case result of
          Left _ ->
            H.modify_ \s -> s
              { status    = Contradiction
              , stepCount = s.stepCount + 1
              , stepTimes = Array.snoc s.stepTimes elapsed
              , totalTime = s.totalTime + elapsed
              }
          Right Nothing ->
            H.modify_ \s -> s
              { status    = Done
              , stepCount = s.stepCount + 1
              , stepTimes = Array.snoc s.stepTimes elapsed
              , totalTime = s.totalTime + elapsed
              }
          Right (Just wave') ->
            H.modify_ \s -> s
              { wave      = Just wave'
              , status    = Stepped
              , stepCount = s.stepCount + 1
              , stepTimes = Array.snoc s.stepTimes elapsed
              , totalTime = s.totalTime + elapsed
              }
        drawCanvas

  ResetWave -> do
    H.modify_ \st -> st
      { wave      = st.initWave_
      , status    = Ready
      , stepCount = 0
      , stepTimes = ([] :: Array Number)
      , totalTime = 0.0
      }
    drawCanvas

  RunAll -> do
    st <- H.get
    case st.wave of
      Nothing   -> pure unit
      Just wave -> do
        t0 <- H.liftEffect now
        res <- H.liftEffect (runAllEffect wave)
        t1 <- H.liftEffect now
        let elapsed = timeDiff t0 t1
        case res.wave of
          Nothing ->
            H.modify_ \s -> s
              { status    = Contradiction
              , stepCount = res.steps
              , stepTimes = [ elapsed ]
              , totalTime = elapsed
              }
          Just w' ->
            H.modify_ \s -> s
              { wave      = Just w'
              , status    = Done
              , stepCount = res.steps
              , stepTimes = [ elapsed ]
              , totalTime = elapsed
              }
        drawCanvas

  TogglePatterns ->
    H.modify_ \st -> st { showPats = not st.showPats }
