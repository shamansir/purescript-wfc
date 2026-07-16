module Server.Main (main) where

import Prelude hiding ((/))

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import HTTPurple
  ( class Generic
  , Method(..)
  , Request
  , ResponseHeaders
  , ResponseM
  , RouteDuplex'
  , ServerM
  , badRequest'
  , headers
  , jsonHeaders
  , noArgs
  , notFound
  , ok'
  , response'
  , root
  , segment
  , serve
  , sum
  , (/)
  )
import HTTPurple.Status as Status
import HTTPurple.Body (RequestBody, toString)
import Node.Encoding (Encoding(UTF8))
import Node.EventEmitter as EE
import Node.Stream (Duplex)
import Node.Stream as Stream
import Server.Codec (decodeCreateRequest, errorJson)
import Server.Engine (CreateRequest, Snapshot, initialSnapshot, solveSync, statusOf)
import Server.Session (Store)
import Server.Session as Session

-- ---------------------------------------------------------------------------
-- Routes
-- ---------------------------------------------------------------------------

data Route
  = Solve
  | Sessions
  | Session String
  | SessionStep String
  | SessionRun String
  | SessionStop String
  | SessionHistory String
  | SessionEvents String

derive instance genericRoute :: Generic Route _

routeDuplex :: RouteDuplex' Route
routeDuplex = root $ sum
  { "Solve": "solve" / noArgs
  , "Sessions": "sessions" / noArgs
  , "Session": "sessions" / segment
  , "SessionStep": "sessions" / segment / "step"
  , "SessionRun": "sessions" / segment / "run"
  , "SessionStop": "sessions" / segment / "stop"
  , "SessionHistory": "sessions" / segment / "history"
  , "SessionEvents": "sessions" / segment / "events"
  }

-- ---------------------------------------------------------------------------
-- Request-body decoding, shared by every POST handler that takes a body.
-- ---------------------------------------------------------------------------

withCreateRequest :: RequestBody -> (CreateRequest -> ResponseM) -> ResponseM
withCreateRequest body handler = do
  bodyStr <- toString body
  case parseJson bodyStr >>= decodeCreateRequest of
    Left err -> badRequest' jsonHeaders (stringify (errorJson (printJsonDecodeError err)))
    Right req -> handler req

-- ---------------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------------

-- POST /solve — stateless: builds the wave, solves it (single pass, or
-- `backtracking: true` with `maxAttempts`, default 50), returns the result
-- or a contradiction. No session is kept.
handleSolve :: RequestBody -> ResponseM
handleSolve body = withCreateRequest body \req -> do
  res <- liftEffect (solveSync req)
  ok' jsonHeaders (stringify (encodeJson res))

-- POST /sessions — builds the wave and stores it, ready to be advanced via
-- /step or /run. Returns the initial ("ready") snapshot.
handleCreateSession :: Store -> RequestBody -> ResponseM
handleCreateSession store body = withCreateRequest body \req -> do
  created <- liftEffect (Session.createSession store req)
  ok' jsonHeaders (stringify (encodeJson { id: created.id, status: "ready", snapshot: created.snapshot }))

-- GET /sessions/:id — current status + the last recorded step, without
-- advancing anything.
handleGetSession :: Store -> String -> ResponseM
handleGetSession store sid = do
  mEntry <- liftEffect (Session.getSession store sid)
  case mEntry of
    Nothing -> notFound
    Just entry -> do
      sd <- liftEffect (Ref.read entry.dataRef)
      ok' jsonHeaders
        ( stringify
            ( encodeJson
                { id: sid
                , status: statusOf sd
                , stepIdx: sd.stepIdx
                , solved: sd.solvedSoFar
                , running: sd.running
                , finished: sd.finished
                , lastSnapshot: sd.lastSnapshot
                }
            )
        )

-- GET /sessions/:id/history — every stage recorded so far (capped at the
-- session's own `maxHistory`, see Server.Engine).
handleHistory :: Store -> String -> ResponseM
handleHistory store sid = do
  mEntry <- liftEffect (Session.getSession store sid)
  case mEntry of
    Nothing -> notFound
    Just entry -> do
      sd <- liftEffect (Ref.read entry.dataRef)
      ok' jsonHeaders (stringify (encodeJson { id: sid, history: sd.history }))

sseHeaders :: ResponseHeaders
sseHeaders = headers
  { "Content-Type": "text/event-stream"
  , "Cache-Control": "no-cache"
  , "Connection": "keep-alive"
  }

isTerminalKind :: String -> Boolean
isTerminalKind k = k == "solved" || k == "contradiction" || k == "timedOut"

sseEvent :: Snapshot -> String
sseEvent snap = "data: " <> stringify (encodeJson snap) <> "\n\n"

-- A silent comment line, just to keep an otherwise-idle connection alive
-- through any proxy/load balancer sitting in front of this that would
-- otherwise time out a long-quiet stream (SSE's own spec-sanctioned
-- keep-alive mechanism — a line starting with `:` is a comment, ignored by
-- `EventSource` but still resets the client/proxy's read timeout).
heartbeatLoop :: Ref Boolean -> Duplex -> Aff Unit
heartbeatLoop closedRef stream = do
  delay (Milliseconds 15000.0)
  closed <- liftEffect (Ref.read closedRef)
  if closed then pure unit
  else do
    liftEffect (void (Stream.writeString stream UTF8 ": keep-alive\n\n"))
    heartbeatLoop closedRef stream

-- GET /sessions/:id/events — Server-Sent Events. Pushes the session's
-- current snapshot immediately (so a client connecting mid-run isn't
-- staring at a blank state until the next step happens to land), then
-- every subsequent one as `/step`/`/run` (whichever is actually driving
-- the session — this route only *observes*, it never steps anything
-- itself) records it, until the session reaches a terminal state or the
-- client disconnects.
handleEvents :: Store -> String -> ResponseM
handleEvents store sid = do
  mEntry <- liftEffect (Session.getSession store sid)
  case mEntry of
    Nothing -> notFound
    Just entry -> do
      passThrough <- liftEffect Stream.newPassThrough
      closedRef <- liftEffect (Ref.new false)
      subIdRef <- liftEffect (Ref.new Nothing)

      let
        closeStream = do
          already <- Ref.read closedRef
          when (not already) do
            Ref.write true closedRef
            mSubId <- Ref.read subIdRef
            for_ mSubId (Session.unsubscribe entry)
            Stream.end passThrough

        push snap = do
          closed <- Ref.read closedRef
          when (not closed) do
            void (Stream.writeString passThrough UTF8 (sseEvent snap))
            when (isTerminalKind snap.kind) closeStream

      liftEffect (EE.on_ Stream.errorH (\_ -> closeStream) passThrough)
      liftEffect (EE.on_ Stream.closeH closeStream passThrough)

      sd0 <- liftEffect (Ref.read entry.dataRef)
      liftEffect (push (fromMaybe (initialSnapshot sd0) sd0.lastSnapshot))

      if sd0.finished then
        liftEffect closeStream
      else do
        subId <- liftEffect (Session.subscribe entry push)
        liftEffect (Ref.write (Just subId) subIdRef)
        liftEffect (launchAff_ (heartbeatLoop closedRef passThrough))

      response' Status.ok sseHeaders passThrough

-- POST /sessions/:id/step — exactly one unit of work (one plain step, or
-- one backtrack-step, depending on how the session was created), returned
-- synchronously.
handleStep :: Store -> String -> ResponseM
handleStep store sid = do
  mEntry <- liftEffect (Session.getSession store sid)
  case mEntry of
    Nothing -> notFound
    Just entry -> do
      snap <- liftEffect (Session.stepOnce entry)
      ok' jsonHeaders (stringify (encodeJson snap))

-- POST /sessions/:id/run — starts (or resumes) a background loop that
-- keeps stepping until solved, contradiction/attempts-exhausted, or a
-- later /stop cancels it. Returns immediately (202) with the session's
-- current status; poll GET /sessions/:id for progress.
handleRun :: Store -> String -> ResponseM
handleRun store sid = do
  mEntry <- liftEffect (Session.getSession store sid)
  case mEntry of
    Nothing -> notFound
    Just entry -> do
      liftEffect (Session.startRun entry)
      sd <- liftEffect (Ref.read entry.dataRef)
      response' Status.accepted jsonHeaders (stringify (encodeJson { id: sid, status: statusOf sd }))

-- POST /sessions/:id/stop — cancels an in-flight /run loop and returns the
-- last step it managed to record before stopping.
handleStop :: Store -> String -> ResponseM
handleStop store sid = do
  mEntry <- liftEffect (Session.getSession store sid)
  case mEntry of
    Nothing -> notFound
    Just entry -> do
      lastStep <- liftEffect (Session.stopSession entry)
      sd <- liftEffect (Ref.read entry.dataRef)
      ok' jsonHeaders (stringify (encodeJson { id: sid, status: statusOf sd, lastStep }))

-- DELETE /sessions/:id
handleDeleteSession :: Store -> String -> ResponseM
handleDeleteSession store sid = do
  liftEffect (Session.deleteSession store sid)
  ok' jsonHeaders (stringify (encodeJson { id: sid, status: "deleted" }))

-- ---------------------------------------------------------------------------
-- Router + main
-- ---------------------------------------------------------------------------

router :: Store -> Request Route -> ResponseM
router _ { method: Post, route: Solve, body } = handleSolve body
router store { method: Post, route: Sessions, body } = handleCreateSession store body
router store { method: Get, route: Session sid } = handleGetSession store sid
router store { method: Delete, route: Session sid } = handleDeleteSession store sid
router store { method: Get, route: SessionHistory sid } = handleHistory store sid
router store { method: Get, route: SessionEvents sid } = handleEvents store sid
router store { method: Post, route: SessionStep sid } = handleStep store sid
router store { method: Post, route: SessionRun sid } = handleRun store sid
router store { method: Post, route: SessionStop sid } = handleStop store sid
router _ _ = notFound

main :: ServerM
main = do
  store <- Session.newStore
  serve { port: 8080 } { route: routeDuplex, router: router store }
