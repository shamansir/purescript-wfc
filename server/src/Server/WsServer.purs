module Server.WsServer (startWsServer) where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Combinators ((.:))
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut.Decode.Parser (parseJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Server.Codec (decodeCreateRequest)
import Server.Engine (initialSnapshot, statusOf)
import Server.Session (SessionEntry, Store)
import Server.Session as Session
import Server.Ws (WsConnection, createWsServer, onClose, onConnection, onMessage, send)

-- One WebSocket connection, bidirectional: a client sends JSON commands
-- (`{"cmd": "...", ...}`), the server replies/pushes JSON events
-- (`{"type": "...", ...}`) on the same socket. Session logic itself is
-- unchanged from the REST API — every command below just calls the same
-- `Server.Session` functions `Server.Main`'s HTTP handlers do, so a step
-- taken here shows up on any other subscriber (another WS connection, or
-- an SSE `/events` stream) via the session's existing
-- `subscribe`/`notifySubscribers` mechanism, and vice versa.
data WsCommand
  = CmdCreate Json
  | CmdSubscribe String
  | CmdUnsubscribe String
  | CmdStep String
  | CmdRun String
  | CmdStop String
  | CmdGet String
  | CmdDelete String

decodeWsCommand :: Json -> Either JsonDecodeError WsCommand
decodeWsCommand json = do
  o <- decodeJson json
  cmd <- o .: "cmd"
  case cmd :: String of
    "create" -> pure (CmdCreate json)
    "subscribe" -> CmdSubscribe <$> o .: "id"
    "unsubscribe" -> CmdUnsubscribe <$> o .: "id"
    "step" -> CmdStep <$> o .: "id"
    "run" -> CmdRun <$> o .: "id"
    "stop" -> CmdStop <$> o .: "id"
    "get" -> CmdGet <$> o .: "id"
    "delete" -> CmdDelete <$> o .: "id"
    other -> Left (TypeMismatch ("cmd: unknown command " <> show other))

sendJson :: forall a. EncodeJson a => WsConnection -> a -> Effect Unit
sendJson conn payload = send conn (stringify (encodeJson payload))

-- Every session this connection is currently `subscribe`d to, keyed by
-- session id, so `onClose` can unregister exactly those (and only those)
-- subscriptions instead of leaking one per connection per session forever.
type Subscriptions = Ref (Map String Int)

startWsServer :: Store -> Int -> Effect Unit
startWsServer store port = do
  wss <- createWsServer port
  onConnection wss (handleConnection store)

handleConnection :: Store -> WsConnection -> Effect Unit
handleConnection store conn = do
  subsRef <- Ref.new Map.empty
  onMessage conn (handleMessage store conn subsRef)
  onClose conn (handleClose store subsRef)

handleClose :: Store -> Subscriptions -> Effect Unit
handleClose store subsRef = do
  subs <- Ref.read subsRef
  for_ (Map.toUnfoldable subs :: Array (Tuple String Int)) \(Tuple sid subId) -> do
    mEntry <- Session.getSession store sid
    for_ mEntry \entry -> Session.unsubscribe entry subId

handleMessage :: Store -> WsConnection -> Subscriptions -> String -> Effect Unit
handleMessage store conn subsRef raw =
  case parseJson raw >>= decodeWsCommand of
    Left err -> sendJson conn { type: "error", message: printJsonDecodeError err }
    Right cmd -> runCommand store conn subsRef cmd

withEntry :: Store -> WsConnection -> String -> (SessionEntry -> Effect Unit) -> Effect Unit
withEntry store conn sid handler = do
  mEntry <- Session.getSession store sid
  case mEntry of
    Nothing -> sendJson conn { type: "error", id: sid, message: "session not found" }
    Just entry -> handler entry

runCommand :: Store -> WsConnection -> Subscriptions -> WsCommand -> Effect Unit
runCommand store conn subsRef = case _ of
  CmdCreate json -> case decodeCreateRequest json of
    Left err -> sendJson conn { type: "error", message: printJsonDecodeError err }
    Right req -> do
      created <- Session.createSession store req
      sendJson conn { type: "created", id: created.id, status: "ready", snapshot: created.snapshot }

  CmdSubscribe sid -> withEntry store conn sid \entry -> do
    sd <- Ref.read entry.dataRef
    sendJson conn { type: "snapshot", id: sid, snapshot: fromMaybe (initialSnapshot sd) sd.lastSnapshot }
    subId <- Session.subscribe entry \snap -> sendJson conn { type: "snapshot", id: sid, snapshot: snap }
    Ref.modify_ (Map.insert sid subId) subsRef

  CmdUnsubscribe sid -> do
    subs <- Ref.read subsRef
    for_ (Map.lookup sid subs) \subId ->
      withEntry store conn sid \entry -> do
        Session.unsubscribe entry subId
        Ref.modify_ (Map.delete sid) subsRef

  -- `stepOnce` already notifies every subscriber, this connection included
  -- if it's `subscribe`d to `sid` — sending the same snapshot again here
  -- unconditionally would double it up on one socket (unlike the REST
  -- API, where a `/step` reply and an SSE viewer are two different
  -- connections and don't collide).
  CmdStep sid -> withEntry store conn sid \entry -> do
    snap <- Session.stepOnce entry
    subs <- Ref.read subsRef
    when (not (Map.member sid subs)) (sendJson conn { type: "snapshot", id: sid, snapshot: snap })

  CmdRun sid -> withEntry store conn sid \entry -> do
    Session.startRun entry
    sd <- Ref.read entry.dataRef
    sendJson conn { type: "status", id: sid, status: statusOf sd }

  CmdStop sid -> withEntry store conn sid \entry -> do
    lastStep <- Session.stopSession entry
    sd <- Ref.read entry.dataRef
    sendJson conn { type: "status", id: sid, status: statusOf sd, lastStep }

  CmdGet sid -> withEntry store conn sid \entry -> do
    sd <- Ref.read entry.dataRef
    sendJson conn
      { type: "status"
      , id: sid
      , status: statusOf sd
      , stepIdx: sd.stepIdx
      , solved: sd.solvedSoFar
      , running: sd.running
      , finished: sd.finished
      , lastSnapshot: sd.lastSnapshot
      }

  CmdDelete sid -> do
    Session.deleteSession store sid
    sendJson conn { type: "deleted", id: sid }
