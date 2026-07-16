module Server.Session
  ( Store
  , SessionEntry
  , newStore
  , createSession
  , getSession
  , deleteSession
  , stepOnce
  , startRun
  , stopSession
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Server.Engine (CreateRequest, SessionData, Snapshot, freshSessionData, initialSnapshot, takeStep)
import Server.Node (randomId)

-- `token` is the same "bump to cancel" mechanism `Bench.Main`/`Demo.Worker`
-- both use: `startRun` bumps it before launching a loop, and `stopSession`
-- bumps it again — any loop still checking a now-stale token just stops
-- posting instead of needing to be forcibly killed.
type SessionEntry =
  { dataRef :: Ref SessionData
  , token   :: Ref Int
  }

type Store = Ref (Map String SessionEntry)

newStore :: Effect Store
newStore = Ref.new Map.empty

createSession :: Store -> CreateRequest -> Effect { id :: String, snapshot :: Snapshot }
createSession store req = do
  sd0 <- freshSessionData req
  let snap = initialSnapshot sd0
      sd1 = sd0 { lastSnapshot = pure snap, history = if sd0.keepHistory then [ snap ] else [] }
  dataRef <- Ref.new sd1
  token <- Ref.new 0
  sid <- randomId
  Ref.modify_ (Map.insert sid { dataRef, token }) store
  pure { id: sid, snapshot: snap }

getSession :: Store -> String -> Effect (Maybe SessionEntry)
getSession store sid = Map.lookup sid <$> Ref.read store

deleteSession :: Store -> String -> Effect Unit
deleteSession store sid = Ref.modify_ (Map.delete sid) store

stepOnce :: SessionEntry -> Effect Snapshot
stepOnce entry = do
  sd <- Ref.read entry.dataRef
  Tuple snap sd' <- takeStep sd
  Ref.write sd' entry.dataRef
  pure snap

-- Starts (or resumes) a background loop that keeps calling `takeStep` until
-- the session finishes or `stopSession` bumps the token out from under it.
-- Returns immediately — the loop itself runs as a detached `Aff` fiber, its
-- progress only visible via a later `getSession`/`stepOnce` read of
-- `dataRef`.
startRun :: SessionEntry -> Effect Unit
startRun entry = do
  sd <- Ref.read entry.dataRef
  if sd.finished then pure unit
  else do
    myToken <- Ref.modify (_ + 1) entry.token
    Ref.write (sd { running = true }) entry.dataRef
    launchAff_ (runLoop entry myToken)

-- Same reasoning as `Bench.Main`'s trial loop: `takeStep` runs a
-- synchronous `Effect` (one `WFC.Algorithm.step`/`WFC.Backtrack.stepSearch`
-- call), so without a `delay` yielding back to the event loop between
-- steps, a `stopSession` bumping the token from a concurrent HTTP request
-- would never actually get scheduled until the whole run finished on its
-- own.
runLoop :: SessionEntry -> Int -> Aff Unit
runLoop entry myToken = do
  current <- liftEffect (Ref.read entry.token)
  if current /= myToken then pure unit
  else do
    sd <- liftEffect (Ref.read entry.dataRef)
    if sd.finished then pure unit
    else do
      Tuple _ sd' <- liftEffect (takeStep sd)
      liftEffect (Ref.write sd' entry.dataRef)
      if sd'.finished then pure unit
      else do
        delay (Milliseconds 0.0)
        runLoop entry myToken

-- Cancels any in-flight `startRun` loop and reports whatever the session's
-- own last recorded step already was — the "optionally getting the last
-- step data" half of a manual stop.
stopSession :: SessionEntry -> Effect Snapshot
stopSession entry = do
  _ <- Ref.modify (_ + 1) entry.token
  sd <- Ref.read entry.dataRef
  let sd' = sd { running = false }
  Ref.write sd' entry.dataRef
  pure (fromMaybe (initialSnapshot sd') sd'.lastSnapshot)
