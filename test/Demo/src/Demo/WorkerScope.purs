-- The installed `web-workers` registry version (1.1.0) doesn't yet ship
-- `Web.Worker.DedicatedWorkerGlobalScope` (added upstream after that release),
-- so this is a minimal FFI shim for the two calls a dedicated worker needs:
-- posting to, and listening from, its parent via the global `self`.
module Demo.WorkerScope
  ( postMessage
  , onMessage
  ) where

import Prelude

import Effect (Effect)
import Web.Worker.MessageEvent (MessageEvent)

foreign import postMessage :: forall msg. msg -> Effect Unit

foreign import onMessage :: (MessageEvent -> Effect Unit) -> Effect Unit
