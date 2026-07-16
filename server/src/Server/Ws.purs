module Server.Ws
  ( WsServer
  , WsConnection
  , createWsServer
  , onConnection
  , onMessage
  , onClose
  , send
  ) where

import Prelude

import Effect (Effect)

-- Thin FFI over the `ws` npm package — no server-side WebSocket library
-- exists in this workspace's pinned package set (only `web-socket`, the
-- browser *client* API bindings Demo already uses), same situation as
-- `Bench.PngDecode` reaching for `pngjs`.
foreign import data WsServer :: Type
foreign import data WsConnection :: Type

foreign import createWsServer :: Int -> Effect WsServer

foreign import onConnection :: WsServer -> (WsConnection -> Effect Unit) -> Effect Unit

foreign import onMessage :: WsConnection -> (String -> Effect Unit) -> Effect Unit

foreign import onClose :: WsConnection -> Effect Unit -> Effect Unit

-- No-ops if the connection isn't open (closing/closed) rather than
-- throwing — the WS analogue of `Server.Main`'s SSE `closedRef` guard.
foreign import send :: WsConnection -> String -> Effect Unit
