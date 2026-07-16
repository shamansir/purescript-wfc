module Server.Node (randomId) where

import Effect (Effect)

-- A session id — `crypto.randomUUID()` needs no extra npm dependency (it's
-- been a Node global since 14.17), unlike pulling in a UUID package just
-- for this one call.
foreign import randomId :: Effect String
