module Demo.Fetch
  ( fetchText
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)

-- Fetch a same-origin static asset's text content (used for the XML
-- tileset files, the same way `Demo.ImageUpload` fetches a PNG's pixels) —
-- always resolves with an `Either`, a failed request is data, not a thrown
-- Aff error.
foreign import fetchTextImpl :: String -> (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit

fetchText :: String -> Aff (Either String String)
fetchText url = makeAff \respond -> do
  fetchTextImpl url
    (\body -> respond (Right (Right body)))
    (\err -> respond (Right (Left err)))
  pure nonCanceler
