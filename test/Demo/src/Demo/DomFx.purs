-- Small imperative DOM helpers that don't have a PureScript binding in this
-- project's dependency set: scrolling a specific element into view (no
-- `Element.scrollIntoView` in `web-dom`), and a click event's position
-- relative to its target (`Web.UIEvent.MouseEvent` only exposes
-- viewport-relative `clientX`/`clientY`, not `offsetX`/`offsetY`).
module Demo.DomFx
  ( scrollIntoView
  , mouseOffset
  ) where

import Prelude

import Effect (Effect)
import Web.UIEvent.MouseEvent (MouseEvent)

foreign import scrollIntoViewImpl :: String -> Effect Unit

-- Scrolls the element with this id into its scrollable ancestors' view
-- (smoothly, nearest edge) if it currently exists; a no-op otherwise (e.g.
-- a stale id from a step that's since been cleared by Reset/Extract).
scrollIntoView :: String -> Effect Unit
scrollIntoView = scrollIntoViewImpl

foreign import mouseOffsetImpl :: MouseEvent -> Effect { x :: Number, y :: Number }

-- The click position in the target element's own (CSS pixel) coordinate
-- space — i.e. `event.offsetX`/`offsetY`, unaffected by page scroll and
-- not needing a separate `getBoundingClientRect` call.
mouseOffset :: MouseEvent -> Effect { x :: Number, y :: Number }
mouseOffset = mouseOffsetImpl
