module Bench.Node (argv, onSigint, exitProcess) where

import Prelude

import Effect (Effect)

-- The CLI's own arguments (after the script path) — how `Bench.Main` is
-- configured (which examples, run count, output size, ...) without a
-- PureScript-side CLI-parsing dependency.
foreign import argv :: Effect (Array String)

-- Node's default Ctrl+C behavior exits immediately with no output; a
-- benchmark mid-run should instead print whatever results it already has.
foreign import onSigint :: Effect Unit -> Effect Unit

foreign import exitProcess :: Int -> Effect Unit
