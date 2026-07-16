#!/usr/bin/env bash
# Builds and runs the WFC HTTP server (server/src/Server/Main.purs).
# Build needs the nix dev shell (spago/purs); running the compiled ESM
# output only needs a plain `node` on PATH. Listens on :8080.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

nix develop --command bash -c "spago build --json-errors 2>&1"
node server.mjs
