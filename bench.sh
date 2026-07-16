#!/usr/bin/env bash
# Builds and runs the WFC CLI benchmark (test/Demo/src/Bench/Main.purs).
# Build needs the nix dev shell (spago/purs); running the compiled ESM
# output only needs a plain `node` on PATH.
#
# Usage: ./bench.sh [--examples=A,B,C] [--runs=N] [--width=W] [--height=H]
#                    [--pattern=N] [--attempts=N] [--dir=path]
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

nix develop --command bash -c "spago build --json-errors 2>&1"
node bench.mjs "$@"
