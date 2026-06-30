# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

All PureScript tooling runs inside the Nix dev shell. Bare `spago`/`purs` calls will fail.

```bash
# Enter dev shell
nix develop

# Build
nix develop --command bash -c "spago build --json-errors 2>&1"

# Run tests
nix develop --command bash -c "spago test 2>&1"

# Bundle for web
nix develop --command bash -c "spago bundle --module Main 2>&1"

# Build as Nix derivation
nix build

# Run demo app
nix run
```

## Project Overview

PureScript implementation of the **Wave Function Collapse** (WFC) algorithm. WFC is a constraint-propagation algorithm for procedural generation — given a set of tiles and adjacency rules, it collapses a grid of superposed states into a consistent assignment.

Output targets a web frontend (`./web/` dir) served via `serve-demo-games-nix.sh`. Bundler chain: `purs` → `purs-backend-es` → `esbuild`.

## Toolchain

| Tool | Purpose |
|------|---------|
| `purs-unstable` | PureScript compiler |
| `spago-unstable` | Build tool / package manager (spago.yaml config) |
| `purs-backend-es` | ES module backend optimizer |
| `esbuild` | Final JS bundler |
| `dhall` | Config language (dhall-lsp-server in devShell) |

Config files to create when setting up: `spago.yaml`, `spago.lock`.

## Architecture Notes

This repo is in early setup — source files not yet present. When implemented, expect:

- **WFC core** — entropy, observation, propagation, backtracking logic in pure PureScript
- **Tile/rule definitions** — adjacency constraints, likely represented as adjacency maps or weighted rules
- **Web rendering** — canvas or SVG output in `./web/`, driven from compiled JS
- **Demo games** — multiple example tilesets served from `serve-demo-games-nix.sh`
