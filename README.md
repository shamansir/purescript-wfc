# purescript-wfc

A [Wave Function Collapse](https://github.com/mxgmn/WaveFunctionCollapse)
implementation for PureScript — not a port, but a complete rethink of the
algorithm in pure functional style: immutable waves, explicit propagation
state, and no hidden mutation anywhere in the solving core.

**This entire codebase — algorithm, tests, demo, and docs — is 100%
Claude-generated**, written end-to-end by Claude Code.

## What's here

- **Overlapping model** — extract N×N patterns from a sample image/grid,
  with optional rotation/mirroring.
- **Tiled model** — hand-authored tiles with explicit socket adjacency.
- **Original-WFC tileset model** — parses the `mxgmn/WaveFunctionCollapse`
  XML tileset format directly (symmetry classes, `<neighbor>` rules,
  subsets), verified against several of the original tilesets.
- **Incremental backtracking** — undo just the last guess on contradiction,
  instead of a full restart, as an alternative to plain retry.
- A Halogen demo app (`test/Demo`) with pattern/rule inspection, step
  history, and canvas rendering, running the solver in a Web Worker.

See [`docs/WFC.md`](docs/WFC.md) for a language-independent explanation of
the algorithm itself, and [`API.md`](API.md) for the module surface.

## Getting started

All tooling runs inside the Nix dev shell.

```bash
nix develop --command bash -c "spago build --json-errors"
nix develop --command bash -c "spago test"
```

To run the demo, bundle it and serve `test/Demo` as static files:

```bash
nix develop --command bash -c "spago bundle --package demo --module Demo.Main --outfile test/Demo/index.js --platform browser"
nix develop --command bash -c "spago bundle --package demo --module Demo.Worker --outfile test/Demo/worker.js --platform browser"
python3 -m http.server 8765 --directory test/Demo
```

## License

[Unlicense](LICENSE) — public domain.
