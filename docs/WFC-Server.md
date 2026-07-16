# WFC HTTP server

`server/` — a standalone spago workspace package exposing the `src/WFC`
solving engine over HTTP, via [HTTPurple](https://github.com/thought2/purescript-httpurple)
(`httpure`'s current name — the version pinned in this workspace's package
set, registry `77.10.0`, doesn't have `httpure` itself).

## Running it

```bash
./server.sh
```

Builds via the nix dev shell (`spago build`), then runs the compiled ESM
output directly with plain `node` (`server.mjs`). Starts two listeners —
the REST/SSE API on `0.0.0.0:8080`, and a WebSocket server on
`0.0.0.0:8081` (see [WebSockets](#websockets---bidirectional-over-a-second-port)
below) — both ports currently hardcoded (`httpPort`/`wsPort`) in
`server/src/Server/Main.purs`'s `main`.

## Source layout

| Module | Responsibility |
|---|---|
| `Server.Engine` | Pure/`Effect` glue between a decoded request and the WFC engine: builds a catalog/rules/wave, and drives it one step at a time. No HTTP or JSON here. |
| `Server.Codec` | JSON decode of the request body (`CreateRequest`) + input validation. Encoding is done inline at call sites via argonaut's generic `Record` `EncodeJson` instance — every response type here is a plain record, so no hand-written encoders were needed. |
| `Server.Session` | The in-memory session store (`Ref (Map String SessionEntry)`) and the cancellable background stepping loop. |
| `Server.Node` | One FFI: `randomId :: Effect String` (`crypto.randomUUID()`), used for session ids. |
| `Server.Main` | Route table (`routing-duplex`) and HTTP handlers, including the SSE stream (`Node.Stream`/`Node.EventEmitter` directly — HTTPurple's `Body (Stream r)` instance pipes any `Readable` straight to the response), plus starting the WebSocket listener alongside `serve`. |
| `Server.Ws` | FFI over the `ws` npm package — no server-side WebSocket library exists in this workspace's package set (only `web-socket`, the browser-client bindings). |
| `Server.WsServer` | The WebSocket protocol: decodes incoming `{"cmd": ...}` messages, dispatches to the same `Server.Session` functions the REST handlers use, replies/pushes `{"type": ...}` messages. |

## Two input modes

Every endpoint that builds a wave (`POST /solve`, `POST /sessions`) takes the
same request body shape, dispatched on a `mode` field:

### `mode: "matrix"` — pattern extraction (the overlapping model)

```json
{
  "mode": "matrix",
  "matrix": [[0,1,0],[1,0,1],[0,1,0]],
  "patternSize": 3,
  "inputPeriodic": false,
  "useRotations": false,
  "useMirror": false,
  "outputWidth": 20,
  "outputHeight": 20,
  "outputPeriodic": false,
  "backtracking": false,
  "maxAttempts": 50,
  "keepHistory": true,
  "maxHistory": 200
}
```

`matrix` is a rectangular grid of small integers (palette indices), exactly
what `WFC.Catalog.extractPatterns` already takes. Rejected with a `400` if
`patternSize` doesn't fit the matrix at all (see [Validation](#validation)
below) — this used to crash the process instead.

### `mode: "tiles"` — the hand-authored tile/socket model

```json
{
  "mode": "tiles",
  "tiles": [
    { "value": 0, "weight": 6.0, "sockets": { "left": "0", "right": "0", "up": "0", "down": "0" } },
    { "value": 1, "weight": 2.0, "sockets": { "left": "1", "right": "1", "up": "0", "down": "0" } },
    { "value": 2, "weight": 2.0, "sockets": { "left": "0", "right": "0", "up": "1", "down": "1" } },
    { "value": 3, "weight": 1.0, "sockets": { "left": "0", "right": "1", "up": "0", "down": "1" } }
  ],
  "outputWidth": 20,
  "outputHeight": 20,
  "outputPeriodic": true,
  "backtracking": false,
  "maxAttempts": 50
}
```

Each entry is exactly `WFC.Tiles.TileDef Int` — no pattern extraction, no
`patternSize`/`inputPeriodic`/`useRotations`/`useMirror` (those are ignored
in this mode); adjacency comes directly from matching socket labels
(`WFC.Tiles.buildTiledCatalog`/`buildTiledRules`), same as
`test/Demo`'s hand-tiled samples.

### Fields common to both modes

| Field | Default | Meaning |
|---|---|---|
| `outputWidth`, `outputHeight` | `20`, `20` | Output grid size. Must be `>= 1`. |
| `outputPeriodic` | `false` | Wrap the output wave's own edges. |
| `backtracking` | `false` | `false`: a single non-retrying pass (`WFC.Algorithm.wfc`/`step`). `true`: incremental backtracking (`WFC.Backtrack`), retrying a bad guess in place instead of restarting. |
| `maxAttempts` | `50` | Only used when `backtracking: true` — total value-attempts across the whole search before giving up. |
| `keepHistory` | `true` | Session-only: whether `/step`/`/run` record every stage into `history`. |
| `maxHistory` | `200` | Session-only: `history` is capped to this many most-recent entries (oldest dropped first). |

### Validation

`Server.Codec.decodeCreateRequest` rejects, with `400` and a JSON
`{"error": "..."}` body, before ever touching the engine:

- `mode` not `"matrix"` or `"tiles"`.
- `matrix` empty.
- `patternSize < 1`.
- `patternSize` bigger than the matrix in either dimension while
  `inputPeriodic: false` (no window that size exists in a non-wrapping
  source — the concrete crash this was added to catch).
- `tiles` empty.
- `outputWidth`/`outputHeight < 1`.

## `POST /solve` — stateless

Builds the wave, solves it once (single pass or full backtracking depending
on `backtracking`), and returns the outcome. No session is created or kept.

Response:

```json
{ "status": "solved", "grid": [[0,1,0], ...] }
```

or, on failure:

```json
{ "status": "contradiction", "grid": null }
```

`grid` is `null` on `"contradiction"` — `wfc`/`solveWithBacktracking` only
ever return the final wave (or nothing), so there's no partial grid to
report here on failure. Use the session API below if you need to see how
far a failed attempt got.

## Sessions — stepped, stoppable, resumable

A session keeps a live `Wave` (plus, in backtracking mode, the live
`SearchState`) server-side, addressable by id, so it can be advanced one
piece at a time from separate requests instead of solved in one blocking
call.

### `POST /sessions` — create

Body: same as `/solve`. Builds the wave (and, if `backtracking: true`, the
initial search frame) but does **not** take any solving step yet.

```json
{ "id": "<uuid>", "status": "ready", "snapshot": { "step": 0, "kind": "ready", "grid": [[-1,-1,...], ...], "solved": 0, "totalCells": 400, "elapsedMs": 0 } }
```

`grid` uses `-1` for every still-uncollapsed cell (`WFC.Render.renderWaveWith
(-1)` — always renders something, unlike `renderWave`, which needs a fully
collapsed wave).

### `GET /sessions/:id` — current status

```json
{
  "id": "<uuid>",
  "status": "ready",
  "stepIdx": 0,
  "solved": 0,
  "running": false,
  "finished": false,
  "lastSnapshot": { "step": 0, "kind": "ready", "grid": [...], "solved": 0, "totalCells": 400, "elapsedMs": 0 }
}
```

`404` if the id doesn't exist (never created, or already `DELETE`d).

### `POST /sessions/:id/step` — advance one unit of work

One `WFC.Algorithm.step` call (`backtracking: false`), or one
`WFC.Backtrack.stepSearch` call (`backtracking: true`) — whichever the
session was created with. Returns the resulting snapshot directly:

```json
{ "step": 1, "kind": "progress", "grid": [...], "solved": 3, "totalCells": 400, "elapsedMs": 2.4 }
```

Once a session is `finished`, further `/step` calls are a no-op that just
replay the last snapshot — a caller never has to check `finished` before
calling this.

### `POST /sessions/:id/run` — solve to completion, in the background

Starts (or resumes) a loop that keeps calling the same step logic as
`/step`, yielding back to the event loop between each one (`delay
(Milliseconds 0.0)`), until the session finishes or a later `/stop` cancels
it. Returns immediately:

```
202 Accepted
{ "id": "<uuid>", "status": "running" }
```

Poll `GET /sessions/:id` for progress. Calling `/run` again on an
already-`finished` session is a no-op.

### `POST /sessions/:id/stop` — cancel a run, get the last step

```json
{ "id": "<uuid>", "status": "ready", "lastStep": { "step": 5, "kind": "progress", "grid": [...], "solved": 12, "totalCells": 400, "elapsedMs": 8.1 } }
```

Bumps the session's internal generation token, which the running loop
checks between steps — the loop notices on its next iteration and simply
stops, rather than being forcibly killed. If the run had already finished
by the time `/stop` is called (a race is possible — nothing prevents it),
`status` reflects that instead (`"solved"`/`"contradiction"`/`"timedOut"`)
and `lastStep` is whatever the final step actually was.

### `GET /sessions/:id/history` — every stage recorded so far

```json
{ "id": "<uuid>", "history": [ { "step": 0, "kind": "ready", ... }, { "step": 1, "kind": "progress", ... }, ... ] }
```

Populated by both `/step` and `/run`, capped at the session's own
`maxHistory` (oldest entries dropped first), or not recorded at all if the
session was created with `keepHistory: false`.

### `GET /sessions/:id/events` — live progress via Server-Sent Events

An alternative to polling `GET /sessions/:id`: opens a long-lived
`text/event-stream` connection and pushes a `data: <snapshot JSON>\n\n`
event for the session's current state immediately, then one more for every
subsequent snapshot `/step` or `/run` records — until the session reaches a
terminal `kind` (`solved`/`contradiction`/`timedOut`), at which point the
server ends the stream itself, or until the client disconnects.

This route is **observe-only** — connecting to it does not itself start or
advance anything. Pair it with a `POST /sessions/:id/run` (either just
before or any time after opening the stream) to actually see it solve;
multiple `/events` connections can watch the same session at once.

```js
const es = new EventSource(`/sessions/${id}/events`);
es.onmessage = (ev) => {
  const snap = JSON.parse(ev.data);
  render(snap.grid);
  if (["solved", "contradiction", "timedOut"].includes(snap.kind)) es.close();
};
fetch(`/sessions/${id}/run`, { method: "POST" });
```

A `: keep-alive\n\n` comment line is written every 15s while otherwise idle,
so a proxy/load balancer sitting in front of this doesn't time out a quiet
connection mid-solve. On client disconnect, the stream's own `close`/`error`
events unregister the session's subscriber and stop writing — confirmed by
killing an open connection mid-run and checking the server keeps running
the solve to completion without erroring.

Why SSE and not long-polling: a plain blocking `GET` (true long-polling)
has the same "how long can a client/proxy actually wait on one HTTP
response" problem this was added to sidestep in the first place. SSE is
plain HTTP, no extra protocol handshake, and `EventSource` retries a
dropped connection natively — the right default when a client only needs
to *receive* progress and can still issue commands as ordinary POSTs (as
every example above does). See [WebSockets](#websockets---bidirectional-over-a-second-port)
below for the case where a client wants both directions over one
connection instead.

## WebSockets — bidirectional, over a second port

`ws://<host>:8081` (`wsPort` in `Server.Main`, separate from the REST/SSE
server on `8080` — HTTPurple owns that server's `upgrade` handling
internally and doesn't expose it, so this runs its own listener via the
`ws` npm package instead). Every connected client sends and receives plain
JSON messages on the same socket; there's no routing-duplex path structure
here, just a `"cmd"`/`"type"`-tagged protocol handled by
`Server.WsServer`.

Session logic itself is completely unchanged from the REST API — every
command below just calls the same `Server.Session` functions the HTTP
handlers do (`Session.createSession`, `Session.stepOnce`,
`Session.startRun`, ...). Concretely, that means a step taken over one
WebSocket connection is visible on any other subscriber — another
WebSocket connection, or an SSE `/events` stream — via the session's
existing `subscribe`/`notifySubscribers` mechanism, and vice versa: they
all share the one in-memory `Store`.

### Client → server commands

| `cmd` | Fields | Effect |
|---|---|---|
| `create` | same body as `POST /sessions` (flattened alongside `cmd`) | Creates a session, replies `created`. |
| `subscribe` | `id` | Immediately replies with the current snapshot (`snapshot`), then pushes one more for every future step until `unsubscribe`d or the connection closes. |
| `unsubscribe` | `id` | Stops future pushes for that session; the connection itself stays open. |
| `step` | `id` | One unit of work, same as `POST /sessions/:id/step`. |
| `run` | `id` | Starts/resumes the background loop, same as `POST /sessions/:id/run`; replies `status`. |
| `stop` | `id` | Cancels an in-flight run, same as `POST /sessions/:id/stop`; replies `status` with `lastStep`. |
| `get` | `id` | Current status, same shape as `GET /sessions/:id`. |
| `delete` | `id` | Same as `DELETE /sessions/:id`; replies `deleted`. |

### Server → client messages

| `type` | Meaning |
|---|---|
| `created` | Reply to `create`: `{ type, id, status, snapshot }`. |
| `snapshot` | A step, pushed to every connection `subscribe`d to that `id`: `{ type, id, snapshot }`. |
| `status` | Reply to `run`/`stop`/`get`: `{ type, id, status, ... }` (`stop`/`get` also include `lastStep`/`lastSnapshot` respectively, matching their REST counterparts). |
| `deleted` | Reply to `delete`: `{ type, id }`. |
| `error` | Malformed JSON, an unknown `cmd`, or `id` not found: `{ type, message, id? }`. |

A `step`/`run` command's own reply is skipped — not sent twice — for a
session the same connection is already `subscribe`d to, since
`stepOnce`/the run loop already notify every subscriber (this connection
included) independently of the command reply; without that check, a
subscribed connection driving its own session would see each snapshot
twice on the one socket. (A REST `/step` response and an SSE viewer don't
have this problem — they're two separate connections.)

```js
const ws = new WebSocket("ws://localhost:8081");
ws.onopen = () => ws.send(JSON.stringify({
  cmd: "create", mode: "matrix", matrix: [[0,1,0],[1,0,1],[0,1,0]],
  patternSize: 2, inputPeriodic: true, outputPeriodic: true,
  outputWidth: 30, outputHeight: 30, backtracking: true, maxAttempts: 50
}));
ws.onmessage = (ev) => {
  const msg = JSON.parse(ev.data);
  if (msg.type === "created") {
    ws.send(JSON.stringify({ cmd: "subscribe", id: msg.id }));
    ws.send(JSON.stringify({ cmd: "run", id: msg.id }));
  }
  if (msg.type === "snapshot") render(msg.snapshot.grid);
};
```

Verified: two independent connections, one issuing `create`/`run`, the
other only `subscribe`d, the second receiving every step without ever
sending a command of its own; malformed JSON and unknown-`id`/unknown-`cmd`
messages each get a clean `error` reply without affecting the connection or
the server; abruptly terminating a connection mid-`run` (`ws.terminate()`,
no close handshake) leaves the run itself completing normally in the
background with no error surfaced anywhere.

### `DELETE /sessions/:id`

```json
{ "id": "<uuid>", "status": "deleted" }
```

Frees the session from the in-memory store. Sessions are **never** cleaned
up automatically — there is no TTL or idle-eviction — so a long-running
server accumulating sessions without ever deleting them will leak memory.
Worth revisiting before this is anything but a local/dev tool.

## Snapshot `kind` values

| `kind` | Meaning |
|---|---|
| `ready` | Just created, no step taken yet. |
| `progress` | Made forward progress (a cell collapsed, or — in backtracking mode — moved to a new decision frame). |
| `backedOut` | Backtracking mode only: exhausted every value at a cell and unwound to the parent's decision. |
| `contradiction` | Non-backtracking mode: `step` hit an impossible cell. Terminal — `finished: true`. |
| `timedOut` | Backtracking mode: `maxAttempts` exhausted before a solution was found. Terminal. |
| `solved` | Fully collapsed. Terminal. |

## Concurrency model

Everything is single-process, in-memory, no persistence. `WFC.Algorithm.step`
and `WFC.Backtrack.stepSearch` are synchronous `Effect` calls — a single
`/step` or one iteration of a `/run` loop fully occupies the Node event loop
for its duration. `/run`'s loop inserts a zero-length `Aff.delay` between
iterations specifically so that a concurrent `/stop` request (or any other
pending request) gets a chance to actually run — without it, `/stop` would
only ever be serviced after the whole run finished on its own, since Node
can't preempt a synchronous JS call stack (confirmed the same way for the
CLI benchmark's own Ctrl+C handling, see `test/Demo/src/Bench/Main.purs`).

This means a single very expensive `/step` (a huge grid, or a
backtracking step that has to unwind a long stack) still blocks the whole
server for that duration — `/run`'s cancellability is between steps, not
within one. `/events` doesn't change this: it's a passive subscriber woken
up by the same `notifySubscribers` call `/step`/`/run` already make after
each step, not a separate polling loop of its own.

## Known gaps

- No auth, no rate limiting, no request size limit on the JSON body — not
  intended to be exposed beyond localhost/a trusted network as-is.
- No session expiry (see `DELETE` above).
- `POST /solve`'s `grid: null` on contradiction loses whatever partial
  progress was made; only the stepped session API preserves that.
- Value type is fixed to `Int` for both modes (`PatternCatalog Int`) — no
  way to solve over an arbitrary domain type, matching how the CLI
  benchmark and `Demo.Samples` work too.
- `/events` subscribers live only in `SessionEntry.subscribers`, in memory
  — no reconnect/replay support beyond the one current-state push a fresh
  connection gets; a client that misses events during a network blip has
  no way to ask "what did I miss," only "what's true right now."
- The WebSocket server is a second, independent listener (`wsPort`) rather
  than sharing `httpPort` — a client (or a proxy in front of this) has to
  know both ports. No ping/pong heartbeat, either (unlike `/events`' SSE
  keep-alive comment) — a half-dead connection is only noticed on the next
  write attempt, via the `ws` library's own `readyState` check in
  `Server.Ws`'s `send`.
- Neither WebSocket connections nor SSE streams carry any auth/identity —
  same "not intended past localhost/a trusted network" caveat as the REST
  API, just now on two more ports.
