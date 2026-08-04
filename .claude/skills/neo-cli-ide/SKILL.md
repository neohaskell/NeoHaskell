---
name: neo-cli-ide
description: Work on the Neo CLI's bundled browser IDE under `neo/**` — the Vite/React frontend at `neo/assets/ide/`, its embedded `dist/` synchronization into the Rust binary, and visual critique of the rendered event-modeling canvas. Use when touching `assets/ide/**`, the `neo ide` Rust backend (`src/ide/**`), or when asked to screenshot/critique the IDE screen. Frontend styling flows only from the central Mantine theme; a visual critique without a fresh screenshot is invalid.
---

# Neo CLI IDE

`neo ide` serves a bundled in-browser event-modeling IDE. Two halves:

- **Frontend** — `neo/assets/ide/` (Vite + React + Mantine). Built to
  `assets/ide/dist/`.
- **Rust backend** — `src/ide/**` (`server.rs`, `rpc.rs`, `registry.rs`,
  `methods/*.rs`, `heal/*`, `sync.rs`, `validate.rs`). Embeds `dist/` via
  `rust-embed` and speaks JSON-RPC over WebSocket.

All toolchain commands run inside `nix develop --command` (node/npm/pnpm are
flake-pinned).

## Build + embedded `dist/` synchronization

The Rust binary embeds `assets/ide/dist/` at compile time via `rust-embed`. So the
frontend must be **built before** a release binary that should ship the new UI.

```sh
# Both halves in one shot (frontend then cargo):
nix develop --command ./scripts/build.sh            # debug
nix develop --command ./scripts/build.sh --release  # release
```

`scripts/build.sh` runs `npm install` only on first use (when
`assets/ide/node_modules/` is absent), then `npm run build` (`tsc -b && vite
build`), then `cargo build`.

Iteration shortcuts:
- **Frontend-only, debug:** `cd assets/ide && nix develop --command npm run build`.
  The debug binary's `rust-embed` reads `dist/` from disk per request, so no cargo
  rebuild is needed to see frontend changes.
- **Rust-only:** `nix develop --command cargo build` reuses the already-built
  `dist/`.
- **Release:** always rebuild the frontend first — a release binary embeds `dist/`
  at compile time; a stale `dist/` ships a stale UI. `./scripts/build.sh --release`
  does both in order and is the safe default.

Adding an IDE JSON-RPC method should be a **one-file change** under
`src/ide/methods/`; if you find yourself editing `src/ide/rpc.rs` or
`registry.rs`, the change is wrong-shaped — reconsider before proceeding.

## Frontend styling — one source of truth (HARD rule from `assets/ide/CLAUDE.md`)

All visual styling flows from the central Mantine theme in
`assets/ide/src/theme.ts`. **Never** style in place: no Tailwind (removed on
purpose), no inline `style={{…}}` carrying colors/borders/backgrounds/shadows, no
per-call Mantine style props duplicating a theme value. A one-off visual becomes a
self-contained reusable component under `src/ui/primitives/` (with a co-located
`*.module.css` when it needs real CSS). The only allowed inline styles are
runtime-computed values (React Flow node `x/y`, cursor anchors, dynamic
`flex`/`minWidth`). Follow `assets/ide/CLAUDE.md` for the full rule.

## Visual critique — screenshot first, always

The IDE's UX bar is Figma-level ease of use. A critique reasoned from source alone
is invalid: label collisions, fit-to-view framing, faint swim-lane labels,
edge-routing noise, and contrast only appear on screen. Capture a **populated**
model (never an empty canvas).

From `assets/ide/`:

```sh
nix develop --command npm run build                                  # preview serves dist/
nix develop --command npx playwright test critique-shot --reporter=list
```

Then **read the image** at `assets/ide/e2e-out/critique-main.png` — actually view
it. The harness (`assets/ide/e2e/critique-shot.e2e.ts`, driven by
`playwright.config.ts` against `vite preview`) seeds a realistic two-feature
checkout model into `localStorage` (`neoide:model`).

Env knobs: `NEO_SHOT_MODEL=/path/to/model.json` (critique a real model),
`NEO_SHOT_OUT=e2e-out/foo.png`, `NEO_SHOT_FULL=1` (full-page).

Gotchas:
- `nix develop --command` is mandatory; a bare `npx` fails.
- The status bar shows `disconnected` under `vite preview` **by design** — there is
  no Rust JSON-RPC server at that origin; the model comes from `localStorage` so
  the canvas still renders. Don't chase the disconnect — but treat the cryptic
  `disconnected` text itself as a legitimate finding (it violates the
  errors-instruct-a-tiny-LLM invariant; see `neo-cli-implementer`).
- Brave is the intended browser (`BRAVE_PATH`); Playwright falls back to bundled
  Chromium when absent — fine for screenshots.

Critique through three functional dimensions against the event-modeling + Figma
lens: **visual hierarchy** (entry point, eye flow, legibility of swim-lane /
chapter scaffolding), **affordance** (is placing a node and connecting two nodes
discoverable? React Flow handles are hover-only — the usual failure), and
**information density** (the 8-term vocabulary — Event/Command/Query/Integration/
UI/Slice/Chapter/Feature/Entity — legend presence, default framing). Output a
prioritized fix list (P1 blocks a task / P2 slows or confuses / P3 polish), each
pointing at a real component (`Toolbar.tsx`, `FileMenu.tsx`, `StatusBar.tsx`,
`ui/Canvas.tsx`, `ui/nodes/*`). Lead with what works; end with the single biggest
functional risk on screen. The colour↔node mapping lives in
`assets/ide/src/ui/Toolbar.tsx` and `assets/ide/src/ui/nodes/`.
