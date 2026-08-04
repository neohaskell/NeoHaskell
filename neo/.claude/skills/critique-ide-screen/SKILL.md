---
name: critique-ide-screen
description: Use when asked to visually critique, screenshot, or review the UX/look of the neo IDE (the Vite/React event-modeling frontend at `assets/ide/`) in the NeoCLI repo at `/Users/nick/repos/neo` — phrases like "critique the IDE screen", "screenshot the IDE", "how does the main canvas look", "review the IDE UX", "/critique-ux on the neo ide". Captures the REAL rendered app (a seeded, populated event model — never an empty canvas) via the Playwright+Brave harness, then judges it. A visual critique without a fresh screenshot is invalid — reasoning from source code alone misses collisions, framing, contrast, and density problems that only appear on screen. Skips when the change is purely backend/Rust with no frontend surface, or when the user supplies their own screenshot to critique.
kind: leaf
executor: script
---

# Critiquing the neo IDE screen

The neo IDE is an event-modeling tool (Adam Dymitruk / Martin Dilger style) whose
UX bar is **Figma-level ease of use**. To critique it you must look at the *actual
rendered screen* on a populated model — empty-canvas screenshots and code-only
reasoning both miss the real problems (label collisions, poor fit-to-view framing,
faint swim-lane labels, edge-routing noise, contrast).

## 1. Capture the screenshot

The harness is `assets/ide/e2e/critique-shot.e2e.ts`, driven by Playwright against
`vite preview` (config: `assets/ide/playwright.config.ts`). It seeds a realistic
two-feature checkout model into `localStorage` (key `neoide:model`) so the canvas
renders real nodes, swim-lanes, chapter arrows, and edges.

Everything runs inside the flake dev shell. From `assets/ide/`:

```sh
# 1. Build the frontend (the preview server serves dist/).
nix develop --command npm run build

# 2. Run the screenshot test → writes e2e-out/critique-main.png
nix develop --command npx playwright test critique-shot --reporter=list
```

Then **Read `assets/ide/e2e-out/critique-main.png`** — actually view the image.

### Knobs (env vars)

- `NEO_SHOT_MODEL=/path/to/model.json` — critique a specific model instead of the
  built-in demo (e.g. a real project's `event-model.json`).
- `NEO_SHOT_OUT=e2e-out/foo.png` — output path.
- `NEO_SHOT_FULL=1` — full-page capture instead of the 1400×900 viewport.

### Gotchas

- **`nix develop --command`** is mandatory — node/npm are flake-pinned, not on the
  host PATH. A bare `npx` fails.
- The status bar shows **`disconnected`** under `vite preview` by design — there is
  no Rust JSON-RPC server at that origin. The model comes from `localStorage`, so
  the canvas still renders fully. Do not chase the disconnect; treat the cryptic
  disconnected message itself as a legitimate critique finding.
- **Brave** is the intended browser (`BRAVE_PATH`); when absent Playwright falls
  back to bundled Chromium automatically — fine for screenshots.
- `npm install` runs only on first use (when `node_modules/` is absent).

## 2. Run the critique

Apply the three functional dimensions (this is what `/visual-critique:critique-ux`
covers) against the screenshot, through the event-modeling + Figma lens:

1. **Visual hierarchy** — entry point, eye flow, what reads as primary. Watch for:
   stacked toolbars with no weighted primary action; faint/colliding entity
   swim-lane and chapter-arrow labels (the structural scaffolding must be the most
   legible thing, not the least).
2. **Affordance** — what looks clickable/draggable/connectable, state visibility,
   action discoverability. The two load-bearing canvas gestures — *placing a node*
   and *connecting two nodes* (React Flow handles are hover-only) — are the usual
   discoverability failures. Cryptic status/error text violates this repo's
   "errors must instruct a tiny LLM" invariant — flag it here.
3. **Information density** — cognitive load, the 8-term vocabulary (Event/Command/
   Query/Integration/UI/Slice/Chapter/Feature/Entity), legend/colour-key presence,
   default canvas framing (fit-to-view often top-clusters nodes and wastes space).

## 3. Output

A prioritised fix list. Each item: **Issue / Dimension / Fix** (point the Fix at a
real component — `Toolbar.tsx`, `FileMenu.tsx`, `StatusBar.tsx`, `FeatureNavigator.tsx`,
`ui/nodes/*`, `ui/Canvas.tsx`). Rank:

- **P1** — blocks task completion or hides a required action.
- **P2** — slows the user or creates confusion.
- **P3** — polish.

Lead with what already works (don't regress it), end with one sentence naming the
single biggest functional risk on the screen.
```

## Related

- The colour↔node mapping lives in `assets/ide/src/ui/Toolbar.tsx` and the node
  components under `assets/ide/src/ui/nodes/` — cross-check the screenshot's colours
  against these when judging the legend.
- Error-message findings: see the `error-messages-instruct-llms` skill for the
  invariant any status/error text on screen must meet.
