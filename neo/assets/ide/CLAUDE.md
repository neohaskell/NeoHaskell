# neo IDE frontend — agent guide

The Vite/React frontend for `neo ide`. Built into `dist/` and embedded into the
Rust binary via `rust-embed`. See the repo-root `CLAUDE.md` for build/test
commands (everything runs inside `nix develop --command`).

## Styling is centralized — this is a HARD RULE

All visual styling MUST flow from the centralized Mantine theme in
[`src/theme.ts`](src/theme.ts). There is exactly one source of truth for color,
spacing, radius, typography, and per-component defaults.

**Never** style "in place":
- no Tailwind (it was removed on purpose — do not reintroduce it),
- no inline `style={{ ... }}` carrying colors, borders, backgrounds, shadows, or
  any reusable visual value,
- no per-call Mantine style props duplicating a value that belongs in the theme.

**If a component needs a specific interaction or one-off visual**, create a
**self-contained, reusable component** (with a co-located `*.module.css` when it
needs real CSS) under `src/ui/primitives/` (or the relevant feature folder) and
reuse it — do not inline the styling. Examples already in the tree:
`src/ui/primitives/NodeShell.tsx` (+ `.module.css`), `Dot.tsx`,
`src/ui/nodes/*.module.css`, `src/ui/shell/*.module.css`.

**The only inline styles allowed** are values that genuinely cannot live in a
theme because they are computed at runtime:
- positions/dimensions React Flow assigns or measures (node `x/y`, widths),
- coordinate anchors (e.g. a fixed-position menu target at the cursor),
- dynamic flex/size layout values (`flex: 1`, `minWidth`, `whiteSpace`).

Colors in CSS modules MUST reference theme tokens — Mantine vars
(`var(--mantine-color-*)`) or the event-modeling tokens emitted by
`cssVariablesResolver` in `src/theme.ts` (`var(--em-event)`, `--em-command`,
`--em-query`, `--em-integration`, `--em-ui`, `--em-feature`, `--em-edge`,
`--em-edge-portal`, `--em-selection`, `--em-trace`, `--em-trace-portal`, `--em-canvas-bg`,
`--em-grid-dot`). No raw hex outside `theme.ts`.

Tests render through `src/test/render.tsx` (wraps `MantineProvider` + theme +
Notifications + ModalsProvider). New `.tsx` component tests MUST use it, not bare
`@testing-library/react`.

## Architecture seams (touch one file, not the framework)

- **Theme**: `src/theme.ts` — all tokens + component defaults + the
  `cssVariablesResolver`.
- **Shell / lenses**: `src/App.tsx` holds a `lens` state; `src/ui/shell/`
  (HeaderBar, ActivityRail) is the only persistent chrome; `src/ui/lenses/`
  routes Model (built) vs Schema/Logs/Emulate (`EmptyLens` placeholders). Adding
  a real future lens = swap its placeholder, don't add new top-level chrome.
- **Node creation is gesture-driven** (no toolbar): right-click / double-click
  the pane, drag a wire into empty space, or ⌘K. Logic in
  `src/ui/canvas/nodeCreation.ts` + `successorsFor` in `connectionRules.ts`
  (derived from the same `VALID_CONNECTIONS` table the edges use — keep them in
  lockstep).
- **Semantic zoom**: `NodeShell` reveals a node's `fields` (the optional schema)
  above `SEMANTIC_ZOOM_THRESHOLD` (`src/ui/canvas/semanticZoom.ts`); the editor
  is `src/ui/schema/FieldsEditor.tsx`, reused by the future Schema lens. The
  on-disk `fields` shape lives in `src/model/event-model.schema.json` AND the
  Rust validator — keep it **optional/additive** so old files still validate.
