# NeoIDE v1 — TDD Implementation Plan

## Approach

Every phase follows the same cycle: **Types → Tests → Implementation → Refactor**. We write the TypeScript types and interfaces first, then tests that exercise those types, then the minimal implementation to make tests pass.

Testing strategy:
- **Domain logic / model:** Vitest unit tests
- **React components / interactions:** Vitest + React Testing Library
- **Integration (canvas interactions, wiring, save/load):** Vitest + RTL with ReactFlow test utilities

---

## Phase 0: Project Scaffolding

Set up Vite + React + TS + Tailwind + ReactFlow + Vitest + RTL.

**Steps:**
1. `npm create vite@latest . -- --template react-ts`
2. Install deps: `reactflow`, `tailwindcss`, `@testing-library/react`, `@testing-library/user-event`, `jsdom`
3. Configure Vitest with jsdom environment and RTL
4. Configure Tailwind
5. Verify with a smoke test that renders `<App />` and passes

**Deliverable:** Green smoke test, app runs in browser.

---

## Phase 1: Domain Types & Model Logic

The core data model — no UI yet.

### 1a. Types (write first)

Define all domain types in `src/model/types.ts`:

```ts
// Node types on the canvas
type EventNode       // name, entityId?, color: orange
type CommandNode     // name, color: blue
type QueryNode       // name, color: green
type IntegrationNode // name, kind: 'inbound' | 'outbound', color: gray + cogwheel
type UIPlaceholder   // label, represents a future view

// Structural types
type Entity          // id, name, swim lane position
type Slice           // id, name, chapter id
type Chapter         // id, name

// Edge types
type CommandProducesEvent   // commandId → eventId
type EventFeedsQuery        // eventId → queryId
type IntegrationTriggersCommand // integrationId → commandId
type CommandFromUI          // uiPlaceholderId → commandId
type QueryToUI              // queryId → uiPlaceholderId

// The full model
type EventModel {
  id, name,
  chapters: Chapter[],
  entities: Entity[],
  nodes: (EventNode | CommandNode | QueryNode | IntegrationNode | UIPlaceholder)[],
  edges: (CommandProducesEvent | EventFeedsQuery | ...)[],
  slices: Slice[],
  layout: { nodePositions, viewport }
}
```

### 1b. Model operation functions + tests (test first)

File: `src/model/operations.ts` + `src/model/operations.test.ts`

Write tests first for pure functions:
- `createEventModel(name)` → fresh model
- `addEvent(model, event)` → model with event
- `addCommand(model, command)` → model with command
- `addQuery(model, query)` → model with query
- `addIntegration(model, integration)` → model with integration
- `addUIPlaceholder(model, placeholder)` → model with placeholder
- `removeNode(model, nodeId)` → model without node + associated edges removed
- `addEdge(model, edge)` → model with edge (validate: command→event only, etc.)
- `removeEdge(model, edgeId)` → model without edge
- `addEntity(model, entity)` → model with entity
- `removeEntity(model, entityId)` → model without entity + orphan events unassigned
- `assignEventToEntity(model, eventId, entityId)` → event now in entity
- `addChapter(model, chapter)` → model with chapter
- `removeChapter(model, chapterId)` → model without chapter + slices unassigned
- `addSlice(model, slice)` → model with slice
- `removeSlice(model, sliceId)` → model without slice
- `reorderEventsInEntity(model, entityId, eventIds)` → chronological reorder

Edge validation rules (tested):
- Command → Event: both must exist, event must belong to same entity as command targets
- Event → Query: both must exist
- Integration → Command: both must exist
- UI → Command / Query → UI: both must exist

### 1c. Serialization + tests (test first)

File: `src/model/serialization.ts` + `src/model/serialization.test.ts`

- `serialize(model)` → JSON string
- `deserialize(json)` → EventModel (with validation)
- `validateModel(model)` → ValidationError[]

Tests:
- Round-trip: serialize then deserialize = identity
- Rejects malformed JSON
- Rejects invalid references (edge to nonexistent node)
- Handles empty model
- Handles model with all node types

---

## Phase 2: Canvas Foundation

### 2a. ReactFlow adapter types + mapping (types & tests first)

File: `src/ui/adapter.ts` + `src/ui/adapter.test.ts`

Convert domain model ↔ ReactFlow nodes/edges:
- `toReactFlowNodes(model)` → ReactFlow Node[]
- `toReactFlowEdges(model)` → ReactFlow Edge[]
- `applyPositionChanges(model, nodeChanges)` → updated model layout

Tests:
- Each node type maps to correct ReactFlow node type with correct color/style
- Edges map with correct source/target
- Position changes update layout without touching semantic data

### 2b. Custom node components + tests

Files in `src/ui/nodes/`:
- `EventNode.tsx` — orange box with name
- `CommandNode.tsx` — blue box with name
- `QueryNode.tsx` — green box with name
- `IntegrationNode.tsx` — gray box with cogwheel icon + name
- `UIPlaceholderNode.tsx` — dashed border placeholder

Tests (RTL): each renders correct label, correct color/style, correct handles for connections.

### 2c. Canvas component + tests

File: `src/ui/Canvas.tsx` + `src/ui/Canvas.test.tsx`

- Renders ReactFlow with custom node types registered
- Takes model as prop, renders all nodes and edges
- Fires callbacks: `onNodeAdd`, `onNodeMove`, `onConnect`, `onNodeDelete`, `onEdgeDelete`

Tests:
- Renders correct number of nodes from a model
- Renders correct number of edges
- Node types are correct per domain type

---

## Phase 3: Entity Swim Lanes

### 3a. Swim lane layout logic + tests (test first)

File: `src/ui/layout/swimlanes.ts` + `src/ui/layout/swimlanes.test.ts`

- `calculateSwimLanes(model)` → SwimLaneLayout[] (y ranges per entity)
- `getSwimLaneForPosition(lanes, y)` → entityId | null
- `snapToSwimLane(lanes, entityId, x)` → {x, y} snapped position

Tests:
- Lanes are non-overlapping
- Lane heights accommodate their events
- Free events (no entity) are above all lanes
- Position mapping returns correct entity

### 3b. Swim lane visual component + tests

File: `src/ui/SwimLane.tsx` + `src/ui/SwimLane.test.tsx`

- Renders horizontal band with entity name label
- Rendered as ReactFlow background/overlay

Tests:
- Renders entity name
- Renders with correct vertical bounds

---

## Phase 4: Chapters & Slices

### 4a. Chapter & slice layout logic + tests (test first)

File: `src/ui/layout/slices.ts` + `src/ui/layout/slices.test.ts`

- `calculateSliceBounds(model, sliceId)` → { x range covering contained nodes }
- `calculateChapterBounds(model, chapterId)` → { x range covering contained slices }

Tests:
- Slice bounds enclose all nodes assigned to it
- Chapter bounds enclose all its slices
- Empty slice/chapter has zero-width bounds

### 4b. Slice & chapter visual components + tests

Files: `src/ui/Slice.tsx`, `src/ui/Chapter.tsx` + tests

- Slice: vertical dashed line/band with label
- Chapter: horizontal grouping header above its slices

Tests:
- Renders label
- Visual containment of child elements

---

## Phase 5: Toolbar & Interactions

### 5a. Toolbar component + tests

File: `src/ui/Toolbar.tsx` + `src/ui/Toolbar.test.tsx`

Buttons: Add Event, Add Command, Add Query, Add Integration, Add UI Placeholder, Add Entity, Add Slice, Add Chapter

Tests:
- All buttons render
- Each button fires correct callback with correct node type

### 5b. Node editing (inline rename) + tests

File: `src/ui/NodeEditor.tsx` + `src/ui/NodeEditor.test.tsx`

- Double-click a node → inline text input
- Enter/blur → save name

Tests:
- Double-click activates edit mode
- Enter confirms, Escape cancels
- Updated name propagates to model

### 5c. Connection validation + tests

File: `src/ui/connectionRules.ts` + `src/ui/connectionRules.test.ts`

Rules:
- Command → Event: allowed
- Event → Query: allowed
- Integration → Command: allowed
- UI Placeholder → Command: allowed
- Query → UI Placeholder: allowed
- Everything else: rejected

Tests: all valid combinations accepted, all invalid combinations rejected.

---

## Phase 6: File Save/Load

### 6a. File operations + tests

File: `src/io/fileOps.ts` + `src/io/fileOps.test.ts`

- `saveModel(model)` → triggers browser file download as `.json`
- `loadModel(file)` → parsed & validated EventModel
- `newModel(name)` → fresh empty model

Tests:
- Save produces valid JSON
- Load parses saved JSON correctly
- Load rejects invalid files with user-friendly error
- New model has correct defaults

### 6b. File UI (menu/buttons) + tests

File: `src/ui/FileMenu.tsx` + `src/ui/FileMenu.test.tsx`

- New, Open (file picker), Save (download)
- Dirty state indicator (unsaved changes)

Tests:
- New resets model
- Open triggers file picker
- Save triggers download
- Dirty indicator shows after changes

---

## Phase 7: App Shell & Integration

### 7a. State management + tests

File: `src/state/store.ts` + `src/state/store.test.ts`

Simple React context + useReducer holding the `EventModel`. All mutations go through the pure `operations.ts` functions.

Actions map 1:1 to operation functions. Reducer is thin — just dispatches to the right operation.

Tests:
- Each action produces correct state via reducer
- Undo/redo stack (if time, otherwise defer)

### 7b. App component + integration test

File: `src/App.tsx` + `src/App.test.tsx`

Wires together: Toolbar + Canvas + FileMenu + state store.

Tests:
- App renders without crashing
- Adding a node via toolbar shows it on canvas
- Connecting two nodes creates an edge
- Save/load round-trip preserves model

---

## Phase Summary

| Phase | What | Tests Written First |
|-------|------|-------------------|
| 0 | Scaffolding | Smoke test |
| 1 | Domain types, operations, serialization | Unit tests for all pure model functions |
| 2 | Canvas, custom nodes, ReactFlow adapter | Adapter unit tests + component render tests |
| 3 | Swim lanes | Layout logic unit tests + component tests |
| 4 | Chapters & slices | Layout logic unit tests + component tests |
| 5 | Toolbar, editing, connection rules | Interaction tests + validation unit tests |
| 6 | File save/load | I/O unit tests + UI tests |
| 7 | App shell, state, integration | Reducer tests + end-to-end integration tests |

Each phase: **Types → Failing Tests → Implementation → Green Tests → Refactor**
