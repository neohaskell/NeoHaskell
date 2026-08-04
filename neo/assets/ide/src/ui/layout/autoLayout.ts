// Auto-layout for event models loaded from disk.
//
// Two jobs:
//  1. Fill missing positions (or `(0, 0)` defaults) for nodes that have
//     none. Common after AI healing, which often omits `nodePositions`.
//  2. Snap nodes whose `y` falls in the WRONG band for their kind. This
//     fixes models authored before the IDE settled on its layout
//     convention — most visibly, integrations dumped below the entity
//     swim lane (y > 500) instead of above events with the commands and
//     queries (y ≈ 120).
//
// Y-band convention (must stay in sync with `heal_event_model.rs`'s
// prompt and `grid.ts`'s `TOP_MARGIN`/`HEADER_HEIGHT`):
//
//   UI placeholders                         y < ABOVE_BAND_MIN
//   commands, queries, integrations         ABOVE_BAND_MIN ≤ y < BELOW_BAND_MIN
//   events (inside their entity lane)       y ≥ BELOW_BAND_MIN
//
// Integrations live in the SAME band as commands and queries — they are
// "things that happen at the command/query level", just on the I/O
// boundary. Putting them below events stretches edges into spaghetti.
//
// Existing positions that already sit in the correct band are preserved
// untouched; only out-of-band y values are snapped.

import type { EventModel, NodeType } from '../../model/types'
import { buildNodeSubmodelMap } from './submodels'

/** Default per-slice column width when we have to invent positions. */
const SLICE_AUTO_WIDTH = 400
/** Left padding inside a slice before the first node. */
const SLICE_LEFT_PADDING = 40
/** Vertical stacking offset for sibling nodes of the same kind in the same slice. */
const STACK_DY = 80
/** Visual layout constants — must stay in lockstep with `grid.ts`. */
const HEADER_HEIGHT = 40
const TOP_MARGIN = 300
const LANE_HEIGHT = 200

/** Lower edge of the "command/query/integration" band — UI placeholders
 *  go above this. */
const ABOVE_BAND_MIN = 50
/** Lower edge of the "events" band — commands/queries/integrations stop
 *  before this; events start at or after this. */
const BELOW_BAND_MIN = 300

/**
 * Returns a new `EventModel` with sensible positions:
 *  - Missing or `(0, 0)` positions are assigned defaults based on the
 *    node's slice / entity / type.
 *  - Existing positions whose `y` falls in the wrong band for their kind
 *    (e.g. an integration at `y = 575`) are snapped to the correct band.
 *  - Existing in-band positions are preserved untouched.
 *
 * Idempotent: running twice on the same input yields the same output.
 * Safe to run unconditionally after every load.
 */
export function autoLayoutMissingPositions(model: EventModel): EventModel {
  const sortedSlices = [...model.slices].sort((a, b) => a.order - b.order)
  const sortedEntities = [...model.entities].sort((a, b) => a.order - b.order)
  const sliceIndex = new Map(sortedSlices.map((s, i) => [s.id, i]))
  const entityIndex = new Map(sortedEntities.map((e, i) => [e.id, i]))

  // Per-(slice, type, entity) bucket counter for stacking siblings.
  const stackCounter = new Map<string, number>()

  // The absolute y-band convention only holds for the single ungrouped
  // timeline. Nodes that belong to a submodel live in that submodel's own
  // vertical band (a command there can legitimately sit at y = 1020), so we
  // must NOT snap their y — only fill a genuinely missing position.
  const nodeSubmodel = buildNodeSubmodelMap(model)

  const updated = { ...model.layout.nodePositions }
  let touched = false

  for (const node of model.nodes) {
    const existing = updated[node.id]
    const hasRealPosition =
      existing && (existing.x !== 0 || existing.y !== 0)
    const inSubmodel = (nodeSubmodel.get(node.id) ?? null) !== null
    const yInCorrectBand =
      hasRealPosition && isYInCorrectBand(node.type, existing.y)

    if (hasRealPosition && (yInCorrectBand || inSubmodel)) continue

    const sIdx =
      node.sliceId !== null ? sliceIndex.get(node.sliceId) ?? 0 : 0
    // Preserve x when the node already has a real position — only the y
    // was off-band. Otherwise allocate a fresh x based on slice index.
    const x = hasRealPosition
      ? existing.x
      : sIdx * SLICE_AUTO_WIDTH + SLICE_LEFT_PADDING

    const entityIdx =
      entityIdOf(node) !== null ? entityIndex.get(entityIdOf(node)!) ?? 0 : 0
    const baseY = bandY(node.type, entityIdx)

    const bucket = `${node.sliceId ?? 'noslice'}|${node.type}|${entityIdOf(node) ?? 'noentity'}`
    const rank = stackCounter.get(bucket) ?? 0
    stackCounter.set(bucket, rank + 1)
    const y = baseY + rank * STACK_DY

    updated[node.id] = { x, y }
    touched = true
  }

  if (!touched) return model
  return {
    ...model,
    layout: { ...model.layout, nodePositions: updated },
  }
}

function entityIdOf(node: EventModel['nodes'][number]): string | null {
  if (node.type === 'event' || node.type === 'command') {
    return node.entityId
  }
  return null
}

function bandY(type: NodeType, entityIdx: number): number {
  switch (type) {
    case 'uiPlaceholder':
      return -60
    case 'command':
    case 'query':
    case 'integration':
      // All three live in the same "above the swim lane" band. Stack
      // counter (`STACK_DY`) separates siblings of the same kind.
      return HEADER_HEIGHT + 80 // = 120
    case 'event':
      return HEADER_HEIGHT + TOP_MARGIN + entityIdx * LANE_HEIGHT + 60
  }
}

function isYInCorrectBand(type: NodeType, y: number): boolean {
  switch (type) {
    case 'uiPlaceholder':
      return y < ABOVE_BAND_MIN
    case 'command':
    case 'query':
    case 'integration':
      return y >= ABOVE_BAND_MIN && y < BELOW_BAND_MIN
    case 'event':
      return y >= BELOW_BAND_MIN
  }
}
