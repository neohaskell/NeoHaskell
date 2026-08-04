/** Zoom at/above which a node reaches its most-detailed (`edit`) level. Node
 *  fields are read-only in the canvas, so this level no longer reveals an
 *  editor — it is retained as the densest read-only LOD. */
export const SEMANTIC_ZOOM_THRESHOLD = 1.5
/** Zoom below which a node collapses to its header only — the zoomed-out "flow"
 *  view, where the board reads as a wall of colored type headers with no
 *  field-text noise. Kept well below the typical fit-to-view zoom so the full
 *  record cards remain the default on-screen state. */
export const COLLAPSE_THRESHOLD = 0.5

export type NodeDetail = 'header' | 'card' | 'edit'

/**
 * Level-of-detail for a node at the given canvas zoom:
 * - `'header'` (far out): just the colored type header — the causal flow reads
 *   as a wall of headers without field-text clutter.
 * - `'card'` (normal): header + read-only field rows.
 * - `'edit'` (zoomed in): the densest read-only LOD (fields are read-only — no
 *   inline editor; the name is the legacy LOD label, not an edit affordance).
 *
 * Pure + unit-testable; `NodeShell` subscribes via a selector so it re-renders
 * only when the level changes (not on every pan).
 */
export function nodeDetailLevel(zoom: number): NodeDetail {
  if (zoom < COLLAPSE_THRESHOLD) return 'header'
  if (zoom >= SEMANTIC_ZOOM_THRESHOLD) return 'edit'
  return 'card'
}

/** True when a node should show its editable field affordances. */
export function shouldShowFields(zoom: number): boolean {
  return zoom >= SEMANTIC_ZOOM_THRESHOLD
}
