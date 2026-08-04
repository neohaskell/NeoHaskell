// Estimated rendered size of a domain node (event/command/query/etc.), shared
// between the layout math (`computeSliceLayouts`, `computeEntityLaneLayouts`,
// `bandGrid`) and the visual components so columns / lanes / positions all
// agree on how big a node actually is on screen.
//
// We don't measure the DOM here — layout math runs synchronously off model
// state with no DOM available — so we approximate with conservative constants
// tuned to the record-card chrome in NodeShell.module.css: a monospace HEADER
// (type name) over a BODY of `name : Type` field rows. Width fits the widest of
// the header and any field row; height grows by one row per visible field.

import type { Field } from '../../model/types'

/** Monospace glyph advance at the node's font sizes (~13px header / 12px body),
 *  rounded up so estimates never under-size a card and cause overflow. */
const CHAR_WIDTH = 8
/** Horizontal padding inside a zone (14px left clears the rail + 12px right). */
const HORIZONTAL_PADDING = 28
/** Gap reserved between a field's name and its right-aligned type. */
const FIELD_COL_GAP = 12

/** Height of the colored header zone (padding + 13px/1.3 line). */
export const HEADER_H = 30
/** Height of one field row in the body (12px/1.5 line + inter-row gap). */
export const ROW_H = 20
/** Vertical padding of the body zone (6px top + 6px bottom). */
const BODY_PAD_Y = 12
/** Card top+bottom border. */
const CARD_BORDER_Y = 2
/** Max field rows rendered before collapsing the rest into a `+N more` row. */
export const FIELD_CAP = 6

export const MIN_NODE_WIDTH = 180
export const MAX_NODE_WIDTH = 260
/** Extra breathing room added on top of the node's intrinsic size when
 *  computing the surrounding slice column / entity lane bounds. */
export const NODE_BREATHING_ROOM = 16

export interface NodeDimensions {
  width: number
  height: number
  /** Number of body rows actually rendered (fields, capped, or the empty row). */
  lines: number
}

/** Body rows rendered for `n` fields: at least one (the `no fields` row), capped
 *  at FIELD_CAP with one extra `+N more` row when there are more. */
export function visibleRowCount(n: number): number {
  if (n === 0) return 1
  return Math.min(n, FIELD_CAP) + (n > FIELD_CAP ? 1 : 0)
}

/**
 * Estimate the rendered `(width, height)` of a record-card node.
 *
 * - Width fits the widest of the header label and any field `name : Type` row,
 *   clamped to `[MIN_NODE_WIDTH, MAX_NODE_WIDTH]` (longer text truncates with an
 *   ellipsis rather than widening or wrapping the card).
 * - Height = border + header + body padding + one `ROW_H` per visible field row.
 *
 * Both the visual node components and the layout math import this so a node
 * never visually overflows the column / lane that contains it.
 */
export function estimateNodeDimensions(
  label: string,
  fields: readonly Field[] = [],
): NodeDimensions {
  const headerWidth = label.length * CHAR_WIDTH + HORIZONTAL_PADDING
  let widest = headerWidth
  for (const f of fields) {
    const rowWidth =
      (f.name.length + f.type.length) * CHAR_WIDTH + FIELD_COL_GAP + HORIZONTAL_PADDING
    if (rowWidth > widest) widest = rowWidth
  }
  const width = Math.min(MAX_NODE_WIDTH, Math.max(MIN_NODE_WIDTH, widest))

  const rows = visibleRowCount(fields.length)
  const height = CARD_BORDER_Y + HEADER_H + BODY_PAD_Y + rows * ROW_H
  return { width, height, lines: rows }
}
