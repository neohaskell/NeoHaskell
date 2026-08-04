import type { Node } from '@xyflow/react'
import type { EventModel, ModelNode, Slice, Entity } from '../../model/types'
import { estimateNodeDimensions } from '../nodes/nodeDimensions'
import { buildNodeSubmodelMap, submodelsInUse } from './submodels'
import type { NodePositionAdjustment, SliceLayout, EntityLaneLayout } from './grid'

// When submodels are in use, each submodel becomes a self-contained vertical
// BAND that replicates the normal layout — slice columns × entity swim-lane
// rows — but COMPACT: a band shows ONLY the entities its own nodes use, as
// consecutive rows, so the same entity is DUPLICATED across bands (no global
// gaps). Ungrouped content keeps its global positions as a region on top.
//
// The band layout is a DETERMINISTIC function of model STRUCTURE (slice.order
// within the submodel, entity.order filtered to the band's events, submodel
// .order). It WRITES node positions — the role `stackSubmodels` used to play —
// and derives backgrounds from the same computation, never from `boxOf` of
// drifting positions. That makes the reflow idempotent (a fixed point).
//
// Constants must stay in lockstep with grid.ts / autoLayout.ts (the same
// re-declare-with-a-lockstep-comment pattern autoLayout.ts uses).
//
// The vertical scaffold (UI row → command band → entity lanes) is DERIVED from
// the actual record-card heights per feature (see layoutFeature), so cards are
// never lost in fixed oceans of empty space. Only the fixed anchors below
// remain constants.
const HEADER_HEIGHT = 40
const SLICE_PADDING = 40
const MIN_COLUMN_WIDTH = 200
/** Vertical gap between the open-region rows (header → UI → command → lanes). */
const ROW_GAP = 36
/** Gap between two record cards stacked in the same lane column. */
const STACK_GAP = 24
/** Band-local y of the slice header bar (top of the slice columns). */
const SLICE_HEADER_TOP = 80
/** Top of the open region (UI row), just below the slice header bar. */
const OPEN_TOP = SLICE_HEADER_TOP + HEADER_HEIGHT + ROW_GAP
/** An event's offset from its lane's top edge (clears the lane label). */
const EVENT_LANE_INSET = 60
/** Bottom padding below the deepest card in a lane. */
const LANE_BOTTOM_PAD = 24
/** Floor height for a lane that has events. */
const MIN_LANE_HEIGHT = 130
/** Collapsed height for an empty lane (an unused/orphan aggregate). */
const EMPTY_LANE_HEIGHT = 96
/** Fallback lane-top for bands with no lanes (rare). */
const FALLBACK_LANES_TOP = 200

// Band rectangle padding + vertical spacing between bands.
const BAND_VGAP = 220
const BAND_PAD = 90
// Horizontal frame padding. The LEFT pad doubles as the entity-label gutter, so
// it must match the entity-lane label column width (EntityLaneNode .labelCol =
// 100px). The slice columns start at band-local x=0, i.e. just right of the
// gutter, and the frame closes with a slim right pad. Keeping these explicit
// (instead of one symmetric BAND_PAD) is what keeps lanes + columns from
// poking outside the frame.
const LANE_LABEL_GUTTER = 100
// Right pad is wide enough to host the "add slice" (+) button inside the frame.
const FRAME_RIGHT_PAD = 64
/** Diameter of the canvas add (+) buttons (mirrors AddButtonNode.module.css). */
const ADD_BTN = 34

const COMMAND_BAND_TYPES = new Set(['command', 'query', 'integration'])
/** Horizontal gap between command-band nodes sharing a slice's top level. */
const COMMAND_GAP = 28
/** Left-to-right order of the command band: command, then query, then integration. */
const CMD_BAND_PRIORITY: Record<string, number> = { command: 0, query: 1, integration: 2 }

export interface BandGrid {
  submodelId: string
  name: string
  /** Absolute y of the band's top (band-local y = 0 maps here). */
  yOrigin: number
  /** Absolute-x slice columns inside this band (left-aligned from 0). */
  slices: SliceLayout[]
  /** Absolute-y entity rows inside this band (compact, this band's entities). */
  lanes: EntityLaneLayout[]
  /** Absolute grid positions for every node owned by this band. */
  positions: Map<string, { x: number; y: number }>
  /** The translucent band rectangle (deterministic, not from node positions). */
  rect: { xStart: number; yStart: number; width: number; height: number }
}

/** Entity id of a node only when it occupies an entity lane (events). */
function eventEntityId(node: ModelNode): string | null {
  return node.type === 'event' ? node.entityId : null
}

/** Bottom edge (max y + height) of the ungrouped region, or null if empty. */
function ungroupedBottom(
  model: EventModel,
  nodeSubmodel: Map<string, string | null>,
): number | null {
  let maxBottom = -Infinity
  let found = false
  for (const node of model.nodes) {
    if ((nodeSubmodel.get(node.id) ?? null) !== null) continue
    const pos = model.layout.nodePositions[node.id]
    if (!pos) continue
    found = true
    const { height } = estimateNodeDimensions(node.name, node.fields)
    maxBottom = Math.max(maxBottom, pos.y + height)
  }
  return found ? maxBottom : null
}

/**
 * Compute a deterministic slice×entity grid for every submodel band.
 * Ungrouped nodes are not laid out here (they keep their global positions);
 * the only position-derived input is where the FIRST band starts (below the
 * ungrouped region), which is stable because ungrouped nodes never move — so
 * the result is a fixed point.
 */
/**
 * Lay out the members of ONE feature (a submodel band, or the ungrouped
 * region) as a self-contained slice×entity grid whose top sits at `yOrigin`.
 * This is the shared core of `computePerBandGrids` (which stacks every
 * submodel down the page) and `computeFeatureGrid` (which lays out a single
 * feature at the origin for the full-screen "Features as pages" view). It is a
 * pure function of model STRUCTURE — never of incoming node positions — so
 * both callers are deterministic and idempotent.
 */
function layoutFeature(
  members: ModelNode[],
  sliceById: Map<string, Slice>,
  entityById: Map<string, Entity>,
  yOrigin: number,
  submodelId: string,
  name: string,
  referencedGlobal: Set<string>,
): BandGrid {
  // Columns: this feature's slices (sorted by slice.order), laid contiguously
  // from band-local x = 0. Column width fits the widest node in the slice.
  const bandSlices = [...new Set(members.map((n) => n.sliceId))]
    .filter((id): id is string => id !== null)
    .map((id) => sliceById.get(id))
    .filter((s): s is Slice => s !== undefined)
    .sort((a, b) => a.order - b.order)

  // Command-band nodes (command / query / integration) share ONE horizontal
  // level per slice — laid side by side (command → query → integration, then
  // by name) so the column WIDENS to host them instead of stacking them
  // vertically. Precompute each node's x-offset within its slice + the total
  // band spread (used to size the column).
  const cmdBandBySlice = new Map<string, ModelNode[]>()
  for (const n of members) {
    if (n.sliceId === null || !COMMAND_BAND_TYPES.has(n.type)) continue
    const arr = cmdBandBySlice.get(n.sliceId)
    if (arr) arr.push(n)
    else cmdBandBySlice.set(n.sliceId, [n])
  }
  const cmdXOffset = new Map<string, number>()
  const cmdSpread = new Map<string, number>()
  for (const [sid, arr] of cmdBandBySlice) {
    arr.sort(
      (a, b) =>
        (CMD_BAND_PRIORITY[a.type] ?? 0) - (CMD_BAND_PRIORITY[b.type] ?? 0) ||
        a.name.localeCompare(b.name),
    )
    let cursor = 0
    for (const n of arr) {
      cmdXOffset.set(n.id, cursor)
      cursor += estimateNodeDimensions(n.name, n.fields).width + COMMAND_GAP
    }
    cmdSpread.set(sid, Math.max(0, cursor - COMMAND_GAP)) // drop the trailing gap
  }

  const heightOf = (n: ModelNode) => estimateNodeDimensions(n.name, n.fields).height

  const sliceLayouts: SliceLayout[] = []
  const sliceX = new Map<string, number>()
  let xCursor = 0
  for (const slice of bandSlices) {
    // Widest single event/UI node (these stack vertically in one sub-column).
    let singleMax = MIN_COLUMN_WIDTH
    for (const n of members) {
      if (n.sliceId !== slice.id || COMMAND_BAND_TYPES.has(n.type)) continue
      singleMax = Math.max(
        singleMax,
        estimateNodeDimensions(n.name, n.fields).width + SLICE_PADDING * 2,
      )
    }
    const spread = cmdSpread.get(slice.id) ?? 0
    const cmdWidth = spread > 0 ? spread + SLICE_PADDING * 2 : 0
    const width = Math.max(MIN_COLUMN_WIDTH, singleMax, cmdWidth)
    sliceLayouts.push({ sliceId: slice.id, xStart: xCursor, width })
    sliceX.set(slice.id, xCursor)
    xCursor += width
  }
  const bandWidth = Math.max(xCursor, MIN_COLUMN_WIDTH)

  // Rows: entities owning >=1 EVENT in this feature, PLUS "orphan" entities that
  // have no events anywhere yet — so a freshly added entity shows as an empty
  // lane you can drop events into. Entities used only by OTHER features stay
  // hidden (keeps each feature compact). Sorted by entity.order.
  const featureEventEntities = new Set(
    members.map(eventEntityId).filter((e): e is string => e !== null),
  )
  const bandEntities = [...entityById.values()]
    .filter((e) => featureEventEntities.has(e.id) || !referencedGlobal.has(e.id))
    .sort((a, b) => a.order - b.order)

  // Vertical scaffold DERIVED from the tallest card in each open-region row, so
  // the band is as tall as its content needs and no taller. UI placeholders sit
  // in the top row; the command/query/integration band sits below them; entity
  // lanes begin below the command band.
  const maxUiH = members
    .filter((n) => n.type === 'uiPlaceholder')
    .reduce((m, n) => Math.max(m, heightOf(n)), 0)
  const maxCmdH = members
    .filter((n) => COMMAND_BAND_TYPES.has(n.type))
    .reduce((m, n) => Math.max(m, heightOf(n)), 0)
  const uiY = OPEN_TOP
  const commandBandY = OPEN_TOP + (maxUiH > 0 ? maxUiH + ROW_GAP : 0)
  const lanesTop = commandBandY + (maxCmdH > 0 ? maxCmdH + ROW_GAP : 0) + ROW_GAP

  // Deterministic lane height: the deepest slice column's stacked event cards
  // (summed REAL heights + gaps) drive it. An empty lane collapses to a thin
  // rail so an unused aggregate reads as intentionally empty, not as a void.
  const laneHeight = new Map<string, number>()
  for (const entity of bandEntities) {
    const perColumn = new Map<string, number>()
    for (const n of members) {
      if (n.type !== 'event' || n.entityId !== entity.id || n.sliceId === null) continue
      perColumn.set(n.sliceId, (perColumn.get(n.sliceId) ?? 0) + heightOf(n) + STACK_GAP)
    }
    if (perColumn.size === 0) {
      laneHeight.set(entity.id, EMPTY_LANE_HEIGHT)
      continue
    }
    const deepest = Math.max(...perColumn.values()) - STACK_GAP // drop trailing gap
    laneHeight.set(
      entity.id,
      Math.max(MIN_LANE_HEIGHT, EVENT_LANE_INSET + deepest + LANE_BOTTOM_PAD),
    )
  }

  const laneLayouts: EntityLaneLayout[] = []
  const laneLocalY = new Map<string, number>()
  let laneCursor = lanesTop
  for (const entity of bandEntities) {
    const h = laneHeight.get(entity.id) ?? MIN_LANE_HEIGHT
    laneLayouts.push({ entityId: entity.id, yStart: yOrigin + laneCursor, height: h })
    laneLocalY.set(entity.id, laneCursor)
    laneCursor += h
  }

  // Place nodes. Process in a STABLE order (name, id) so stacked siblings get a
  // deterministic vertical order regardless of model.nodes ordering. Siblings
  // sharing a bucket stack by their REAL heights (a running cursor), so a tall
  // card never overlaps the one below it.
  const positions = new Map<string, { x: number; y: number }>()
  const stackCursor = new Map<string, number>()
  const ordered = [...members].sort(
    (a, b) => a.name.localeCompare(b.name) || a.id.localeCompare(b.id),
  )
  for (const node of ordered) {
    const colX = node.sliceId !== null ? sliceX.get(node.sliceId) ?? 0 : 0
    let x = colX + SLICE_PADDING
    let localY: number
    if (node.type === 'event') {
      const lane = node.entityId !== null ? laneLocalY.get(node.entityId) : undefined
      const base = (lane ?? lanesTop) + EVENT_LANE_INSET
      const bucket = `${node.sliceId}|event|${node.entityId}`
      localY = stackCursor.get(bucket) ?? base
      stackCursor.set(bucket, localY + heightOf(node) + STACK_GAP)
    } else if (node.type === 'uiPlaceholder') {
      const bucket = `${node.sliceId}|ui`
      localY = stackCursor.get(bucket) ?? uiY
      stackCursor.set(bucket, localY + heightOf(node) + STACK_GAP)
    } else if (COMMAND_BAND_TYPES.has(node.type)) {
      // Command / query / integration: one shared level, side by side (the
      // column was widened above to fit them).
      localY = commandBandY
      x = colX + SLICE_PADDING + (cmdXOffset.get(node.id) ?? 0)
    } else {
      localY = commandBandY
    }
    positions.set(node.id, { x, y: yOrigin + localY })
  }

  const bandHeight = laneCursor + BAND_PAD

  return {
    submodelId,
    name,
    yOrigin,
    slices: sliceLayouts,
    lanes: laneLayouts,
    positions,
    rect: {
      // Left edge sits a label-gutter to the left of the columns; right edge a
      // slim pad past the last column — so entity lanes (which span the full
      // rect) and the columns are both fully contained.
      xStart: -LANE_LABEL_GUTTER,
      yStart: yOrigin,
      width: bandWidth + LANE_LABEL_GUTTER + FRAME_RIGHT_PAD,
      height: bandHeight,
    },
  }
}

/**
 * Entity ids referenced by ANY node (event or command) anywhere in the model.
 * An entity NOT in this set is "orphan" — newly created and unused — and gets a
 * lane in every feature so you can drop events into it. A command-only entity is
 * referenced, so it stays lane-less (lanes are for events/aggregates).
 */
function referencedEntities(model: EventModel): Set<string> {
  const s = new Set<string>()
  for (const n of model.nodes) {
    const eid = (n as { entityId?: string | null }).entityId
    if (typeof eid === 'string') s.add(eid)
  }
  return s
}

export function computePerBandGrids(model: EventModel): BandGrid[] {
  const nodeSubmodel = buildNodeSubmodelMap(model)
  const sliceById = new Map(model.slices.map((s) => [s.id, s]))
  const entityById = new Map(model.entities.map((e) => [e.id, e]))
  const referenced = referencedEntities(model)

  const membersBySubmodel = new Map<string, ModelNode[]>()
  for (const node of model.nodes) {
    const sm = nodeSubmodel.get(node.id) ?? null
    if (sm === null) continue
    const arr = membersBySubmodel.get(sm)
    if (arr) arr.push(node)
    else membersBySubmodel.set(sm, [node])
  }

  const ub = ungroupedBottom(model, nodeSubmodel)
  let yCursor = ub !== null ? ub + BAND_VGAP : 0

  const orderedSubmodels = [...model.submodels].sort((a, b) => a.order - b.order)
  const grids: BandGrid[] = []

  for (const submodel of orderedSubmodels) {
    const members = membersBySubmodel.get(submodel.id)
    if (!members || members.length === 0) continue
    const grid = layoutFeature(members, sliceById, entityById, yCursor, submodel.id, submodel.name, referenced)
    grids.push(grid)
    yCursor += grid.rect.height + BAND_VGAP
  }

  return grids
}

/**
 * Lay out a SINGLE feature at the origin — the per-screen layout for the
 * "Features as pages" view, where only one feature is shown at a time.
 * `featureId` is a submodel id, or `null` for the ungrouped region. The
 * returned grid's `submodelId` is the submodel id or the literal
 * `'__ungrouped__'` sentinel (kept in lockstep with `featurePages.ts`'s
 * `UNGROUPED_FEATURE`).
 */
export function computeFeatureGrid(model: EventModel, featureId: string | null): BandGrid {
  const nodeSubmodel = buildNodeSubmodelMap(model)
  const sliceById = new Map(model.slices.map((s) => [s.id, s]))
  const entityById = new Map(model.entities.map((e) => [e.id, e]))
  const members = model.nodes.filter((n) => (nodeSubmodel.get(n.id) ?? null) === featureId)
  const name =
    featureId === null
      ? 'Ungrouped'
      : model.submodels.find((s) => s.id === featureId)?.name ?? 'Feature'
  return layoutFeature(
    members,
    sliceById,
    entityById,
    0,
    featureId ?? '__ungrouped__',
    name,
    referencedEntities(model),
  )
}

/**
 * Position adjustments that lay every submodel band out as a grid. Returns
 * `[]` (a no-op) when no chapter is assigned to a submodel, so legacy models
 * are never disturbed. Idempotent: re-running yields the same positions.
 * REPLACES `stackSubmodels` as the band position writer.
 */
export function reflowBands(model: EventModel): NodePositionAdjustment[] {
  if (!submodelsInUse(model)) return []
  const adjustments: NodePositionAdjustment[] = []
  for (const band of computePerBandGrids(model)) {
    for (const [nodeId, pos] of band.positions) {
      const cur = model.layout.nodePositions[nodeId]
      if (!cur || cur.x !== pos.x || cur.y !== pos.y) {
        adjustments.push({ nodeId, x: pos.x, y: pos.y })
      }
    }
  }
  return adjustments
}

/** The band whose rectangle contains absolute y, or null (the ungrouped region). */
export function resolveBandAtY(grids: BandGrid[], y: number): BandGrid | null {
  for (const band of grids) {
    if (y >= band.rect.yStart && y < band.rect.yStart + band.rect.height) {
      return band
    }
  }
  return null
}

export interface BandNodeOptions {
  entityName: Map<string, string>
  sliceName: Map<string, string>
  highlightedSliceId?: string | null
  flashingSliceId?: string | null
  onRenameSlice?: (sliceId: string, name: string) => void
  onSliceSelect?: (sliceId: string) => void
  highlightedEntityId?: string | null
  flashingEntityId?: string | null
  selectedEntityId?: string | null
  onRenameEntity?: (entityId: string, name: string) => void
  onEntitySelect?: (entityId: string) => void
  onSubmodelRename?: (submodelId: string, name: string) => void
  onSubmodelDelete?: (submodelId: string) => void
  /** Canvas "+" buttons: add a slice (right of the columns) / entity (below the lanes). */
  onAddSlice?: () => void
  onAddEntity?: () => void
}

/**
 * Background nodes for every submodel band: the translucent band rectangle,
 * its per-band slice columns, and its per-band (duplicated) entity lanes.
 * Reuses the same `submodelBand` / `sliceColumn` / `entityLane` components and
 * z-indices, and wires the same rename / select / highlight data as the global
 * grid (`buildGridNodes`) — the components are all `w-full h-full`, so only the
 * band-scoped `style.width/height` and positions differ.
 */
export function buildPerBandGridNodes(grids: BandGrid[], opts: BandNodeOptions): Node[] {
  const nodes: Node[] = []
  for (const band of grids) {
    nodes.push({
      id: `__submodel-band-${band.submodelId}`,
      type: 'submodelBand',
      position: { x: band.rect.xStart, y: band.rect.yStart },
      data: {
        label: band.name,
        onRename: opts.onSubmodelRename
          ? (name: string) => opts.onSubmodelRename!(band.submodelId, name)
          : undefined,
      },
      draggable: false,
      selectable: false,
      focusable: false,
      style: {
        width: band.rect.width,
        height: band.rect.height,
        zIndex: -3,
        pointerEvents: 'none' as const,
      },
    })

    const columnTop = band.yOrigin + SLICE_HEADER_TOP
    const columnHeight = band.rect.yStart + band.rect.height - columnTop
    for (const col of band.slices) {
      nodes.push({
        id: `__band-slice-${band.submodelId}-${col.sliceId}`,
        type: 'sliceColumn',
        position: { x: col.xStart, y: columnTop },
        data: {
          label: opts.sliceName.get(col.sliceId) ?? '',
          sliceId: col.sliceId,
          chapterName: null,
          highlighted: opts.highlightedSliceId === col.sliceId,
          flashing: opts.flashingSliceId === col.sliceId,
          onRename: opts.onRenameSlice
            ? (name: string) => opts.onRenameSlice!(col.sliceId, name)
            : undefined,
          onSelect: opts.onSliceSelect ? () => opts.onSliceSelect!(col.sliceId) : undefined,
        },
        draggable: false,
        selectable: false,
        focusable: false,
        style: {
          width: col.width,
          height: columnHeight,
          zIndex: -1,
          pointerEvents: 'all' as const,
        },
      })
    }

    for (const lane of band.lanes) {
      nodes.push({
        id: `__band-lane-${band.submodelId}-${lane.entityId}`,
        type: 'entityLane',
        // Span the band rectangle EXACTLY (left gutter through right pad) so the
        // swim lane is contained by the frame, never poking out the sides.
        position: { x: band.rect.xStart, y: lane.yStart },
        data: {
          label: opts.entityName.get(lane.entityId) ?? '',
          entityId: lane.entityId,
          highlighted: (opts.highlightedEntityId ?? opts.selectedEntityId) === lane.entityId,
          flashing: opts.flashingEntityId === lane.entityId,
          onRename: opts.onRenameEntity
            ? (name: string) => opts.onRenameEntity!(lane.entityId, name)
            : undefined,
          onSelect: opts.onEntitySelect ? () => opts.onEntitySelect!(lane.entityId) : undefined,
        },
        draggable: false,
        selectable: false,
        focusable: false,
        style: {
          width: band.rect.width,
          height: lane.height,
          zIndex: -2,
          pointerEvents: 'all' as const,
        },
      })
    }

    // "+" to add a slice — just right of the last column, on the header row.
    if (opts.onAddSlice) {
      const lastRight = band.slices.reduce((m, s) => Math.max(m, s.xStart + s.width), 0)
      nodes.push({
        id: `__add-slice-${band.submodelId}`,
        type: 'addButton',
        position: { x: lastRight + 14, y: columnTop + (HEADER_HEIGHT - ADD_BTN) / 2 },
        data: { label: 'Add slice', onClick: opts.onAddSlice, testId: 'add-slice-button' },
        draggable: false,
        selectable: false,
        focusable: false,
        style: { width: ADD_BTN, height: ADD_BTN, zIndex: 2, pointerEvents: 'all' as const },
      })
    }

    // "+" to add an entity — below the last lane, in the entity-label gutter.
    if (opts.onAddEntity) {
      const lastBottom = band.lanes.reduce(
        (m, l) => Math.max(m, l.yStart + l.height),
        band.yOrigin + FALLBACK_LANES_TOP,
      )
      nodes.push({
        id: `__add-entity-${band.submodelId}`,
        type: 'addButton',
        position: { x: band.rect.xStart + (LANE_LABEL_GUTTER - ADD_BTN) / 2, y: lastBottom + 14 },
        data: { label: 'Add entity', onClick: opts.onAddEntity, testId: 'add-entity-button' },
        draggable: false,
        selectable: false,
        focusable: false,
        style: { width: ADD_BTN, height: ADD_BTN, zIndex: 2, pointerEvents: 'all' as const },
      })
    }
  }
  return nodes
}
