import { MarkerType, type Node, type Edge } from '@xyflow/react'
import type { EventModel, ModelEdge } from '../model/types'
import { buildNodeSubmodelMap } from './layout/submodels'
import type { BandGrid } from './layout/bandGrid'

// ── "Features as pages" core ────────────────────────────────
//
// A submodel is rendered as a full-screen FEATURE (page). Membership stays
// transitive — a node belongs to the feature of its slice's chapter's submodel
// (`node → slice → chapter → submodel`), or to the synthetic UNGROUPED feature
// when that chain is broken. This module derives that mapping and classifies
// every edge as intra-feature or cross-feature, WITHOUT ever storing a feature
// id on the edge (locality is a pure function of node membership).
//
// The load-bearing invariant: when the canvas filters to one feature, React
// Flow silently drops any edge whose other endpoint is off-screen. So a
// cross-feature edge is NEVER dropped — it is rendered as a clickable boundary
// PORTAL stub on each feature it touches (see `buildFeatureRenderEdges`).

/** Sentinel feature id for content not assigned to any submodel. Kept in
 *  lockstep with `bandGrid.ts`'s `computeFeatureGrid` ungrouped literal. */
export const UNGROUPED_FEATURE = '__ungrouped__'

export type FeatureId = string

/** The feature a node belongs to: its submodel id, or `UNGROUPED_FEATURE`. */
export function featureOfNode(
  nodeSubmodel: Map<string, string | null>,
  nodeId: string,
): FeatureId {
  return nodeSubmodel.get(nodeId) ?? UNGROUPED_FEATURE
}

export interface EdgeLocality {
  sourceFeature: FeatureId
  targetFeature: FeatureId
  crossesFeature: boolean
}

/** Classify an edge against a prebuilt node→submodel map (the hot path). */
export function classifyEdgeLocalityWith(
  nodeSubmodel: Map<string, string | null>,
  edge: ModelEdge,
): EdgeLocality {
  const sourceFeature = featureOfNode(nodeSubmodel, edge.sourceId)
  const targetFeature = featureOfNode(nodeSubmodel, edge.targetId)
  return { sourceFeature, targetFeature, crossesFeature: sourceFeature !== targetFeature }
}

/** Classify an edge as intra- vs cross-feature (builds the map internally). */
export function classifyEdgeLocality(model: EventModel, edge: ModelEdge): EdgeLocality {
  return classifyEdgeLocalityWith(buildNodeSubmodelMap(model), edge)
}

/** The feature a node renders on (builds the map internally). */
export function pageOf(model: EventModel, nodeId: string): FeatureId {
  return featureOfNode(buildNodeSubmodelMap(model), nodeId)
}

export interface FeatureEdgePlan {
  /** Both endpoints on the active feature — render as a normal edge. */
  intra: ModelEdge[]
  /** Source on the active feature, target elsewhere — render via an OUT portal. */
  outgoing: ModelEdge[]
  /** Target on the active feature, source elsewhere — render via an IN portal. */
  incoming: ModelEdge[]
}

/**
 * Partition a model's edges relative to `activeFeatureId`. Edges with NEITHER
 * endpoint on the active feature are omitted (they belong to other screens).
 *
 * A cross-feature edge appears as `outgoing` on its source feature AND
 * `incoming` on its target feature, so EVERY edge is represented on every
 * screen it touches and is never silently dropped (the no-edge-lost invariant).
 */
export function planFeatureEdges(
  model: EventModel,
  activeFeatureId: FeatureId,
  nodeSubmodel: Map<string, string | null> = buildNodeSubmodelMap(model),
): FeatureEdgePlan {
  const intra: ModelEdge[] = []
  const outgoing: ModelEdge[] = []
  const incoming: ModelEdge[] = []
  for (const edge of model.edges) {
    const { sourceFeature, targetFeature } = classifyEdgeLocalityWith(nodeSubmodel, edge)
    const srcActive = sourceFeature === activeFeatureId
    const tgtActive = targetFeature === activeFeatureId
    if (srcActive && tgtActive) intra.push(edge)
    else if (srcActive) outgoing.push(edge)
    else if (tgtActive) incoming.push(edge)
  }
  return { intra, outgoing, incoming }
}

// ── React Flow rendering of a single feature's edges + portals ──

const EDGE_STYLE = { strokeWidth: 3, stroke: 'var(--em-edge)' }
const PORTAL_EDGE_STYLE = { strokeWidth: 2, stroke: 'var(--em-edge-portal)', strokeDasharray: '6 4' }
const ARROW = { type: MarkerType.ArrowClosed, width: 12, height: 12 }

const PORTAL_GAP = 90
const PORTAL_WIDTH = 200
const PORTAL_VSTEP = 80

export interface FeatureRender {
  edges: Edge[]
  portalNodes: Node[]
}

function portalNode(
  id: string,
  x: number,
  y: number,
  label: string,
  featureName: string,
  direction: 'in' | 'out',
  onNavigate?: () => void,
): Node {
  return {
    id,
    type: 'boundaryPortal',
    position: { x, y },
    data: { label, featureName, direction, onNavigate },
    draggable: false,
    selectable: false,
    // Non-selectable React Flow nodes get `pointer-events: none` (clicks fall
    // through to the pane), so opt back in explicitly — mirrors sliceColumn /
    // entityLane — otherwise the portal's click-to-navigate never fires.
    style: { zIndex: 5, pointerEvents: 'all' as const },
  }
}

/**
 * Build the React Flow edges + boundary-portal nodes for the active feature.
 * Intra-feature edges render normally. Cross-feature edges render as a real
 * edge from the on-feature endpoint to a portal stub placed just outside the
 * feature's grid — so React Flow always has two resolvable endpoints and can
 * never silently drop the edge. Clicking a portal navigates to the other
 * feature (`onNavigate`). Edge ids are preserved so the on-disk model edge is
 * traceable to exactly one rendered edge per screen it touches.
 */
export function buildFeatureRenderEdges(
  model: EventModel,
  activeFeatureId: FeatureId,
  featureGrid: BandGrid,
  nodeSubmodel: Map<string, string | null>,
  featureName: (featureId: FeatureId) => string,
  onNavigate?: (featureId: FeatureId, focusNodeId?: string) => void,
): FeatureRender {
  const plan = planFeatureEdges(model, activeFeatureId, nodeSubmodel)
  const nameOf = new Map(model.nodes.map((n) => [n.id, n.name]))
  const edges: Edge[] = []
  const portalNodes: Node[] = []

  for (const e of plan.intra) {
    edges.push({
      id: e.id,
      source: e.sourceId,
      target: e.targetId,
      sourceHandle: e.sourceHandle ?? undefined,
      targetHandle: e.targetHandle ?? undefined,
      type: 'default',
      style: EDGE_STYLE,
      markerEnd: { ...ARROW, color: 'var(--em-edge)' },
    })
  }

  const rect = featureGrid.rect
  const rightX = rect.xStart + rect.width + PORTAL_GAP
  const leftX = rect.xStart - PORTAL_GAP - PORTAL_WIDTH
  const fallbackTop = rect.yStart + 120
  let outCursor = 0
  let inCursor = 0

  for (const e of plan.outgoing) {
    const portalId = `__portal-out-${e.id}`
    const srcPos = featureGrid.positions.get(e.sourceId)
    const y = srcPos ? srcPos.y : fallbackTop + outCursor++ * PORTAL_VSTEP
    const targetFeature = featureOfNode(nodeSubmodel, e.targetId)
    portalNodes.push(
      portalNode(portalId, rightX, y, nameOf.get(e.targetId) ?? 'node', featureName(targetFeature), 'out', () =>
        onNavigate?.(targetFeature, e.targetId),
      ),
    )
    edges.push({
      id: e.id,
      source: e.sourceId,
      target: portalId,
      sourceHandle: 'right',
      targetHandle: 'left',
      type: 'default',
      style: PORTAL_EDGE_STYLE,
      markerEnd: { ...ARROW, color: 'var(--em-edge-portal)' },
    })
  }

  for (const e of plan.incoming) {
    const portalId = `__portal-in-${e.id}`
    const tgtPos = featureGrid.positions.get(e.targetId)
    const y = tgtPos ? tgtPos.y : fallbackTop + inCursor++ * PORTAL_VSTEP
    const sourceFeature = featureOfNode(nodeSubmodel, e.sourceId)
    portalNodes.push(
      portalNode(portalId, leftX, y, nameOf.get(e.sourceId) ?? 'node', featureName(sourceFeature), 'in', () =>
        onNavigate?.(sourceFeature, e.sourceId),
      ),
    )
    edges.push({
      id: e.id,
      source: portalId,
      target: e.targetId,
      sourceHandle: 'right',
      targetHandle: 'left',
      type: 'default',
      style: PORTAL_EDGE_STYLE,
      markerEnd: { ...ARROW, color: 'var(--em-edge-portal)' },
    })
  }

  return { edges, portalNodes }
}
