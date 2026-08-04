import type { EventModel, NodeType, EdgeType } from '../../model/types'
import { reducer, type Action } from '../../state/store'
import { getEdgeTypeForConnection } from '../connectionRules'

// Default names for gesture-created nodes (mirror the old toolbar defaults).
const DEFAULT_NAME: Record<NodeType, string> = {
  event: 'New Event',
  command: 'New Command',
  query: 'New Query',
  integration: 'New Integration',
  uiPlaceholder: 'New UI',
}

function addActionFor(type: NodeType): Action {
  switch (type) {
    case 'event':
      return { type: 'addEvent', name: DEFAULT_NAME.event }
    case 'command':
      return { type: 'addCommand', name: DEFAULT_NAME.command }
    case 'query':
      return { type: 'addQuery', name: DEFAULT_NAME.query }
    case 'integration':
      return { type: 'addIntegration', name: DEFAULT_NAME.integration, kind: 'outbound' }
    case 'uiPlaceholder':
      return { type: 'addUIPlaceholder', name: DEFAULT_NAME.uiPlaceholder }
  }
}

/** Where a gesture-created node should land. */
export interface CreatePlacement {
  x: number
  y: number
  sliceId?: string | null
  entityId?: string | null
}

function placeNode(
  model: EventModel,
  nodeId: string,
  type: NodeType,
  place: CreatePlacement,
): EventModel {
  let next = model
  if (place.sliceId != null) {
    next = reducer(next, { type: 'assignNodeToSlice', nodeId, sliceId: place.sliceId })
  }
  if (place.entityId != null && type === 'event') {
    next = reducer(next, { type: 'assignNodeToEntity', nodeId, entityId: place.entityId })
  }
  // Seed a global position so the flat-timeline fallback / band reflow have a
  // sensible starting point. In feature/page mode the deterministic grid owns
  // x/y from the node's slice+entity, so this value is ignored on screen — no
  // per-feature override is written (the grid is the single source of truth).
  next = reducer(next, { type: 'updatePosition', nodeId, x: place.x, y: place.y })
  return next
}

/**
 * Create a node of `type` at a placement. Returns the next model and the new
 * node's id (add operations append last, so the new node is recoverable for
 * follow-up selection/edges). Pure — caller dispatches `loadModel`.
 */
export function createNode(
  model: EventModel,
  type: NodeType,
  place: CreatePlacement,
): { model: EventModel; nodeId: string } {
  let next = reducer(model, addActionFor(type))
  const created = next.nodes[next.nodes.length - 1]
  next = placeNode(next, created.id, type, place)
  return { model: next, nodeId: created.id }
}

/**
 * Create the valid successor of `sourceId` (e.g. command → event) AND the typed
 * edge connecting them. Used by drag-a-wire-into-empty-space and the node
 * "Add successor" menu. The edge type comes from getEdgeTypeForConnection so
 * creation and connection rules never disagree.
 */
export function createSuccessor(
  model: EventModel,
  sourceId: string,
  targetType: NodeType,
  place: CreatePlacement,
): { model: EventModel; nodeId: string } {
  const source = model.nodes.find((n) => n.id === sourceId)
  if (!source) return { model, nodeId: '' }
  const edgeType = getEdgeTypeForConnection(source.type, targetType)
  let next = reducer(model, addActionFor(targetType))
  const created = next.nodes[next.nodes.length - 1]
  if (edgeType) {
    next = reducer(next, {
      type: 'addEdge',
      edgeType: edgeType as EdgeType,
      sourceId,
      targetId: created.id,
      sourceHandle: 'bottom',
      targetHandle: 'top',
    })
  }
  next = placeNode(next, created.id, targetType, place)
  return { model: next, nodeId: created.id }
}
