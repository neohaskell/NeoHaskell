import { MarkerType, type Node, type Edge } from '@xyflow/react'
import type { EventModel, ModelNode, Field } from '../model/types'

export interface PositionChange {
  id: string
  x: number
  y: number
}

// Shared empty fields array so field-less nodes get a stable reference (avoids a
// fresh [] per render — matters if node components are ever React.memo'd).
const EMPTY_FIELDS: readonly Field[] = []

function nodeData(
  node: ModelNode,
  onRename?: (nodeId: string, name: string) => void,
): Record<string, unknown> {
  const data: Record<string, unknown> = { label: node.name, fields: node.fields ?? EMPTY_FIELDS }
  if (node.type === 'integration') {
    data.kind = node.kind
  }
  if (onRename) {
    data.onRename = (name: string) => onRename(node.id, name)
  }
  return data
}

export interface ToReactFlowNodeOptions {
  /** Override positions (e.g. a per-feature grid) instead of layout.nodePositions. */
  positions?: Map<string, { x: number; y: number }>
  /** Only emit nodes whose id is in this set (e.g. one feature's members). */
  includeIds?: Set<string>
}

export function toReactFlowNodes(
  model: EventModel,
  onRename?: (nodeId: string, name: string) => void,
  opts?: ToReactFlowNodeOptions,
): Node[] {
  const source = opts?.includeIds
    ? model.nodes.filter((n) => opts.includeIds!.has(n.id))
    : model.nodes
  return source.map((node) => ({
    id: node.id,
    type: node.type,
    position:
      opts?.positions?.get(node.id) ?? model.layout.nodePositions[node.id] ?? { x: 0, y: 0 },
    data: nodeData(node, onRename),
  }))
}

export function toReactFlowEdges(model: EventModel): Edge[] {
  return model.edges.map((edge) => ({
    id: edge.id,
    source: edge.sourceId,
    target: edge.targetId,
    sourceHandle: edge.sourceHandle ?? undefined,
    targetHandle: edge.targetHandle ?? undefined,
    type: 'default',
    style: { strokeWidth: 3, stroke: 'var(--em-edge)' },
    markerEnd: { type: MarkerType.ArrowClosed, width: 12, height: 12, color: 'var(--em-edge)' },
  }))
}

export function applyPositionChanges(
  model: EventModel,
  changes: PositionChange[],
): EventModel {
  const updatedPositions = { ...model.layout.nodePositions }
  for (const change of changes) {
    updatedPositions[change.id] = { x: change.x, y: change.y }
  }
  return {
    ...model,
    layout: { ...model.layout, nodePositions: updatedPositions },
  }
}
