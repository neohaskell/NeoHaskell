import type { NodeType } from '../model/types'

const VALID_CONNECTIONS: [NodeType, NodeType][] = [
  ['uiPlaceholder', 'command'],
  ['command', 'event'],
  ['event', 'query'],
  ['event', 'integration'],
  ['integration', 'command'],
  ['query', 'uiPlaceholder'],
]

export function isConnectionValid(
  sourceType: NodeType,
  targetType: NodeType,
): boolean {
  return VALID_CONNECTIONS.some(
    ([s, t]) => s === sourceType && t === targetType,
  )
}

/**
 * The node types a `sourceType` can validly flow INTO — drives "drag a wire
 * into empty space → spawn the next node" (and the right-click "Add successor"
 * menu). Derived from the same VALID_CONNECTIONS table so creation and
 * connection never disagree. E.g. command → [event]; event → [query, integration].
 */
export function successorsFor(sourceType: NodeType): NodeType[] {
  return VALID_CONNECTIONS.filter(([s]) => s === sourceType).map(([, t]) => t)
}

export function getEdgeTypeForConnection(
  sourceType: NodeType,
  targetType: NodeType,
): string | null {
  if (sourceType === 'command' && targetType === 'event') return 'commandProducesEvent'
  if (sourceType === 'event' && targetType === 'query') return 'eventFeedsQuery'
  if (sourceType === 'event' && targetType === 'integration') return 'eventTriggersIntegration'
  if (sourceType === 'integration' && targetType === 'command') return 'integrationTriggersCommand'
  if (sourceType === 'uiPlaceholder' && targetType === 'command') return 'commandFromUI'
  if (sourceType === 'query' && targetType === 'uiPlaceholder') return 'queryToUI'
  return null
}
