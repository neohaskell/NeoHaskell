import type { EventModel } from '../model/types'

// Direct-outgoing highlighting for the "highlight a selected node's edges"
// feature. Selecting a node highlights ONLY the arrows that leave it
// (source === selected node) — its 1-hop outgoing edges — plus the nodes those
// arrows point at, so the immediate targets stay legible while everything else
// dims. Transitive following (the whole upstream + downstream causal path) is
// deliberately gone: a selection answers "what does this node directly
// trigger?", not "what is the entire flow it sits on?". Cycles are a non-issue
// because we never recurse.

export interface Trace {
  /** The selected node plus the direct targets of its outgoing edges. */
  nodeIds: Set<string>
  /** The selected node's direct (1-hop) outgoing edges. */
  edgeIds: Set<string>
}

export function emptyTrace(): Trace {
  return { nodeIds: new Set(), edgeIds: new Set() }
}

/**
 * Highlight the selected node's direct outgoing edges (1-hop, not transitive).
 * Returns the selected node plus the immediate targets of those edges, and the
 * outgoing edge ids. Returns an empty trace if `nodeId` is null/unknown.
 */
export function traceFromNode(model: EventModel, nodeId: string | null): Trace {
  const trace = emptyTrace()
  if (!nodeId) return trace
  const exists = model.nodes.some((n) => n.id === nodeId)
  if (!exists) return trace

  trace.nodeIds.add(nodeId)
  for (const e of model.edges) {
    if (e.sourceId !== nodeId) continue
    trace.edgeIds.add(e.id)
    trace.nodeIds.add(e.targetId)
  }

  return trace
}
