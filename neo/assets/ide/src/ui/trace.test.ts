import { describe, it, expect } from 'vitest'
import type { EventModel } from '../model/types'
import { traceFromNode } from './trace'

// ui(form) -> cmd -> ev -> query, plus ev -> integration -> cmd (a cycle back).
function model(): EventModel {
  return {
    id: 'm',
    name: 'demo',
    submodels: [],
    chapters: [],
    entities: [],
    slices: [],
    nodes: [
      { id: 'ui', type: 'uiPlaceholder', name: 'Form', sliceId: null },
      { id: 'cmd', type: 'command', name: 'Place', entityId: null, sliceId: null },
      { id: 'ev', type: 'event', name: 'Placed', entityId: null, sliceId: null },
      { id: 'q', type: 'query', name: 'View', sliceId: null },
      { id: 'int', type: 'integration', name: 'Email', kind: 'outbound', sliceId: null },
      { id: 'far', type: 'query', name: 'Unrelated', sliceId: null },
    ],
    edges: [
      { id: 'e_ui_cmd', type: 'commandFromUI', sourceId: 'ui', targetId: 'cmd' },
      { id: 'e_cmd_ev', type: 'commandProducesEvent', sourceId: 'cmd', targetId: 'ev' },
      { id: 'e_ev_q', type: 'eventFeedsQuery', sourceId: 'ev', targetId: 'q' },
      { id: 'e_ev_int', type: 'eventTriggersIntegration', sourceId: 'ev', targetId: 'int' },
      { id: 'e_int_cmd', type: 'integrationTriggersCommand', sourceId: 'int', targetId: 'cmd' }, // cycle
    ],
    layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
  }
}

describe('traceFromNode', () => {
  it('highlights only the selected node\'s direct outgoing edges', () => {
    const t = traceFromNode(model(), 'ev')
    // ev's outgoing edges: ev -> q, ev -> int.
    expect(t.edgeIds.has('e_ev_q')).toBe(true)
    expect(t.edgeIds.has('e_ev_int')).toBe(true)
    expect(t.edgeIds.size).toBe(2)
  })

  it('includes the selected node and its direct targets, nothing else', () => {
    const t = traceFromNode(model(), 'ev')
    expect(t.nodeIds.has('ev')).toBe(true) // the node itself
    expect(t.nodeIds.has('q')).toBe(true) // direct target
    expect(t.nodeIds.has('int')).toBe(true) // direct target
    expect(t.nodeIds.has('cmd')).toBe(false) // upstream — not highlighted
    expect(t.nodeIds.has('ui')).toBe(false) // transitive upstream
    expect(t.nodeIds.has('far')).toBe(false) // unrelated
  })

  it('does not follow edges transitively', () => {
    const t = traceFromNode(model(), 'cmd')
    // cmd -> ev is direct; ev -> q is one hop further and must NOT be included.
    expect(t.edgeIds.has('e_cmd_ev')).toBe(true)
    expect(t.edgeIds.has('e_ev_q')).toBe(false)
    expect(t.edgeIds.has('e_ev_int')).toBe(false)
    expect(t.nodeIds.has('ev')).toBe(true)
    expect(t.nodeIds.has('q')).toBe(false)
  })

  it('excludes incoming edges (only arrows that go FROM the node)', () => {
    const t = traceFromNode(model(), 'cmd')
    // cmd has two incoming edges (ui -> cmd, int -> cmd); neither is highlighted.
    expect(t.edgeIds.has('e_ui_cmd')).toBe(false)
    expect(t.edgeIds.has('e_int_cmd')).toBe(false)
    expect(t.nodeIds.has('ui')).toBe(false)
    expect(t.nodeIds.has('int')).toBe(false)
  })

  it('returns just the node when it has no outgoing edges', () => {
    const t = traceFromNode(model(), 'q')
    expect(t.edgeIds.size).toBe(0)
    expect(t.nodeIds.has('q')).toBe(true)
    expect(t.nodeIds.size).toBe(1)
  })

  it('returns an empty trace for null or unknown node', () => {
    expect(traceFromNode(model(), null).nodeIds.size).toBe(0)
    expect(traceFromNode(model(), 'nope').edgeIds.size).toBe(0)
  })
})
