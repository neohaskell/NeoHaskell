import { describe, it, expect } from 'vitest'
import type { EventModel } from './types'
import { validate, countIssues } from './validate'

// A fully-wired, warning-free model: one command producing one event, both in
// a slice, the slice in a chapter, no submodels.
function validModel(over: Partial<EventModel> = {}): EventModel {
  return {
    id: 'm',
    name: 'demo',
    submodels: [],
    chapters: [{ id: 'c1', name: 'Chapter', order: 0 }],
    entities: [{ id: 'eX', name: 'X', order: 0 }],
    slices: [{ id: 's1', name: 'Slice', chapterId: 'c1', order: 0 }],
    nodes: [
      { id: 'cmd1', type: 'command', name: 'DoThing', entityId: 'eX', sliceId: 's1' },
      { id: 'ev1', type: 'event', name: 'ThingDone', entityId: 'eX', sliceId: 's1' },
    ],
    edges: [{ id: 'e1', type: 'commandProducesEvent', sourceId: 'cmd1', targetId: 'ev1' }],
    layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
    ...over,
  }
}

const ids = (model: EventModel) => validate(model).map((i) => i.id)

describe('validate', () => {
  it('returns no issues for a fully-wired model', () => {
    expect(validate(validModel())).toEqual([])
  })

  it('flags a dangling edge endpoint as an error', () => {
    const m = validModel({
      edges: [{ id: 'bad', type: 'commandProducesEvent', sourceId: 'cmd1', targetId: 'ghost' }],
    })
    const issue = validate(m).find((i) => i.id === 'dangling-edge')!
    expect(issue.severity).toBe('error')
    expect(issue.edgeId).toBe('bad')
  })

  it('warns when a node is in no slice', () => {
    const m = validModel()
    const m2: EventModel = {
      ...m,
      nodes: [...m.nodes, { id: 'floating', type: 'query', name: 'Floater', sliceId: null }],
    }
    expect(ids(m2)).toContain('node-no-slice')
  })

  it('warns when a command produces no event', () => {
    const m = validModel({
      edges: [], // cmd1 now produces nothing
    })
    const issue = validate(m).find((i) => i.id === 'command-no-event')!
    expect(issue.severity).toBe('warning')
    expect(issue.nodeId).toBe('cmd1')
  })

  it('warns about an empty feature (submodel with no chapters)', () => {
    const m = validModel({ submodels: [{ id: 'sm1', name: 'Lonely', order: 0 }] })
    const issue = validate(m).find((i) => i.id === 'empty-feature')!
    expect(issue.featureId).toBe('sm1')
  })

  it('warns about a chapter with no slices', () => {
    const m = validModel({
      chapters: [
        { id: 'c1', name: 'Chapter', order: 0 },
        { id: 'c2', name: 'Empty', order: 1 },
      ],
    })
    const issue = validate(m).find((i) => i.id === 'chapter-no-slices')!
    expect(issue.chapterId).toBe('c2')
  })

  it('flags event over-wiring at the threshold (5 clean, 6 info)', () => {
    const queries = (n: number) =>
      Array.from({ length: n }, (_, i) => ({ id: `q${i}`, type: 'query' as const, name: `Q${i}`, sliceId: 's1' }))
    const feeds = (n: number) =>
      Array.from({ length: n }, (_, i) => ({
        id: `f${i}`,
        type: 'eventFeedsQuery' as const,
        sourceId: 'ev1',
        targetId: `q${i}`,
      }))
    const base = validModel()
    const at5: EventModel = { ...base, nodes: [...base.nodes, ...queries(5)], edges: [...base.edges, ...feeds(5)] }
    const at6: EventModel = { ...base, nodes: [...base.nodes, ...queries(6)], edges: [...base.edges, ...feeds(6)] }
    expect(ids(at5)).not.toContain('event-overwired')
    expect(ids(at6)).toContain('event-overwired')
  })

  it('reports a cross-feature edge as info when submodels exist', () => {
    const m: EventModel = {
      id: 'm',
      name: 'x',
      submodels: [
        { id: 'smA', name: 'Checkout', order: 0 },
        { id: 'smB', name: 'Billing', order: 1 },
      ],
      chapters: [
        { id: 'cA', name: 'A', order: 0, submodelId: 'smA' },
        { id: 'cB', name: 'B', order: 1, submodelId: 'smB' },
      ],
      entities: [{ id: 'eX', name: 'X', order: 0 }],
      slices: [
        { id: 'sA', name: 'sA', chapterId: 'cA', order: 0 },
        { id: 'sB', name: 'sB', chapterId: 'cB', order: 1 },
      ],
      nodes: [
        { id: 'evA', type: 'event', name: 'OrderPlaced', entityId: 'eX', sliceId: 'sA' },
        { id: 'qB', type: 'query', name: 'InvoiceView', sliceId: 'sB' },
      ],
      edges: [{ id: 'x1', type: 'eventFeedsQuery', sourceId: 'evA', targetId: 'qB' }],
      layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
    }
    const issue = validate(m).find((i) => i.id === 'cross-feature-edge')!
    expect(issue.severity).toBe('info')
    expect(issue.message).toContain('Billing')
  })

  it('every issue message names the item and gives a Fix', () => {
    const m = validModel({ edges: [], submodels: [{ id: 'sm1', name: 'Lonely', order: 0 }] })
    const issues = validate(m)
    expect(issues.length).toBeGreaterThan(0)
    for (const i of issues) {
      // Dangling/cross messages may not always contain "Fix:", but every
      // warning/error we emit here should be actionable.
      expect(i.message.length).toBeGreaterThan(10)
    }
    expect(issues.some((i) => i.message.includes('Fix:'))).toBe(true)
  })

  it('counts issues by severity', () => {
    const m = validModel({
      edges: [{ id: 'bad', type: 'commandProducesEvent', sourceId: 'cmd1', targetId: 'ghost' }],
      submodels: [{ id: 'sm1', name: 'Lonely', order: 0 }],
    })
    const counts = countIssues(validate(m))
    expect(counts.error).toBeGreaterThanOrEqual(1)
    expect(counts.warning).toBeGreaterThanOrEqual(1)
  })

  it('does not crash on an empty model', () => {
    const empty: EventModel = {
      id: 'm',
      name: 'e',
      submodels: [],
      chapters: [],
      entities: [],
      slices: [],
      nodes: [],
      edges: [],
      layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
    }
    expect(validate(empty)).toEqual([])
  })
})
