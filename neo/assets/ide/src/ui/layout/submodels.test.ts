import { describe, it, expect } from 'vitest'
import type { EventModel } from '../../model/types'
import {
  stackSubmodels,
  computeSubmodelBands,
  submodelsInUse,
  buildNodeSubmodelMap,
} from './submodels'

// Two features (submodels A, B), each owning one chapter → slice → command,
// laid out side by side on one timeline. An ungrouped command sits off to the
// side. Used to prove the vertical-stacking reflow.
function fixture(opts?: { assign?: boolean; withUngrouped?: boolean }): EventModel {
  const assign = opts?.assign ?? true
  return {
    id: 'm',
    name: 'T',
    submodels: [
      { id: 'smA', name: 'Checkout', order: 0 },
      { id: 'smB', name: 'Fulfilment', order: 1 },
    ],
    chapters: [
      { id: 'cA', name: 'CA', order: 0, submodelId: assign ? 'smA' : null },
      { id: 'cB', name: 'CB', order: 1, submodelId: assign ? 'smB' : null },
    ],
    entities: [],
    slices: [
      { id: 'sA', name: 'SA', chapterId: 'cA', order: 0 },
      { id: 'sB', name: 'SB', chapterId: 'cB', order: 1 },
      { id: 'sU', name: 'SU', chapterId: null, order: 2 },
    ],
    nodes: [
      { id: 'nA', type: 'command', name: 'A', entityId: null, sliceId: 'sA' },
      { id: 'nB', type: 'command', name: 'B', entityId: null, sliceId: 'sB' },
      ...(opts?.withUngrouped
        ? [{ id: 'nU', type: 'command' as const, name: 'U', entityId: null, sliceId: 'sU' }]
        : []),
    ],
    edges: [],
    layout: {
      nodePositions: {
        nA: { x: 500, y: 100 },
        nB: { x: 900, y: 100 },
        ...(opts?.withUngrouped ? { nU: { x: 50, y: 60 } } : {}),
      },
      viewport: { x: 0, y: 0, zoom: 1 },
    },
  }
}

describe('buildNodeSubmodelMap', () => {
  it('resolves node → submodel through slice → chapter', () => {
    const map = buildNodeSubmodelMap(fixture())
    expect(map.get('nA')).toBe('smA')
    expect(map.get('nB')).toBe('smB')
  })

  it('returns null for ungrouped nodes', () => {
    const map = buildNodeSubmodelMap(fixture({ withUngrouped: true }))
    expect(map.get('nU')).toBeNull()
  })
})

describe('submodelsInUse', () => {
  it('is false when no chapter is assigned', () => {
    expect(submodelsInUse(fixture({ assign: false }))).toBe(false)
  })
  it('is true once a chapter has a submodelId', () => {
    expect(submodelsInUse(fixture({ assign: true }))).toBe(true)
  })
})

describe('stackSubmodels', () => {
  it('left-aligns each submodel and stacks them vertically', () => {
    const adj = stackSubmodels(fixture())
    const byId = new Map(adj.map((a) => [a.nodeId, a]))
    const a = byId.get('nA')!
    const b = byId.get('nB')!
    // Both bands normalise their leftmost node to x = 0.
    expect(a.x).toBe(0)
    expect(b.x).toBe(0)
    // First submodel sits at the top; the second is well below it.
    expect(a.y).toBe(0)
    expect(b.y).toBeGreaterThan(a.y + 200)
  })

  it('leaves ungrouped nodes untouched and starts bands below them', () => {
    const adj = stackSubmodels(fixture({ withUngrouped: true }))
    expect(adj.find((a) => a.nodeId === 'nU')).toBeUndefined()
    // Submodel A no longer starts at y=0 — it begins below the ungrouped node.
    const a = adj.find((a) => a.nodeId === 'nA')!
    expect(a.y).toBeGreaterThan(0)
  })

  it('is a no-op when no chapter is assigned to a submodel', () => {
    expect(stackSubmodels(fixture({ assign: false }))).toEqual([])
  })
})

describe('computeSubmodelBands', () => {
  it('emits one band per submodel that owns positioned nodes', () => {
    const bands = computeSubmodelBands(fixture())
    expect(bands.map((b) => b.submodelId).sort()).toEqual(['smA', 'smB'])
    for (const band of bands) {
      expect(band.width).toBeGreaterThan(0)
      expect(band.height).toBeGreaterThan(0)
    }
  })

  it('omits submodels with no member nodes', () => {
    const model = fixture()
    // Detach chapter B from its submodel → smB has no members.
    const detached: EventModel = {
      ...model,
      chapters: model.chapters.map((c) => (c.id === 'cB' ? { ...c, submodelId: null } : c)),
    }
    const bands = computeSubmodelBands(detached)
    expect(bands.map((b) => b.submodelId)).toEqual(['smA'])
  })
})
