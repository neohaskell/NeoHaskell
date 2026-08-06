import { describe, it, expect } from 'vitest'
import type { EventModel, ModelEdge } from '../model/types'
import { buildNodeSubmodelMap } from './layout/submodels'
import { computeFeatureGrid } from './layout/bandGrid'
import {
  UNGROUPED_FEATURE,
  classifyEdgeLocality,
  pageOf,
  planFeatureEdges,
  buildFeatureRenderEdges,
  type FeatureId,
} from './featurePages'

// Two features (smA, smB) plus ungrouped content. Edges:
//   e1  cmdA -> evA   (intra smA)
//   e2  evA  -> qB    (cross  smA -> smB)
//   e3  evU  -> qB    (cross  ungrouped -> smB)
function model(): EventModel {
  return {
    id: 'm',
    name: 'demo',
    submodels: [
      { id: 'smA', name: 'Checkout', order: 0 },
      { id: 'smB', name: 'Billing', order: 1 },
    ],
    chapters: [
      { id: 'cA', name: 'A', order: 0, submodelId: 'smA' },
      { id: 'cB', name: 'B', order: 1, submodelId: 'smB' },
      { id: 'cU', name: 'U', order: 2, submodelId: null },
    ],
    entities: [{ id: 'eX', name: 'X', order: 0 }],
    slices: [
      { id: 'sA', name: 'sA', chapterId: 'cA', order: 0 },
      { id: 'sB', name: 'sB', chapterId: 'cB', order: 1 },
      { id: 'sU', name: 'sU', chapterId: 'cU', order: 2 },
    ],
    nodes: [
      { id: 'cmdA', type: 'command', name: 'CmdA', entityId: 'eX', sliceId: 'sA' },
      { id: 'evA', type: 'event', name: 'EvA', entityId: 'eX', sliceId: 'sA' },
      { id: 'qB', type: 'query', name: 'QB', sliceId: 'sB' },
      { id: 'evU', type: 'event', name: 'EvU', entityId: 'eX', sliceId: 'sU' },
    ],
    edges: [
      { id: 'e1', type: 'commandProducesEvent', sourceId: 'cmdA', targetId: 'evA' },
      { id: 'e2', type: 'eventFeedsQuery', sourceId: 'evA', targetId: 'qB' },
      { id: 'e3', type: 'eventFeedsQuery', sourceId: 'evU', targetId: 'qB' },
    ],
    layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
  }
}

describe('pageOf / featureOfNode', () => {
  it('resolves a node to its submodel via slice -> chapter -> submodel', () => {
    expect(pageOf(model(), 'cmdA')).toBe('smA')
    expect(pageOf(model(), 'qB')).toBe('smB')
  })

  it('maps a node with no (or broken) submodel chain to UNGROUPED', () => {
    expect(pageOf(model(), 'evU')).toBe(UNGROUPED_FEATURE)
    expect(pageOf(model(), 'does-not-exist')).toBe(UNGROUPED_FEATURE)
  })
})

describe('classifyEdgeLocality', () => {
  it('marks an edge whose endpoints share a feature as intra', () => {
    const e = model().edges.find((x) => x.id === 'e1')!
    const loc = classifyEdgeLocality(model(), e)
    expect(loc).toEqual({ sourceFeature: 'smA', targetFeature: 'smA', crossesFeature: false })
  })

  it('marks an edge across two submodels as crossing', () => {
    const e = model().edges.find((x) => x.id === 'e2')!
    expect(classifyEdgeLocality(model(), e)).toEqual({
      sourceFeature: 'smA',
      targetFeature: 'smB',
      crossesFeature: true,
    })
  })

  it('treats ungrouped <-> submodel as crossing', () => {
    const e = model().edges.find((x) => x.id === 'e3')!
    expect(classifyEdgeLocality(model(), e)).toEqual({
      sourceFeature: UNGROUPED_FEATURE,
      targetFeature: 'smB',
      crossesFeature: true,
    })
  })
})

describe('planFeatureEdges', () => {
  const m = model()
  const map = buildNodeSubmodelMap(m)

  it('smA: e1 intra, e2 outgoing, nothing incoming', () => {
    const plan = planFeatureEdges(m, 'smA', map)
    expect(plan.intra.map((e) => e.id)).toEqual(['e1'])
    expect(plan.outgoing.map((e) => e.id)).toEqual(['e2'])
    expect(plan.incoming).toEqual([])
  })

  it('smB: both cross edges arrive as incoming', () => {
    const plan = planFeatureEdges(m, 'smB', map)
    expect(plan.intra).toEqual([])
    expect(plan.outgoing).toEqual([])
    expect(plan.incoming.map((e) => e.id).sort()).toEqual(['e2', 'e3'])
  })

  it('ungrouped: e3 outgoing', () => {
    const plan = planFeatureEdges(m, UNGROUPED_FEATURE, map)
    expect(plan.outgoing.map((e) => e.id)).toEqual(['e3'])
  })
})

describe('no-edge-lost invariant (MUST)', () => {
  it('every edge is represented on every feature it touches — never silently dropped', () => {
    const m = model()
    const map = buildNodeSubmodelMap(m)
    const features: FeatureId[] = [...m.submodels.map((s) => s.id), UNGROUPED_FEATURE]

    // Tally how each edge is rendered across ALL feature screens.
    const seen = new Map<string, { intra: number; out: number; in: number }>()
    const bump = (id: string, kind: 'intra' | 'out' | 'in') => {
      const t = seen.get(id) ?? { intra: 0, out: 0, in: 0 }
      t[kind] += 1
      seen.set(id, t)
    }
    for (const f of features) {
      const plan = planFeatureEdges(m, f, map)
      plan.intra.forEach((e) => bump(e.id, 'intra'))
      plan.outgoing.forEach((e) => bump(e.id, 'out'))
      plan.incoming.forEach((e) => bump(e.id, 'in'))
    }

    // EVERY model edge must appear at least once across the screens.
    for (const e of m.edges) {
      expect(seen.has(e.id), `edge ${e.id} is rendered on at least one screen`).toBe(true)
    }
    // An intra edge appears once; a cross edge appears as exactly one OUT (on
    // its source feature) and one IN (on its target feature).
    expect(seen.get('e1')).toEqual({ intra: 1, out: 0, in: 0 })
    expect(seen.get('e2')).toEqual({ intra: 0, out: 1, in: 1 })
    expect(seen.get('e3')).toEqual({ intra: 0, out: 1, in: 1 })
  })
})

describe('buildFeatureRenderEdges', () => {
  const m = model()
  const map = buildNodeSubmodelMap(m)
  const name = (fid: FeatureId) =>
    fid === UNGROUPED_FEATURE ? 'Ungrouped' : m.submodels.find((s) => s.id === fid)?.name ?? 'Feature'

  it('renders intra edges directly and outgoing cross-edges to an OUT portal', () => {
    const grid = computeFeatureGrid(m, 'smA')
    const { edges, portalNodes } = buildFeatureRenderEdges(m, 'smA', grid, map, name)

    // e1 intra (cmdA -> evA), e2 outgoing (evA -> portal).
    expect(edges).toHaveLength(2)
    const e2 = edges.find((e) => e.id === 'e2')!
    expect(e2.source).toBe('evA')
    expect(e2.target).toBe('__portal-out-e2')

    expect(portalNodes).toHaveLength(1)
    const portal = portalNodes[0]
    expect(portal.id).toBe('__portal-out-e2')
    expect(portal.type).toBe('boundaryPortal')
    expect((portal.data as { direction: string }).direction).toBe('out')
    expect((portal.data as { featureName: string }).featureName).toBe('Billing')
    expect((portal.data as { label: string }).label).toBe('QB') // the off-feature node's name
  })

  it('renders incoming cross-edges from an IN portal on the target feature', () => {
    const grid = computeFeatureGrid(m, 'smB')
    const { edges, portalNodes } = buildFeatureRenderEdges(m, 'smB', grid, map, name)

    // Two incoming edges (e2 from smA, e3 from ungrouped), each via an IN portal.
    expect(edges.map((e) => e.id).sort()).toEqual(['e2', 'e3'])
    for (const e of edges) expect(e.target).toBe('qB')
    expect(portalNodes).toHaveLength(2)
    expect(portalNodes.every((p) => (p.data as { direction: string }).direction === 'in')).toBe(true)
  })

  it('navigating a portal calls onNavigate with the OTHER feature id', () => {
    const grid = computeFeatureGrid(m, 'smA')
    let navigatedTo: string | null = null
    const { portalNodes } = buildFeatureRenderEdges(m, 'smA', grid, map, name, (fid) => {
      navigatedTo = fid
    })
    ;(portalNodes[0].data as { onNavigate: () => void }).onNavigate()
    expect(navigatedTo).toBe('smB')
  })
})

// A cross-feature edge must be representable even if heal omits an endpoint's
// position — the portal still renders (proves no reliance on node positions).
describe('portal robustness', () => {
  it('builds portals when the off-feature endpoint has no grid position', () => {
    const m: EventModel = model()
    const grid = computeFeatureGrid(m, 'smA')
    const map = buildNodeSubmodelMap(m)
    const out = buildFeatureRenderEdges(m, 'smA', grid, map, () => 'Billing')
    expect(out.portalNodes.length).toBeGreaterThan(0)
  })
})
