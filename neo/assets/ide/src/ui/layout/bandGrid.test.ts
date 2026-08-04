import { describe, it, expect } from 'vitest'
import type { EventModel, ModelNode } from '../../model/types'
import {
  computePerBandGrids,
  computeFeatureGrid,
  reflowBands,
  resolveBandAtY,
  buildPerBandGridNodes,
} from './bandGrid'

// Two submodels that SHARE entity "User": Onboarding uses {User, Acct},
// Billing uses {User, Inv}. Proves compact, duplicated, gap-free rows.
function twoSubmodelModel(extra?: Partial<EventModel>): EventModel {
  const nodes: ModelNode[] = [
    { id: 'n1', type: 'command', name: 'Signup', entityId: 'User', sliceId: 's1' },
    { id: 'n2', type: 'event', name: 'SignedUp', entityId: 'User', sliceId: 's1' },
    { id: 'n3', type: 'event', name: 'AcctOpened', entityId: 'Acct', sliceId: 's2' },
    { id: 'n4', type: 'command', name: 'Charge', entityId: 'User', sliceId: 's3' },
    { id: 'n5', type: 'event', name: 'Charged', entityId: 'User', sliceId: 's3' },
    { id: 'n6', type: 'event', name: 'Invoiced', entityId: 'Inv', sliceId: 's4' },
  ]
  return {
    id: 'm',
    name: 'demo',
    entities: [
      { id: 'User', name: 'User', order: 0 },
      { id: 'Acct', name: 'Acct', order: 1 },
      { id: 'Inv', name: 'Inv', order: 2 },
    ],
    submodels: [
      { id: 'smOnb', name: 'Onboarding', order: 0 },
      { id: 'smBill', name: 'Billing', order: 1 },
    ],
    chapters: [
      { id: 'ch1', name: 'Onb', order: 0, submodelId: 'smOnb' },
      { id: 'ch2', name: 'Bill', order: 1, submodelId: 'smBill' },
    ],
    slices: [
      { id: 's1', name: 'Signup', chapterId: 'ch1', order: 0 },
      { id: 's2', name: 'OpenAcct', chapterId: 'ch1', order: 1 },
      { id: 's3', name: 'Charge', chapterId: 'ch2', order: 2 },
      { id: 's4', name: 'Invoice', chapterId: 'ch2', order: 3 },
    ],
    nodes,
    edges: [],
    layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
    ...extra,
  }
}

function applyAdjustments(model: EventModel): EventModel {
  const next = { ...model.layout.nodePositions }
  for (const a of reflowBands(model)) next[a.nodeId] = { x: a.x, y: a.y }
  return { ...model, layout: { ...model.layout, nodePositions: next } }
}

describe('computePerBandGrids', () => {
  it('produces one band per non-empty submodel, in order', () => {
    const grids = computePerBandGrids(twoSubmodelModel())
    expect(grids.map((g) => g.submodelId)).toEqual(['smOnb', 'smBill'])
  })

  it('each_band_columns_start_at_local_x_zero', () => {
    const grids = computePerBandGrids(twoSubmodelModel())
    for (const g of grids) expect(g.slices[0].xStart).toBe(0)
  })

  it('slice_columns_ordered_by_slice_order', () => {
    const grids = computePerBandGrids(twoSubmodelModel())
    expect(grids[0].slices.map((s) => s.sliceId)).toEqual(['s1', 's2'])
    expect(grids[1].slices.map((s) => s.sliceId)).toEqual(['s3', 's4'])
    // contiguous, non-overlapping columns
    expect(grids[0].slices[1].xStart).toBe(
      grids[0].slices[0].xStart + grids[0].slices[0].width,
    )
  })

  it('entities_compact_consecutive_rows_no_global_gap', () => {
    const [onb] = computePerBandGrids(twoSubmodelModel())
    expect(onb.lanes.map((l) => l.entityId)).toEqual(['User', 'Acct'])
    // Acct row begins exactly where the User row ends — no Inv-shaped gap.
    expect(onb.lanes[1].yStart).toBe(onb.lanes[0].yStart + onb.lanes[0].height)
  })

  it('entity_duplicated_across_bands', () => {
    const [onb, bill] = computePerBandGrids(twoSubmodelModel())
    expect(onb.lanes.some((l) => l.entityId === 'User')).toBe(true)
    expect(bill.lanes.some((l) => l.entityId === 'User')).toBe(true)
    // ...but each band only shows its OWN second entity.
    expect(onb.lanes.some((l) => l.entityId === 'Acct')).toBe(true)
    expect(onb.lanes.some((l) => l.entityId === 'Inv')).toBe(false)
    expect(bill.lanes.some((l) => l.entityId === 'Inv')).toBe(true)
    expect(bill.lanes.some((l) => l.entityId === 'Acct')).toBe(false)
  })

  it('command_only_entity_gets_no_lane', () => {
    // A band whose only node for entity "Ghost" is a command (no event).
    const m = twoSubmodelModel()
    const nodes: ModelNode[] = [
      ...m.nodes,
      { id: 'g', type: 'command', name: 'GhostCmd', entityId: 'Ghost', sliceId: 's1' },
    ]
    const model: EventModel = {
      ...m,
      entities: [...m.entities, { id: 'Ghost', name: 'Ghost', order: 9 }],
      nodes,
    }
    const [onb] = computePerBandGrids(model)
    expect(onb.lanes.some((l) => l.entityId === 'Ghost')).toBe(false)
  })

  it('command_query_integration_in_band_top_band and event_in_band_entity_lane', () => {
    const [onb] = computePerBandGrids(twoSubmodelModel())
    const cmd = onb.positions.get('n1')!
    const evt = onb.positions.get('n2')!
    // event sits inside its entity lane (User lane yStart + EVENT_LANE_INSET 60)
    const userLane = onb.lanes.find((l) => l.entityId === 'User')!
    expect(evt.y).toBe(userLane.yStart + 60)
    // command band sits in the open region ABOVE the entity lanes.
    expect(cmd.y).toBeGreaterThan(onb.yOrigin)
    expect(cmd.y).toBeLessThan(userLane.yStart)
  })

  it('uiPlaceholder_above_band', () => {
    const m = twoSubmodelModel()
    const model: EventModel = {
      ...m,
      nodes: [...m.nodes, { id: 'ui', type: 'uiPlaceholder', name: 'Form', sliceId: 's1' }],
    }
    const [onb] = computePerBandGrids(model)
    // UI placeholder sits in the open region, above the command band (n1, s1).
    const uiY = onb.positions.get('ui')!.y
    expect(uiY).toBeGreaterThan(onb.yOrigin)
    expect(uiY).toBeLessThanOrEqual(onb.positions.get('n1')!.y)
  })

  it('ungrouped_region_stays_on_top', () => {
    const m = twoSubmodelModel()
    // An ungrouped node (its chapter has no submodel) sitting at y=500.
    const model: EventModel = {
      ...m,
      chapters: [...m.chapters, { id: 'chFree', name: 'Free', order: 5, submodelId: null }],
      slices: [...m.slices, { id: 's9', name: 'Free', chapterId: 'chFree', order: 9 }],
      nodes: [...m.nodes, { id: 'free', type: 'event', name: 'FreeEvt', entityId: 'User', sliceId: 's9' }],
      layout: { ...m.layout, nodePositions: { free: { x: 10, y: 500 } } },
    }
    const grids = computePerBandGrids(model)
    // bands start below the ungrouped region; the ungrouped node is not moved.
    expect(grids[0].yOrigin).toBeGreaterThan(500)
    expect(reflowBands(model).some((a) => a.nodeId === 'free')).toBe(false)
  })

  it('band_height_is_deterministic_not_from_positions', () => {
    const a = computePerBandGrids(twoSubmodelModel())
    // Same structure, wildly different incoming node positions.
    const scattered = twoSubmodelModel({
      layout: {
        nodePositions: { n1: { x: 9000, y: 9000 }, n6: { x: -500, y: 4000 } },
        viewport: { x: 0, y: 0, zoom: 1 },
      },
    })
    const b = computePerBandGrids(scattered)
    expect(b.map((g) => g.rect.height)).toEqual(a.map((g) => g.rect.height))
  })

  it('bands_do_not_vertically_overlap', () => {
    const [onb, bill] = computePerBandGrids(twoSubmodelModel())
    expect(bill.rect.yStart).toBeGreaterThanOrEqual(onb.rect.yStart + onb.rect.height)
  })
})

describe('reflowBands', () => {
  it('reflow_noop_when_no_submodel_assigned', () => {
    const m = twoSubmodelModel()
    const noSub: EventModel = {
      ...m,
      submodels: [],
      chapters: m.chapters.map((c) => ({ ...c, submodelId: null })),
    }
    expect(reflowBands(noSub)).toEqual([])
  })

  it('reflow_idempotent_fixed_point', () => {
    const once = applyAdjustments(twoSubmodelModel())
    expect(reflowBands(once)).toEqual([])
  })

  it('reflow_determinism_under_input_shuffle', () => {
    const m = twoSubmodelModel()
    const shuffled: EventModel = {
      ...m,
      nodes: [...m.nodes].reverse(),
      slices: [...m.slices].reverse(),
      entities: [...m.entities].reverse(),
      submodels: [...m.submodels].reverse(),
      chapters: [...m.chapters].reverse(),
    }
    const key = (a: { nodeId: string; x: number; y: number }) => `${a.nodeId}:${a.x},${a.y}`
    expect(reflowBands(shuffled).map(key).sort()).toEqual(reflowBands(m).map(key).sort())
  })
})

describe('resolveBandAtY', () => {
  it('resolveBandAtY_maps_y_to_correct_band', () => {
    const grids = computePerBandGrids(twoSubmodelModel())
    const [onb, bill] = grids
    expect(resolveBandAtY(grids, onb.yOrigin + 130)?.submodelId).toBe('smOnb')
    expect(resolveBandAtY(grids, bill.yOrigin + 130)?.submodelId).toBe('smBill')
    expect(resolveBandAtY(grids, -9999)).toBeNull()
  })
})

describe('buildPerBandGridNodes', () => {
  it('emits a band rect + per-band columns + per-band (duplicated) lanes', () => {
    const m = twoSubmodelModel()
    const nodes = buildPerBandGridNodes(computePerBandGrids(m), {
      entityName: new Map(m.entities.map((e) => [e.id, e.name])),
      sliceName: new Map(m.slices.map((s) => [s.id, s.name])),
    })
    const byType = (t: string) => nodes.filter((n) => n.type === t)
    expect(byType('submodelBand')).toHaveLength(2) // one rect per band
    expect(byType('sliceColumn')).toHaveLength(4) // 2 slices per band
    expect(byType('entityLane')).toHaveLength(4) // {User,Acct} + {User,Inv}
    // The shared entity "User" gets a lane in BOTH bands (duplicated).
    const userLanes = byType('entityLane').filter(
      (n) => (n.data as { label: string }).label === 'User',
    )
    expect(userLanes).toHaveLength(2)
  })
})

describe('computeFeatureGrid', () => {
  it('lays a single submodel out at the origin (yOrigin 0)', () => {
    const grid = computeFeatureGrid(twoSubmodelModel(), 'smOnb')
    expect(grid.submodelId).toBe('smOnb')
    expect(grid.name).toBe('Onboarding')
    expect(grid.yOrigin).toBe(0)
    expect(grid.rect.yStart).toBe(0)
    expect(grid.slices.map((s) => s.sliceId)).toEqual(['s1', 's2'])
    expect(grid.lanes.map((l) => l.entityId)).toEqual(['User', 'Acct'])
    // command band sits in the open region above the first entity lane.
    expect(grid.positions.get('n1')!.y).toBeGreaterThan(0)
    expect(grid.positions.get('n1')!.y).toBeLessThan(grid.lanes[0].yStart)
    expect(grid.lanes[0].yStart).toBeGreaterThan(0)
  })

  it('only includes the requested feature’s members', () => {
    const grid = computeFeatureGrid(twoSubmodelModel(), 'smOnb')
    // smOnb owns n1 (cmd), n2 (evt s1), n3 (evt s2) — not Billing's n4/n5/n6.
    expect([...grid.positions.keys()].sort()).toEqual(['n1', 'n2', 'n3'])
  })

  it('matches computePerBandGrids slice order for the same submodel', () => {
    const m = twoSubmodelModel()
    const banded = computePerBandGrids(m).find((g) => g.submodelId === 'smBill')!
    const single = computeFeatureGrid(m, 'smBill')
    expect(single.slices.map((s) => s.sliceId)).toEqual(banded.slices.map((s) => s.sliceId))
  })

  it('null featureId lays out the ungrouped region with the sentinel id', () => {
    const m = twoSubmodelModel()
    const model = {
      ...m,
      chapters: [...m.chapters, { id: 'chFree', name: 'Free', order: 5, submodelId: null }],
      slices: [...m.slices, { id: 's9', name: 'Free', chapterId: 'chFree', order: 9 }],
      nodes: [...m.nodes, { id: 'free', type: 'event' as const, name: 'FreeEvt', entityId: 'User', sliceId: 's9' }],
    }
    const grid = computeFeatureGrid(model, null)
    expect(grid.submodelId).toBe('__ungrouped__')
    expect(grid.name).toBe('Ungrouped')
    expect([...grid.positions.keys()]).toEqual(['free'])
  })

  it('an empty feature yields a valid (empty) grid at origin', () => {
    const m = twoSubmodelModel()
    // No ungrouped content in the base model.
    const grid = computeFeatureGrid(m, null)
    expect(grid.positions.size).toBe(0)
    expect(grid.slices).toEqual([])
    expect(grid.yOrigin).toBe(0)
  })

  it('places command/query/integration side by side on one level, widening the column', () => {
    const m = twoSubmodelModel()
    const model: EventModel = {
      ...m,
      // Slice s1 (Onboarding) already has command n1; add a query + integration.
      nodes: [
        ...m.nodes,
        { id: 'q1', type: 'query', name: 'Lookup', sliceId: 's1' },
        { id: 'i1', type: 'integration', name: 'Email', kind: 'outbound', sliceId: 's1' },
      ],
    }
    const grid = computeFeatureGrid(model, 'smOnb')
    const cmd = grid.positions.get('n1')!
    const q = grid.positions.get('q1')!
    const i = grid.positions.get('i1')!
    // Same horizontal level (the command band y) — above the lanes.
    expect(q.y).toBe(cmd.y)
    expect(i.y).toBe(cmd.y)
    expect(cmd.y).toBeLessThan(grid.lanes[0].yStart)
    // Laid out side by side: command → query → integration.
    expect(q.x).toBeGreaterThan(cmd.x)
    expect(i.x).toBeGreaterThan(q.x)
    // The column widened to host all three.
    const s1 = grid.slices.find((s) => s.sliceId === 's1')!
    expect(s1.width).toBeGreaterThan(i.x - cmd.x)
  })
})

// The deterministic grid is the SINGLE source of truth for node positions in
// feature/page mode: a node's x is always its slice column + padding, so when a
// slice is pushed (a sibling grows, slices reorder) every node moves WITH its
// column. This is the regression guard for "slices move but nodes stay put".
describe('node follows its slice column', () => {
  // SLICE_PADDING in bandGrid.ts (event/UI nodes sit at colX + this).
  const SLICE_PADDING = 40

  it('every_node_x_aligns_with_its_slice_column', () => {
    const grid = computeFeatureGrid(twoSubmodelModel(), 'smOnb')
    const colX = new Map(grid.slices.map((s) => [s.sliceId, s.xStart]))
    // n2 (event, s1) and n3 (event, s2) each sit at their column + padding.
    for (const [id, sliceId] of [['n2', 's1'], ['n3', 's2']] as const) {
      expect(grid.positions.get(id)!.x).toBe((colX.get(sliceId) ?? 0) + SLICE_PADDING)
    }
  })

  it('node_follows_column_when_upstream_slice_grows', () => {
    const base = twoSubmodelModel()
    const baseGrid = computeFeatureGrid(base, 'smOnb')
    const baseS2 = baseGrid.slices.find((s) => s.sliceId === 's2')!.xStart
    const baseN3 = baseGrid.positions.get('n3')!.x
    expect(baseN3).toBe(baseS2 + SLICE_PADDING)

    // Grow the node in the UPSTREAM slice s1 (n2) with a long name so its card —
    // and therefore s1's column — gets wider, pushing s2's column to the right.
    const grown: EventModel = {
      ...base,
      nodes: base.nodes.map((n) =>
        n.id === 'n2'
          ? { ...n, name: 'SignedUpWithAnExtremelyLongDescriptiveEventName' }
          : n,
      ),
    }
    const grownGrid = computeFeatureGrid(grown, 'smOnb')
    const grownS2 = grownGrid.slices.find((s) => s.sliceId === 's2')!.xStart
    const grownN3 = grownGrid.positions.get('n3')!.x

    // The upstream slice grew, so s2's column shifted right …
    expect(grownS2).toBeGreaterThan(baseS2)
    // … and n3 moved with its column by exactly the same delta (still padded).
    expect(grownN3).toBe(grownS2 + SLICE_PADDING)
    expect(grownN3 - baseN3).toBe(grownS2 - baseS2)
  })

  it('computeFeatureGrid_ignores_legacy_bySubmodel_overrides', () => {
    const base = twoSubmodelModel()
    const withOverride: EventModel = {
      ...base,
      layout: {
        ...base.layout,
        // A stale per-feature override far from the grid spot — must be ignored.
        bySubmodel: { smOnb: { n3: { x: 99999, y: 99999 } } },
      },
    }
    const plain = computeFeatureGrid(base, 'smOnb').positions.get('n3')
    const overridden = computeFeatureGrid(withOverride, 'smOnb').positions.get('n3')
    expect(overridden).toEqual(plain)
  })
})
