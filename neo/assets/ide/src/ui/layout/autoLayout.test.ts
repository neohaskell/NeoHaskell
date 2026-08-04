import { describe, it, expect } from 'vitest'
import { autoLayoutMissingPositions } from './autoLayout'
import type { EventModel } from '../../model/types'

function baseModel(): EventModel {
  return {
    id: 'm1',
    name: 'demo',
    chapters: [],
    entities: [
      { id: 'ent-a', name: 'A', order: 0 },
      { id: 'ent-b', name: 'B', order: 1 },
    ],
    slices: [
      { id: 'sl-1', name: 'One', chapterId: null, order: 0 },
      { id: 'sl-2', name: 'Two', chapterId: null, order: 1 },
    ],
    nodes: [],
    edges: [],
    layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
  }
}

describe('autoLayoutMissingPositions', () => {
  it('returns the same model when every node already has an in-band non-origin position', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        // Event in the events band (y ≥ 300) — already correct.
        { id: 'n1', type: 'event', name: 'E', entityId: 'ent-a', sliceId: 'sl-1' },
      ],
      layout: {
        ...model.layout,
        nodePositions: { n1: { x: 100, y: 400 } },
      },
    }
    const out = autoLayoutMissingPositions(withNodes)
    expect(out).toBe(withNodes)
  })

  it('assigns a position to a node that is missing one', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'n1', type: 'event', name: 'E', entityId: 'ent-a', sliceId: 'sl-1' },
      ],
    }
    const out = autoLayoutMissingPositions(withNodes)
    expect(out.layout.nodePositions.n1).toBeDefined()
    expect(out.layout.nodePositions.n1.x).toBeGreaterThan(0)
  })

  it('treats (0, 0) as missing and re-assigns', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'n1', type: 'event', name: 'E', entityId: 'ent-a', sliceId: 'sl-1' },
      ],
      layout: {
        ...model.layout,
        nodePositions: { n1: { x: 0, y: 0 } },
      },
    }
    const out = autoLayoutMissingPositions(withNodes)
    const pos = out.layout.nodePositions.n1
    expect(pos.x !== 0 || pos.y !== 0).toBe(true)
  })

  it('places nodes in later slices to the right of earlier slices', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'n1', type: 'event', name: 'A', entityId: 'ent-a', sliceId: 'sl-1' },
        { id: 'n2', type: 'event', name: 'B', entityId: 'ent-a', sliceId: 'sl-2' },
      ],
    }
    const out = autoLayoutMissingPositions(withNodes)
    expect(out.layout.nodePositions.n2.x).toBeGreaterThan(
      out.layout.nodePositions.n1.x,
    )
  })

  it('places events in later entity lanes below earlier ones', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'n1', type: 'event', name: 'A', entityId: 'ent-a', sliceId: 'sl-1' },
        { id: 'n2', type: 'event', name: 'B', entityId: 'ent-b', sliceId: 'sl-1' },
      ],
    }
    const out = autoLayoutMissingPositions(withNodes)
    expect(out.layout.nodePositions.n2.y).toBeGreaterThan(
      out.layout.nodePositions.n1.y,
    )
  })

  it('stacks two events in the same slice + entity vertically (no overlap)', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'n1', type: 'event', name: 'A', entityId: 'ent-a', sliceId: 'sl-1' },
        { id: 'n2', type: 'event', name: 'B', entityId: 'ent-a', sliceId: 'sl-1' },
      ],
    }
    const out = autoLayoutMissingPositions(withNodes)
    expect(out.layout.nodePositions.n1.y).not.toEqual(
      out.layout.nodePositions.n2.y,
    )
  })

  it('places commands above events visually (smaller y)', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'cmd', type: 'command', name: 'Do', entityId: null, sliceId: 'sl-1' },
        { id: 'evt', type: 'event', name: 'Done', entityId: 'ent-a', sliceId: 'sl-1' },
      ],
    }
    const out = autoLayoutMissingPositions(withNodes)
    expect(out.layout.nodePositions.cmd.y).toBeLessThan(
      out.layout.nodePositions.evt.y,
    )
  })

  it('places UI placeholders above commands', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'ui', type: 'uiPlaceholder', name: 'Form', sliceId: 'sl-1' },
        { id: 'cmd', type: 'command', name: 'Do', entityId: null, sliceId: 'sl-1' },
      ],
    }
    const out = autoLayoutMissingPositions(withNodes)
    expect(out.layout.nodePositions.ui.y).toBeLessThan(
      out.layout.nodePositions.cmd.y,
    )
  })

  it('places integrations in the SAME y band as commands and queries (above events)', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'cmd', type: 'command', name: 'Do', entityId: null, sliceId: 'sl-1' },
        { id: 'qry', type: 'query', name: 'Read', sliceId: 'sl-2' },
        { id: 'int', type: 'integration', name: 'Bank', kind: 'outbound', sliceId: 'sl-2' },
        { id: 'evt', type: 'event', name: 'Done', entityId: 'ent-a', sliceId: 'sl-1' },
      ],
    }
    const out = autoLayoutMissingPositions(withNodes)
    // Commands, queries, integrations all share the same base y.
    expect(out.layout.nodePositions.cmd.y).toEqual(out.layout.nodePositions.qry.y)
    expect(out.layout.nodePositions.qry.y).toEqual(out.layout.nodePositions.int.y)
    // Events sit below them.
    expect(out.layout.nodePositions.int.y).toBeLessThan(out.layout.nodePositions.evt.y)
  })

  it('snaps an integration positioned BELOW events back into the above-events band', () => {
    // Regression for the CIOS payments model: integrations stored at
    // y=575 (below the entity lane) should be re-anchored to the
    // command/query band, NOT left where the file said.
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'int', type: 'integration', name: 'Bank', kind: 'outbound', sliceId: 'sl-1' },
      ],
      layout: {
        ...model.layout,
        nodePositions: { int: { x: 500, y: 575 } },
      },
    }
    const out = autoLayoutMissingPositions(withNodes)
    expect(out.layout.nodePositions.int.y).toBeLessThan(300)
    // x is preserved — only the off-band y was snapped.
    expect(out.layout.nodePositions.int.x).toEqual(500)
  })

  it('preserves an in-band y for integration even if it differs from the band default', () => {
    // An integration at y=238 is already above events — leave it alone.
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'int', type: 'integration', name: 'Bank', kind: 'outbound', sliceId: 'sl-1' },
      ],
      layout: {
        ...model.layout,
        nodePositions: { int: { x: 1100, y: 238 } },
      },
    }
    const out = autoLayoutMissingPositions(withNodes)
    expect(out.layout.nodePositions.int).toEqual({ x: 1100, y: 238 })
  })

  it('snaps a UI placeholder dropped into the events band back to above the slice header', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'ui', type: 'uiPlaceholder', name: 'Form', sliceId: 'sl-1' },
      ],
      layout: {
        ...model.layout,
        nodePositions: { ui: { x: 100, y: 400 } },
      },
    }
    const out = autoLayoutMissingPositions(withNodes)
    expect(out.layout.nodePositions.ui.y).toBeLessThan(50)
    expect(out.layout.nodePositions.ui.x).toEqual(100)
  })

  it('preserves existing non-origin positions when only some nodes lack positions', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'has', type: 'event', name: 'A', entityId: 'ent-a', sliceId: 'sl-1' },
        { id: 'lacks', type: 'event', name: 'B', entityId: 'ent-a', sliceId: 'sl-1' },
      ],
      layout: {
        ...model.layout,
        nodePositions: { has: { x: 555, y: 999 } },
      },
    }
    const out = autoLayoutMissingPositions(withNodes)
    expect(out.layout.nodePositions.has).toEqual({ x: 555, y: 999 })
    expect(out.layout.nodePositions.lacks).toBeDefined()
  })

  it('is idempotent: running twice produces the same positions', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'n1', type: 'event', name: 'A', entityId: 'ent-a', sliceId: 'sl-1' },
        { id: 'n2', type: 'command', name: 'Do', entityId: null, sliceId: 'sl-2' },
      ],
    }
    const once = autoLayoutMissingPositions(withNodes)
    const twice = autoLayoutMissingPositions(once)
    expect(twice.layout.nodePositions).toEqual(once.layout.nodePositions)
  })

  it('handles unassigned slice / entity gracefully', () => {
    const model = baseModel()
    const withNodes: EventModel = {
      ...model,
      nodes: [
        { id: 'lonely', type: 'event', name: 'X', entityId: null, sliceId: null },
      ],
    }
    const out = autoLayoutMissingPositions(withNodes)
    expect(out.layout.nodePositions.lonely).toBeDefined()
  })

  it('does NOT band-snap a node that belongs to a submodel', () => {
    // A command inside a submodel band legitimately sits far below the
    // global command band (y = 1020). The absolute y-band rule must not
    // yank it back to y ≈ 120, or vertical stacking would be destroyed.
    const model: EventModel = {
      id: 'm',
      name: 'demo',
      submodels: [{ id: 'sm1', name: 'Feature', order: 0 }],
      chapters: [{ id: 'c1', name: 'C', order: 0, submodelId: 'sm1' }],
      entities: [],
      slices: [{ id: 's1', name: 'S', chapterId: 'c1', order: 0 }],
      nodes: [{ id: 'cmd', type: 'command', name: 'Do', entityId: null, sliceId: 's1' }],
      edges: [],
      layout: {
        nodePositions: { cmd: { x: 60, y: 1020 } },
        viewport: { x: 0, y: 0, zoom: 1 },
      },
    }
    const out = autoLayoutMissingPositions(model)
    expect(out.layout.nodePositions.cmd).toEqual({ x: 60, y: 1020 })
    // Same node WITHOUT submodel membership IS snapped back into band.
    const ungrouped: EventModel = {
      ...model,
      submodels: [],
      chapters: [{ id: 'c1', name: 'C', order: 0, submodelId: null }],
    }
    const snapped = autoLayoutMissingPositions(ungrouped)
    expect(snapped.layout.nodePositions.cmd.y).toBeLessThan(300)
  })
})
