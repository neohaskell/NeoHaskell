import { describe, it, expect } from 'vitest'
import { buildGridNodes, computeSliceLayouts, computeEntityLaneLayouts } from './grid'
import { MIN_NODE_WIDTH } from '../nodes/nodeDimensions'
import type { EventModel } from '../../model/types'
import {
  createEventModel,
  addEntity,
  addSlice,
  addChapter,
} from '../../model/operations'

describe('buildGridNodes', () => {
  it('returns empty arrays for model with no entities or slices', () => {
    const model = createEventModel('Test')
    const result = buildGridNodes(model)
    expect(result.entityLaneNodes).toEqual([])
    expect(result.sliceColumnNodes).toEqual([])
    expect(result.sliceLayouts).toEqual([])
  })

  it('creates one lane node per entity', () => {
    let model = createEventModel('Test')
    model = addEntity(model, { name: 'Order' })
    model = addEntity(model, { name: 'Stock' })
    const result = buildGridNodes(model)
    expect(result.entityLaneNodes).toHaveLength(2)
  })

  it('entity lane nodes have correct labels', () => {
    let model = createEventModel('Test')
    model = addEntity(model, { name: 'Proposal' })
    const result = buildGridNodes(model)
    expect(result.entityLaneNodes[0].data.label).toBe('Proposal')
  })

  it('entity lanes are positioned vertically by order', () => {
    let model = createEventModel('Test')
    model = addEntity(model, { name: 'A' })
    model = addEntity(model, { name: 'B' })
    const result = buildGridNodes(model)
    expect(result.entityLaneNodes[0].position.y).toBeLessThan(
      result.entityLaneNodes[1].position.y,
    )
  })

  it('entity lane nodes are not draggable or selectable', () => {
    let model = createEventModel('Test')
    model = addEntity(model, { name: 'Order' })
    const result = buildGridNodes(model)
    expect(result.entityLaneNodes[0].draggable).toBe(false)
    expect(result.entityLaneNodes[0].selectable).toBe(false)
  })

  it('creates one column node per slice', () => {
    let model = createEventModel('Test')
    model = addSlice(model, { name: 'Upload PDF' })
    model = addSlice(model, { name: 'Transcribe' })
    const result = buildGridNodes(model)
    expect(result.sliceColumnNodes).toHaveLength(2)
  })

  it('slice column nodes have correct labels', () => {
    let model = createEventModel('Test')
    model = addSlice(model, { name: 'Upload PDF' })
    const result = buildGridNodes(model)
    expect(result.sliceColumnNodes[0].data.label).toBe('Upload PDF')
  })

  it('slice columns are positioned horizontally by order', () => {
    let model = createEventModel('Test')
    model = addSlice(model, { name: 'A' })
    model = addSlice(model, { name: 'B' })
    const result = buildGridNodes(model)
    expect(result.sliceColumnNodes[0].position.x).toBeLessThan(
      result.sliceColumnNodes[1].position.x,
    )
  })

  it('slice column nodes are not draggable or selectable', () => {
    let model = createEventModel('Test')
    model = addSlice(model, { name: 'S' })
    const result = buildGridNodes(model)
    expect(result.sliceColumnNodes[0].draggable).toBe(false)
    expect(result.sliceColumnNodes[0].selectable).toBe(false)
  })

  it('includes chapter info in slice data when slice has a chapter', () => {
    let model = createEventModel('Test')
    model = addChapter(model, { name: 'Evaluate' })
    model = addSlice(model, { name: 'Upload', chapterId: model.chapters[0].id })
    const result = buildGridNodes(model)
    expect(result.sliceColumnNodes[0].data.chapterName).toBe('Evaluate')
  })
})

describe('computeSliceLayouts — anchor to actual node positions', () => {
  // The CIOS payments model has nodes hand-positioned at x=270, 490, 710,
  // etc. (about 220px apart). Before this fix, slice columns rendered at
  // cumulative MIN_COLUMN_WIDTH=200 starting at x=0, so nodes drew
  // OUTSIDE their columns. These tests pin the new anchor-to-positions
  // behaviour so the regression can't sneak back in.

  function fixture(): EventModel {
    return {
      id: 'm',
      name: 'demo',
      chapters: [],
      entities: [],
      slices: [
        { id: 's1', name: 'A', chapterId: null, order: 0 },
        { id: 's2', name: 'B', chapterId: null, order: 1 },
        { id: 's3', name: 'C', chapterId: null, order: 2 },
      ],
      nodes: [
        { id: 'n1', type: 'event', name: 'a', entityId: null, sliceId: 's1' },
        { id: 'n2', type: 'event', name: 'b', entityId: null, sliceId: 's2' },
        { id: 'n3', type: 'event', name: 'c', entityId: null, sliceId: 's3' },
      ],
      edges: [],
      layout: {
        nodePositions: {
          // Spaced for record-card columns (≥180px wide + padding); the old
          // 220px spacing predates the wider cards.
          n1: { x: 270, y: 400 },
          n2: { x: 580, y: 400 },
          n3: { x: 890, y: 400 },
        },
        viewport: { x: 0, y: 0, zoom: 1 },
      },
    }
  }

  it('xStart of each slice tracks its leftmost node (minus padding + breathing room)', () => {
    const layouts = computeSliceLayouts(fixture())
    // Each column anchors near its node's x minus (SLICE_PADDING 40 + breathing
    // 16 = 56), clamped right so columns never overlap. node x=270 → ≈214.
    for (let i = 0; i < layouts.length; i++) {
      const nodeX = [270, 580, 890][i]
      expect(layouts[i].xStart).toBeLessThanOrEqual(nodeX)
      expect(layouts[i].xStart).toBeGreaterThanOrEqual(nodeX - 90)
    }
  })

  it('every node renders inside its slice column horizontally', () => {
    const model = fixture()
    const layouts = computeSliceLayouts(model)
    for (const node of model.nodes) {
      const pos = model.layout.nodePositions[node.id]
      const layout = layouts.find((l) => l.sliceId === node.sliceId)!
      const right = layout.xStart + layout.width
      expect(pos.x).toBeGreaterThanOrEqual(layout.xStart)
      expect(pos.x + MIN_NODE_WIDTH).toBeLessThanOrEqual(right)
    }
  })

  it('adjacent slices never overlap', () => {
    const layouts = computeSliceLayouts(fixture())
    for (let i = 1; i < layouts.length; i++) {
      const prevRight = layouts[i - 1].xStart + layouts[i - 1].width
      expect(layouts[i].xStart).toBeGreaterThanOrEqual(prevRight)
    }
  })

  it('left padding inside a slice column matches right padding (symmetric breathing room)', () => {
    // Regression: pre-fix, the right edge added NODE_BREATHING_ROOM + SLICE_PADDING
    // but the left only subtracted SLICE_PADDING, leaving the leftmost node
    // hugging the column edge. Both sides should now give equal visual gap.
    const model: EventModel = {
      id: 'm',
      name: 'demo',
      chapters: [],
      entities: [],
      slices: [{ id: 's1', name: 'A', chapterId: null, order: 0 }],
      nodes: [
        { id: 'left', type: 'event', name: 'L', entityId: null, sliceId: 's1' },
        { id: 'right', type: 'event', name: 'R', entityId: null, sliceId: 's1' },
      ],
      edges: [],
      layout: {
        // Force xCursor to NOT clamp xStart so the desiredStart actually fires.
        nodePositions: {
          left: { x: 200, y: 400 },
          right: { x: 500, y: 400 },
        },
        viewport: { x: 0, y: 0, zoom: 1 },
      },
    }
    const layouts = computeSliceLayouts(model)
    const layout = layouts[0]
    const leftGap = model.layout.nodePositions.left.x - layout.xStart
    // Right edge is `xStart + width`; right node's right edge is x + estimated node width.
    const rightNodeRight = model.layout.nodePositions.right.x + MIN_NODE_WIDTH
    const rightGap = layout.xStart + layout.width - rightNodeRight
    expect(leftGap).toEqual(rightGap)
  })

  it('slice column widens to fit a node with a long title', () => {
    const longName = 'BankPaymentStatusAPIWithReallyLongName'
    const model: EventModel = {
      id: 'm',
      name: 'demo',
      chapters: [],
      entities: [],
      slices: [{ id: 's1', name: 'A', chapterId: null, order: 0 }],
      nodes: [
        { id: 'n1', type: 'event', name: longName, entityId: null, sliceId: 's1' },
      ],
      edges: [],
      layout: {
        nodePositions: { n1: { x: 100, y: 400 } },
        viewport: { x: 0, y: 0, zoom: 1 },
      },
    }
    const layouts = computeSliceLayouts(model)
    // Column must extend at least to cover the node's right edge plus
    // breathing room — the old hard-coded NODE_WIDTH=120 would have
    // capped this at 220 (start ≈ 60 + width 200 → 260). With the
    // estimator, MAX_NODE_WIDTH=220 + breathing room means the column
    // right edge is at LEAST 100 + 220 = 320.
    const right = layouts[0].xStart + layouts[0].width
    expect(right).toBeGreaterThanOrEqual(320)
  })

  it('empty slice falls back to MIN_COLUMN_WIDTH starting at prev right edge', () => {
    const model = fixture()
    model.slices.push({ id: 's4', name: 'D', chapterId: null, order: 3 })
    const layouts = computeSliceLayouts(model)
    const s3 = layouts.find((l) => l.sliceId === 's3')!
    const s4 = layouts.find((l) => l.sliceId === 's4')!
    expect(s4.xStart).toEqual(s3.xStart + s3.width)
    expect(s4.width).toBeGreaterThanOrEqual(200)
  })
})

describe('computeEntityLaneLayouts — grow lane to fit entity events', () => {
  it('stays at default LANE_HEIGHT when no events extend below the lane', () => {
    const model: EventModel = {
      id: 'm',
      name: 'demo',
      chapters: [],
      entities: [{ id: 'e1', name: 'A', order: 0 }],
      slices: [],
      nodes: [
        { id: 'n1', type: 'event', name: 'x', entityId: 'e1', sliceId: null },
      ],
      edges: [],
      layout: {
        nodePositions: { n1: { x: 0, y: 400 } }, // inside default lane (340..540)
        viewport: { x: 0, y: 0, zoom: 1 },
      },
    }
    const layouts = computeEntityLaneLayouts(model)
    expect(layouts[0].height).toBe(200)
  })

  it('grows lane to contain every event assigned to the entity', () => {
    const model: EventModel = {
      id: 'm',
      name: 'demo',
      chapters: [],
      entities: [{ id: 'e1', name: 'A', order: 0 }],
      slices: [],
      nodes: [
        { id: 'n1', type: 'event', name: 'x', entityId: 'e1', sliceId: null },
      ],
      edges: [],
      layout: {
        nodePositions: { n1: { x: 0, y: 900 } }, // far below default lane
        viewport: { x: 0, y: 0, zoom: 1 },
      },
    }
    const layouts = computeEntityLaneLayouts(model)
    // Lane should extend at least to y=900+60 (event bottom)
    expect(layouts[0].yStart + layouts[0].height).toBeGreaterThanOrEqual(960)
  })

  it('grows lane to fit a node with a long title (multi-line wrap)', () => {
    const model: EventModel = {
      id: 'm',
      name: 'demo',
      chapters: [],
      entities: [{ id: 'e1', name: 'A', order: 0 }],
      slices: [],
      nodes: [
        {
          id: 'n1',
          // ~30 characters → wraps to at least 2 lines at MAX_NODE_WIDTH
          type: 'event',
          name: 'PaymentFormPreparationFailed',
          entityId: 'e1',
          sliceId: null,
        },
      ],
      edges: [],
      layout: {
        nodePositions: { n1: { x: 0, y: 400 } },
        viewport: { x: 0, y: 0, zoom: 1 },
      },
    }
    const layouts = computeEntityLaneLayouts(model)
    // The lane's lower edge must include the node's bottom (y + estimated
    // multi-line height + breathing room) — strictly larger than the
    // single-line-node case.
    expect(layouts[0].yStart + layouts[0].height).toBeGreaterThanOrEqual(
      400 + 54 /* 2-line ≈ 36 + 18 */,
    )
  })

  it('does NOT absorb nodes belonging to a different entity', () => {
    const model: EventModel = {
      id: 'm',
      name: 'demo',
      chapters: [],
      entities: [
        { id: 'e1', name: 'A', order: 0 },
        { id: 'e2', name: 'B', order: 1 },
      ],
      slices: [],
      nodes: [
        // Far-down event but belongs to e2 — must NOT inflate e1's lane.
        { id: 'n1', type: 'event', name: 'x', entityId: 'e2', sliceId: null },
      ],
      edges: [],
      layout: {
        nodePositions: { n1: { x: 0, y: 2000 } },
        viewport: { x: 0, y: 0, zoom: 1 },
      },
    }
    const layouts = computeEntityLaneLayouts(model)
    expect(layouts[0].height).toBe(200)
  })
})
