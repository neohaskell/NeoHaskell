import { describe, it, expect } from 'vitest'
import { calculateSliceBounds, calculateChapterBounds } from './slices'
import {
  createEventModel,
  addSlice,
  addChapter,
  addEvent,
  addCommand,
  addEntity,
} from '../../model/operations'
import type { EventModel } from '../../model/types'

function modelWithSlice(): EventModel {
  let model = createEventModel('Test')
  model = addEntity(model, { name: 'Order' })
  model = addChapter(model, { name: 'Ch1' })
  model = addSlice(model, { name: 'Place Order', chapterId: model.chapters[0].id })

  const sliceId = model.slices[0].id
  const entityId = model.entities[0].id

  model = addEvent(model, { name: 'OrderPlaced', entityId })
  model = addCommand(model, { name: 'PlaceOrder', entityId })

  // Assign nodes to slice
  model = {
    ...model,
    nodes: model.nodes.map((n) => ({ ...n, sliceId })),
    layout: {
      ...model.layout,
      nodePositions: {
        [model.nodes[0].id]: { x: 100, y: 50 },
        [model.nodes[1].id]: { x: 100, y: 150 },
      },
    },
  }
  return model
}

describe('calculateSliceBounds', () => {
  it('returns bounds covering all nodes in the slice', () => {
    const model = modelWithSlice()
    const sliceId = model.slices[0].id
    const bounds = calculateSliceBounds(model, sliceId)
    expect(bounds.xStart).toBeLessThanOrEqual(100)
    expect(bounds.xEnd).toBeGreaterThanOrEqual(100)
  })

  it('returns zero-width bounds for empty slice', () => {
    let model = createEventModel('Test')
    model = addSlice(model, { name: 'Empty' })
    const bounds = calculateSliceBounds(model, model.slices[0].id)
    expect(bounds.xEnd - bounds.xStart).toBe(0)
  })

  it('includes slice name in bounds', () => {
    const model = modelWithSlice()
    const sliceId = model.slices[0].id
    const bounds = calculateSliceBounds(model, sliceId)
    expect(bounds.name).toBe('Place Order')
  })
})

describe('calculateChapterBounds', () => {
  it('returns bounds covering all its slices', () => {
    let model = modelWithSlice()
    // Add a second slice in the same chapter
    const chapterId = model.chapters[0].id
    model = addSlice(model, { name: 'Confirm Order', chapterId })
    model = addEvent(model, { name: 'OrderConfirmed' })

    const secondSliceId = model.slices[1].id
    const newNodeId = model.nodes[model.nodes.length - 1].id
    model = {
      ...model,
      nodes: model.nodes.map((n) =>
        n.id === newNodeId ? { ...n, sliceId: secondSliceId } : n,
      ),
      layout: {
        ...model.layout,
        nodePositions: {
          ...model.layout.nodePositions,
          [newNodeId]: { x: 400, y: 50 },
        },
      },
    }

    const bounds = calculateChapterBounds(model, chapterId)
    expect(bounds.xStart).toBeLessThanOrEqual(100)
    expect(bounds.xEnd).toBeGreaterThanOrEqual(400)
  })

  it('returns zero-width bounds for chapter with no slices', () => {
    let model = createEventModel('Test')
    model = addChapter(model, { name: 'Empty' })
    const bounds = calculateChapterBounds(model, model.chapters[0].id)
    expect(bounds.xEnd - bounds.xStart).toBe(0)
  })

  it('includes chapter name', () => {
    const model = modelWithSlice()
    const chapterId = model.chapters[0].id
    const bounds = calculateChapterBounds(model, chapterId)
    expect(bounds.name).toBe('Ch1')
  })
})
