import { describe, it, expect } from 'vitest'
import { reducer, type Action } from './store'
import { createEventModel } from '../model/operations'

describe('reducer', () => {
  it('handles addEvent', () => {
    const model = createEventModel('T')
    const result = reducer(model, { type: 'addEvent', name: 'OrderPlaced' })
    expect(result.nodes).toHaveLength(1)
    expect(result.nodes[0].type).toBe('event')
    expect(result.nodes[0].name).toBe('OrderPlaced')
  })

  it('handles addCommand', () => {
    const model = createEventModel('T')
    const result = reducer(model, { type: 'addCommand', name: 'PlaceOrder' })
    expect(result.nodes).toHaveLength(1)
    expect(result.nodes[0].type).toBe('command')
  })

  it('handles addQuery', () => {
    const model = createEventModel('T')
    const result = reducer(model, { type: 'addQuery', name: 'OrderSummary' })
    expect(result.nodes).toHaveLength(1)
    expect(result.nodes[0].type).toBe('query')
  })

  it('handles addIntegration', () => {
    const model = createEventModel('T')
    const result = reducer(model, {
      type: 'addIntegration',
      name: 'SendEmail',
      kind: 'outbound',
    })
    expect(result.nodes).toHaveLength(1)
    expect(result.nodes[0].type).toBe('integration')
  })

  it('handles addUIPlaceholder', () => {
    const model = createEventModel('T')
    const result = reducer(model, { type: 'addUIPlaceholder', name: 'Form' })
    expect(result.nodes).toHaveLength(1)
    expect(result.nodes[0].type).toBe('uiPlaceholder')
  })

  it('handles removeNode', () => {
    let model = createEventModel('T')
    model = reducer(model, { type: 'addEvent', name: 'E' })
    const nodeId = model.nodes[0].id
    const result = reducer(model, { type: 'removeNode', nodeId })
    expect(result.nodes).toHaveLength(0)
  })

  it('handles addEntity', () => {
    const model = createEventModel('T')
    const result = reducer(model, { type: 'addEntity', name: 'Order' })
    expect(result.entities).toHaveLength(1)
    expect(result.entities[0].name).toBe('Order')
  })

  it('handles removeEntity', () => {
    let model = createEventModel('T')
    model = reducer(model, { type: 'addEntity', name: 'Order' })
    const entityId = model.entities[0].id
    const result = reducer(model, { type: 'removeEntity', entityId })
    expect(result.entities).toHaveLength(0)
  })

  it('handles addChapter', () => {
    const model = createEventModel('T')
    const result = reducer(model, { type: 'addChapter', name: 'Ch1' })
    expect(result.chapters).toHaveLength(1)
  })

  it('handles removeChapter', () => {
    let model = createEventModel('T')
    model = reducer(model, { type: 'addChapter', name: 'Ch1' })
    const chapterId = model.chapters[0].id
    const result = reducer(model, { type: 'removeChapter', chapterId })
    expect(result.chapters).toHaveLength(0)
  })

  it('handles reorderChapters and renormalizes order', () => {
    let model = createEventModel('T')
    model = reducer(model, { type: 'addChapter', name: 'A' })
    model = reducer(model, { type: 'addChapter', name: 'B' })
    model = reducer(model, { type: 'addChapter', name: 'C' })
    const [a, b, c] = model.chapters.map((ch) => ch.id)
    const result = reducer(model, {
      type: 'reorderChapters',
      orderedChapterIds: [c, a, b],
    })
    expect(result.chapters.map((ch) => ch.name)).toEqual(['C', 'A', 'B'])
    expect(result.chapters.map((ch) => ch.order)).toEqual([0, 1, 2])
    // Reorder touches only chapters — other model fields are untouched.
    expect(result.nodes).toBe(model.nodes)
    expect(result.slices).toBe(model.slices)
    expect(result.entities).toBe(model.entities)
  })

  it('handles addSlice', () => {
    const model = createEventModel('T')
    const result = reducer(model, { type: 'addSlice', name: 'S1' })
    expect(result.slices).toHaveLength(1)
  })

  it('handles removeSlice', () => {
    let model = createEventModel('T')
    model = reducer(model, { type: 'addSlice', name: 'S1' })
    const sliceId = model.slices[0].id
    const result = reducer(model, { type: 'removeSlice', sliceId })
    expect(result.slices).toHaveLength(0)
  })

  it('handles addEdge', () => {
    let model = createEventModel('T')
    model = reducer(model, { type: 'addCommand', name: 'C' })
    model = reducer(model, { type: 'addEvent', name: 'E' })
    const cmdId = model.nodes.find((n) => n.type === 'command')!.id
    const evtId = model.nodes.find((n) => n.type === 'event')!.id
    const result = reducer(model, {
      type: 'addEdge',
      edgeType: 'commandProducesEvent',
      sourceId: cmdId,
      targetId: evtId,
    })
    expect(result.edges).toHaveLength(1)
  })

  it('handles removeEdge', () => {
    let model = createEventModel('T')
    model = reducer(model, { type: 'addCommand', name: 'C' })
    model = reducer(model, { type: 'addEvent', name: 'E' })
    const cmdId = model.nodes.find((n) => n.type === 'command')!.id
    const evtId = model.nodes.find((n) => n.type === 'event')!.id
    model = reducer(model, {
      type: 'addEdge',
      edgeType: 'commandProducesEvent',
      sourceId: cmdId,
      targetId: evtId,
    })
    const edgeId = model.edges[0].id
    const result = reducer(model, { type: 'removeEdge', edgeId })
    expect(result.edges).toHaveLength(0)
  })

  it('handles updatePosition', () => {
    let model = createEventModel('T')
    model = reducer(model, { type: 'addEvent', name: 'E' })
    const nodeId = model.nodes[0].id
    const result = reducer(model, {
      type: 'updatePosition',
      nodeId,
      x: 500,
      y: 300,
    })
    expect(result.layout.nodePositions[nodeId]).toEqual({ x: 500, y: 300 })
  })

  it('handles loadModel', () => {
    const model = createEventModel('T')
    const newModel = createEventModel('New')
    const result = reducer(model, { type: 'loadModel', model: newModel })
    expect(result).toBe(newModel)
  })
})
