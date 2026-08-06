import { describe, it, expect } from 'vitest'
import { successorsFor } from '../connectionRules'
import { createNode, createSuccessor } from './nodeCreation'
import type { EventModel } from '../../model/types'

function emptyModel(): EventModel {
  return {
    id: 'm',
    name: 'T',
    chapters: [],
    submodels: [],
    entities: [],
    nodes: [],
    edges: [],
    slices: [],
    layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
  }
}

describe('successorsFor', () => {
  it('node_creation_successors_for_type', () => {
    expect(successorsFor('command')).toEqual(['event'])
    expect(successorsFor('event')).toEqual(['query', 'integration'])
    expect(successorsFor('integration')).toEqual(['command'])
    expect(successorsFor('uiPlaceholder')).toEqual(['command'])
    expect(successorsFor('query')).toEqual(['uiPlaceholder'])
  })
})

describe('createNode', () => {
  it('appends a node of the requested type at the placement', () => {
    const { model, nodeId } = createNode(emptyModel(), 'command', { x: 10, y: 20 })
    expect(model.nodes).toHaveLength(1)
    const created = model.nodes[0]
    expect(created.id).toBe(nodeId)
    expect(created.type).toBe('command')
    expect(model.layout.nodePositions[nodeId]).toEqual({ x: 10, y: 20 })
  })

  it('assigns the node to the slice it was dropped on', () => {
    const base: EventModel = {
      ...emptyModel(),
      slices: [{ id: 's1', name: 'S', chapterId: null, order: 0 }],
    }
    const { model, nodeId } = createNode(base, 'event', { x: 0, y: 0, sliceId: 's1' })
    const created = model.nodes.find((n) => n.id === nodeId)!
    expect(created.sliceId).toBe('s1')
  })

  it('feature_create_assigns_slice_entity_without_override', () => {
    const base: EventModel = {
      ...emptyModel(),
      slices: [{ id: 's1', name: 'S', chapterId: null, order: 0 }],
      entities: [{ id: 'e1', name: 'E', order: 0 }],
    }
    const { model, nodeId } = createNode(base, 'event', {
      x: 5,
      y: 6,
      sliceId: 's1',
      entityId: 'e1',
    })
    const created = model.nodes.find((n) => n.id === nodeId)!
    // The drop slice/entity are assigned …
    expect(created.sliceId).toBe('s1')
    expect(created.entityId).toBe('e1')
    // … and the grid owns layout: NO per-feature override is written, but a
    // global seed position is still recorded for the flat fallback / reflow.
    expect(model.layout.bySubmodel).toBeUndefined()
    expect(model.layout.nodePositions[nodeId]).toEqual({ x: 5, y: 6 })
  })
})

describe('createSuccessor', () => {
  it('drag_to_empty_autospawns_successor: creates the successor node + typed edge', () => {
    const base = createNode(emptyModel(), 'command', { x: 0, y: 0 })
    const { model, nodeId } = createSuccessor(base.model, base.nodeId, 'event', { x: 0, y: 100 })
    // New event node exists.
    const created = model.nodes.find((n) => n.id === nodeId)!
    expect(created.type).toBe('event')
    // A commandProducesEvent edge connects source → new event.
    const edge = model.edges.find((e) => e.sourceId === base.nodeId && e.targetId === nodeId)
    expect(edge).toBeDefined()
    expect(edge!.type).toBe('commandProducesEvent')
  })

  it('returns the model unchanged when the source node is missing', () => {
    const base = emptyModel()
    const { model, nodeId } = createSuccessor(base, 'nope', 'event', { x: 0, y: 0 })
    expect(nodeId).toBe('')
    expect(model).toBe(base)
  })
})
