import { describe, it, expect } from 'vitest'
import { toReactFlowNodes, toReactFlowEdges, applyPositionChanges } from './adapter'
import {
  createEventModel,
  addEvent,
  addCommand,
  addQuery,
  addIntegration,
  addUIPlaceholder,
  addEdge,
  addEntity,
} from '../model/operations'
import type { EventModel } from '../model/types'

function modelWithAllTypes(): EventModel {
  let model = createEventModel('Test')
  model = addEntity(model, { name: 'Order' })
  const entityId = model.entities[0].id
  model = addEvent(model, { name: 'OrderPlaced', entityId })
  model = addCommand(model, { name: 'PlaceOrder', entityId })
  model = addQuery(model, { name: 'OrderSummary' })
  model = addIntegration(model, { name: 'SendEmail', kind: 'outbound' })
  model = addUIPlaceholder(model, { name: 'Order Form' })

  // Add positions
  const positions: Record<string, { x: number; y: number }> = {}
  model.nodes.forEach((n, i) => {
    positions[n.id] = { x: i * 200, y: 100 }
  })
  model = { ...model, layout: { ...model.layout, nodePositions: positions } }

  return model
}

describe('toReactFlowNodes', () => {
  it('maps each domain node to a ReactFlow node', () => {
    const model = modelWithAllTypes()
    const rfNodes = toReactFlowNodes(model)
    expect(rfNodes).toHaveLength(5)
  })

  it('maps event nodes with type "event"', () => {
    const model = modelWithAllTypes()
    const rfNodes = toReactFlowNodes(model)
    const eventNode = rfNodes.find((n) => n.type === 'event')
    expect(eventNode).toBeDefined()
    expect(eventNode!.data.label).toBe('OrderPlaced')
  })

  it('maps command nodes with type "command"', () => {
    const model = modelWithAllTypes()
    const rfNodes = toReactFlowNodes(model)
    const cmdNode = rfNodes.find((n) => n.type === 'command')
    expect(cmdNode).toBeDefined()
    expect(cmdNode!.data.label).toBe('PlaceOrder')
  })

  it('maps query nodes with type "query"', () => {
    const model = modelWithAllTypes()
    const rfNodes = toReactFlowNodes(model)
    const qNode = rfNodes.find((n) => n.type === 'query')
    expect(qNode).toBeDefined()
    expect(qNode!.data.label).toBe('OrderSummary')
  })

  it('maps integration nodes with type "integration"', () => {
    const model = modelWithAllTypes()
    const rfNodes = toReactFlowNodes(model)
    const intNode = rfNodes.find((n) => n.type === 'integration')
    expect(intNode).toBeDefined()
    expect(intNode!.data.label).toBe('SendEmail')
    expect(intNode!.data.kind).toBe('outbound')
  })

  it('maps UI placeholder nodes with type "uiPlaceholder"', () => {
    const model = modelWithAllTypes()
    const rfNodes = toReactFlowNodes(model)
    const uiNode = rfNodes.find((n) => n.type === 'uiPlaceholder')
    expect(uiNode).toBeDefined()
    expect(uiNode!.data.label).toBe('Order Form')
  })

  it('uses positions from layout', () => {
    const model = modelWithAllTypes()
    const rfNodes = toReactFlowNodes(model)
    const firstNode = rfNodes[0]
    const expectedPos = model.layout.nodePositions[model.nodes[0].id]
    expect(firstNode.position).toEqual(expectedPos)
  })

  it('defaults position to 0,0 if not in layout', () => {
    let model = createEventModel('T')
    model = addEvent(model, { name: 'E' })
    const rfNodes = toReactFlowNodes(model)
    expect(rfNodes[0].position).toEqual({ x: 0, y: 0 })
  })
})

describe('toReactFlowEdges', () => {
  it('maps each domain edge to a ReactFlow edge', () => {
    let model = modelWithAllTypes()
    const cmdId = model.nodes.find((n) => n.type === 'command')!.id
    const evtId = model.nodes.find((n) => n.type === 'event')!.id
    model = addEdge(model, {
      id: 'e1',
      type: 'commandProducesEvent',
      sourceId: cmdId,
      targetId: evtId,
    })
    const rfEdges = toReactFlowEdges(model)
    expect(rfEdges).toHaveLength(1)
    expect(rfEdges[0].source).toBe(cmdId)
    expect(rfEdges[0].target).toBe(evtId)
  })

  it('preserves edge id', () => {
    let model = modelWithAllTypes()
    const cmdId = model.nodes.find((n) => n.type === 'command')!.id
    const evtId = model.nodes.find((n) => n.type === 'event')!.id
    model = addEdge(model, {
      id: 'my-edge',
      type: 'commandProducesEvent',
      sourceId: cmdId,
      targetId: evtId,
    })
    const rfEdges = toReactFlowEdges(model)
    expect(rfEdges[0].id).toBe('my-edge')
  })
})

describe('applyPositionChanges', () => {
  it('updates node positions without touching semantic data', () => {
    let model = modelWithAllTypes()
    const nodeId = model.nodes[0].id
    const changes = [{ id: nodeId, x: 500, y: 300 }]
    const updated = applyPositionChanges(model, changes)
    expect(updated.layout.nodePositions[nodeId]).toEqual({ x: 500, y: 300 })
    // semantic data unchanged
    expect(updated.nodes).toBe(model.nodes)
    expect(updated.edges).toBe(model.edges)
    expect(updated.entities).toBe(model.entities)
  })

  it('preserves positions of other nodes', () => {
    let model = modelWithAllTypes()
    const firstId = model.nodes[0].id
    const secondId = model.nodes[1].id
    const changes = [{ id: firstId, x: 999, y: 888 }]
    const updated = applyPositionChanges(model, changes)
    expect(updated.layout.nodePositions[secondId]).toEqual(
      model.layout.nodePositions[secondId],
    )
  })
})
