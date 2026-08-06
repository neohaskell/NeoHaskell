import { describe, it, expect } from 'vitest'
import {
  calculateSwimLanes,
  getSwimLaneForPosition,
  type SwimLaneLayout,
} from './swimlanes'
import {
  createEventModel,
  addEntity,
  addEvent,
} from '../../model/operations'
import type { EventModel } from '../../model/types'

function modelWithTwoEntities(): EventModel {
  let model = createEventModel('Test')
  model = addEntity(model, { name: 'Order' })
  model = addEntity(model, { name: 'Stock' })
  const orderEntityId = model.entities[0].id
  const stockEntityId = model.entities[1].id
  model = addEvent(model, { name: 'OrderPlaced', entityId: orderEntityId })
  model = addEvent(model, { name: 'StockReserved', entityId: stockEntityId })

  // Set positions for the events
  const orderEvtId = model.nodes[0].id
  const stockEvtId = model.nodes[1].id
  model = {
    ...model,
    layout: {
      ...model.layout,
      nodePositions: {
        [orderEvtId]: { x: 100, y: 50 },
        [stockEvtId]: { x: 100, y: 250 },
      },
    },
  }
  return model
}

describe('calculateSwimLanes', () => {
  it('returns empty array for model with no entities', () => {
    const model = createEventModel('Test')
    const lanes = calculateSwimLanes(model)
    expect(lanes).toEqual([])
  })

  it('returns one lane per entity', () => {
    const model = modelWithTwoEntities()
    const lanes = calculateSwimLanes(model)
    expect(lanes).toHaveLength(2)
  })

  it('lanes are ordered by entity order', () => {
    const model = modelWithTwoEntities()
    const lanes = calculateSwimLanes(model)
    expect(lanes[0].entityId).toBe(model.entities[0].id)
    expect(lanes[1].entityId).toBe(model.entities[1].id)
  })

  it('lanes include entity name', () => {
    const model = modelWithTwoEntities()
    const lanes = calculateSwimLanes(model)
    expect(lanes[0].name).toBe('Order')
    expect(lanes[1].name).toBe('Stock')
  })

  it('lanes are non-overlapping', () => {
    const model = modelWithTwoEntities()
    const lanes = calculateSwimLanes(model)
    for (let i = 1; i < lanes.length; i++) {
      expect(lanes[i].yStart).toBeGreaterThanOrEqual(lanes[i - 1].yEnd)
    }
  })

  it('lanes have minimum height', () => {
    let model = createEventModel('Test')
    model = addEntity(model, { name: 'Empty' })
    const lanes = calculateSwimLanes(model)
    expect(lanes[0].yEnd - lanes[0].yStart).toBeGreaterThanOrEqual(100)
  })
})

describe('getSwimLaneForPosition', () => {
  it('returns entityId when y is within a lane', () => {
    const lanes: SwimLaneLayout[] = [
      { entityId: 'e1', name: 'Order', yStart: 0, yEnd: 150 },
      { entityId: 'e2', name: 'Stock', yStart: 150, yEnd: 300 },
    ]
    expect(getSwimLaneForPosition(lanes, 75)).toBe('e1')
    expect(getSwimLaneForPosition(lanes, 200)).toBe('e2')
  })

  it('returns null when y is outside all lanes', () => {
    const lanes: SwimLaneLayout[] = [
      { entityId: 'e1', name: 'Order', yStart: 100, yEnd: 200 },
    ]
    expect(getSwimLaneForPosition(lanes, 50)).toBeNull()
    expect(getSwimLaneForPosition(lanes, 250)).toBeNull()
  })

  it('returns null for empty lanes', () => {
    expect(getSwimLaneForPosition([], 100)).toBeNull()
  })
})
