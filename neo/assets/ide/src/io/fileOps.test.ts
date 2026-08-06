import { describe, it, expect } from 'vitest'
import { modelToJson, jsonToModel, newModel } from './fileOps'
import {
  createEventModel,
  addEvent,
  addCommand,
  addEntity,
  addEdge,
} from '../model/operations'

describe('modelToJson', () => {
  it('produces valid JSON string', () => {
    const model = createEventModel('Test')
    const json = modelToJson(model)
    expect(() => JSON.parse(json)).not.toThrow()
  })

  it('includes all model data', () => {
    let model = createEventModel('Test')
    model = addEntity(model, { name: 'Order' })
    model = addEvent(model, { name: 'E', entityId: model.entities[0].id })
    const json = modelToJson(model)
    const parsed = JSON.parse(json)
    expect(parsed.name).toBe('Test')
    expect(parsed.entities).toHaveLength(1)
    expect(parsed.nodes).toHaveLength(1)
  })
})

describe('jsonToModel', () => {
  it('parses saved JSON correctly (round-trip)', () => {
    let model = createEventModel('RT')
    model = addEntity(model, { name: 'Order' })
    const entityId = model.entities[0].id
    model = addEvent(model, { name: 'E', entityId })
    model = addCommand(model, { name: 'C', entityId })
    const cmdId = model.nodes.find((n) => n.type === 'command')!.id
    const evtId = model.nodes.find((n) => n.type === 'event')!.id
    model = addEdge(model, {
      id: 'e1',
      type: 'commandProducesEvent',
      sourceId: cmdId,
      targetId: evtId,
    })

    const json = modelToJson(model)
    const restored = jsonToModel(json)
    expect(restored).toEqual(model)
  })

  it('rejects invalid JSON', () => {
    expect(() => jsonToModel('not json')).toThrow()
  })
})

describe('newModel', () => {
  it('creates model with given name', () => {
    const model = newModel('My Model')
    expect(model.name).toBe('My Model')
  })

  it('creates empty model', () => {
    const model = newModel('Empty')
    expect(model.nodes).toHaveLength(0)
    expect(model.edges).toHaveLength(0)
    expect(model.entities).toHaveLength(0)
    expect(model.chapters).toHaveLength(0)
    expect(model.slices).toHaveLength(0)
  })

  it('defaults to "Untitled" if no name', () => {
    const model = newModel()
    expect(model.name).toBe('Untitled')
  })
})
