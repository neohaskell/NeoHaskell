import { describe, it, expect, beforeEach } from 'vitest'
import { saveToStorage, loadFromStorage, STORAGE_KEY } from './persistence'
import { createEventModel, addEvent, addEntity } from '../model/operations'

beforeEach(() => {
  localStorage.clear()
})

describe('saveToStorage', () => {
  it('saves model to localStorage', () => {
    const model = createEventModel('Test')
    saveToStorage(model)
    expect(localStorage.getItem(STORAGE_KEY)).not.toBeNull()
  })

  it('saved data is valid JSON', () => {
    const model = createEventModel('Test')
    saveToStorage(model)
    expect(() => JSON.parse(localStorage.getItem(STORAGE_KEY)!)).not.toThrow()
  })
})

describe('loadFromStorage', () => {
  it('returns null when nothing is stored', () => {
    expect(loadFromStorage()).toBeNull()
  })

  it('round-trips a model', () => {
    let model = createEventModel('Persisted')
    model = addEntity(model, { name: 'Order' })
    model = addEvent(model, { name: 'OrderPlaced', entityId: model.entities[0].id })
    saveToStorage(model)
    const loaded = loadFromStorage()
    expect(loaded).toEqual(model)
  })

  it('returns null for corrupted data', () => {
    localStorage.setItem(STORAGE_KEY, 'not valid json')
    expect(loadFromStorage()).toBeNull()
  })
})
