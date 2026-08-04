import type { EventModel } from '../model/types'
import { serialize, deserialize } from '../model/serialization'

export const STORAGE_KEY = 'neoide:model'

export function saveToStorage(model: EventModel): void {
  localStorage.setItem(STORAGE_KEY, serialize(model))
}

export function loadFromStorage(): EventModel | null {
  const raw = localStorage.getItem(STORAGE_KEY)
  if (!raw) return null
  try {
    return deserialize(raw)
  } catch {
    return null
  }
}
