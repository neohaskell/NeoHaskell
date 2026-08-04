import type { EventModel } from '../model/types'
import { serialize, deserialize } from '../model/serialization'
import { createEventModel } from '../model/operations'

export function modelToJson(model: EventModel): string {
  return serialize(model)
}

export function jsonToModel(json: string): EventModel {
  return deserialize(json)
}

export function newModel(name?: string): EventModel {
  return createEventModel(name ?? 'Untitled')
}

export function downloadModel(model: EventModel): void {
  const json = modelToJson(model)
  const blob = new Blob([json], { type: 'application/json' })
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url
  a.download = `${model.name}.json`
  a.click()
  URL.revokeObjectURL(url)
}

export function loadModelFromFile(text: string): EventModel {
  return jsonToModel(text)
}
