// Thin JSON pass-throughs. The Rust backend (`src/ide/validate.rs` +
// `assets/ide/src/model/event-model.schema.json`) is the authoritative
// validator for the on-disk shape; by the time content reaches
// `deserialize` here, it has already been schema-checked and is
// guaranteed to match `EventModel`. If you find yourself adding
// runtime checks here, the boundary moved — fix it on the backend.

import type { EventModel } from './types'

export function serialize(model: EventModel): string {
  return JSON.stringify(model, null, 2)
}

export function deserialize(json: string): EventModel {
  const parsed = JSON.parse(json) as EventModel
  // `submodels` is a newer, optional-on-disk field (the Rust schema does not
  // require it). Normalise its absence to an empty array so every in-memory
  // model satisfies the `EventModel` type and the layout code can treat it
  // uniformly. Chapters keep `submodelId` as-is (undefined ≡ ungrouped).
  if (!Array.isArray(parsed.submodels)) {
    return { ...parsed, submodels: [] }
  }
  return parsed
}
