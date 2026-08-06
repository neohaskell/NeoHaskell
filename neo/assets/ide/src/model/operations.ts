import type {
  EventModel,
  EventNode,
  CommandNode,
  QueryNode,
  IntegrationNode,
  UIPlaceholderNode,
  ModelEdge,
  ModelNode,
  Entity,
  Chapter,
  Slice,
  Submodel,
  Field,
} from './types'

let counter = 0
function uid(): string {
  return `${Date.now()}-${++counter}-${Math.random().toString(36).slice(2, 8)}`
}

// ── Create ──────────────────────────────────────────────────

export function createEventModel(name: string): EventModel {
  return {
    id: uid(),
    name,
    chapters: [],
    submodels: [],
    entities: [],
    nodes: [],
    edges: [],
    slices: [],
    layout: {
      nodePositions: {},
      viewport: { x: 0, y: 0, zoom: 1 },
    },
  }
}

// ── Add nodes ───────────────────────────────────────────────

export function addEvent(
  model: EventModel,
  params: { name: string; entityId?: string },
): EventModel {
  const node: EventNode = {
    id: uid(),
    type: 'event',
    name: params.name,
    entityId: params.entityId ?? null,
    sliceId: null,
  }
  return { ...model, nodes: [...model.nodes, node] }
}

export function addCommand(
  model: EventModel,
  params: { name: string; entityId?: string },
): EventModel {
  const node: CommandNode = {
    id: uid(),
    type: 'command',
    name: params.name,
    entityId: params.entityId ?? null,
    sliceId: null,
  }
  return { ...model, nodes: [...model.nodes, node] }
}

export function addQuery(
  model: EventModel,
  params: { name: string },
): EventModel {
  const node: QueryNode = {
    id: uid(),
    type: 'query',
    name: params.name,
    sliceId: null,
  }
  return { ...model, nodes: [...model.nodes, node] }
}

export function addIntegration(
  model: EventModel,
  params: { name: string; kind: 'inbound' | 'outbound' },
): EventModel {
  const node: IntegrationNode = {
    id: uid(),
    type: 'integration',
    name: params.name,
    kind: params.kind,
    sliceId: null,
  }
  return { ...model, nodes: [...model.nodes, node] }
}

export function addUIPlaceholder(
  model: EventModel,
  params: { name: string },
): EventModel {
  const node: UIPlaceholderNode = {
    id: uid(),
    type: 'uiPlaceholder',
    name: params.name,
    sliceId: null,
  }
  return { ...model, nodes: [...model.nodes, node] }
}

// ── Remove node ─────────────────────────────────────────────

export function removeNode(model: EventModel, nodeId: string): EventModel {
  if (!model.nodes.some((n) => n.id === nodeId)) return model
  return {
    ...model,
    nodes: model.nodes.filter((n) => n.id !== nodeId),
    edges: model.edges.filter(
      (e) => e.sourceId !== nodeId && e.targetId !== nodeId,
    ),
  }
}

// ── Update node name ────────────────────────────────────────

export function updateNodeName(
  model: EventModel,
  nodeId: string,
  name: string,
): EventModel {
  if (!model.nodes.some((n) => n.id === nodeId)) return model
  return {
    ...model,
    nodes: model.nodes.map((n) => (n.id === nodeId ? { ...n, name } : n)),
  }
}

/** Replace a node's schema `fields` (semantic zoom / Schema lens editor). */
export function setNodeFields(
  model: EventModel,
  nodeId: string,
  fields: readonly Field[],
): EventModel {
  if (!model.nodes.some((n) => n.id === nodeId)) return model
  return {
    ...model,
    nodes: model.nodes.map((n) => (n.id === nodeId ? { ...n, fields } : n)),
  }
}

// ── Edges ───────────────────────────────────────────────────

const EDGE_RULES: Record<
  ModelEdge['type'],
  { sourceType: ModelNode['type']; targetType: ModelNode['type'] }
> = {
  commandProducesEvent: { sourceType: 'command', targetType: 'event' },
  eventFeedsQuery: { sourceType: 'event', targetType: 'query' },
  eventTriggersIntegration: { sourceType: 'event', targetType: 'integration' },
  integrationTriggersCommand: {
    sourceType: 'integration',
    targetType: 'command',
  },
  commandFromUI: { sourceType: 'uiPlaceholder', targetType: 'command' },
  queryToUI: { sourceType: 'query', targetType: 'uiPlaceholder' },
}

export function addEdge(model: EventModel, edge: ModelEdge): EventModel {
  const source = model.nodes.find((n) => n.id === edge.sourceId)
  const target = model.nodes.find((n) => n.id === edge.targetId)

  if (!source) throw new Error(`Source node ${edge.sourceId} not found`)
  if (!target) throw new Error(`Target node ${edge.targetId} not found`)

  const rule = EDGE_RULES[edge.type]
  if (source.type !== rule.sourceType) {
    throw new Error(
      `Edge type ${edge.type} requires source type ${rule.sourceType}, got ${source.type}`,
    )
  }
  if (target.type !== rule.targetType) {
    throw new Error(
      `Edge type ${edge.type} requires target type ${rule.targetType}, got ${target.type}`,
    )
  }

  return { ...model, edges: [...model.edges, edge] }
}

export function removeEdge(model: EventModel, edgeId: string): EventModel {
  if (!model.edges.some((e) => e.id === edgeId)) return model
  return { ...model, edges: model.edges.filter((e) => e.id !== edgeId) }
}

// ── Entities ────────────────────────────────────────────────

export function addEntity(
  model: EventModel,
  params: { name: string },
): EventModel {
  const entity: Entity = {
    id: uid(),
    name: params.name,
    order: model.entities.length,
  }
  return { ...model, entities: [...model.entities, entity] }
}

export function renameEntity(
  model: EventModel,
  entityId: string,
  name: string,
): EventModel {
  return {
    ...model,
    entities: model.entities.map((e) =>
      e.id === entityId ? { ...e, name } : e,
    ),
  }
}

export function removeEntity(
  model: EventModel,
  entityId: string,
): EventModel {
  return {
    ...model,
    entities: model.entities.filter((e) => e.id !== entityId),
    nodes: model.nodes.map((n) => {
      if (
        (n.type === 'event' || n.type === 'command') &&
        n.entityId === entityId
      ) {
        return { ...n, entityId: null }
      }
      return n
    }),
  }
}

// ── Assign event to entity ──────────────────────────────────

export function assignEventToEntity(
  model: EventModel,
  eventId: string,
  entityId: string | null,
): EventModel {
  return {
    ...model,
    nodes: model.nodes.map((n) => {
      if (n.id === eventId && n.type === 'event') {
        return { ...n, entityId }
      }
      return n
    }),
  }
}

// ── Chapters ────────────────────────────────────────────────

export function addChapter(
  model: EventModel,
  params: { name: string; submodelId?: string | null },
): EventModel {
  // A chapter created from a feature's sidebar lands IN that feature; an unknown
  // submodel id (or null/undefined) falls back to ungrouped so the chapter is
  // never orphaned to a band that doesn't exist.
  const submodelId =
    params.submodelId != null && model.submodels.some((s) => s.id === params.submodelId)
      ? params.submodelId
      : null
  const chapter: Chapter = {
    id: uid(),
    name: params.name,
    order: model.chapters.length,
    submodelId,
  }
  return { ...model, chapters: [...model.chapters, chapter] }
}

export function renameChapter(
  model: EventModel,
  chapterId: string,
  name: string,
): EventModel {
  return {
    ...model,
    chapters: model.chapters.map((c) =>
      c.id === chapterId ? { ...c, name } : c,
    ),
  }
}

export function assignSliceToChapter(
  model: EventModel,
  sliceId: string,
  chapterId: string | null,
): EventModel {
  if (!model.slices.some((s) => s.id === sliceId)) return model
  if (chapterId !== null && !model.chapters.some((c) => c.id === chapterId)) return model
  return {
    ...model,
    slices: model.slices.map((s) =>
      s.id === sliceId ? { ...s, chapterId } : s,
    ),
  }
}

/**
 * Assigns all slices within an order range to a chapter.
 * Clears chapterId from any slices previously assigned to this chapter
 * that fall outside the new range.
 */
export function setChapterSliceRange(
  model: EventModel,
  chapterId: string,
  startSliceId: string,
  endSliceId: string,
): EventModel {
  if (!model.chapters.some((c) => c.id === chapterId)) return model
  const sortedSlices = [...model.slices].sort((a, b) => a.order - b.order)
  const startIdx = sortedSlices.findIndex((s) => s.id === startSliceId)
  const endIdx = sortedSlices.findIndex((s) => s.id === endSliceId)
  if (startIdx === -1 || endIdx === -1) return model

  const lo = Math.min(startIdx, endIdx)
  const hi = Math.max(startIdx, endIdx)

  // Only include slices that are free or already owned by this chapter
  const inRangeIds = new Set(
    sortedSlices
      .slice(lo, hi + 1)
      .filter((s) => s.chapterId === null || s.chapterId === chapterId)
      .map((s) => s.id),
  )

  return {
    ...model,
    slices: model.slices.map((s) => {
      if (inRangeIds.has(s.id)) {
        return { ...s, chapterId }
      }
      if (s.chapterId === chapterId) {
        return { ...s, chapterId: null }
      }
      return s
    }),
  }
}

export function removeChapter(
  model: EventModel,
  chapterId: string,
): EventModel {
  return {
    ...model,
    chapters: model.chapters.filter((c) => c.id !== chapterId),
    slices: model.slices.map((s) =>
      s.chapterId === chapterId ? { ...s, chapterId: null } : s,
    ),
  }
}

/**
 * Reorders chapters to match `orderedChapterIds` and renormalizes every
 * chapter's `order` to a contiguous `0..n-1`. Unknown ids are ignored; any
 * chapter omitted from the list is appended after (by ascending current
 * order), so a partial list never drops a chapter. Immutable, and changes
 * ONLY `chapter.order` — `submodelId` and every `slice.chapterId` are left
 * untouched (reorder resequences the horizontal flow, it does not regroup).
 * `chapter.order` is the user-authoritative horizontal axis; the Rust wave
 * pass reads it and never overwrites it.
 */
export function reorderChapters(
  model: EventModel,
  orderedChapterIds: string[],
): EventModel {
  const byId = new Map(model.chapters.map((c) => [c.id, c]))
  const seen = new Set<string>()
  const ordered: Chapter[] = []
  for (const id of orderedChapterIds) {
    const c = byId.get(id)
    if (c && !seen.has(id)) {
      ordered.push(c)
      seen.add(id)
    }
  }
  for (const c of [...model.chapters].sort((a, b) => a.order - b.order)) {
    if (!seen.has(c.id)) ordered.push(c)
  }
  return {
    ...model,
    chapters: ordered.map((c, i) => ({ ...c, order: i })),
  }
}

/**
 * Move a slice into `chapterId` (or detach with `chapterId = null`) AND
 * resequence ALL slices to `orderedSliceIds`, in one pass. This is the single
 * slice-mutation the navigator's drag-and-drop emits — it covers reordering
 * WITHIN a chapter (chapterId unchanged) AND moving a slice ACROSS chapters
 * (chapterId reassigned). `order` is renormalized to a contiguous `0..n-1`;
 * unknown ids are ignored and any slice omitted from the list is appended (by
 * ascending current order), so a partial list never drops a slice. Node
 * `sliceId`s are untouched — a slice's nodes follow it. NOTE: "Tidy by flow"
 * later re-derives slice order from the causal wave, overriding a manual one.
 */
export function moveSliceToChapter(
  model: EventModel,
  sliceId: string,
  chapterId: string | null,
  orderedSliceIds: string[],
): EventModel {
  if (!model.slices.some((s) => s.id === sliceId)) return model
  if (chapterId !== null && !model.chapters.some((c) => c.id === chapterId)) return model
  const byId = new Map(model.slices.map((s) => [s.id, s]))
  const seen = new Set<string>()
  const ordered: Slice[] = []
  for (const id of orderedSliceIds) {
    const s = byId.get(id)
    if (s && !seen.has(id)) {
      ordered.push(s)
      seen.add(id)
    }
  }
  for (const s of [...model.slices].sort((a, b) => a.order - b.order)) {
    if (!seen.has(s.id)) ordered.push(s)
  }
  return {
    ...model,
    slices: ordered.map((s, i) => ({
      ...s,
      chapterId: s.id === sliceId ? chapterId : s.chapterId,
      order: i,
    })),
  }
}

// ── Submodels (vertical feature bands grouping chapters) ────

export function addSubmodel(
  model: EventModel,
  params: { name: string },
): EventModel {
  const submodel: Submodel = {
    id: uid(),
    name: params.name,
    order: model.submodels.length,
  }
  return { ...model, submodels: [...model.submodels, submodel] }
}

export function renameSubmodel(
  model: EventModel,
  submodelId: string,
  name: string,
): EventModel {
  return {
    ...model,
    submodels: model.submodels.map((s) =>
      s.id === submodelId ? { ...s, name } : s,
    ),
  }
}

/** Remove a submodel and detach (do not delete) any chapters it owned. */
export function removeSubmodel(
  model: EventModel,
  submodelId: string,
): EventModel {
  return {
    ...model,
    submodels: model.submodels.filter((s) => s.id !== submodelId),
    chapters: model.chapters.map((c) =>
      c.submodelId === submodelId ? { ...c, submodelId: null } : c,
    ),
  }
}

/** Assign a chapter to a submodel (or detach it when `submodelId` is null). */
export function assignChapterToSubmodel(
  model: EventModel,
  chapterId: string,
  submodelId: string | null,
): EventModel {
  if (!model.chapters.some((c) => c.id === chapterId)) return model
  if (submodelId !== null && !model.submodels.some((s) => s.id === submodelId)) return model
  return {
    ...model,
    chapters: model.chapters.map((c) =>
      c.id === chapterId ? { ...c, submodelId } : c,
    ),
  }
}

// ── Slices ──────────────────────────────────────────────────

export function addSlice(
  model: EventModel,
  params: { name: string; chapterId?: string },
): EventModel {
  const slice: Slice = {
    id: uid(),
    name: params.name,
    chapterId: params.chapterId ?? null,
    order: model.slices.length,
  }
  return { ...model, slices: [...model.slices, slice] }
}

export function removeSlice(
  model: EventModel,
  sliceId: string,
): EventModel {
  return {
    ...model,
    slices: model.slices.filter((s) => s.id !== sliceId),
    nodes: model.nodes.map((n) =>
      n.sliceId === sliceId ? { ...n, sliceId: null } : n,
    ),
  }
}

export function assignNodeToSlice(
  model: EventModel,
  nodeId: string,
  sliceId: string | null,
): EventModel {
  if (!model.nodes.some((n) => n.id === nodeId)) return model
  if (sliceId !== null && !model.slices.some((s) => s.id === sliceId)) return model
  return {
    ...model,
    nodes: model.nodes.map((n) =>
      n.id === nodeId ? { ...n, sliceId } : n,
    ),
  }
}

export function renameSlice(
  model: EventModel,
  sliceId: string,
  name: string,
): EventModel {
  return {
    ...model,
    slices: model.slices.map((s) =>
      s.id === sliceId ? { ...s, name } : s,
    ),
  }
}

// ── Reorder events in entity ────────────────────────────────

export function reorderEventsInEntity(
  model: EventModel,
  entityId: string,
  orderedEventIds: string[],
): EventModel {
  const entityEvents = model.nodes.filter(
    (n) => n.type === 'event' && n.entityId === entityId,
  )
  const otherNodes = model.nodes.filter(
    (n) => !(n.type === 'event' && n.entityId === entityId),
  )
  const reordered = orderedEventIds
    .map((id) => entityEvents.find((n) => n.id === id))
    .filter((n): n is ModelNode => n !== undefined)

  return { ...model, nodes: [...otherNodes, ...reordered] }
}
