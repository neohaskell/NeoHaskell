// ── Node types ──────────────────────────────────────────────

/**
 * A schema field on a concept node (command/event/query/…). Optional & additive
 * — absent on legacy models. Surfaced inline via semantic zoom and (later) the
 * Schema lens. Backward-compatible in the on-disk schema (not `required`).
 */
export interface Field {
  readonly name: string
  readonly type: string
}

export interface EventNode {
  readonly id: string
  readonly type: 'event'
  readonly name: string
  readonly entityId: string | null
  readonly sliceId: string | null
  readonly fields?: readonly Field[]
}

export interface CommandNode {
  readonly id: string
  readonly type: 'command'
  readonly name: string
  readonly entityId: string | null
  readonly sliceId: string | null
  readonly fields?: readonly Field[]
}

export interface QueryNode {
  readonly id: string
  readonly type: 'query'
  readonly name: string
  readonly sliceId: string | null
  readonly fields?: readonly Field[]
}

export interface IntegrationNode {
  readonly id: string
  readonly type: 'integration'
  readonly name: string
  readonly kind: 'inbound' | 'outbound'
  readonly sliceId: string | null
  readonly fields?: readonly Field[]
}

export interface UIPlaceholderNode {
  readonly id: string
  readonly type: 'uiPlaceholder'
  readonly name: string
  readonly sliceId: string | null
  readonly fields?: readonly Field[]
}

export type ModelNode =
  | EventNode
  | CommandNode
  | QueryNode
  | IntegrationNode
  | UIPlaceholderNode

export type NodeType = ModelNode['type']

// ── Edge types ──────────────────────────────────────────────

export interface CommandProducesEvent {
  readonly id: string
  readonly type: 'commandProducesEvent'
  readonly sourceId: string // commandId
  readonly targetId: string // eventId
  readonly sourceHandle?: string | null
  readonly targetHandle?: string | null
}

export interface EventFeedsQuery {
  readonly id: string
  readonly type: 'eventFeedsQuery'
  readonly sourceId: string // eventId
  readonly targetId: string // queryId
  readonly sourceHandle?: string | null
  readonly targetHandle?: string | null
}

export interface EventTriggersIntegration {
  readonly id: string
  readonly type: 'eventTriggersIntegration'
  readonly sourceId: string // eventId
  readonly targetId: string // integrationId
  readonly sourceHandle?: string | null
  readonly targetHandle?: string | null
}

export interface IntegrationTriggersCommand {
  readonly id: string
  readonly type: 'integrationTriggersCommand'
  readonly sourceId: string // integrationId
  readonly targetId: string // commandId
  readonly sourceHandle?: string | null
  readonly targetHandle?: string | null
}

export interface CommandFromUI {
  readonly id: string
  readonly type: 'commandFromUI'
  readonly sourceId: string // uiPlaceholderId
  readonly targetId: string // commandId
  readonly sourceHandle?: string | null
  readonly targetHandle?: string | null
}

export interface QueryToUI {
  readonly id: string
  readonly type: 'queryToUI'
  readonly sourceId: string // queryId
  readonly targetId: string // uiPlaceholderId
  readonly sourceHandle?: string | null
  readonly targetHandle?: string | null
}

export type ModelEdge =
  | CommandProducesEvent
  | EventFeedsQuery
  | EventTriggersIntegration
  | IntegrationTriggersCommand
  | CommandFromUI
  | QueryToUI

export type EdgeType = ModelEdge['type']

// ── Structural types ────────────────────────────────────────

export interface Entity {
  readonly id: string
  readonly name: string
  readonly order: number // swim lane ordering (top to bottom)
}

export interface Slice {
  readonly id: string
  readonly name: string
  readonly chapterId: string | null
  readonly order: number // left to right ordering within chapter
}

export interface Chapter {
  readonly id: string
  readonly name: string
  readonly order: number // left to right ordering
  readonly submodelId?: string | null // owning submodel (vertical band); null/absent = ungrouped
}

// A submodel is a FEATURE-level grouping that owns one or more chapters
// (which in turn own slices). Submodels stack vertically (top to bottom by
// `order`) so a large model reads as a vertical list of self-contained
// features instead of one endless horizontal timeline.
export interface Submodel {
  readonly id: string
  readonly name: string
  readonly order: number // top to bottom ordering
}

// ── Layout ──────────────────────────────────────────────────

export interface NodePosition {
  readonly x: number
  readonly y: number
}

export interface Viewport {
  readonly x: number
  readonly y: number
  readonly zoom: number
}

export interface Layout {
  readonly nodePositions: Record<string, NodePosition>
  readonly viewport: Viewport
  /**
   * Per-feature node-position OVERRIDES, keyed by feature id (submodel id or
   * the `__ungrouped__` sentinel) then node id. In the "Features as pages"
   * view the deterministic grid provides defaults; once a user drags a node it
   * gets an entry here (in the feature's own origin-based coordinate space) so
   * the drag sticks instead of snapping back to the grid. Optional / additive.
   */
  readonly bySubmodel?: Record<string, Record<string, NodePosition>>
}

// ── Event Model (root) ──────────────────────────────────────

export interface EventModel {
  readonly id: string
  readonly name: string
  readonly chapters: readonly Chapter[]
  readonly submodels: readonly Submodel[]
  readonly entities: readonly Entity[]
  readonly nodes: readonly ModelNode[]
  readonly edges: readonly ModelEdge[]
  readonly slices: readonly Slice[]
  readonly layout: Layout
}
