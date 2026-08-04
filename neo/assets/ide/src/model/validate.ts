import type { EventModel } from './types'

// Live, client-side SEMANTIC validation of an event model. The Rust backend
// owns JSON-schema + referential validation at load time; by construction the
// in-memory model (built via typed reducer ops) is structurally valid, so the
// useful live checks are semantic: a node in no slice, an empty feature, a
// command that produces nothing, etc.
//
// Severity philosophy (resolved from the expert panel): mid-construction is
// NORMAL, so almost everything is a WARNING or INFO — the only ERROR is a
// genuinely broken artifact (a dangling edge endpoint). Validation NEVER
// blocks autosave. Messages follow the repo's error invariant: name the rule,
// quote the offending item, and give a concrete fix.

export type Severity = 'error' | 'warning' | 'info'

export interface Issue {
  /** Stable rule id (also used as a test handle). */
  id: string
  severity: Severity
  message: string
  /** The primary offending element, for jump-to / inline badge. */
  nodeId?: string
  edgeId?: string
  featureId?: string
  chapterId?: string
}

/** Number of read-models an event may feed before it smells over-wired. */
const OVERWIRE_THRESHOLD = 6

export function validate(model: EventModel): Issue[] {
  const issues: Issue[] = []
  const nodeById = new Map(model.nodes.map((n) => [n.id, n]))
  const nameOf = (id: string) => nodeById.get(id)?.name ?? id

  // node → feature resolution (node → slice → chapter → submodel), inlined to
  // keep this module dependency-free (only imports types).
  const sliceToChapter = new Map(model.slices.map((s) => [s.id, s.chapterId]))
  const chapterToSubmodel = new Map(model.chapters.map((c) => [c.id, c.submodelId ?? null]))
  const submodelName = new Map(model.submodels.map((s) => [s.id, s.name]))
  const featureOf = (nodeId: string): string | null => {
    const node = nodeById.get(nodeId)
    if (!node?.sliceId) return null
    const chapterId = sliceToChapter.get(node.sliceId) ?? null
    if (!chapterId) return null
    return chapterToSubmodel.get(chapterId) ?? null
  }

  // ── error: dangling edge endpoint ───────────────────────────
  for (const e of model.edges) {
    const srcMissing = !nodeById.has(e.sourceId)
    const tgtMissing = !nodeById.has(e.targetId)
    if (srcMissing || tgtMissing) {
      issues.push({
        id: 'dangling-edge',
        severity: 'error',
        edgeId: e.id,
        message: `Error — edge "${e.type}" points to a node that no longer exists. Fix: delete the edge, or reconnect it to an existing node.`,
      })
    }
  }

  // outgoing/incoming edge tallies by type
  const outByType = new Map<string, Map<string, number>>() // edgeType -> sourceId -> count
  const bump = (type: string, id: string) => {
    let m = outByType.get(type)
    if (!m) {
      m = new Map()
      outByType.set(type, m)
    }
    m.set(id, (m.get(id) ?? 0) + 1)
  }
  for (const e of model.edges) bump(e.type, e.sourceId)
  const produces = outByType.get('commandProducesEvent') ?? new Map()
  const feeds = outByType.get('eventFeedsQuery') ?? new Map()

  for (const node of model.nodes) {
    // ── warning: node in no slice (won't appear in any feature flow) ──
    if (node.sliceId === null) {
      issues.push({
        id: 'node-no-slice',
        severity: 'warning',
        nodeId: node.id,
        message: `Warning — "${node.name}" is in no slice, so it won't appear in any feature flow. Fix: drag it onto a slice column.`,
      })
    }
    // ── warning: command that produces no event ──
    if (node.type === 'command' && (produces.get(node.id) ?? 0) === 0) {
      issues.push({
        id: 'command-no-event',
        severity: 'warning',
        nodeId: node.id,
        message: `Warning — command "${node.name}" produces no event. Every command should emit at least one. Fix: draw an edge from "${node.name}" to an event.`,
      })
    }
    // ── info: event feeding too many read models (over-wiring smell) ──
    if (node.type === 'event') {
      const n = feeds.get(node.id) ?? 0
      if (n >= OVERWIRE_THRESHOLD) {
        issues.push({
          id: 'event-overwired',
          severity: 'info',
          nodeId: node.id,
          message: `Info — event "${node.name}" feeds ${n} read models, which often means over-wiring. Fix: review and prune the queries it doesn't actually feed.`,
        })
      }
    }
  }

  // ── warning: empty feature (submodel owns no chapters) ──
  const chapterCountBySubmodel = new Map<string, number>()
  for (const c of model.chapters) {
    if (c.submodelId) chapterCountBySubmodel.set(c.submodelId, (chapterCountBySubmodel.get(c.submodelId) ?? 0) + 1)
  }
  for (const sm of model.submodels) {
    if ((chapterCountBySubmodel.get(sm.id) ?? 0) === 0) {
      issues.push({
        id: 'empty-feature',
        severity: 'warning',
        featureId: sm.id,
        message: `Warning — feature "${sm.name}" has no chapters, so its page is empty. Fix: assign a chapter to it, or delete the feature.`,
      })
    }
  }

  // ── warning: chapter with no slices ──
  const sliceCountByChapter = new Map<string, number>()
  for (const s of model.slices) {
    if (s.chapterId) sliceCountByChapter.set(s.chapterId, (sliceCountByChapter.get(s.chapterId) ?? 0) + 1)
  }
  for (const c of model.chapters) {
    if ((sliceCountByChapter.get(c.id) ?? 0) === 0) {
      issues.push({
        id: 'chapter-no-slices',
        severity: 'warning',
        chapterId: c.id,
        message: `Warning — chapter "${c.name}" has no slices, so it renders as an empty page. Fix: add a slice to it.`,
      })
    }
  }

  // ── info: cross-feature edge (the expected boundary-portal case) ──
  if (model.submodels.length > 0) {
    for (const e of model.edges) {
      if (!nodeById.has(e.sourceId) || !nodeById.has(e.targetId)) continue // already flagged
      const sf = featureOf(e.sourceId)
      const tf = featureOf(e.targetId)
      if (sf !== tf) {
        const target = tf === null ? 'Ungrouped' : submodelName.get(tf) ?? 'another feature'
        issues.push({
          id: 'cross-feature-edge',
          severity: 'info',
          edgeId: e.id,
          message: `Info — "${nameOf(e.sourceId)}" connects into "${target}", shown as a boundary portal. Click the portal to trace it across features.`,
        })
      }
    }
  }

  return issues
}

export interface IssueCounts {
  error: number
  warning: number
  info: number
}

export function countIssues(issues: readonly Issue[]): IssueCounts {
  const counts: IssueCounts = { error: 0, warning: 0, info: 0 }
  for (const i of issues) counts[i.severity] += 1
  return counts
}
