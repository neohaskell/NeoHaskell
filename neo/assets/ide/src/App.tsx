import { useReducer, useCallback, useState, useEffect, useRef, useMemo } from 'react'
import { ReactFlowProvider } from '@xyflow/react'
import { Flex, Box } from '@mantine/core'
import { notifications } from '@mantine/notifications'
import { reducer, ModelContext } from './state/store'
import { Canvas } from './ui/Canvas'
import { HeaderBar } from './ui/shell/HeaderBar'
import { ActivityRail } from './ui/shell/ActivityRail'
import { EmptyLens } from './ui/lenses/EmptyLens'
import type { Lens } from './ui/lenses/lenses'
import { CommandPalette } from './ui/CommandPalette'
import { createNode, createSuccessor, type CreatePlacement } from './ui/canvas/nodeCreation'
import { InvalidModelModal } from './ui/InvalidModelModal'
import { FeatureNavigator } from './ui/FeatureNavigator'
import { HealingOverlay, type HealLogLine } from './ui/HealingOverlay'
import { newModel, jsonToModel, modelToJson } from './io/fileOps'
import { saveToStorage, loadFromStorage } from './io/persistence'
import { getEdgeTypeForConnection } from './ui/connectionRules'
import { computeNodeAlignments } from './ui/layout/grid'
import { autoLayoutMissingPositions } from './ui/layout/autoLayout'
import { reflowBands } from './ui/layout/bandGrid'
import { buildNodeSubmodelMap } from './ui/layout/submodels'
import { UNGROUPED_FEATURE } from './ui/featurePages'
import type { EdgeType, EventModel, NodeType } from './model/types'
import { IdeClient, type ConnectionState, type RpcResult } from './ipc/client'
import { initialize, type InitializeResult } from './ipc/initialize'
import {
  readEventModel,
  writeEventModel,
  healEventModel,
  cancelHealEventModel,
  relayoutEventModel,
  type ReadEventModelResult,
  type ValidationError,
} from './ipc/eventModel'
import { StatusBar } from './ipc/StatusBar'
import { useAutosave } from './io/useAutosave'
import { validate, countIssues, type Issue } from './model/validate'
import { ProblemsPanel } from './ui/ProblemsPanel'

const CLIENT_INFO = { name: 'neoide-frontend', version: '0.0.1' }

// Transient status messages now go through Mantine's notification system
// (replaces the old bespoke <Toast>). Auto-dismisses; bottom-center per main.tsx.
const notify = (message: string) => notifications.show({ message })

function getInitialModel() {
  return loadFromStorage() ?? newModel()
}

/**
 * When submodels are in use, lay each one out as its own band grid. No-op
 * (returns the same model) for legacy models with no submodel assigned, so
 * the global-timeline layout is untouched. Idempotent.
 */
function applyBandReflow(model: EventModel): EventModel {
  const adjustments = reflowBands(model)
  if (adjustments.length === 0) return model
  const positions = { ...model.layout.nodePositions }
  for (const a of adjustments) positions[a.nodeId] = { x: a.x, y: a.y }
  return { ...model, layout: { ...model.layout, nodePositions: positions } }
}

function App() {
  const [model, dispatch] = useReducer(reducer, null, getInitialModel)
  const [dirty, setDirty] = useState(false)
  const [flashingSliceId, setFlashingSliceId] = useState<string | null>(null)
  const [flashingEntityId, setFlashingEntityId] = useState<string | null>(null)
  const modelRef = useRef(model)
  modelRef.current = model

  // ── "Features as pages" view state ──────────────────────────
  // A model with >=1 submodel shows exactly one FEATURE (page) at a time. The
  // active feature is local UI state (not part of the persisted model). When
  // there are no submodels, the canvas falls back to the flat global timeline.
  const [activeFeatureId, setActiveFeatureId] = useState<string>(UNGROUPED_FEATURE)
  // Active lens (Model built; Schema/Logs/Emulate are roadmap placeholders).
  const [lens, setLens] = useState<Lens>('model')
  const featureMode = model.submodels.length > 0
  const nodeSubmodel = useMemo(() => buildNodeSubmodelMap(model), [model])
  const hasUngrouped = useMemo(
    () =>
      model.chapters.some((c) => !c.submodelId) ||
      model.nodes.some((n) => (nodeSubmodel.get(n.id) ?? null) === null),
    [model.chapters, model.nodes, nodeSubmodel],
  )

  // Breadcrumb: the active feature's name (null in flat mode / no submodels).
  const activeFeatureName = useMemo(() => {
    if (!featureMode) return null
    if (activeFeatureId === UNGROUPED_FEATURE) return 'Ungrouped'
    return model.submodels.find((s) => s.id === activeFeatureId)?.name ?? null
  }, [featureMode, activeFeatureId, model.submodels])

  // Auto-save to localStorage on every model change
  useEffect(() => {
    saveToStorage(model)
  }, [model])

  // Open the JSON-RPC connection to the embedded `neo` server and call
  // `initialize` once. The StatusBar at the bottom reflects connection state
  // and shows the server version + workspace it connected to. When served
  // by `vite dev` (no Rust server at the same origin), the WS open fails and
  // the StatusBar shows the disconnect — by design.
  //
  // `clientRef` is hoisted so handleSave / handleOpen can reach the same
  // connection. The effect closes the client on unmount.
  const [conn, setConn] = useState<ConnectionState>({ status: 'connecting' })
  const [init, setInit] = useState<InitializeResult | null>(null)
  const [pendingInvalid, setPendingInvalid] = useState<{
    errors: ValidationError[]
    preamble?: string
  } | null>(null)
  const [healing, setHealing] = useState(false)
  const [cancellingHeal, setCancellingHeal] = useState(false)
  const [relayouting, setRelayouting] = useState(false)
  // Bumped to re-fit the canvas after a server reload moves nodes (e.g.
  // "Tidy by flow"), so the tidied diagram is framed instead of off-screen.
  const [fitSignal, setFitSignal] = useState<number | undefined>(undefined)
  const [healLog, setHealLog] = useState<HealLogLine[]>([])
  const clientRef = useRef<IdeClient | null>(null)

  // Keep the active feature valid: when the model has features but the current
  // selection no longer exists (deleted, or Ungrouped emptied), fall back to
  // the first feature so the canvas never renders an unknown empty page.
  useEffect(() => {
    if (!featureMode) return
    const valid =
      activeFeatureId === UNGROUPED_FEATURE
        ? hasUngrouped
        : model.submodels.some((s) => s.id === activeFeatureId)
    if (!valid) {
      const first = [...model.submodels].sort((a, b) => a.order - b.order)[0]
      setActiveFeatureId(first ? first.id : UNGROUPED_FEATURE)
      setFitSignal((n) => (n ?? 0) + 1)
    }
  }, [featureMode, activeFeatureId, model.submodels, hasUngrouped])

  // ── Autosave (git is the rollback layer; no Save button) ────
  const connOpen = conn.status === 'open'
  const writeFn = useCallback(async (content: string) => {
    const client = clientRef.current
    if (!client) return false
    const res = await writeEventModel(client, content)
    return res.ok
  }, [])
  const { saveState, flushNow } = useAutosave({
    model,
    dirty,
    connOpen,
    write: writeFn,
    onSaved: () => setDirty(false),
  })

  // Cmd/Ctrl+S flushes the autosave immediately (muscle memory; not a git op).
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if ((e.metaKey || e.ctrlKey) && (e.key === 's' || e.key === 'S')) {
        e.preventDefault()
        void flushNow()
      }
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [flushNow])

  // ── Live validation (debounced 350ms, never blocks autosave) ─
  const [issues, setIssues] = useState<Issue[]>([])
  useEffect(() => {
    const id = setTimeout(() => setIssues(validate(model)), 350)
    return () => clearTimeout(id)
  }, [model])
  const issueCounts = useMemo(() => countIssues(issues), [issues])
  const [problemsOpen, setProblemsOpen] = useState(false)

  // ── Focus primitive: select + frame a node, switching feature if needed.
  // Drives Problems-panel jump and cross-feature portal hops. `focusRequest`
  // is consumed by Canvas (selects + fits the node once its feature mounts).
  const [focusRequest, setFocusRequest] = useState<{ nodeId: string; nonce: number } | null>(null)
  const focusNode = useCallback((nodeId: string) => {
    const m = modelRef.current
    if (!m.nodes.some((n) => n.id === nodeId)) return
    if (m.submodels.length > 0) {
      const feature = buildNodeSubmodelMap(m).get(nodeId) ?? null
      setActiveFeatureId(feature ?? UNGROUPED_FEATURE)
    }
    setFocusRequest((prev) => ({ nodeId, nonce: (prev?.nonce ?? 0) + 1 }))
    setFitSignal((n) => (n ?? 0) + 1)
  }, [])

  // Subscribe to `$/progress` notifications while a heal is in flight so the
  // HealingOverlay can render claude's stdout/stderr line by line. Each
  // notification arrives as `{ token, value: { kind: "log"|"begin"|"end",
  // stream?, line? } }`.
  useEffect(() => {
    if (!healing) return
    const client = clientRef.current
    if (!client) return
    setHealLog([])
    const unsubscribe = client.onNotification('$/progress', (params: unknown) => {
      const p = params as
        | {
            token?: string
            value?: {
              kind?: string
              stream?: 'stdout' | 'stderr'
              line?: string
              appliedCount?: number
              residualCount?: number
              summary?: string
            }
          }
        | undefined
      if (!p || p.token !== 'healEventModel') return
      const value = p.value
      if (!value) return
      if (
        value.kind === 'log' &&
        typeof value.line === 'string' &&
        (value.stream === 'stdout' || value.stream === 'stderr')
      ) {
        setHealLog((prev) => [...prev, { stream: value.stream!, line: value.line! }])
        return
      }
      if (value.kind === 'autoRepair') {
        // The deterministic Rust pre-pass announces what it just patched.
        // Surface it as a synthetic stdout line so the existing reducer
        // can turn it into a structured `auto_repair` event card.
        setHealLog((prev) => [
          ...prev,
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'neo_auto_repair',
              appliedCount: value.appliedCount ?? 0,
              residualCount: value.residualCount ?? 0,
              summary: value.summary ?? '',
            }),
          },
        ])
      }
    })
    return unsubscribe
  }, [healing])

  const applyReadResult = useCallback(
    (readRes: RpcResult<ReadEventModelResult>) => {
      if (!readRes.ok) {
        notify(`Failed to read event-model.json: ${readRes.error.message}`)
        return
      }
      const { content, validation } = readRes.result
      switch (validation.status) {
        case 'notFound':
          // Fresh project — keep whatever the reducer started with
          // (localStorage or the empty `newModel()`).
          return
        case 'valid':
          if (content !== null) {
            const parsed = jsonToModel(content)
            // Fill in any missing node positions so the canvas renders a
            // sensible default layout instead of stacking everything at the
            // origin (common after AI healing, which often omits positions).
            // Idempotent — a no-op when every node already has a real position.
            const laidOut = autoLayoutMissingPositions(parsed)
            // Then, if submodels are in use, lay each one out as its own band
            // grid — so reloads after Tidy / heal / chapter-reorder restore the
            // banded layout instead of leaving the global single timeline.
            const loaded = applyBandReflow(laidOut)
            dispatch({ type: 'loadModel', model: loaded })
            setDirty(loaded !== parsed)
          }
          return
        case 'invalid':
          setPendingInvalid({ errors: validation.errors })
          return
        case 'malformedJson':
          setPendingInvalid({
            errors: [
              {
                pointer: '',
                message: `File is not valid JSON: ${validation.parseError}`,
                kind: 'schema',
              },
            ],
            preamble:
              'event-model.json on disk is not valid JSON. An AI agent can attempt to repair it, or you can cancel and keep your local copy.',
          })
          return
      }
    },
    [],
  )

  useEffect(() => {
    const client = new IdeClient()
    clientRef.current = client
    const unsubscribe = client.onState(setConn)
    // The Rust background sync rewrites `event-model.json` whenever source code
    // changes the schema, then broadcasts a `$/eventModelChanged` notification.
    // Fields are source-owned/read-only, so the IDE adopts the on-disk model on
    // every such push: re-read and replace the in-memory store (same load path
    // as the initial read — parse → auto-layout → band reflow → loadModel).
    const unsubscribeChanged = client.onNotification(
      '$/eventModelChanged',
      () => {
        ;(async () => {
          const reload = await readEventModel(client)
          applyReadResult(reload)
        })()
      },
    )
    ;(async () => {
      const initRes = await initialize(client, CLIENT_INFO)
      if (!initRes.ok) {
        console.error('neo initialize failed', initRes.error)
        return
      }
      setInit(initRes.result)
      const readRes = await readEventModel(client)
      applyReadResult(readRes)
    })()
    return () => {
      unsubscribe()
      unsubscribeChanged()
      client.close()
      clientRef.current = null
    }
  }, [applyReadResult])

  const handleHealAccept = useCallback(async () => {
    const client = clientRef.current
    if (!client) {
      notify('not connected to neo — cannot heal')
      setPendingInvalid(null)
      return
    }
    setHealing(true)
    const healRes = await healEventModel(client)
    if (!healRes.ok) {
      notify(`Healing failed: ${healRes.error.message}`)
      setHealing(false)
      setCancellingHeal(false)
      setPendingInvalid(null)
      return
    }
    if (healRes.result.outcome.status === 'stillInvalid') {
      setPendingInvalid({
        errors: healRes.result.outcome.errors,
        preamble:
          'Healing ran but the file is still invalid. Click Heal to try again, or Cancel to keep your local copy.',
      })
      setHealing(false)
      setCancellingHeal(false)
      return
    }
    if (healRes.result.outcome.status === 'cancelled') {
      const applied = healRes.result.outcome.deterministicApplied
      notify(
        applied > 0
          ? `Heal cancelled — kept ${applied} deterministic fix${applied === 1 ? '' : 'es'}.`
          : 'Heal cancelled.',
      )
      setHealing(false)
      setCancellingHeal(false)
      setPendingInvalid(null)
      // The deterministic patches are on disk — pull them in so the canvas reflects them.
      const reload = await readEventModel(client)
      applyReadResult(reload)
      return
    }
    // Healed — reload from disk.
    const reload = await readEventModel(client)
    setHealing(false)
    setCancellingHeal(false)
    setPendingInvalid(null)
    applyReadResult(reload)
  }, [applyReadResult])

  const handleHealCancel = useCallback(() => {
    setPendingInvalid(null)
  }, [])

  // Cancel button on the in-flight HealingOverlay. Fires the
  // `workspace/cancelHealEventModel` RPC; the in-flight heal promise
  // resolves with `outcome.status === 'cancelled'` and the handlers
  // above transition the UI back out of the healing state.
  const handleAbortHeal = useCallback(async () => {
    const client = clientRef.current
    if (!client || cancellingHeal) return
    setCancellingHeal(true)
    const res = await cancelHealEventModel(client)
    if (!res.ok) {
      notify(`Could not cancel heal: ${res.error.message}`)
      setCancellingHeal(false)
    }
    // On success we deliberately leave `cancellingHeal=true` until the
    // in-flight `healEventModel` promise resolves with the cancelled
    // outcome (or with the agent's already-completed result if cancel
    // raced the LLM exit). That handler resets the state.
  }, [cancellingHeal])

  // Manual "Heal with AI" button on the FileMenu. Always invokes claude
  // (mode='improve'), even when validation is passing — that's how the
  // user asks the agent to refine layout / add inferred edges.
  const handleManualHeal = useCallback(async () => {
    const client = clientRef.current
    if (!client) {
      notify('not connected to neo — cannot heal')
      return
    }
    // Autosave already persisted local edits; flush any pending write so the
    // agent operates on the latest model.
    await flushNow()
    setHealing(true)
    const healRes = await healEventModel(client, 'improve')
    if (!healRes.ok) {
      notify(`Healing failed: ${healRes.error.message}`)
      setHealing(false)
      setCancellingHeal(false)
      return
    }
    if (healRes.result.outcome.status === 'stillInvalid') {
      setPendingInvalid({
        errors: healRes.result.outcome.errors,
        preamble:
          'The AI improved the file but it now has validation errors. Cancel to keep the agent’s changes on disk, or Heal to ask the agent to fix them.',
      })
      setHealing(false)
      setCancellingHeal(false)
      return
    }
    if (healRes.result.outcome.status === 'cancelled') {
      const applied = healRes.result.outcome.deterministicApplied
      notify(
        applied > 0
          ? `Heal cancelled — kept ${applied} deterministic fix${applied === 1 ? '' : 'es'}.`
          : 'Heal cancelled.',
      )
      setHealing(false)
      setCancellingHeal(false)
      const reload = await readEventModel(client)
      applyReadResult(reload)
      return
    }
    // Reload from disk.
    const reload = await readEventModel(client)
    setHealing(false)
    setCancellingHeal(false)
    applyReadResult(reload)
  }, [applyReadResult, flushNow])

  // "Tidy by flow" button (FileMenu). Runs the deterministic layout +
  // wave-ordering pass server-side — no LLM, no model structural changes —
  // and reloads the file. Disabled when a heal is in flight (mutual exclusion).
  const handleRelayout = useCallback(async () => {
    const client = clientRef.current
    if (!client) {
      notify('not connected to neo — cannot tidy')
      return
    }
    await flushNow()
    setRelayouting(true)
    const res = await relayoutEventModel(client)
    if (!res.ok) {
      notify(`Tidy by flow failed: ${res.error.message}`)
      setRelayouting(false)
      return
    }
    if (res.result.applied === 0) {
      notify('Already tidy — slices already follow the flow.')
    } else {
      notify(
        `Tidied by flow — ${res.result.applied} change${res.result.applied === 1 ? '' : 's'}.`,
      )
    }
    const reload = await readEventModel(client)
    setRelayouting(false)
    applyReadResult(reload)
    // Re-frame the canvas on the tidied (possibly moved) nodes.
    setFitSignal((n) => (n ?? 0) + 1)
  }, [applyReadResult, flushNow])

  // Chapters panel: drag-reorder a chapter. Persists the new `chapter.order`,
  // then runs the SAME server-side relayout as "Tidy by flow" — which now
  // ranks flows by `chapter.order` — so the timeline visibly resequences and
  // the order survives later tidy/heal. Writing first preserves any unsaved
  // edits (unlike Tidy, which discards them). Gated on `relayouting` so an
  // in-flight round-trip blocks overlapping drops (race-safe).
  const handleReorderChapters = useCallback(
    async (orderedChapterIds: string[]) => {
      const client = clientRef.current
      if (!client) {
        notify('not connected to neo — cannot reorder chapters')
        return
      }
      if (relayouting) return
      // `dispatch` schedules a re-render; it does NOT update modelRef.current
      // synchronously, so compute the next model forward for the disk write
      // (mirrors handleAssignNodeToSlice / handleAssignChapterToSubmodel).
      const action = { type: 'reorderChapters' as const, orderedChapterIds }
      const next = reducer(modelRef.current, action)
      dispatch(action) // optimistic: the panel reorders instantly
      setRelayouting(true)
      const written = await writeEventModel(client, modelToJson(next))
      if (!written.ok) {
        notify(`Reorder failed: ${written.error.message}`)
        setRelayouting(false)
        return
      }
      const res = await relayoutEventModel(client)
      if (!res.ok) {
        notify(`Reorder failed: ${res.error.message}`)
        setRelayouting(false)
        return
      }
      const reload = await readEventModel(client)
      setRelayouting(false)
      applyReadResult(reload)
      setFitSignal((n) => (n ?? 0) + 1)
    },
    [applyReadResult, relayouting],
  )

  const markDirty = useCallback(() => setDirty(true), [])

  // Navigator slice drag-and-drop: reorder a slice and/or move it to another
  // chapter, in one op. Writes `slice.chapterId`/`order` and lets autosave +
  // the per-feature grid re-lay out the columns — no relayout round-trip (the
  // wave pass would override a manual slice order; "Tidy by flow" still can).
  const handleMoveSlice = useCallback(
    (sliceId: string, chapterId: string | null, orderedSliceIds: string[]) => {
      dispatch({ type: 'moveSliceToChapter', sliceId, chapterId, orderedSliceIds })
      markDirty()
    },
    [markDirty],
  )

  const handleAddEvent = useCallback(() => {
    dispatch({ type: 'addEvent', name: 'New Event' })
    markDirty()
  }, [markDirty])

  const handleAddCommand = useCallback(() => {
    dispatch({ type: 'addCommand', name: 'New Command' })
    markDirty()
  }, [markDirty])

  const handleAddQuery = useCallback(() => {
    dispatch({ type: 'addQuery', name: 'New Query' })
    markDirty()
  }, [markDirty])

  const handleAddIntegration = useCallback(() => {
    dispatch({ type: 'addIntegration', name: 'New Integration', kind: 'outbound' })
    markDirty()
  }, [markDirty])

  const handleAddUIPlaceholder = useCallback(() => {
    dispatch({ type: 'addUIPlaceholder', name: 'New UI' })
    markDirty()
  }, [markDirty])

  const handleAddEntity = useCallback(() => {
    dispatch({ type: 'addEntity', name: 'New Entity' })
    markDirty()
  }, [markDirty])

  const handleRemoveEntity = useCallback(
    (entityId: string) => {
      const hasEvents = modelRef.current.nodes.some(
        (n) => n.type === 'event' && n.entityId === entityId,
      )
      if (hasEvents) {
        notify('You can only delete entities without events')
        setFlashingEntityId(entityId)
        setTimeout(() => setFlashingEntityId(null), 600)
        return
      }
      dispatch({ type: 'removeEntity', entityId })
      markDirty()
    },
    [markDirty],
  )

  const handleAddSlice = useCallback(() => {
    dispatch({ type: 'addSlice', name: 'New Slice' })
    markDirty()
  }, [markDirty])

  // Feature-aware add: the canvas "+" attaches the new slice to a chapter (the
  // active feature's last) so it appears as a column inside the feature instead
  // of landing ungrouped/invisible.
  const handleAddSliceToChapter = useCallback(
    (chapterId: string | null) => {
      dispatch({ type: 'addSlice', name: 'New Slice', chapterId: chapterId ?? undefined })
      markDirty()
    },
    [markDirty],
  )

  const handleRemoveSlice = useCallback(
    (sliceId: string) => {
      const hasNodes = modelRef.current.nodes.some((n) => n.sliceId === sliceId)
      if (hasNodes) {
        notify('You can only delete empty slices')
        setFlashingSliceId(sliceId)
        setTimeout(() => setFlashingSliceId(null), 600)
        return
      }
      dispatch({ type: 'removeSlice', sliceId })
      markDirty()
    },
    [markDirty],
  )

  const handleAddChapter = useCallback(
    (submodelId?: string | null) => {
      dispatch({ type: 'addChapter', name: 'New Chapter', submodelId })
      markDirty()
    },
    [markDirty],
  )

  const handleRemoveChapter = useCallback(
    (chapterId: string) => {
      dispatch({ type: 'removeChapter', chapterId })
      markDirty()
    },
    [markDirty],
  )

  const handleRenameChapter = useCallback(
    (chapterId: string, name: string) => {
      dispatch({ type: 'renameChapter', chapterId, name })
      markDirty()
    },
    [markDirty],
  )


  const handleChapterSliceRange = useCallback(
    (chapterId: string, startSliceId: string, endSliceId: string) => {
      dispatch({ type: 'setChapterSliceRange', chapterId, startSliceId, endSliceId })
      markDirty()
    },
    [markDirty],
  )

  const handlePositionChange = useCallback(
    (nodeId: string, x: number, y: number) => {
      dispatch({ type: 'updatePosition', nodeId, x, y })
      markDirty()
    },
    [markDirty],
  )

  const handleConnect = useCallback(
    (sourceId: string, targetId: string, sourceHandle: string | null, targetHandle: string | null) => {
      const sourceNode = modelRef.current.nodes.find((n) => n.id === sourceId)
      const targetNode = modelRef.current.nodes.find((n) => n.id === targetId)
      if (!sourceNode || !targetNode) return

      const edgeType = getEdgeTypeForConnection(sourceNode.type, targetNode.type)
      if (!edgeType) return

      dispatch({
        type: 'addEdge',
        edgeType: edgeType as EdgeType,
        sourceId,
        targetId,
        sourceHandle,
        targetHandle,
      })
      markDirty()
    },
    [markDirty],
  )

  const handleNodesDelete = useCallback(
    (nodeIds: string[]) => {
      for (const id of nodeIds) {
        dispatch({ type: 'removeNode', nodeId: id })
      }
      markDirty()
    },
    [markDirty],
  )

  // Gesture creation: a node where the cursor is (right-click / double-click).
  // Forward-compute so the new node id is known, then select + frame it.
  const handleCreateNodeAt = useCallback(
    (type: NodeType, place: CreatePlacement) => {
      const { model: next, nodeId } = createNode(modelRef.current, type, place)
      dispatch({ type: 'loadModel', model: next })
      markDirty()
      setFocusRequest((prev) => ({ nodeId, nonce: (prev?.nonce ?? 0) + 1 }))
    },
    [markDirty],
  )

  // Drag-a-wire-into-empty / "Add successor": create the valid successor + edge.
  const handleCreateSuccessor = useCallback(
    (sourceId: string, targetType: NodeType, place: CreatePlacement) => {
      const { model: next, nodeId } = createSuccessor(modelRef.current, sourceId, targetType, place)
      if (!nodeId) return
      dispatch({ type: 'loadModel', model: next })
      markDirty()
      setFocusRequest((prev) => ({ nodeId, nonce: (prev?.nonce ?? 0) + 1 }))
    },
    [markDirty],
  )

  const handleEdgesDelete = useCallback(
    (edgeIds: string[]) => {
      for (const id of edgeIds) {
        dispatch({ type: 'removeEdge', edgeId: id })
      }
      markDirty()
    },
    [markDirty],
  )

  const handleNodeRename = useCallback(
    (nodeId: string, name: string) => {
      dispatch({ type: 'updateNodeName', nodeId, name })
      markDirty()
    },
    [markDirty],
  )

  const handleEntityRename = useCallback(
    (entityId: string, name: string) => {
      dispatch({ type: 'renameEntity', entityId, name })
      markDirty()
    },
    [markDirty],
  )

  const handleAssignNodeToSlice = useCallback(
    (nodeId: string, sliceId: string | null, x: number, y: number) => {
      // Chain: position update → slice assignment → alignment, all computed forward
      const afterPosition = reducer(modelRef.current, { type: 'updatePosition', nodeId, x, y })
      const afterSlice = reducer(afterPosition, { type: 'assignNodeToSlice', nodeId, sliceId })
      const adjustments = computeNodeAlignments(afterSlice)

      dispatch({ type: 'updatePosition', nodeId, x, y })
      dispatch({ type: 'assignNodeToSlice', nodeId, sliceId })
      if (adjustments.length > 0) {
        dispatch({ type: 'batchUpdatePositions', changes: adjustments })
      }
      markDirty()
    },
    [markDirty],
  )

  const handleAssignNodeToEntity = useCallback(
    (nodeId: string, entityId: string | null) => {
      dispatch({ type: 'assignNodeToEntity', nodeId, entityId })
      markDirty()
    },
    [markDirty],
  )

  // Feature-mode node drop: (re)assign the node to the slice column / entity
  // lane it landed on. The deterministic grid (computeFeatureGrid) then owns its
  // x/y and re-stacks on the next render, so the node snaps into its cell and
  // always follows its slice — no per-feature position override is written.
  const handleFeatureNodeMove = useCallback(
    (
      nodeId: string,
      sliceId: string | null,
      entityId: string | null | undefined,
    ) => {
      if (sliceId !== null) dispatch({ type: 'assignNodeToSlice', nodeId, sliceId })
      if (entityId !== undefined) dispatch({ type: 'assignNodeToEntity', nodeId, entityId })
      markDirty()
    },
    [markDirty],
  )

  // Switch the canvas to a feature. `focusNodeId` (from a cross-feature portal
  // hop) selects + frames that node on arrival so the trace continues.
  const handleSelectFeature = useCallback((featureId: string, focusNodeId?: string) => {
    setActiveFeatureId(featureId)
    setFitSignal((n) => (n ?? 0) + 1)
    if (focusNodeId) setFocusRequest((prev) => ({ nodeId: focusNodeId, nonce: (prev?.nonce ?? 0) + 1 }))
  }, [])

  // Problems-panel row click → jump to the offending element.
  const handleFocusIssue = useCallback(
    (issue: Issue) => {
      if (issue.nodeId) focusNode(issue.nodeId)
      else if (issue.featureId) handleSelectFeature(issue.featureId)
      else if (issue.edgeId) {
        const edge = modelRef.current.edges.find((e) => e.id === issue.edgeId)
        if (edge) focusNode(edge.sourceId)
      }
    },
    [focusNode, handleSelectFeature],
  )

  // Create an empty feature and switch to it.
  const handleAddFeature = useCallback(() => {
    const next = reducer(modelRef.current, { type: 'addSubmodel', name: 'New Feature' })
    const created = next.submodels[next.submodels.length - 1]
    dispatch({ type: 'loadModel', model: next })
    if (created) setActiveFeatureId(created.id)
    setFitSignal((n) => (n ?? 0) + 1)
    markDirty()
  }, [markDirty])

  const handleRenameSubmodel = useCallback(
    (submodelId: string, name: string) => {
      dispatch({ type: 'renameSubmodel', submodelId, name })
      markDirty()
    },
    [markDirty],
  )

  // Delete a feature. removeSubmodel detaches its chapters to Ungrouped (never
  // deletes them). If the deleted feature was active, fall back to the first
  // remaining feature (or Ungrouped).
  const handleDeleteFeature = useCallback(
    (submodelId: string) => {
      const next = reducer(modelRef.current, { type: 'removeSubmodel', submodelId })
      dispatch({ type: 'loadModel', model: next })
      setActiveFeatureId((cur) => {
        if (cur !== submodelId) return cur
        const first = [...next.submodels].sort((a, b) => a.order - b.order)[0]
        return first ? first.id : UNGROUPED_FEATURE
      })
      setFitSignal((n) => (n ?? 0) + 1)
      markDirty()
    },
    [markDirty],
  )

  // Create-from-selection: a new feature owning the selected chapters (their
  // nodes follow transitively via slice → chapter → submodel). Switch to it.
  const handleCreateFeatureFromChapters = useCallback(
    (chapterIds: string[]) => {
      let next = reducer(modelRef.current, { type: 'addSubmodel', name: 'New Feature' })
      const created = next.submodels[next.submodels.length - 1]
      if (created) {
        for (const chapterId of chapterIds) {
          next = reducer(next, {
            type: 'assignChapterToSubmodel',
            chapterId,
            submodelId: created.id,
          })
        }
      }
      dispatch({ type: 'loadModel', model: next })
      if (created) setActiveFeatureId(created.id)
      setFitSignal((n) => (n ?? 0) + 1)
      markDirty()
    },
    [markDirty],
  )

  // Flat-mode chapter-arrow dropdown still assigns a chapter to a submodel.
  const handleAssignChapterToSubmodel = useCallback(
    (chapterId: string, submodelId: string | null) => {
      dispatch({ type: 'assignChapterToSubmodel', chapterId, submodelId })
      markDirty()
    },
    [markDirty],
  )

  const handleSliceRename = useCallback(
    (sliceId: string, name: string) => {
      dispatch({ type: 'renameSlice', sliceId, name })
      markDirty()
    },
    [markDirty],
  )

  const handleNew = useCallback(() => {
    // Persist the current model before resetting (git keeps the history).
    void flushNow()
    dispatch({ type: 'loadModel', model: newModel() })
    setDirty(false)
  }, [flushNow])

  const handleOpen = useCallback(async () => {
    const client = clientRef.current
    if (!client) {
      notify('not connected to neo — cannot Open')
      return
    }
    await flushNow()
    const res = await readEventModel(client)
    if (res.ok && res.result.validation.status === 'notFound') {
      notify('event-model.json does not exist in the workspace yet')
      return
    }
    applyReadResult(res)
  }, [flushNow, applyReadResult])

  return (
    <ModelContext.Provider value={{ model, dispatch }}>
      <ReactFlowProvider>
        <Flex direction="column" w="100%" h="100vh">
          <HeaderBar
            projectName={init?.workspace.project?.name ?? model.name}
            featureName={activeFeatureName}
            onNew={handleNew}
            onOpen={handleOpen}
            onHeal={handleManualHeal}
            onRelayout={handleRelayout}
            healing={healing}
            relayouting={relayouting}
            modelActive={lens === 'model'}
          />
          <Flex flex={1} mih={0}>
            <Box flex={1} mih={0}>
              {lens === 'model' ? (
                <Flex direction="column" h="100%">
                  <Flex flex={1} mih={0}>
                    <FeatureNavigator
                      chapters={model.chapters}
                      slices={model.slices}
                      submodels={model.submodels}
                      activeFeatureId={activeFeatureId}
                      hasUngrouped={hasUngrouped}
                      busy={relayouting || healing}
                      onSelectFeature={handleSelectFeature}
                      onReorder={handleReorderChapters}
                      onMoveSlice={handleMoveSlice}
                      onCreateFeatureFromChapters={handleCreateFeatureFromChapters}
                      onAddFeature={handleAddFeature}
                      onRenameFeature={handleRenameSubmodel}
                      onDeleteFeature={handleDeleteFeature}
                      onAddChapter={handleAddChapter}
                      onDeleteChapter={handleRemoveChapter}
                      onRenameChapter={handleRenameChapter}
                    />
                    <Box flex={1} mih={0}>
                      <Canvas
                        model={model}
                        activeFeatureId={activeFeatureId}
                        onNavigateToFeature={handleSelectFeature}
                        focusRequest={focusRequest ?? undefined}
                        onPositionChange={handlePositionChange}
                        onConnect={handleConnect}
                        onNodesDelete={handleNodesDelete}
                        onEdgesDelete={handleEdgesDelete}
                        onNodeRename={handleNodeRename}
                        onEntityRename={handleEntityRename}
                        onSliceRename={handleSliceRename}
                        onAssignNodeToSlice={handleAssignNodeToSlice}
                        onAssignNodeToEntity={handleAssignNodeToEntity}
                        onFeatureNodeMove={handleFeatureNodeMove}
                        onSliceDelete={handleRemoveSlice}
                        onEntityDelete={handleRemoveEntity}
                        onChapterRename={handleRenameChapter}
                        onChapterSliceRange={handleChapterSliceRange}
                        onChapterDelete={handleRemoveChapter}
                        onSubmodelRename={handleRenameSubmodel}
                        onSubmodelDelete={handleDeleteFeature}
                        onAssignChapterToSubmodel={handleAssignChapterToSubmodel}
                        onCreateNode={handleCreateNodeAt}
                        onCreateSuccessor={handleCreateSuccessor}
                        onAddEntity={handleAddEntity}
                        onAddSlice={handleAddSlice}
                        onAddSliceToChapter={handleAddSliceToChapter}
                        onAddChapter={handleAddChapter}
                        flashingSliceId={flashingSliceId}
                        flashingEntityId={flashingEntityId}
                        fitSignal={fitSignal}
                      />
                    </Box>
                    <ProblemsPanel
                      issues={issues}
                      open={problemsOpen}
                      onClose={() => setProblemsOpen(false)}
                      onFocusIssue={handleFocusIssue}
                    />
                  </Flex>
                </Flex>
              ) : (
                <EmptyLens lens={lens} />
              )}
            </Box>
            <ActivityRail lens={lens} onChange={setLens} />
          </Flex>
          <StatusBar
            state={conn}
            init={init}
            saveState={saveState}
            issueCounts={issueCounts}
            onToggleProblems={() => setProblemsOpen((o) => !o)}
            onRetrySave={() => void flushNow()}
          />
        </Flex>
      </ReactFlowProvider>
      <CommandPalette
        onNew={handleNew}
        onOpen={handleOpen}
        onRelayout={handleRelayout}
        onHeal={handleManualHeal}
        onAddEvent={handleAddEvent}
        onAddCommand={handleAddCommand}
        onAddQuery={handleAddQuery}
        onAddIntegration={handleAddIntegration}
        onAddUIPlaceholder={handleAddUIPlaceholder}
        onAddEntity={handleAddEntity}
        onAddSlice={handleAddSlice}
        onAddChapter={handleAddChapter}
        onSelectLens={setLens}
      />
      {pendingInvalid && !healing && (
        <InvalidModelModal
          errors={pendingInvalid.errors}
          preamble={pendingInvalid.preamble}
          onHeal={handleHealAccept}
          onCancel={handleHealCancel}
        />
      )}
      {healing && (
        <HealingOverlay
          log={healLog}
          onCancel={handleAbortHeal}
          cancelling={cancellingHeal}
        />
      )}
    </ModelContext.Provider>
  )
}

export default App
