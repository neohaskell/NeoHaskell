import { useMemo, useCallback, useState, useEffect, useRef, type MouseEvent as ReactMouseEvent } from 'react'
import {
  ReactFlow,
  Background,
  Controls,
  ConnectionMode,
  type OnNodesChange,
  type OnEdgesChange,
  type OnConnect,
  type OnConnectEnd,
  type Connection,
  type Node,
  type Edge,
  applyNodeChanges,
  applyEdgeChanges,
  useReactFlow,
  useStore,
} from '@xyflow/react'
import { useComputedColorScheme, Menu } from '@mantine/core'
import '@xyflow/react/dist/style.css'
import { nodeTypes } from './nodes'
import { CanvasMenu } from './canvas/CanvasMenu'
import type { CreatePlacement } from './canvas/nodeCreation'
import { successorsFor } from './connectionRules'
import { toReactFlowNodes, toReactFlowEdges } from './adapter'
import { buildGridNodes, computeSliceLayouts, computeEntityLaneLayouts, getEntityAtY, CHAPTER_ARROW_Y, CHAPTER_ARROW_HEIGHT, type SliceLayout } from './layout/grid'
import { buildNodeSubmodelMap } from './layout/submodels'
import {
  computeFeatureGrid,
  buildPerBandGridNodes,
  type BandGrid,
} from './layout/bandGrid'
import {
  UNGROUPED_FEATURE,
  buildFeatureRenderEdges,
  type FeatureId,
} from './featurePages'
import { traceFromNode, type Trace } from './trace'
import type { EventModel, NodeType } from '../model/types'

interface CanvasProps {
  model: EventModel
  /** Active feature (submodel id or UNGROUPED_FEATURE). When the model has
   *  submodels, the canvas shows ONLY this feature's nodes (one screen at a
   *  time). Ignored for legacy models with no submodels (flat timeline). */
  activeFeatureId?: string
  /** Navigate to another feature; `focusNodeId` (a cross-feature portal hop)
   *  selects + frames that node on arrival so the trace continues. */
  onNavigateToFeature?: (featureId: string, focusNodeId?: string) => void
  /** When set (changing nonce), select + frame this node (problems jump / portal hop). */
  focusRequest?: { nodeId: string; nonce: number }
  onPositionChange?: (nodeId: string, x: number, y: number) => void
  onConnect?: (sourceId: string, targetId: string, sourceHandle: string | null, targetHandle: string | null) => void
  onNodesDelete?: (nodeIds: string[]) => void
  onEdgesDelete?: (edgeIds: string[]) => void
  onNodeRename?: (nodeId: string, name: string) => void
  onEntityRename?: (entityId: string, name: string) => void
  onSliceRename?: (sliceId: string, name: string) => void
  onAssignNodeToSlice?: (nodeId: string, sliceId: string | null, x: number, y: number) => void
  onAssignNodeToEntity?: (nodeId: string, entityId: string | null) => void
  /** Feature-mode drop: (re)assign the node to the slice column / entity lane it
   *  was dropped on. The deterministic grid then owns its x/y (it snaps into the
   *  cell), so a node always follows its slice when a slice is pushed/resized.
   *  `entityId === undefined` means leave the node's entity untouched (it isn't
   *  an event, or it was dropped off any lane). */
  onFeatureNodeMove?: (
    nodeId: string,
    sliceId: string | null,
    entityId: string | null | undefined,
  ) => void
  onSliceDelete?: (sliceId: string) => void
  onEntityDelete?: (entityId: string) => void
  onChapterRename?: (chapterId: string, name: string) => void
  onChapterSliceRange?: (chapterId: string, startSliceId: string, endSliceId: string) => void
  onChapterDelete?: (chapterId: string) => void
  onSubmodelRename?: (submodelId: string, name: string) => void
  onSubmodelDelete?: (submodelId: string) => void
  onAssignChapterToSubmodel?: (chapterId: string, submodelId: string | null) => void
  /** Create a node of `type` at a placement (right-click / double-click pane). */
  onCreateNode?: (type: NodeType, place: CreatePlacement) => void
  /** Create the valid successor of a node + the typed edge (drag-to-empty / menu). */
  onCreateSuccessor?: (sourceId: string, targetType: NodeType, place: CreatePlacement) => void
  /** Structural adds from the pane menu (no position). */
  onAddEntity?: () => void
  onAddSlice?: () => void
  /** Feature-aware add-slice for the canvas "+" — attaches to a chapter so the
   *  new slice appears inside the active feature. */
  onAddSliceToChapter?: (chapterId: string | null) => void
  onAddChapter?: () => void
  flashingSliceId?: string | null
  flashingEntityId?: string | null
  /** Bump this to re-fit the viewport to the content (e.g. after a server
   *  reload such as "Tidy by flow", which moves nodes while React Flow's
   *  mount-only `fitView` does not re-run, or after switching feature). */
  fitSignal?: number
}

/** Node types that are visual background (lanes/columns/bands) — excluded
 *  from fit-to-view so the viewport frames the actual content. Chapter arrows
 *  are deliberately NOT excluded: they sit at the top of the diagram and must
 *  be framed (otherwise the viewport scrolls past them and chapters look
 *  missing). */
// Excluded from fit-to-view so the viewport frames real content. The submodel
// band is deliberately NOT excluded: in feature ("page") mode it is the frame
// that bounds the whole feature, and framing it keeps the entity-label gutter
// on-screen (otherwise the lanes' left labels are clipped off the left edge).
const BACKGROUND_NODE_TYPES = new Set([
  'entityLane',
  'sliceColumn',
])

function getSliceAtX(layouts: SliceLayout[], x: number): string | null {
  for (const layout of layouts) {
    if (x >= layout.xStart && x < layout.xStart + layout.width) {
      return layout.sliceId
    }
  }
  return null
}

/**
 * The slice nearest to `x` (containing it, else closest by edge distance).
 * Used for feature-mode node creation: a node MUST land in a slice to be a
 * member of the active feature (membership = slice→chapter→submodel), so a drop
 * outside any column snaps to the nearest one instead of silently becoming
 * ungrouped and vanishing from the page. Returns null only when there are no
 * slices at all.
 */
function nearestSliceAtX(layouts: SliceLayout[], x: number): string | null {
  if (layouts.length === 0) return null
  let best = layouts[0]
  let bestDist = Infinity
  for (const l of layouts) {
    const dist = x < l.xStart ? l.xStart - x : x > l.xStart + l.width ? x - (l.xStart + l.width) : 0
    if (dist < bestDist) {
      bestDist = dist
      best = l
    }
  }
  return best.sliceId
}

const CHAPTER_ARROW_PREFIX = '__chapter-arrow-'

const NODE_LABEL: Record<NodeType, string> = {
  event: 'Event',
  command: 'Command',
  query: 'Query',
  integration: 'Integration',
  uiPlaceholder: 'UI Placeholder',
}

/** Returns the slice at x only if it's free or already owned by chapterId */
function getAvailableSliceAtX(
  layouts: SliceLayout[],
  x: number,
  slices: readonly { id: string; chapterId: string | null }[],
  chapterId: string,
): string | null {
  const sliceId = getSliceAtX(layouts, x)
  if (!sliceId) return null
  const slice = slices.find((s) => s.id === sliceId)
  if (!slice) return null
  if (slice.chapterId !== null && slice.chapterId !== chapterId) return null
  return sliceId
}

// ── Trace styling (selected-node edge highlighting) ──────────
// Colors are theme tokens (see cssVariablesResolver) so the causal line stays
// visible on both the light and dark canvas.
const TRACED_DOMAIN_STYLE = { stroke: 'var(--em-trace)', strokeWidth: 4.5, opacity: 1 }
const TRACED_PORTAL_STYLE = { stroke: 'var(--em-trace-portal)', strokeWidth: 3, opacity: 1 }
const DIM_OPACITY = 0.18

function portalEdgeId(nodeId: string): string | null {
  const m = /^__portal-(?:out|in)-(.+)$/.exec(nodeId)
  return m ? m[1] : null
}

/** Restyle edges for an active trace: highlight the selected node's direct
 *  outgoing edges (animated, dash-marching outward), dim the rest. No-op when
 *  nothing is selected. */
function applyTraceToEdges(edges: Edge[], trace: Trace, active: boolean): Edge[] {
  if (!active) return edges
  return edges.map((e) => {
    if (trace.edgeIds.has(e.id)) {
      const isPortal =
        (e.source?.startsWith('__portal') ?? false) || (e.target?.startsWith('__portal') ?? false)
      return {
        ...e,
        animated: true,
        zIndex: 10,
        style: { ...e.style, ...(isPortal ? TRACED_PORTAL_STYLE : TRACED_DOMAIN_STYLE) },
      }
    }
    return { ...e, animated: false, style: { ...e.style, opacity: DIM_OPACITY } }
  })
}

const TRACEABLE_NODE_TYPES = new Set([
  'event',
  'command',
  'query',
  'integration',
  'uiPlaceholder',
  'boundaryPortal',
])

/** Dim off-trace domain nodes + portals; keep on-trace ones bright (and ring an
 *  on-trace portal — "the flow continues through this door"). Background grid
 *  nodes (lanes/columns/bands) are never touched. */
function applyTraceToNodes(nodes: Node[], trace: Trace, active: boolean): Node[] {
  if (!active) return nodes
  return nodes.map((n) => {
    const type = n.type ?? ''
    if (!TRACEABLE_NODE_TYPES.has(type)) return n
    if (type === 'boundaryPortal') {
      const eid = portalEdgeId(n.id)
      const on = eid ? trace.edgeIds.has(eid) : false
      return {
        ...n,
        style: {
          ...n.style,
          opacity: on ? 1 : DIM_OPACITY,
          outline: on ? '2px solid var(--em-trace-portal)' : undefined,
          borderRadius: on ? 8 : undefined,
        },
      }
    }
    const on = trace.nodeIds.has(n.id)
    return { ...n, style: { ...n.style, opacity: on ? 1 : 0.22 } }
  })
}

export function Canvas({
  model,
  activeFeatureId,
  onNavigateToFeature,
  focusRequest,
  onPositionChange,
  onConnect: onConnectProp,
  onNodesDelete,
  onEdgesDelete,
  onNodeRename,
  onEntityRename,
  onSliceRename,
  onAssignNodeToSlice,
  onAssignNodeToEntity,
  onFeatureNodeMove,
  onSliceDelete,
  onEntityDelete,
  onChapterRename,
  onChapterSliceRange,
  onChapterDelete,
  onSubmodelRename,
  onSubmodelDelete,
  onAssignChapterToSubmodel,
  onCreateNode,
  onCreateSuccessor,
  onAddEntity,
  onAddSlice,
  onAddSliceToChapter,
  onAddChapter,
  flashingSliceId,
  flashingEntityId,
  fitSignal,
}: CanvasProps) {
  const reactFlow = useReactFlow()
  const colorScheme = useComputedColorScheme('dark')
  // Rendered pane dimensions — needed to compute a height-fit zoom for feature
  // mode (fitView only fits a node set, it can't fit one axis). Kept in refs so
  // the fit effect can read the latest size without re-firing on every resize.
  const paneWidth = useStore((s) => s.width)
  const paneHeight = useStore((s) => s.height)
  const paneWidthRef = useRef(paneWidth)
  paneWidthRef.current = paneWidth
  const paneHeightRef = useRef(paneHeight)
  paneHeightRef.current = paneHeight
  const [highlightedSliceId, setHighlightedSliceId] = useState<string | null>(null)
  const [highlightedEntityId, setHighlightedEntityId] = useState<string | null>(null)
  const [selectedSliceId, setSelectedSliceId] = useState<string | null>(null)
  const [selectedEntityId, setSelectedEntityId] = useState<string | null>(null)
  const [selectedChapterId, setSelectedChapterId] = useState<string | null>(null)
  const [selectedNodeId, setSelectedNodeId] = useState<string | null>(null)
  const selectedSliceRef = useRef<string | null>(null)
  selectedSliceRef.current = selectedSliceId
  const selectedEntityRef = useRef<string | null>(null)
  selectedEntityRef.current = selectedEntityId
  const selectedChapterRef = useRef<string | null>(null)
  selectedChapterRef.current = selectedChapterId
  const draggingNodeRef = useRef<string | null>(null)
  const draggingNodeTypeRef = useRef<string | null>(null)
  // Track chapter arrow drag state
  const draggingChapterRef = useRef<string | null>(null)
  const chapterStartSliceRef = useRef<string | null>(null)
  const chapterDragOriginRef = useRef<{ x: number; y: number } | null>(null)
  const sliceLayouts = useMemo(() => computeSliceLayouts(model), [model])
  const sliceLayoutsRef = useRef(sliceLayouts)
  sliceLayoutsRef.current = sliceLayouts
  const entityLaneLayouts = useMemo(() => computeEntityLaneLayouts(model), [model])
  const entityLaneLayoutsRef = useRef(entityLaneLayouts)
  entityLaneLayoutsRef.current = entityLaneLayouts

  // ── Feature ("page") mode ────────────────────────────────────
  // A model with >=1 submodel renders exactly ONE feature at a time (the
  // active one); the stacked-bands view is gone. A legacy model with no
  // submodels renders the flat global timeline, exactly as before.
  const nodeSubmodel = useMemo(() => buildNodeSubmodelMap(model), [model])
  const featureMode = model.submodels.length > 0
  const activeFeature: FeatureId = activeFeatureId ?? UNGROUPED_FEATURE
  const featureFid = activeFeature === UNGROUPED_FEATURE ? null : activeFeature

  const featureGrid = useMemo<BandGrid | null>(
    () => (featureMode ? computeFeatureGrid(model, featureFid) : null),
    [featureMode, model, featureFid],
  )
  const featureGridRef = useRef<BandGrid | null>(featureGrid)
  featureGridRef.current = featureGrid
  const featureModeRef = useRef(featureMode)
  featureModeRef.current = featureMode
  const activeFeatureRef = useRef(activeFeature)
  activeFeatureRef.current = activeFeature

  // Re-fit the viewport whenever `fitSignal` changes (a server reload moved the
  // nodes, or the user switched feature). React Flow's `fitView` prop only runs
  // on mount, so without this an off-origin layout would leave the canvas
  // looking empty until the user manually zoomed to fit.
  //
  // In feature ("page") mode we do NOT fit the whole bounding box. A feature
  // reads top-to-bottom across entity lanes and left-to-right along the causal
  // flow, so the initial camera fits the frame's HEIGHT and anchors to its LEFT
  // edge — the entry point of the flow is always on screen, and a wide flow
  // simply scrolls to the right instead of being zoomed away to nothing.
  useEffect(() => {
    if (fitSignal === undefined) return
    const id = setTimeout(() => {
      const grid = featureGridRef.current
      const w = paneWidthRef.current
      const h = paneHeightRef.current
      if (featureModeRef.current && grid && w > 0 && h > 0) {
        const PAD_Y = 32 // px breathing room above & below the frame
        const PAD_X = 24 // px gutter to the left of the frame's leftmost edge
        const rawZoom = (h - PAD_Y * 2) / grid.rect.height
        const zoom = Math.max(0.2, Math.min(1.5, rawZoom))
        // React Flow viewport maps flowX → screenX = flowX * zoom + x. Pin the
        // frame's left edge at PAD_X and vertically center the height-fit frame.
        const x = PAD_X - grid.rect.xStart * zoom
        const y = (h - grid.rect.height * zoom) / 2 - grid.rect.yStart * zoom
        reactFlow.setViewport({ x, y, zoom }, { duration: 300 })
        return
      }
      const contentNodes = reactFlow
        .getNodes()
        .filter((n) => !BACKGROUND_NODE_TYPES.has(n.type ?? ''))
      if (contentNodes.length > 0) {
        reactFlow.fitView({ nodes: contentNodes, padding: 0.2, duration: 300 })
      }
    }, 0)
    return () => clearTimeout(id)
  }, [fitSignal, reactFlow])

  const featureMemberIds = useMemo(() => {
    if (!featureMode) return null
    const ids = new Set<string>()
    for (const n of model.nodes) if ((nodeSubmodel.get(n.id) ?? null) === featureFid) ids.add(n.id)
    return ids
  }, [featureMode, model.nodes, nodeSubmodel, featureFid])

  // Feature-mode positions come SOLELY from the deterministic grid — it is the
  // single source of truth for node x/y, so every node always follows its slice
  // column when a slice is pushed / resized / reordered. (Legacy per-feature
  // `bySubmodel` overrides are intentionally ignored; see `handleFeatureNodeMove`.)
  const featurePositions = useMemo(() => {
    if (!featureMode || !featureGrid) return null
    return new Map(featureGrid.positions)
  }, [featureMode, featureGrid])

  const domainNodes = useMemo(() => {
    if (featureMode && featurePositions && featureMemberIds) {
      return toReactFlowNodes(model, onNodeRename, {
        positions: featurePositions,
        includeIds: featureMemberIds,
      })
    }
    return toReactFlowNodes(model, onNodeRename)
  }, [featureMode, featurePositions, featureMemberIds, model, onNodeRename])

  const entityNames = useMemo(
    () => new Map(model.entities.map((e) => [e.id, e.name])),
    [model.entities],
  )
  const sliceNames = useMemo(
    () => new Map(model.slices.map((s) => [s.id, s.name])),
    [model.slices],
  )

  const featureName = useCallback(
    (fid: FeatureId) =>
      fid === UNGROUPED_FEATURE
        ? 'Ungrouped'
        : model.submodels.find((s) => s.id === fid)?.name ?? 'Feature',
    [model.submodels],
  )

  const featureRender = useMemo(() => {
    if (!featureMode || !featureGrid) return { edges: [] as Edge[], portalNodes: [] as Node[] }
    return buildFeatureRenderEdges(
      model,
      activeFeature,
      featureGrid,
      nodeSubmodel,
      featureName,
      onNavigateToFeature,
    )
  }, [featureMode, featureGrid, model, activeFeature, nodeSubmodel, featureName, onNavigateToFeature])

  const modelEdges = useMemo(
    () => (featureMode ? featureRender.edges : toReactFlowEdges(model)),
    [featureMode, featureRender, model],
  )
  const [edges, setEdges] = useState<Edge[]>(modelEdges)

  useEffect(() => {
    setEdges(modelEdges)
  }, [modelEdges])
  const handleSliceSelect = useCallback((sliceId: string) => {
    setSelectedSliceId((prev) => (prev === sliceId ? null : sliceId))
    setSelectedEntityId(null)
    setSelectedChapterId(null)
  }, [])

  const handleEntitySelect = useCallback((entityId: string) => {
    setSelectedEntityId((prev) => (prev === entityId ? null : entityId))
    setSelectedSliceId(null)
    setSelectedChapterId(null)
  }, [])

  const handleChapterSelect = useCallback((chapterId: string) => {
    setSelectedChapterId((prev) => (prev === chapterId ? null : chapterId))
    setSelectedSliceId(null)
    setSelectedEntityId(null)
  }, [])

  // Chapter end-handle drag: highlight slice at pointer x (only if available)
  const handleChapterEndDrag = useCallback(
    (chapterId: string, flowX: number) => {
      const sliceId = getAvailableSliceAtX(sliceLayoutsRef.current, flowX, model.slices, chapterId)
      setHighlightedSliceId(sliceId)
    },
    [model.slices],
  )

  // Chapter end-handle drop: set range from start to end slice
  const handleChapterEndDrop = useCallback(
    (chapterId: string, flowX: number) => {
      setHighlightedSliceId(null)
      const endSliceId = getAvailableSliceAtX(sliceLayoutsRef.current, flowX, model.slices, chapterId)
      if (!endSliceId) return

      // Find start slice: the slice the chapter arrow's left edge is over
      const chapterSlices = model.slices.filter((s) => s.chapterId === chapterId)
      let startSliceId: string | null = null
      if (chapterSlices.length > 0) {
        const sorted = [...chapterSlices].sort((a, b) => a.order - b.order)
        startSliceId = sorted[0].id
      } else if (chapterStartSliceRef.current) {
        startSliceId = chapterStartSliceRef.current
      }
      if (!startSliceId) {
        startSliceId = endSliceId
      }
      onChapterSliceRange?.(chapterId, startSliceId, endSliceId)
    },
    [model.slices, onChapterSliceRange],
  )

  const activeHighlight = highlightedSliceId ?? selectedSliceId

  // Global flat-timeline grid (only used when there are no submodels).
  const gridNodes = useMemo(
    () => buildGridNodes(
      model, onEntityRename, onSliceRename, activeHighlight, flashingSliceId,
      handleSliceSelect, highlightedEntityId, flashingEntityId, selectedEntityId,
      handleEntitySelect, onChapterRename, selectedChapterId, handleChapterSelect,
      handleChapterEndDrag, handleChapterEndDrop, model.submodels, onAssignChapterToSubmodel,
    ),
    [model, onEntityRename, onSliceRename, activeHighlight, flashingSliceId, handleSliceSelect, highlightedEntityId, flashingEntityId, selectedEntityId, handleEntitySelect, onChapterRename, selectedChapterId, handleChapterSelect, handleChapterEndDrag, handleChapterEndDrop, onAssignChapterToSubmodel],
  )

  // Background nodes for the single active feature: its slice columns + its
  // (compact) entity lanes + a titled band rectangle. The Ungrouped feature
  // gets no rename/delete affordances (it isn't a real submodel).
  // Canvas "+ slice": attach the new slice to the active feature's last chapter
  // so it lands as a column inside the feature (not ungrouped/invisible). Falls
  // back to the plain add when no chapter-aware handler is wired.
  const handleAddSliceToFeature = useCallback(() => {
    if (onAddSliceToChapter) {
      const fid = activeFeatureRef.current
      const chapters = model.chapters
        .filter((c) => (fid === UNGROUPED_FEATURE ? !c.submodelId : c.submodelId === fid))
        .sort((a, b) => a.order - b.order)
      onAddSliceToChapter(chapters.length ? chapters[chapters.length - 1].id : null)
    } else {
      onAddSlice?.()
    }
  }, [model.chapters, onAddSliceToChapter, onAddSlice])

  const featureBackgroundNodes = useMemo(() => {
    if (!featureMode || !featureGrid) return [] as Node[]
    return buildPerBandGridNodes([featureGrid], {
      entityName: entityNames,
      sliceName: sliceNames,
      highlightedSliceId: activeHighlight,
      flashingSliceId,
      onRenameSlice: onSliceRename,
      onSliceSelect: handleSliceSelect,
      highlightedEntityId,
      flashingEntityId,
      selectedEntityId,
      onRenameEntity: onEntityRename,
      onEntitySelect: handleEntitySelect,
      onSubmodelRename: featureFid ? onSubmodelRename : undefined,
      onSubmodelDelete: featureFid ? onSubmodelDelete : undefined,
      onAddSlice: handleAddSliceToFeature,
      onAddEntity,
    })
  }, [
    featureMode, featureGrid, entityNames, sliceNames, activeHighlight, flashingSliceId,
    onSliceRename, handleSliceSelect, highlightedEntityId, flashingEntityId, selectedEntityId,
    onEntityRename, handleEntitySelect, featureFid, onSubmodelRename, onSubmodelDelete,
    handleAddSliceToFeature, onAddEntity,
  ])

  // Chapter arrows for the active feature: a labeled bar spanning each
  // chapter's slices (positioned against the feature's band-local slice
  // layout). Display-only here — rename / select(delete) / submodel-assign
  // work, but range-resize stays in the flat view (slice ranges are computed
  // over the GLOBAL slice order, which a per-feature drag can't honour).
  const featureChapterArrows = useMemo(() => {
    if (!featureMode || !featureGrid) return [] as Node[]
    const sliceById = new Map(featureGrid.slices.map((s) => [s.sliceId, s]))
    const y = featureGrid.yOrigin + 2
    const arrows: Node[] = []
    for (const chapter of [...model.chapters].sort((a, b) => a.order - b.order)) {
      const layouts = model.slices
        .filter((s) => s.chapterId === chapter.id)
        .map((s) => sliceById.get(s.id))
        .filter((l): l is SliceLayout => l !== undefined)
      if (layouts.length === 0) continue // chapter has no slice on this feature
      const xStart = Math.min(...layouts.map((l) => l.xStart))
      const xEnd = Math.max(...layouts.map((l) => l.xStart + l.width))
      arrows.push({
        id: `__chapter-arrow-${chapter.id}`,
        type: 'chapterArrow',
        position: { x: xStart, y },
        data: {
          label: chapter.name,
          chapterId: chapter.id,
          unassigned: false,
          selected: selectedChapterId === chapter.id,
          onSelect: () => handleChapterSelect(chapter.id),
          onRename: onChapterRename ? (name: string) => onChapterRename(chapter.id, name) : undefined,
          submodels: model.submodels,
          currentSubmodelId: chapter.submodelId ?? null,
          onAssignSubmodel: onAssignChapterToSubmodel
            ? (submodelId: string | null) => onAssignChapterToSubmodel(chapter.id, submodelId)
            : undefined,
        },
        draggable: false,
        selectable: false,
        focusable: false,
        style: {
          width: Math.max(80, xEnd - xStart),
          height: CHAPTER_ARROW_HEIGHT,
          zIndex: 1,
          overflow: 'visible' as const,
          pointerEvents: 'all' as const,
        },
      })
    }
    return arrows
  }, [
    featureMode, featureGrid, model.chapters, model.slices, model.submodels,
    selectedChapterId, handleChapterSelect, onChapterRename, onAssignChapterToSubmodel,
  ])

  const modelNodes = useMemo(() => {
    if (featureMode) {
      return [...featureBackgroundNodes, ...featureChapterArrows, ...featureRender.portalNodes, ...domainNodes]
    }
    return [
      ...gridNodes.entityLaneNodes,
      ...gridNodes.sliceColumnNodes,
      ...gridNodes.chapterArrowNodes,
      ...domainNodes,
    ]
  }, [featureMode, featureBackgroundNodes, featureChapterArrows, featureRender, gridNodes, domainNodes])

  const [nodes, setNodes] = useState<Node[]>(modelNodes)

  useEffect(() => {
    setNodes(modelNodes)
  }, [modelNodes])

  const handleNodesChange: OnNodesChange = useCallback(
    (changes) => {
      setNodes((nds) => applyNodeChanges(changes, nds))

      for (const change of changes) {
        // When a domain node is selected, clear slice/entity selection
        if (change.type === 'select' && change.selected && !change.id.startsWith('__')) {
          setSelectedSliceId(null)
          setSelectedEntityId(null)
          setSelectedChapterId(null)
        }

        if (change.type !== 'position') continue

        // Handle chapter arrow dragging (flat mode only — constrain to Y,
        // highlight slice). Feature mode renders no chapter arrows.
        if (change.id.startsWith(CHAPTER_ARROW_PREFIX)) {
          const chapterId = change.id.slice(CHAPTER_ARROW_PREFIX.length)
          if (change.dragging && change.position) {
            // Save origin on first drag event
            if (!chapterDragOriginRef.current) {
              const currentNode = nodes.find((n) => n.id === change.id)
              if (currentNode) {
                chapterDragOriginRef.current = { ...currentNode.position }
              }
            }
            // Constrain Y: assigned chapters stay on their row,
            // unassigned chapters can drag freely (they'll snap on drop)
            const node = nodes.find((n) => n.id === change.id)
            if (!node?.data?.unassigned) {
              change.position.y = CHAPTER_ARROW_Y
            }
            const sliceId = getAvailableSliceAtX(sliceLayoutsRef.current, change.position.x, model.slices, chapterId)
            setHighlightedSliceId(sliceId)
            draggingChapterRef.current = chapterId
            if (sliceId) chapterStartSliceRef.current = sliceId
          } else if (!change.dragging && change.position) {
            // Dropped — assign start slice or snap back
            setHighlightedSliceId(null)
            const sliceId = getAvailableSliceAtX(sliceLayoutsRef.current, change.position.x, model.slices, chapterId)
            if (sliceId && draggingChapterRef.current) {
              const chapterSlices = model.slices.filter((s) => s.chapterId === chapterId)
              if (chapterSlices.length === 0) {
                onChapterSliceRange?.(chapterId, sliceId, sliceId)
              } else {
                const sorted = [...chapterSlices].sort((a, b) => a.order - b.order)
                const endSliceId = sorted[sorted.length - 1].id
                onChapterSliceRange?.(chapterId, sliceId, endSliceId)
              }
            } else {
              // Invalid drop target — snap back to origin
              const origin = chapterDragOriginRef.current
              if (origin) {
                const nodeId = change.id
                // Defer snap-back to avoid conflicting with applyNodeChanges
                requestAnimationFrame(() => {
                  setNodes((nds) =>
                    nds.map((n) =>
                      n.id === nodeId
                        ? { ...n, position: { x: origin.x, y: origin.y } }
                        : n,
                    ),
                  )
                })
              }
            }
            draggingChapterRef.current = null
            chapterStartSliceRef.current = null
            chapterDragOriginRef.current = null
          }
          continue
        }

        // Skip other __ nodes (slice columns, entity lanes, bands, portals)
        if (change.id.startsWith('__')) continue

        // Hit-test the dragged node against the active feature's local grid
        // (feature mode) or the global grid (flat mode).
        const fGrid = featureModeRef.current ? featureGridRef.current : null
        const sliceLayoutsHit = fGrid ? fGrid.slices : sliceLayoutsRef.current
        const entityLayoutsHit = fGrid ? fGrid.lanes : entityLaneLayoutsRef.current

        if (change.dragging && change.position) {
          // Node is being dragged — highlight the slice under it
          draggingNodeRef.current = change.id
          const nodeType = model.nodes.find((n) => n.id === change.id)?.type ?? null
          draggingNodeTypeRef.current = nodeType
          const sliceId = getSliceAtX(sliceLayoutsHit, change.position.x)
          setHighlightedSliceId(sliceId)
          // Only highlight entity lanes for event nodes
          if (nodeType === 'event') {
            const entityId = getEntityAtY(entityLayoutsHit, change.position.y)
            setHighlightedEntityId(entityId)
          }
        } else if (!change.dragging && change.position) {
          // Node was dropped.
          if (featureModeRef.current) {
            // Feature mode: re-assign the node to the slice column / entity lane
            // it landed on; the deterministic grid then snaps it into that cell,
            // so it stays glued to its slice. Clamp to the NEAREST slice (a
            // feature member must live in a column) so a sloppy drop re-homes
            // instead of floating off-grid.
            const nid = draggingNodeRef.current ?? change.id
            const sliceId = nearestSliceAtX(sliceLayoutsHit, change.position.x)
            const nodeType = model.nodes.find((n) => n.id === nid)?.type ?? null
            // Only reassign the entity when the drop is actually over a lane — a
            // drop off any lane leaves an event on its current aggregate rather
            // than orphaning it (`undefined` = leave entity untouched).
            const entityId =
              nodeType === 'event'
                ? getEntityAtY(entityLayoutsHit, change.position.y) ?? undefined
                : undefined
            onFeatureNodeMove?.(nid, sliceId, entityId)
          } else if (draggingNodeRef.current) {
            const sliceId = getSliceAtX(sliceLayoutsHit, change.position.x)
            onAssignNodeToSlice?.(draggingNodeRef.current, sliceId, change.position.x, change.position.y)
            // Assign event to entity on drop
            if (draggingNodeTypeRef.current === 'event') {
              const entityId = getEntityAtY(entityLayoutsHit, change.position.y)
              onAssignNodeToEntity?.(draggingNodeRef.current, entityId)
            }
          } else {
            onPositionChange?.(change.id, change.position.x, change.position.y)
          }
          draggingNodeRef.current = null
          draggingNodeTypeRef.current = null
          setHighlightedSliceId(null)
          setHighlightedEntityId(null)
        }
      }
    },
    [onPositionChange, onAssignNodeToSlice, onAssignNodeToEntity, onFeatureNodeMove, onChapterSliceRange, model.nodes, model.slices, nodes],
  )

  const handleConnect: OnConnect = useCallback(
    (connection: Connection) => {
      if (onConnectProp && connection.source && connection.target) {
        onConnectProp(connection.source, connection.target, connection.sourceHandle ?? null, connection.targetHandle ?? null)
      }
    },
    [onConnectProp],
  )

  const handleNodesDelete = useCallback(
    (deleted: { id: string }[]) => {
      // Don't delete grid nodes
      const domainDeleted = deleted.filter((n) => !n.id.startsWith('__'))
      if (domainDeleted.length > 0) {
        onNodesDelete?.(domainDeleted.map((n) => n.id))
      }
    },
    [onNodesDelete],
  )

  const handleEdgesChange: OnEdgesChange = useCallback(
    (changes) => {
      setEdges((eds) => applyEdgeChanges(changes, eds))
    },
    [],
  )

  const handleEdgesDelete = useCallback(
    (deleted: { id: string }[]) => {
      // Portal edges share their model edge id, so deleting one removes the
      // underlying model edge — that's correct (the cross-feature edge is
      // gone). Synthetic portal NODES are never deleted (selectable: false).
      onEdgesDelete?.(deleted.map((e) => e.id))
    },
    [onEdgesDelete],
  )

  const handlePaneClick = useCallback(() => {
    setSelectedSliceId(null)
    setSelectedEntityId(null)
    setSelectedChapterId(null)
    setSelectedNodeId(null)
  }, [])

  // ── Gesture node creation (right-click / double-click / drag-into-empty) ──
  // No toolbar: nodes are created where the cursor is. Placement is hit-tested
  // against the active grid so the new node lands in the slice/entity it was
  // dropped on (and, in feature mode, joins that feature).
  type MenuState =
    | { kind: 'none' }
    | { kind: 'pane'; x: number; y: number; place: CreatePlacement }
    | { kind: 'node'; x: number; y: number; nodeId: string; nodeType: NodeType; place: CreatePlacement }
    | { kind: 'successor'; x: number; y: number; sourceId: string; options: NodeType[]; place: CreatePlacement }
  const [menu, setMenu] = useState<MenuState>({ kind: 'none' })
  const closeMenu = useCallback(() => setMenu({ kind: 'none' }), [])

  // Placement from canvas flow coordinates. In feature mode a node MUST land in
  // a slice to be a member of the active feature, so a drop outside any column
  // snaps to the nearest slice (never silently ungrouped → vanished).
  const placementAtFlow = useCallback((fx: number, fy: number): CreatePlacement => {
    const inFeature = featureModeRef.current
    const fGrid = inFeature ? featureGridRef.current : null
    const sliceHit = fGrid ? fGrid.slices : sliceLayoutsRef.current
    const entityHit = fGrid ? fGrid.lanes : entityLaneLayoutsRef.current
    let sliceId = getSliceAtX(sliceHit, fx)
    if (sliceId == null && inFeature) sliceId = nearestSliceAtX(sliceHit, fx)
    return {
      x: fx,
      y: fy,
      sliceId,
      entityId: getEntityAtY(entityHit, fy),
    }
  }, [])

  const placementAtClient = useCallback(
    (clientX: number, clientY: number): CreatePlacement => {
      const flow = reactFlow.screenToFlowPosition({ x: clientX, y: clientY })
      return placementAtFlow(flow.x, flow.y)
    },
    [reactFlow, placementAtFlow],
  )

  const handlePaneContextMenu = useCallback(
    (e: ReactMouseEvent | MouseEvent) => {
      e.preventDefault()
      setMenu({ kind: 'pane', x: e.clientX, y: e.clientY, place: placementAtClient(e.clientX, e.clientY) })
    },
    [placementAtClient],
  )

  const handleNodeContextMenu = useCallback(
    (e: ReactMouseEvent, node: Node) => {
      if (node.id.startsWith('__')) return
      e.preventDefault()
      const nodeType = model.nodes.find((n) => n.id === node.id)?.type
      if (!nodeType) return
      // Hit-test at the source node's own position (same column) and place the
      // successor below it, so it lands in the right slice — not at a pixel
      // offset that may cross into a neighbouring column.
      setMenu({
        kind: 'node',
        x: e.clientX,
        y: e.clientY,
        nodeId: node.id,
        nodeType,
        place: placementAtFlow(node.position.x, node.position.y + 140),
      })
    },
    [placementAtFlow, model.nodes],
  )

  const handleDoubleClick = useCallback(
    (e: ReactMouseEvent) => {
      const target = e.target as HTMLElement
      if (!target.classList?.contains('react-flow__pane')) return
      setMenu({ kind: 'pane', x: e.clientX, y: e.clientY, place: placementAtClient(e.clientX, e.clientY) })
    },
    [placementAtClient],
  )

  // Drop a connection in empty space → spawn the valid successor + typed edge.
  const handleConnectEnd: OnConnectEnd = useCallback(
    (event, connectionState) => {
      // A real edge was made (onConnect handled it), or the wire was dropped
      // onto an existing node (even an invalid target) — don't spawn a duplicate.
      if (connectionState.isValid || connectionState.toNode) return
      const fromId = connectionState.fromNode?.id
      if (!fromId || fromId.startsWith('__')) return
      const sourceType = model.nodes.find((n) => n.id === fromId)?.type
      if (!sourceType) return
      const options = successorsFor(sourceType)
      if (options.length === 0) return
      const pt = 'changedTouches' in event ? event.changedTouches[0] : event
      const place = placementAtClient(pt.clientX, pt.clientY)
      if (options.length === 1) {
        onCreateSuccessor?.(fromId, options[0], place)
      } else {
        setMenu({ kind: 'successor', x: pt.clientX, y: pt.clientY, sourceId: fromId, options, place })
      }
    },
    [model.nodes, placementAtClient, onCreateSuccessor],
  )

  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === 'Escape') {
        setSelectedNodeId(null)
      }
      if (e.key === 'Backspace') {
        if (selectedChapterRef.current) {
          onChapterDelete?.(selectedChapterRef.current)
          setSelectedChapterId(null)
        } else if (selectedSliceRef.current) {
          onSliceDelete?.(selectedSliceRef.current)
        } else if (selectedEntityRef.current) {
          onEntityDelete?.(selectedEntityRef.current)
        }
      }
    }
    window.addEventListener('keydown', handleKeyDown)
    return () => window.removeEventListener('keydown', handleKeyDown)
  }, [onSliceDelete, onEntityDelete, onChapterDelete])

  // ── Trace: highlight the selected node's direct outgoing edges ──
  const trace = useMemo(() => traceFromNode(model, selectedNodeId), [model, selectedNodeId])
  const traceActive = selectedNodeId !== null
  const handleSelectionChange = useCallback(({ nodes: sel }: { nodes: Node[] }) => {
    const domain = sel.find((n) => !n.id.startsWith('__'))
    setSelectedNodeId(domain ? domain.id : null)
  }, [])
  const displayNodes = useMemo(
    () => applyTraceToNodes(nodes, trace, traceActive),
    [nodes, trace, traceActive],
  )
  const displayEdges = useMemo(
    () => applyTraceToEdges(edges, trace, traceActive),
    [edges, trace, traceActive],
  )

  // Focus request (Problems-panel jump / cross-feature portal hop): select +
  // frame the node once the active feature's nodes have mounted.
  useEffect(() => {
    if (!focusRequest) return
    const { nodeId } = focusRequest
    const id = setTimeout(() => {
      setSelectedNodeId(nodeId)
      setNodes((nds) => nds.map((n) => ({ ...n, selected: n.id === nodeId })))
      const target = reactFlow.getNode(nodeId)
      if (target) reactFlow.fitView({ nodes: [target], padding: 0.3, duration: 300, maxZoom: 1.2 })
    }, 60)
    return () => clearTimeout(id)
  }, [focusRequest, reactFlow])

  return (
    <div data-testid="canvas" style={{ width: '100%', height: '100%' }} onDoubleClick={handleDoubleClick}>
      <ReactFlow
        colorMode={colorScheme}
        nodes={displayNodes}
        edges={displayEdges}
        nodeTypes={nodeTypes}
        onNodesChange={handleNodesChange}
        onEdgesChange={handleEdgesChange}
        onConnect={handleConnect}
        onConnectEnd={handleConnectEnd}
        onNodesDelete={handleNodesDelete}
        onEdgesDelete={handleEdgesDelete}
        onPaneClick={handlePaneClick}
        onPaneContextMenu={handlePaneContextMenu}
        onNodeContextMenu={handleNodeContextMenu}
        onSelectionChange={handleSelectionChange}
        connectionMode={ConnectionMode.Loose}
        deleteKeyCode="Backspace"
        zoomOnDoubleClick={false}
        fitView
        // Allow zooming further out than React Flow's 0.5 default so a large
        // model fits on screen and nodes reach the header-only "flow" LOD
        // (collapse below COLLAPSE_THRESHOLD in semanticZoom.ts).
        minZoom={0.2}
        fitViewOptions={{ nodes: nodes.filter((n) => !BACKGROUND_NODE_TYPES.has(n.type ?? '')) }}
      >
        <Background />
        <Controls />
      </ReactFlow>

      <CanvasMenu opened={menu.kind !== 'none'} x={menu.kind === 'none' ? 0 : menu.x} y={menu.kind === 'none' ? 0 : menu.y} onClose={closeMenu}>
        {menu.kind === 'pane' && (
          <>
            <Menu.Label>Add here</Menu.Label>
            {(['event', 'command', 'query', 'integration', 'uiPlaceholder'] as NodeType[]).map((t) => (
              <Menu.Item
                key={t}
                data-testid={`pane-add-${t}`}
                onClick={() => { onCreateNode?.(t, menu.place); closeMenu() }}
              >
                {NODE_LABEL[t]}
              </Menu.Item>
            ))}
            <Menu.Divider />
            <Menu.Item onClick={() => { onAddEntity?.(); closeMenu() }}>Entity</Menu.Item>
            <Menu.Item onClick={() => { onAddSlice?.(); closeMenu() }}>Slice</Menu.Item>
            <Menu.Item onClick={() => { onAddChapter?.(); closeMenu() }}>Chapter</Menu.Item>
          </>
        )}
        {menu.kind === 'node' && (
          <>
            <Menu.Label>Add successor</Menu.Label>
            {successorsFor(menu.nodeType).length === 0 ? (
              <Menu.Item disabled>No valid successor</Menu.Item>
            ) : (
              successorsFor(menu.nodeType).map((t) => (
                <Menu.Item
                  key={t}
                  data-testid={`node-add-successor-${t}`}
                  onClick={() => { onCreateSuccessor?.(menu.nodeId, t, menu.place); closeMenu() }}
                >
                  {NODE_LABEL[t]}
                </Menu.Item>
              ))
            )}
            <Menu.Divider />
            <Menu.Item color="red" data-testid="node-delete" onClick={() => { onNodesDelete?.([menu.nodeId]); closeMenu() }}>
              Delete
            </Menu.Item>
          </>
        )}
        {menu.kind === 'successor' && (
          <>
            <Menu.Label>Create…</Menu.Label>
            {menu.options.map((t) => (
              <Menu.Item
                key={t}
                data-testid={`successor-add-${t}`}
                onClick={() => { onCreateSuccessor?.(menu.sourceId, t, menu.place); closeMenu() }}
              >
                {NODE_LABEL[t]}
              </Menu.Item>
            ))}
          </>
        )}
      </CanvasMenu>
    </div>
  )
}
