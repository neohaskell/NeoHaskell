import type { Node } from '@xyflow/react'
import type { EventModel } from '../../model/types'
import { estimateNodeDimensions } from '../nodes/nodeDimensions'

// Submodels are FEATURE-level groupings that own chapters. On the canvas
// they stack VERTICALLY: ungrouped content stays where it is (the "not yet
// organised" region at the top), and each submodel — in `order` — becomes a
// left-aligned block placed below it. This turns one endless horizontal
// timeline into a navigable vertical list of self-contained features.

export interface NodePositionAdjustment {
  nodeId: string
  x: number
  y: number
}

export interface SubmodelBandLayout {
  submodelId: string
  name: string
  xStart: number
  yStart: number
  width: number
  height: number
}

const BAND_VGAP = 220 // vertical gap between stacked bands
const BAND_PAD = 90 // padding around band content (left/right/bottom)
const BAND_TOP_PAD = 150 // extra top padding to clear chapter arrows + slice headers
const BAND_LEFT = 0 // x each band's leftmost node normalises to

interface Box {
  minX: number
  minY: number
  maxX: number
  maxY: number
}

/** Map every node id to the id of the submodel that owns it (via
 *  slice → chapter → submodel), or null when ungrouped. */
export function buildNodeSubmodelMap(model: EventModel): Map<string, string | null> {
  const sliceToChapter = new Map(model.slices.map((s) => [s.id, s.chapterId]))
  const chapterToSubmodel = new Map(
    model.chapters.map((c) => [c.id, c.submodelId ?? null]),
  )
  const out = new Map<string, string | null>()
  for (const node of model.nodes) {
    let submodelId: string | null = null
    if (node.sliceId) {
      const chapterId = sliceToChapter.get(node.sliceId) ?? null
      if (chapterId) submodelId = chapterToSubmodel.get(chapterId) ?? null
    }
    out.set(node.id, submodelId)
  }
  return out
}

function boxOf(model: EventModel, nodeIds: string[]): Box | null {
  let minX = Infinity
  let minY = Infinity
  let maxX = -Infinity
  let maxY = -Infinity
  let found = false
  for (const node of model.nodes) {
    if (!nodeIds.includes(node.id)) continue
    const pos = model.layout.nodePositions[node.id]
    if (!pos) continue
    found = true
    const { width, height } = estimateNodeDimensions(node.name, node.fields)
    minX = Math.min(minX, pos.x)
    minY = Math.min(minY, pos.y)
    maxX = Math.max(maxX, pos.x + width)
    maxY = Math.max(maxY, pos.y + height)
  }
  return found ? { minX, minY, maxX, maxY } : null
}

/** Group node ids by submodel id; ungrouped nodes go under the '' key. */
function groupNodeIds(model: EventModel): Map<string, string[]> {
  const map = buildNodeSubmodelMap(model)
  const groups = new Map<string, string[]>()
  for (const node of model.nodes) {
    const key = map.get(node.id) ?? null
    const bucket = key ?? ''
    const arr = groups.get(bucket)
    if (arr) arr.push(node.id)
    else groups.set(bucket, [node.id])
  }
  return groups
}

/**
 * Reflow node positions so each submodel sits in its own vertical band.
 * Ungrouped nodes keep their current positions; submodels (in `order`)
 * are stacked below them, each normalised to the left edge. Returns only
 * the position changes — a no-op (empty array) when no chapter is assigned
 * to a submodel, so running it on a legacy model never disturbs it.
 */
export function stackSubmodels(model: EventModel): NodePositionAdjustment[] {
  const groups = groupNodeIds(model)
  const ungrouped = groups.get('') ?? []

  // Submodels start below the ungrouped content (if any).
  const ungroupedBox = boxOf(model, ungrouped)
  let yCursor = ungroupedBox ? ungroupedBox.maxY + BAND_VGAP : 0

  const orderedSubmodels = [...model.submodels].sort((a, b) => a.order - b.order)
  const adjustments: NodePositionAdjustment[] = []

  for (const submodel of orderedSubmodels) {
    const ids = groups.get(submodel.id)
    if (!ids || ids.length === 0) continue
    const box = boxOf(model, ids)
    if (!box) continue

    const dx = BAND_LEFT - box.minX
    const dy = yCursor - box.minY
    if (dx !== 0 || dy !== 0) {
      for (const id of ids) {
        const pos = model.layout.nodePositions[id]
        if (!pos) continue
        adjustments.push({ nodeId: id, x: pos.x + dx, y: pos.y + dy })
      }
    }
    const bandHeight = box.maxY - box.minY + BAND_TOP_PAD + BAND_PAD
    yCursor += bandHeight + BAND_VGAP
  }

  return adjustments
}

/**
 * Compute the band rectangles (for the background + label) of every
 * submodel that currently owns at least one positioned node. Reads the
 * CURRENT positions, so call it after `stackSubmodels` has been applied.
 */
export function computeSubmodelBands(model: EventModel): SubmodelBandLayout[] {
  const groups = groupNodeIds(model)
  const orderedSubmodels = [...model.submodels].sort((a, b) => a.order - b.order)
  const bands: SubmodelBandLayout[] = []
  for (const submodel of orderedSubmodels) {
    const ids = groups.get(submodel.id)
    if (!ids || ids.length === 0) continue
    const box = boxOf(model, ids)
    if (!box) continue
    bands.push({
      submodelId: submodel.id,
      name: submodel.name,
      xStart: box.minX - BAND_PAD,
      yStart: box.minY - BAND_TOP_PAD,
      width: box.maxX - box.minX + BAND_PAD * 2,
      height: box.maxY - box.minY + BAND_TOP_PAD + BAND_PAD,
    })
  }
  return bands
}

/** True when at least one chapter has been assigned to a submodel. */
export function submodelsInUse(model: EventModel): boolean {
  return model.chapters.some((c) => c.submodelId != null)
}

/** Build the React Flow background nodes that draw each submodel band. */
export function buildSubmodelBandNodes(
  model: EventModel,
  onRename?: (submodelId: string, name: string) => void,
  onDelete?: (submodelId: string) => void,
): Node[] {
  return computeSubmodelBands(model).map((band) => ({
    id: `__submodel-band-${band.submodelId}`,
    type: 'submodelBand',
    position: { x: band.xStart, y: band.yStart },
    data: {
      label: band.name,
      onRename: onRename ? (name: string) => onRename(band.submodelId, name) : undefined,
      onDelete: onDelete ? () => onDelete(band.submodelId) : undefined,
    },
    draggable: false,
    selectable: false,
    focusable: false,
    style: {
      width: band.width,
      height: band.height,
      zIndex: -3,
      pointerEvents: 'none' as const,
    },
  }))
}
