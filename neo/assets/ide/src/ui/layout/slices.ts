import type { EventModel } from '../../model/types'

const SLICE_PADDING = 30

export interface SliceBounds {
  sliceId: string
  name: string
  xStart: number
  xEnd: number
}

export interface ChapterBounds {
  chapterId: string
  name: string
  xStart: number
  xEnd: number
}

export function calculateSliceBounds(
  model: EventModel,
  sliceId: string,
): SliceBounds {
  const slice = model.slices.find((s) => s.id === sliceId)
  const name = slice?.name ?? ''

  const sliceNodes = model.nodes.filter((n) => n.sliceId === sliceId)

  if (sliceNodes.length === 0) {
    return { sliceId, name, xStart: 0, xEnd: 0 }
  }

  let minX = Infinity
  let maxX = -Infinity

  for (const node of sliceNodes) {
    const pos = model.layout.nodePositions[node.id]
    if (pos) {
      minX = Math.min(minX, pos.x)
      maxX = Math.max(maxX, pos.x + 120) // approximate node width
    }
  }

  if (minX === Infinity) {
    return { sliceId, name, xStart: 0, xEnd: 0 }
  }

  return {
    sliceId,
    name,
    xStart: minX - SLICE_PADDING,
    xEnd: maxX + SLICE_PADDING,
  }
}

export function calculateChapterBounds(
  model: EventModel,
  chapterId: string,
): ChapterBounds {
  const chapter = model.chapters.find((c) => c.id === chapterId)
  const name = chapter?.name ?? ''

  const chapterSlices = model.slices.filter((s) => s.chapterId === chapterId)

  if (chapterSlices.length === 0) {
    return { chapterId, name, xStart: 0, xEnd: 0 }
  }

  const sliceBounds = chapterSlices.map((s) =>
    calculateSliceBounds(model, s.id),
  )

  const nonEmpty = sliceBounds.filter((b) => b.xEnd - b.xStart > 0)

  if (nonEmpty.length === 0) {
    return { chapterId, name, xStart: 0, xEnd: 0 }
  }

  const xStart = Math.min(...nonEmpty.map((b) => b.xStart))
  const xEnd = Math.max(...nonEmpty.map((b) => b.xEnd))

  return { chapterId, name, xStart, xEnd }
}
