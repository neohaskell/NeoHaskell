import type { EventModel } from '../../model/types'

const MIN_LANE_HEIGHT = 150
const LANE_PADDING = 20

export interface SwimLaneLayout {
  entityId: string
  name: string
  yStart: number
  yEnd: number
}

export function calculateSwimLanes(model: EventModel): SwimLaneLayout[] {
  const sorted = [...model.entities].sort((a, b) => a.order - b.order)
  const lanes: SwimLaneLayout[] = []
  let currentY = 0

  for (const entity of sorted) {
    const entityNodes = model.nodes.filter(
      (n) => 'entityId' in n && n.entityId === entity.id,
    )

    let minY = Infinity
    let maxY = -Infinity
    for (const node of entityNodes) {
      const pos = model.layout.nodePositions[node.id]
      if (pos) {
        minY = Math.min(minY, pos.y)
        maxY = Math.max(maxY, pos.y + 50) // approximate node height
      }
    }

    const contentHeight =
      minY !== Infinity ? maxY - minY + LANE_PADDING * 2 : 0
    const laneHeight = Math.max(MIN_LANE_HEIGHT, contentHeight)

    lanes.push({
      entityId: entity.id,
      name: entity.name,
      yStart: currentY,
      yEnd: currentY + laneHeight,
    })

    currentY += laneHeight
  }

  return lanes
}

export function getSwimLaneForPosition(
  lanes: SwimLaneLayout[],
  y: number,
): string | null {
  for (const lane of lanes) {
    if (y >= lane.yStart && y < lane.yEnd) {
      return lane.entityId
    }
  }
  return null
}
