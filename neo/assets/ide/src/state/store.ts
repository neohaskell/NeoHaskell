import { createContext, useContext } from 'react'
import type { EventModel, EdgeType } from '../model/types'
import {
  addEvent,
  addCommand,
  addQuery,
  addIntegration,
  addUIPlaceholder,
  removeNode,
  addEdge,
  removeEdge,
  addEntity,
  removeEntity,
  renameEntity,
  addChapter,
  removeChapter,
  reorderChapters,
  moveSliceToChapter,
  addSlice,
  removeSlice,
  renameSlice,
  assignNodeToSlice,
  assignSliceToChapter,
  setChapterSliceRange,
  assignEventToEntity,
  renameChapter,
  updateNodeName,
  addSubmodel,
  renameSubmodel,
  removeSubmodel,
  assignChapterToSubmodel,
} from '../model/operations'
import { applyPositionChanges } from '../ui/adapter'

export type Action =
  | { type: 'addEvent'; name: string; entityId?: string }
  | { type: 'addCommand'; name: string; entityId?: string }
  | { type: 'addQuery'; name: string }
  | { type: 'addIntegration'; name: string; kind: 'inbound' | 'outbound' }
  | { type: 'addUIPlaceholder'; name: string }
  | { type: 'removeNode'; nodeId: string }
  | { type: 'updateNodeName'; nodeId: string; name: string }
  | { type: 'addEdge'; edgeType: EdgeType; sourceId: string; targetId: string; sourceHandle?: string | null; targetHandle?: string | null }
  | { type: 'removeEdge'; edgeId: string }
  | { type: 'addEntity'; name: string }
  | { type: 'removeEntity'; entityId: string }
  | { type: 'renameEntity'; entityId: string; name: string }
  | { type: 'addChapter'; name: string; submodelId?: string | null }
  | { type: 'removeChapter'; chapterId: string }
  | { type: 'renameChapter'; chapterId: string; name: string }
  | { type: 'reorderChapters'; orderedChapterIds: string[] }
  | { type: 'moveSliceToChapter'; sliceId: string; chapterId: string | null; orderedSliceIds: string[] }
  | { type: 'addSubmodel'; name: string }
  | { type: 'removeSubmodel'; submodelId: string }
  | { type: 'renameSubmodel'; submodelId: string; name: string }
  | { type: 'assignChapterToSubmodel'; chapterId: string; submodelId: string | null }
  | { type: 'assignSliceToChapter'; sliceId: string; chapterId: string | null }
  | { type: 'setChapterSliceRange'; chapterId: string; startSliceId: string; endSliceId: string }
  | { type: 'addSlice'; name: string; chapterId?: string }
  | { type: 'removeSlice'; sliceId: string }
  | { type: 'renameSlice'; sliceId: string; name: string }
  | { type: 'assignNodeToSlice'; nodeId: string; sliceId: string | null }
  | { type: 'assignNodeToEntity'; nodeId: string; entityId: string | null }
  | { type: 'updatePosition'; nodeId: string; x: number; y: number }
  | { type: 'batchUpdatePositions'; changes: { nodeId: string; x: number; y: number }[] }
  | { type: 'loadModel'; model: EventModel }

let edgeCounter = 0
function edgeId(): string {
  return `edge-${Date.now()}-${++edgeCounter}`
}

export function reducer(model: EventModel, action: Action): EventModel {
  switch (action.type) {
    case 'addEvent':
      return addEvent(model, { name: action.name, entityId: action.entityId })
    case 'addCommand':
      return addCommand(model, { name: action.name, entityId: action.entityId })
    case 'addQuery':
      return addQuery(model, { name: action.name })
    case 'addIntegration':
      return addIntegration(model, { name: action.name, kind: action.kind })
    case 'addUIPlaceholder':
      return addUIPlaceholder(model, { name: action.name })
    case 'removeNode':
      return removeNode(model, action.nodeId)
    case 'updateNodeName':
      return updateNodeName(model, action.nodeId, action.name)
    case 'addEdge':
      return addEdge(model, {
        id: edgeId(),
        type: action.edgeType,
        sourceId: action.sourceId,
        targetId: action.targetId,
        sourceHandle: action.sourceHandle,
        targetHandle: action.targetHandle,
      })
    case 'removeEdge':
      return removeEdge(model, action.edgeId)
    case 'addEntity':
      return addEntity(model, { name: action.name })
    case 'removeEntity':
      return removeEntity(model, action.entityId)
    case 'renameEntity':
      return renameEntity(model, action.entityId, action.name)
    case 'addChapter':
      return addChapter(model, { name: action.name, submodelId: action.submodelId })
    case 'removeChapter':
      return removeChapter(model, action.chapterId)
    case 'renameChapter':
      return renameChapter(model, action.chapterId, action.name)
    case 'reorderChapters':
      return reorderChapters(model, action.orderedChapterIds)
    case 'moveSliceToChapter':
      return moveSliceToChapter(model, action.sliceId, action.chapterId, action.orderedSliceIds)
    case 'addSubmodel':
      return addSubmodel(model, { name: action.name })
    case 'removeSubmodel':
      return removeSubmodel(model, action.submodelId)
    case 'renameSubmodel':
      return renameSubmodel(model, action.submodelId, action.name)
    case 'assignChapterToSubmodel':
      return assignChapterToSubmodel(model, action.chapterId, action.submodelId)
    case 'assignSliceToChapter':
      return assignSliceToChapter(model, action.sliceId, action.chapterId)
    case 'setChapterSliceRange':
      return setChapterSliceRange(model, action.chapterId, action.startSliceId, action.endSliceId)
    case 'addSlice':
      return addSlice(model, { name: action.name, chapterId: action.chapterId })
    case 'removeSlice':
      return removeSlice(model, action.sliceId)
    case 'renameSlice':
      return renameSlice(model, action.sliceId, action.name)
    case 'assignNodeToSlice':
      return assignNodeToSlice(model, action.nodeId, action.sliceId)
    case 'assignNodeToEntity':
      return assignEventToEntity(model, action.nodeId, action.entityId)
    case 'updatePosition':
      return applyPositionChanges(model, [
        { id: action.nodeId, x: action.x, y: action.y },
      ])
    case 'batchUpdatePositions':
      return applyPositionChanges(
        model,
        action.changes.map((c) => ({ id: c.nodeId, x: c.x, y: c.y })),
      )
    case 'loadModel':
      return action.model
  }
}

export const ModelContext = createContext<{
  model: EventModel
  dispatch: React.Dispatch<Action>
} | null>(null)

export function useModel() {
  const ctx = useContext(ModelContext)
  if (!ctx) throw new Error('useModel must be used within ModelContext.Provider')
  return ctx
}
