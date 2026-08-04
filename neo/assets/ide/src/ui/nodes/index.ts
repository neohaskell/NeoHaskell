import type { NodeTypes } from '@xyflow/react'
import { EventNodeComponent } from './EventNode'
import { CommandNodeComponent } from './CommandNode'
import { QueryNodeComponent } from './QueryNode'
import { IntegrationNodeComponent } from './IntegrationNode'
import { UIPlaceholderNodeComponent } from './UIPlaceholderNode'
import { EntityLaneNodeComponent } from './EntityLaneNode'
import { SliceColumnNodeComponent } from './SliceColumnNode'
import { ChapterArrowNodeComponent } from './ChapterArrowNode'
import { SubmodelBandNodeComponent } from './SubmodelBandNode'
import { BoundaryPortalNodeComponent } from './BoundaryPortalNode'
import { AddButtonNodeComponent } from './AddButtonNode'

export const nodeTypes: NodeTypes = {
  event: EventNodeComponent,
  command: CommandNodeComponent,
  query: QueryNodeComponent,
  integration: IntegrationNodeComponent,
  uiPlaceholder: UIPlaceholderNodeComponent,
  entityLane: EntityLaneNodeComponent,
  sliceColumn: SliceColumnNodeComponent,
  chapterArrow: ChapterArrowNodeComponent,
  submodelBand: SubmodelBandNodeComponent,
  boundaryPortal: BoundaryPortalNodeComponent,
  addButton: AddButtonNodeComponent,
}
