import { useCallback, useRef, useState } from 'react'
import { useReactFlow, useNodeId } from '@xyflow/react'
import { EditableLabel } from './EditableLabel'
import classes from './ChapterArrowNode.module.css'

interface Props {
  data: {
    label: string
    chapterId: string
    selected?: boolean
    onSelect?: () => void
    onRename?: (name: string) => void
    onEndHandleDrag?: (flowX: number) => void
    onEndHandleDrop?: (flowX: number) => void
    submodels?: readonly { id: string; name: string }[]
    currentSubmodelId?: string | null
    onAssignSubmodel?: (submodelId: string | null) => void
  }
}

const MIN_ARROW_WIDTH = 80

export function ChapterArrowNodeComponent({ data }: Props) {
  const { screenToFlowPosition, getNode } = useReactFlow()
  const nodeId = useNodeId()!
  const draggingEnd = useRef(false)
  // Width override during drag (in flow px, relative to node left edge)
  const [dragWidth, setDragWidth] = useState<number | null>(null)

  const handleEndPointerDown = useCallback(
    (e: React.PointerEvent) => {
      e.stopPropagation()
      e.preventDefault()
      draggingEnd.current = true
      ;(e.target as HTMLElement).setPointerCapture(e.pointerId)
    },
    [],
  )

  const handleEndPointerMove = useCallback(
    (e: React.PointerEvent) => {
      if (!draggingEnd.current) return
      const flowPos = screenToFlowPosition({ x: e.clientX, y: e.clientY })
      data.onEndHandleDrag?.(flowPos.x)

      const node = getNode(nodeId)
      if (node) {
        const newWidth = Math.max(MIN_ARROW_WIDTH, flowPos.x - node.position.x)
        setDragWidth(newWidth)
      }
    },
    [screenToFlowPosition, data, getNode, nodeId],
  )

  const handleEndPointerUp = useCallback(
    (e: React.PointerEvent) => {
      if (!draggingEnd.current) return
      draggingEnd.current = false
      setDragWidth(null)
      const flowPos = screenToFlowPosition({ x: e.clientX, y: e.clientY })
      data.onEndHandleDrop?.(flowPos.x)
    },
    [screenToFlowPosition, data],
  )

  const handleClick = useCallback(
    (e: React.MouseEvent) => {
      e.stopPropagation()
      data.onSelect?.()
    },
    [data],
  )

  return (
    <div className={classes.root} onClick={handleClick}>
      {/* Selection highlight */}
      {data.selected && <div className={classes.selection} />}
      {/* Arrow container — uses dragWidth override during resize, otherwise fills node */}
      <div
        className={classes.arrow}
        style={{ width: dragWidth != null ? dragWidth : '100%' }}
      >
        {/* Arrow line */}
        <div className={classes.line} />
        {/* Arrowhead. Draggable (range-resize) only when an end-handle drag
            callback is wired; otherwise a static cap (feature-mode chapters
            are display-only — range editing lives in the flat view). */}
        <div
          className={`${classes.head} ${data.onEndHandleDrag ? classes.headDraggable : ''}`}
          onPointerDown={data.onEndHandleDrag ? handleEndPointerDown : undefined}
          onPointerMove={data.onEndHandleDrag ? handleEndPointerMove : undefined}
          onPointerUp={data.onEndHandleDrag ? handleEndPointerUp : undefined}
        />
        {/* Label */}
        <div className={classes.labelWrap}>
          <span className={classes.label}>
            {data.onRename ? (
              <EditableLabel label={data.label} onRename={data.onRename} />
            ) : (
              data.label
            )}
          </span>
        </div>
      </div>
    </div>
  )
}
