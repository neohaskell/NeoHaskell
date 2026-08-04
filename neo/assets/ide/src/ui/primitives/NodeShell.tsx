import type { ReactNode } from 'react'
import { useStore } from '@xyflow/react'
import { EditableLabel } from '../nodes/EditableLabel'
import { NodeHandles } from '../nodes/NodeHandles'
import { FIELD_CAP } from '../nodes/nodeDimensions'
import { nodeDetailLevel } from '../canvas/semanticZoom'
import type { Field } from '../../model/types'
import classes from './NodeShell.module.css'

export type NodeVariant =
  | 'event'
  | 'command'
  | 'query'
  | 'integration'
  | 'uiPlaceholder'

interface NodeShellProps {
  variant: NodeVariant
  label: string
  selected?: boolean
  onRename?: (name: string) => void
  /** Leading glyph (e.g. the integration gear). */
  icon?: ReactNode
  /** Schema fields — rendered inline in the body (always read-only). */
  fields?: readonly Field[]
}

/**
 * The shared record-card chrome for the five domain node types. Two zones: a
 * colored HEADER carrying the type name (the event-modeling identity color,
 * darkened for AA — see --node-header-* in src/theme.ts) and a neutral BODY
 * listing the data type's `fields` inline. Fields are STRICTLY READ-ONLY in the
 * IDE — source code is the author of schema (a Rust background sync rewrites
 * `event-model.json`); the canvas only displays the `name : Type` ledger and
 * never mounts an inline editor. Single source of the node palette, selection
 * ring, dimensions, and the mandatory 4-side source+target handle set (see
 * NodeHandles — React Flow drops edges whose handle id is absent, so every node
 * MUST carry the full set). Per-variant color lives in NodeShell.module.css via
 * theme tokens; nothing is styled in-place here.
 */
export function NodeShell({
  variant,
  label,
  selected,
  onRename,
  icon,
  fields,
}: NodeShellProps) {
  const rows = fields ?? []
  // Level-of-detail: far-zoomed-out nodes collapse to their header only so the
  // whole board reads as a wall of type headers (the causal flow). Re-renders
  // only when the level CHANGES (selector returns a stable string), not on pan.
  const detail = useStore((s) => nodeDetailLevel(s.transform[2]))

  return (
    <div
      className={classes.node}
      data-variant={variant}
      data-selected={selected ? 'true' : undefined}
      data-detail={detail}
    >
      <NodeHandles />
      <div className={classes.header}>
        {icon && <span className={classes.icon}>{icon}</span>}
        {onRename ? <EditableLabel label={label} onRename={onRename} /> : label}
      </div>
      {detail !== 'header' && (
      <div className={classes.body}>
        {rows.length > 0 ? (
          <div className={classes.fields}>
            {rows.slice(0, FIELD_CAP).map((f, i) => (
              <div className={classes.fieldRow} key={i}>
                <span className={classes.fieldName} title={f.name}>
                  {f.name}
                </span>
                <span className={classes.fieldType} title={f.type}>
                  {f.type}
                </span>
              </div>
            ))}
            {rows.length > FIELD_CAP && (
              <div className={classes.empty}>+{rows.length - FIELD_CAP} more</div>
            )}
          </div>
        ) : (
          <div className={classes.empty}>no fields</div>
        )}
      </div>
      )}
    </div>
  )
}
