import { Fragment } from 'react'
import { Handle, Position } from '@xyflow/react'

// Every domain node carries a source AND a target handle on all four sides,
// each keyed by the side name (`top` / `bottom` / `left` / `right`). The
// event-model edge convention reuses the SAME handle id for both roles
// depending on edge direction — e.g. a command's `bottom` handle is the
// SOURCE of `commandProducesEvent` (command → event) and the TARGET of
// `integrationTriggersCommand` (integration → command). React Flow tracks
// source and target handles in separate buckets keyed by id, so an edge's
// `sourceHandle` resolves against the source bucket and `targetHandle`
// against the target bucket. Emitting both per side guarantees every edge
// type renders (and is interactively connectable) regardless of direction.
//
// Before this existed each node exposed only two `type="source"` handles
// with side ids that didn't match what the edges referenced, so
// `commandProducesEvent`, `eventFeedsQuery` and `integrationTriggersCommand`
// edges silently failed to render (React Flow drops an edge whose handle id
// is absent from the endpoint node). See `connectionRules.ts` for the
// node-type → edge-type map these handles serve.
const SIDES = [
  ['top', Position.Top],
  ['bottom', Position.Bottom],
  ['left', Position.Left],
  ['right', Position.Right],
] as const

export function NodeHandles() {
  return (
    <>
      {SIDES.map(([id, position]) => (
        <Fragment key={id}>
          <Handle id={id} type="target" position={position} />
          <Handle id={id} type="source" position={position} />
        </Fragment>
      ))}
    </>
  )
}
