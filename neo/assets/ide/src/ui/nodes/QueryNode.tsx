import { NodeShell } from '../primitives/NodeShell'
import type { Field } from '../../model/types'

interface Props {
  data: { label: string; onRename?: (name: string) => void; fields?: readonly Field[] }
  selected?: boolean
}

export function QueryNodeComponent({ data, selected }: Props) {
  return (
    <NodeShell
      variant="query"
      label={data.label}
      onRename={data.onRename}
      selected={selected}
      fields={data.fields}
    />
  )
}
