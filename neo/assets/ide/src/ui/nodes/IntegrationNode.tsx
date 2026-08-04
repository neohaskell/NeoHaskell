import { IconSettings } from '@tabler/icons-react'
import { NodeShell } from '../primitives/NodeShell'
import type { Field } from '../../model/types'

interface Props {
  data: { label: string; kind: 'inbound' | 'outbound'; onRename?: (name: string) => void; fields?: readonly Field[] }
  selected?: boolean
}

export function IntegrationNodeComponent({ data, selected }: Props) {
  return (
    <NodeShell
      variant="integration"
      label={data.label}
      onRename={data.onRename}
      selected={selected}
      icon={<IconSettings size={14} aria-label="integration" />}
      fields={data.fields}
    />
  )
}
