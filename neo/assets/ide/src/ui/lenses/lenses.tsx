import type { ReactNode } from 'react'
import { IconLayoutBoardSplit, IconSchema, IconListDetails, IconPlayerPlay } from '@tabler/icons-react'

// The IDE is one canvas seen through several LENSES. "model" is built today;
// schema / logs / emulate are on the roadmap and render a placeholder. Adding a
// real lens later = swap its placeholder for a component in LensRouter.
export type Lens = 'model' | 'schema' | 'logs' | 'emulate'

export interface LensMeta {
  id: Lens
  label: string
  icon: ReactNode
  /** Roadmap copy shown on the placeholder for not-yet-built lenses. */
  blurb: string
}

const ICON_SIZE = 20

export const LENSES: LensMeta[] = [
  {
    id: 'model',
    label: 'Model',
    icon: <IconLayoutBoardSplit size={ICON_SIZE} />,
    blurb: 'The event model canvas.',
  },
  {
    id: 'schema',
    label: 'Schema',
    icon: <IconSchema size={ICON_SIZE} />,
    blurb: 'Inspect and edit the fields of every command, event, and read model — the data shape behind each concept on the canvas.',
  },
  {
    id: 'logs',
    label: 'Logs',
    icon: <IconListDetails size={ICON_SIZE} />,
    blurb: 'Live event streams from the running system, docked to the nodes that produced them.',
  },
  {
    id: 'emulate',
    label: 'Emulate',
    icon: <IconPlayerPlay size={ICON_SIZE} />,
    blurb: 'Replay an event stream and watch causality flow across the model — your diagram becomes the debugger.',
  },
]

export const LENS_BY_ID: Record<Lens, LensMeta> = Object.fromEntries(
  LENSES.map((l) => [l.id, l]),
) as Record<Lens, LensMeta>
