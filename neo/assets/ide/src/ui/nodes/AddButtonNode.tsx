import { Tooltip } from '@mantine/core'
import { IconPlus } from '@tabler/icons-react'
import classes from './AddButtonNode.module.css'

interface Props {
  data: {
    label: string
    onClick: () => void
    testId?: string
  }
}

/** A canvas-anchored "+" button (add slice / add entity). Carries `nodrag` so
 *  React Flow doesn't try to drag it. */
export function AddButtonNodeComponent({ data }: Props) {
  return (
    <Tooltip label={data.label} withArrow>
      <button
        type="button"
        className={`${classes.btn} nodrag`}
        aria-label={data.label}
        data-testid={data.testId}
        onClick={(e) => {
          e.stopPropagation()
          data.onClick()
        }}
      >
        <IconPlus size={18} />
      </button>
    </Tooltip>
  )
}
