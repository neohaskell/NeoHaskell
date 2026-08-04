import { Tooltip, UnstyledButton } from '@mantine/core'
import { LENSES, type Lens } from '../lenses/lenses'
import classes from './ActivityRail.module.css'

interface ActivityRailProps {
  lens: Lens
  onChange: (lens: Lens) => void
}

/** Left icon rail that switches the active lens (Model / Schema / Logs / Emulate). */
export function ActivityRail({ lens, onChange }: ActivityRailProps) {
  return (
    <nav className={classes.rail} data-testid="activity-rail" aria-label="Lenses">
      {LENSES.map((l) => (
        <Tooltip key={l.id} label={l.label} position="right">
          <UnstyledButton
            data-testid={`lens-${l.id}`}
            data-active={lens === l.id ? 'true' : 'false'}
            aria-label={l.label}
            aria-pressed={lens === l.id}
            className={`${classes.item} ${lens === l.id ? classes.itemActive : ''}`}
            onClick={() => onChange(l.id)}
          >
            {l.icon}
          </UnstyledButton>
        </Tooltip>
      ))}
    </nav>
  )
}
