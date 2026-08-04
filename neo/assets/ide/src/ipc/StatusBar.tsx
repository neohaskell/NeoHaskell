// Bottom status footer: connection state (left) + live validation chip and
// autosave state (right). Compact, monospace, quiet when healthy — only the
// save-failure state escalates. Single system-status surface (no top-bar chip,
// no dirty dot) per the autosave/validation UX spec. Styled entirely via the
// Mantine theme.

import { Group, Text, UnstyledButton, Anchor, Loader, Tooltip } from '@mantine/core'
import { IconCheck, IconAlertTriangle } from '@tabler/icons-react'
import { Dot } from '../ui/primitives/Dot'
import classes from './StatusBar.module.css'
import type { ConnectionState } from './client'
import type { InitializeResult } from './initialize'
import type { SaveState } from '../io/useAutosave'
import type { IssueCounts } from '../model/validate'

interface Props {
  state: ConnectionState
  init: InitializeResult | null
  saveState: SaveState
  issueCounts: IssueCounts
  onToggleProblems: () => void
  onRetrySave: () => void
}

export function StatusBar({ state, init, saveState, issueCounts, onToggleProblems, onRetrySave }: Props) {
  const dotColor =
    state.status === 'open' ? 'green' : state.status === 'connecting' ? 'yellow' : 'red'

  const left = (() => {
    if (state.status === 'connecting') return 'connecting to neo…'
    if (state.status === 'error') return `disconnected (${state.message})`
    if (state.status === 'closed') return `disconnected (${state.reason})`
    if (!init) return 'connected, awaiting initialize'
    return `${init.serverInfo.name} v${init.serverInfo.version} · session ${init.sessionId}`
  })()

  const right = init ? init.workspace.root : null
  const totalIssues = issueCounts.error + issueCounts.warning + issueCounts.info
  const hasProblems = issueCounts.error + issueCounts.warning > 0

  return (
    <Group
      data-testid="ide-statusbar"
      px="sm"
      py={5}
      gap="xs"
      wrap="nowrap"
      bg="var(--mantine-color-default)"
      className={classes.bar}
    >
      <Dot color={dotColor} />
      <Text size="xs" ff="monospace" c="dimmed" truncate>
        {left}
      </Text>
      {right && (
        <>
          <Text size="xs" c="dimmed">·</Text>
          <Text size="xs" ff="monospace" c="dimmed" truncate title={right}>
            {right}
          </Text>
        </>
      )}
      {init?.workspace.project && (
        <>
          <Text size="xs" c="dimmed">·</Text>
          <Text size="xs" ff="monospace" c="dimmed">
            {init.workspace.project.name} v{init.workspace.project.version}
          </Text>
        </>
      )}

      <Group gap="md" wrap="nowrap" ml="auto">
        {/* Validation chip — click to open the Problems panel. */}
        <Tooltip
          label={totalIssues === 0 ? 'No problems' : `${totalIssues} problem${totalIssues === 1 ? '' : 's'} — click to view`}
        >
          <UnstyledButton
            data-testid="validation-chip"
            onClick={onToggleProblems}
            c={hasProblems ? 'orange' : 'green'}
          >
            <Group gap={4} wrap="nowrap">
              {hasProblems ? <IconAlertTriangle size={12} /> : <IconCheck size={12} />}
              <Text size="xs" ff="monospace" inherit>
                {hasProblems ? `${issueCounts.error} · ${issueCounts.warning}` : 'Valid'}
              </Text>
            </Group>
          </UnstyledButton>
        </Tooltip>

        {/* Autosave state. */}
        <Group
          data-testid="save-state"
          data-kind={saveState.kind}
          gap={4}
          wrap="nowrap"
          c={saveState.kind === 'failed' ? 'orange' : 'dimmed'}
          fw={saveState.kind === 'failed' ? 600 : undefined}
        >
          {saveState.kind === 'saving' && (
            <>
              <Loader size={12} />
              <Text size="xs" ff="monospace" inherit>Saving…</Text>
            </>
          )}
          {saveState.kind === 'saved' && (
            <>
              <IconCheck size={12} />
              <Text size="xs" ff="monospace" inherit>All changes saved</Text>
            </>
          )}
          {saveState.kind === 'offline' && (
            <Text size="xs" ff="monospace" inherit>Will save when reconnected</Text>
          )}
          {saveState.kind === 'failed' && (
            <>
              <Text size="xs" ff="monospace" inherit>Save failed</Text>
              <Anchor component="button" type="button" size="xs" data-testid="retry-save" onClick={onRetrySave} inherit>
                retry
              </Anchor>
            </>
          )}
        </Group>
      </Group>
    </Group>
  )
}
