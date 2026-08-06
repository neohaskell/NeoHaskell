import { Box, Group, Text, ScrollArea, Stack, ActionIcon, UnstyledButton } from '@mantine/core'
import { IconX } from '@tabler/icons-react'
import { Dot, SEVERITY_COLOR } from './primitives/Dot'
import type { Issue, Severity } from '../model/validate'
import classes from './ProblemsPanel.module.css'

interface ProblemsPanelProps {
  issues: readonly Issue[]
  open: boolean
  onClose: () => void
  /** Click a row → jump to + select + trace the offending element. */
  onFocusIssue: (issue: Issue) => void
}

const SEVERITY_ORDER: Severity[] = ['error', 'warning', 'info']
const SEVERITY_LABEL: Record<Severity, string> = {
  error: 'Errors',
  warning: 'Warnings',
  info: 'Info',
}

/**
 * Right-docked, dismissible drawer listing live validation issues grouped by
 * severity. A row click focuses the offending element (select + navigate to its
 * feature + trace). Non-blocking: never gates editing or autosave.
 */
export function ProblemsPanel({ issues, open, onClose, onFocusIssue }: ProblemsPanelProps) {
  if (!open) return null
  return (
    <Box data-testid="problems-panel" w={320} h="100%" className={classes.panel}>
      <Group justify="space-between" px="sm" py="xs" className={classes.header}>
        <Text size="sm" fw={600}>
          Problems{issues.length > 0 ? ` (${issues.length})` : ''}
        </Text>
        <ActionIcon data-testid="close-problems" onClick={onClose} title="Close" aria-label="Close problems">
          <IconX size={16} />
        </ActionIcon>
      </Group>

      {issues.length === 0 ? (
        <Text p="sm" size="sm" c="dimmed">No problems — the model is valid.</Text>
      ) : (
        <ScrollArea style={{ flex: 1 }} p="xs">
          {SEVERITY_ORDER.map((sev) => {
            const group = issues.filter((i) => i.severity === sev)
            if (group.length === 0) return null
            return (
              <Box key={sev} data-testid={`problems-group-${sev}`} mb="md">
                <Text size="xs" fw={600} tt="uppercase" c="dimmed" px={4} mb={4}>
                  {SEVERITY_LABEL[sev]} ({group.length})
                </Text>
                <Stack gap={4}>
                  {group.map((issue, idx) => (
                    <UnstyledButton
                      key={`${issue.id}-${idx}`}
                      data-testid={`problem-row-${issue.id}`}
                      onClick={() => onFocusIssue(issue)}
                      px="xs"
                      py={6}
                      className={classes.row}
                    >
                      <Group gap="xs" wrap="nowrap" align="flex-start">
                        <Box mt={4}><Dot color={SEVERITY_COLOR[sev]} /></Box>
                        <Text size="xs">{issue.message}</Text>
                      </Group>
                    </UnstyledButton>
                  ))}
                </Stack>
              </Box>
            )
          })}
        </ScrollArea>
      )}
    </Box>
  )
}
