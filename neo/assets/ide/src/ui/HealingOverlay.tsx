import { useEffect, useMemo, useRef } from 'react'
import { Group, Text, Box, Stack, Button, Loader, Paper, Code } from '@mantine/core'
import { IconCheck } from '@tabler/icons-react'
import { reduceHealLog, type HealEvent } from './healEvents'
import classes from './HealingOverlay.module.css'

export interface HealLogLine {
  /** Subprocess stream: "stdout" (claude's content / reasoning) or "stderr"
   *  (claude's status / errors). */
  stream: 'stdout' | 'stderr'
  line: string
}

interface HealingOverlayProps {
  message?: string
  /** Live tail of claude's output, streamed via `$/progress` notifications.
   *  Newest line at the bottom. Empty array shows just the spinner. */
  log?: HealLogLine[]
  /** Click handler for the Cancel button. When omitted, no button is rendered. */
  onCancel?: () => void
  /** Disable Cancel while a cancel is already in flight. */
  cancelling?: boolean
}

export function HealingOverlay({ message, log, onCancel, cancelling }: HealingOverlayProps) {
  const scrollerRef = useRef<HTMLDivElement>(null)

  // Parse the raw streamed lines into a typed timeline of "thoughts".
  const events: HealEvent[] = useMemo(() => reduceHealLog(log ?? []), [log])

  useEffect(() => {
    const el = scrollerRef.current
    if (el) el.scrollTop = el.scrollHeight
  }, [events])

  return (
    <div role="status" aria-live="polite" className={classes.backdrop}>
      <Paper className={classes.panel} withBorder shadow="xl" radius="md" p={0}>
        <div className={classes.header}>
          <Loader size="sm" data-testid="heal-spinner" />
          <Text size="sm" style={{ flex: 1 }}>{message ?? 'Healing event model…'}</Text>
          <Text size="xs" c="dimmed">
            {events.length === 0
              ? 'waiting for the agent…'
              : `${events.length} ${events.length === 1 ? 'step' : 'steps'}`}
          </Text>
          {onCancel && (
            <Button
              size="xs"
              variant="default"
              onClick={onCancel}
              disabled={cancelling}
              data-testid="heal-cancel"
            >
              {cancelling ? 'Cancelling…' : 'Cancel'}
            </Button>
          )}
        </div>
        <Box ref={scrollerRef} data-testid="heal-log" className={classes.log}>
          {events.length === 0 ? (
            <Text size="sm" c="dimmed" fs="italic">
              The agent is starting up. Its reasoning, tool calls, and results will
              appear here as soon as the first output arrives.
            </Text>
          ) : (
            <Stack gap="sm">
              {events.map((event) => <HealEventCard key={event.id} event={event} />)}
            </Stack>
          )}
        </Box>
      </Paper>
    </div>
  )
}

function HealEventCard({ event }: { event: HealEvent }) {
  switch (event.kind) {
    case 'thinking':
      return (
        <Box data-testid="heal-event-thinking" pl="sm" className={classes.thinkingCard}>
          <Text size="10px" fw={600} tt="uppercase" c="dimmed" mb={2}>thinking</Text>
          <Text size="sm" c="dimmed" fs="italic" style={{ whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}>
            {event.text || '…'}
          </Text>
        </Box>
      )
    case 'text':
      return (
        <Text data-testid="heal-event-text" size="sm" style={{ whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}>
          {event.text}
        </Text>
      )
    case 'tool_use': {
      const formatted = tryFormatJson(event.input)
      return (
        <Paper data-testid="heal-event-tool-use" withBorder p="xs" bg="color-mix(in srgb, var(--em-feature) 10%, transparent)">
          <Group gap="xs">
            <Text size="sm" ff="monospace" fw={600} c="emFeature">{event.name}</Text>
            <Text size="xs" c="dimmed">tool call</Text>
          </Group>
          {formatted && (
            <Code block mt={4} fz="xs">{formatted}</Code>
          )}
        </Paper>
      )
    }
    case 'tool_result':
      return (
        <Paper data-testid="heal-event-tool-result" withBorder p="xs" bg="color-mix(in srgb, var(--mantine-color-green-6) 10%, transparent)">
          <Text size="10px" fw={600} tt="uppercase" c="green" mb={4}>result</Text>
          <Code block fz="xs">
            {event.preview}
            {event.truncated && ' …(truncated)'}
          </Code>
        </Paper>
      )
    case 'status':
      return (
        <Text data-testid="heal-event-status" size="xs" c="dimmed" fs="italic">{event.label}…</Text>
      )
    case 'auto_repair':
      return (
        <Paper data-testid="heal-event-auto-repair" withBorder p="xs" bg="color-mix(in srgb, var(--mantine-color-green-6) 12%, transparent)">
          <Group gap="xs">
            <IconCheck size={14} color="var(--mantine-color-green-6)" />
            <Text size="sm" fw={500}>
              Auto-repaired {event.appliedCount} {event.appliedCount === 1 ? 'item' : 'items'}
            </Text>
            {event.residualCount > 0 && (
              <Text size="sm" c="green">
                · {event.residualCount} residual{event.residualCount === 1 ? '' : 's'} need LLM
              </Text>
            )}
          </Group>
          {event.summary && <Text size="xs" c="dimmed" mt={2}>{event.summary}</Text>}
        </Paper>
      )
    case 'api_retry': {
      const reason =
        event.errorStatus === 529
          ? 'Anthropic API is overloaded'
          : event.errorStatus === 429
            ? 'Anthropic API rate-limited the request'
            : `Anthropic API returned HTTP ${event.errorStatus} (${event.error})`
      const delaySec = (event.delayMs / 1000).toFixed(1)
      return (
        <Paper data-testid="heal-event-api-retry" withBorder p="xs" bg="color-mix(in srgb, var(--mantine-color-yellow-6) 12%, transparent)">
          <Group gap="xs" wrap="nowrap">
            <Loader size={12} color="yellow" />
            <Text size="sm" fw={500}>{reason}.</Text>
            <Text size="sm" c="yellow.8">
              retrying attempt {event.attempt}
              {event.maxRetries > 0 && `/${event.maxRetries}`} in {delaySec}s…
            </Text>
          </Group>
        </Paper>
      )
    }
    case 'raw':
      return (
        <Text
          data-testid="heal-event-raw"
          size="xs"
          ff="monospace"
          c={event.stream === 'stderr' ? 'orange' : 'dimmed'}
          style={{ whiteSpace: 'pre-wrap', wordBreak: 'break-word' }}
        >
          {event.text}
        </Text>
      )
  }
}

/** Best-effort pretty-print of a (possibly partial) JSON string. */
function tryFormatJson(input: string): string {
  if (!input) return ''
  try {
    return JSON.stringify(JSON.parse(input), null, 2)
  } catch {
    return input
  }
}
