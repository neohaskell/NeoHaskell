// Parse claude's `--output-format stream-json --include-partial-messages`
// frames into structured events the UI can render as readable "thoughts"
// instead of raw JSON. The agent emits one JSON object per stdout line; we
// reduce a sequence of those into a small typed timeline:
//
//   - `thinking`     — the model's chain-of-thought (visible text deltas)
//   - `text`         — the model's spoken reply text
//   - `tool_use`     — a tool call: { name, partial JSON input }
//   - `tool_result`  — short preview of what a tool returned
//   - `status`       — small status markers (claude requesting, etc.)
//   - `raw`          — fallback for any line we can't classify
//
// Bursty incremental deltas (the model emits thinking/text/input_json
// piece by piece) are folded into the LAST matching event so the UI sees
// a single growing message rather than 200 fragments.

import type { HealLogLine } from './HealingOverlay'

export type HealEvent =
  | { id: string; kind: 'thinking'; text: string }
  | { id: string; kind: 'text'; text: string }
  | { id: string; kind: 'tool_use'; name: string; input: string }
  | { id: string; kind: 'tool_result'; preview: string; truncated: boolean }
  | { id: string; kind: 'status'; label: string }
  | {
      id: string
      kind: 'api_retry'
      attempt: number
      maxRetries: number
      delayMs: number
      errorStatus: number
      error: string
    }
  | {
      id: string
      kind: 'auto_repair'
      appliedCount: number
      residualCount: number
      summary: string
    }
  | { id: string; kind: 'raw'; text: string; stream: 'stdout' | 'stderr' }

const TOOL_RESULT_PREVIEW_CHARS = 400

let idCounter = 0
function nextId(): string {
  idCounter += 1
  return `e-${idCounter}`
}

/** Apply one streamed line to the event timeline. Returns a new timeline. */
export function reduceHealLine(
  events: HealEvent[],
  log: HealLogLine,
): HealEvent[] {
  // Anything on stderr is rendered as-is — usually a status line, not JSON.
  if (log.stream === 'stderr') {
    return [...events, { id: nextId(), kind: 'raw', text: log.line, stream: 'stderr' }]
  }

  // Try to parse as JSON. If it isn't, treat as raw stdout.
  let parsed: unknown
  try {
    parsed = JSON.parse(log.line)
  } catch {
    if (log.line.trim() === '') return events
    return [...events, { id: nextId(), kind: 'raw', text: log.line, stream: log.stream }]
  }

  if (!parsed || typeof parsed !== 'object') return events
  const obj = parsed as Record<string, unknown>

  // Top-level kinds we care about ----------------------------------------

  if (obj.type === 'system') {
    if (obj.subtype === 'init') return events // huge tools/MCP dump — skip
    if (obj.subtype === 'status' && typeof obj.status === 'string') {
      // Compact "requesting…" markers — collapse adjacent ones.
      const last = events[events.length - 1]
      if (last?.kind === 'status' && last.label === obj.status) return events
      return [...events, { id: nextId(), kind: 'status', label: obj.status }]
    }
    if (obj.subtype === 'api_retry') {
      // claude-code retries on HTTP 429/529. Render as a single card that
      // updates in place across attempts so the user sees a climbing
      // counter, not a wall of duplicate warnings.
      const attempt = numOr(obj.attempt, 0)
      const maxRetries = numOr(obj.max_retries, 0)
      const delayMs = numOr(obj.retry_delay_ms, 0)
      const errorStatus = numOr(obj.error_status, 0)
      const error = typeof obj.error === 'string' ? obj.error : 'unknown'
      const card: HealEvent = {
        id: nextId(),
        kind: 'api_retry',
        attempt,
        maxRetries,
        delayMs,
        errorStatus,
        error,
      }
      const last = events[events.length - 1]
      if (last?.kind === 'api_retry') {
        return [...events.slice(0, -1), { ...card, id: last.id }]
      }
      return [...events, card]
    }
    return events
  }

  if (obj.type === 'rate_limit_event') return events // not interesting to humans

  if (obj.type === 'neo_auto_repair') {
    // Synthetic event minted by App.tsx when the backend sends the
    // `autoRepair` $/progress notification. Always one card per event —
    // the backend emits it at most once per heal run.
    return [
      ...events,
      {
        id: nextId(),
        kind: 'auto_repair',
        appliedCount: numOr(obj.appliedCount, 0),
        residualCount: numOr(obj.residualCount, 0),
        summary: typeof obj.summary === 'string' ? obj.summary : '',
      },
    ]
  }

  if (obj.type === 'user') {
    // Tool results come back as user messages with `content: [{type: "tool_result", ...}]`
    const msg = (obj.message as { content?: unknown[] } | undefined)?.content
    if (Array.isArray(msg)) {
      for (const item of msg) {
        if (
          item &&
          typeof item === 'object' &&
          (item as { type?: unknown }).type === 'tool_result'
        ) {
          const content = (item as { content?: unknown }).content
          const flat = typeof content === 'string' ? content : JSON.stringify(content)
          const truncated = flat.length > TOOL_RESULT_PREVIEW_CHARS
          const preview = truncated ? flat.slice(0, TOOL_RESULT_PREVIEW_CHARS) : flat
          events = [
            ...events,
            { id: nextId(), kind: 'tool_result', preview, truncated },
          ]
        }
      }
    }
    return events
  }

  if (obj.type === 'assistant') return events // redundant — we already streamed via deltas

  if (obj.type === 'stream_event') {
    const e = obj.event as Record<string, unknown> | undefined
    if (!e || typeof e !== 'object') return events

    // Tool-use start: open a new tool_use card with the name.
    if (
      e.type === 'content_block_start' &&
      e.content_block &&
      typeof e.content_block === 'object'
    ) {
      const cb = e.content_block as { type?: string; name?: string }
      if (cb.type === 'tool_use' && typeof cb.name === 'string') {
        return [
          ...events,
          { id: nextId(), kind: 'tool_use', name: cb.name, input: '' },
        ]
      }
      return events
    }

    if (e.type === 'content_block_delta' && e.delta && typeof e.delta === 'object') {
      const delta = e.delta as {
        type?: string
        thinking?: string
        text?: string
        partial_json?: string
      }

      if (delta.type === 'thinking_delta' && typeof delta.thinking === 'string') {
        return appendOrCreate(events, 'thinking', delta.thinking)
      }

      if (delta.type === 'text_delta' && typeof delta.text === 'string') {
        return appendOrCreate(events, 'text', delta.text)
      }

      if (delta.type === 'input_json_delta' && typeof delta.partial_json === 'string') {
        // Append to the most recent tool_use card, wherever it is.
        for (let i = events.length - 1; i >= 0; i--) {
          const ev = events[i]
          if (ev.kind === 'tool_use') {
            const updated: HealEvent = { ...ev, input: ev.input + delta.partial_json }
            return [...events.slice(0, i), updated, ...events.slice(i + 1)]
          }
        }
        return events
      }

      // signature_delta (binary) and any other delta types — silently skip.
      return events
    }

    // message_start, content_block_stop, message_delta, message_stop — structural noise.
    return events
  }

  // Unknown structured event — fall back to raw so nothing gets silently lost.
  return [...events, { id: nextId(), kind: 'raw', text: log.line, stream: log.stream }]
}

function numOr(value: unknown, fallback: number): number {
  return typeof value === 'number' && Number.isFinite(value) ? value : fallback
}

function appendOrCreate(
  events: HealEvent[],
  kind: 'thinking' | 'text',
  chunk: string,
): HealEvent[] {
  const last = events[events.length - 1]
  if (last?.kind === kind) {
    const updated: HealEvent = { ...last, text: last.text + chunk } as HealEvent
    return [...events.slice(0, -1), updated]
  }
  return [...events, { id: nextId(), kind, text: chunk } as HealEvent]
}

/** Reduce an entire batch of log lines into events. Useful for tests. */
export function reduceHealLog(log: HealLogLine[]): HealEvent[] {
  return log.reduce((acc, line) => reduceHealLine(acc, line), [] as HealEvent[])
}
