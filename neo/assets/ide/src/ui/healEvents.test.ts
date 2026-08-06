import { describe, it, expect } from 'vitest'
import { reduceHealLog } from './healEvents'
import type { HealLogLine } from './HealingOverlay'

const stdout = (obj: unknown): HealLogLine => ({
  stream: 'stdout',
  line: JSON.stringify(obj),
})

describe('reduceHealLog', () => {
  it('drops the system/init dump entirely', () => {
    const events = reduceHealLog([
      stdout({ type: 'system', subtype: 'init', tools: ['a', 'b'], cwd: '/' }),
    ])
    expect(events).toEqual([])
  })

  it('drops rate_limit_event noise', () => {
    const events = reduceHealLog([
      stdout({ type: 'rate_limit_event', rate_limit_info: {} }),
    ])
    expect(events).toEqual([])
  })

  it('collapses adjacent identical status pings into one', () => {
    const events = reduceHealLog([
      stdout({ type: 'system', subtype: 'status', status: 'requesting' }),
      stdout({ type: 'system', subtype: 'status', status: 'requesting' }),
      stdout({ type: 'system', subtype: 'status', status: 'requesting' }),
    ])
    expect(events).toHaveLength(1)
    expect(events[0]).toMatchObject({ kind: 'status', label: 'requesting' })
  })

  it('merges thinking_delta fragments into one growing event', () => {
    const events = reduceHealLog([
      stdout({
        type: 'stream_event',
        event: {
          type: 'content_block_delta',
          delta: { type: 'thinking_delta', thinking: 'I need to ' },
        },
      }),
      stdout({
        type: 'stream_event',
        event: {
          type: 'content_block_delta',
          delta: { type: 'thinking_delta', thinking: 'read the file ' },
        },
      }),
      stdout({
        type: 'stream_event',
        event: {
          type: 'content_block_delta',
          delta: { type: 'thinking_delta', thinking: 'first.' },
        },
      }),
    ])
    expect(events).toHaveLength(1)
    expect(events[0]).toMatchObject({
      kind: 'thinking',
      text: 'I need to read the file first.',
    })
  })

  it('merges text_delta fragments into one growing event', () => {
    const events = reduceHealLog([
      stdout({
        type: 'stream_event',
        event: {
          type: 'content_block_delta',
          delta: { type: 'text_delta', text: 'Done. ' },
        },
      }),
      stdout({
        type: 'stream_event',
        event: {
          type: 'content_block_delta',
          delta: { type: 'text_delta', text: 'I edited the file.' },
        },
      }),
    ])
    expect(events).toHaveLength(1)
    expect(events[0]).toMatchObject({
      kind: 'text',
      text: 'Done. I edited the file.',
    })
  })

  it('opens a tool_use card on content_block_start and folds in partial JSON', () => {
    const events = reduceHealLog([
      stdout({
        type: 'stream_event',
        event: {
          type: 'content_block_start',
          content_block: { type: 'tool_use', id: 't1', name: 'Edit' },
        },
      }),
      stdout({
        type: 'stream_event',
        event: {
          type: 'content_block_delta',
          delta: { type: 'input_json_delta', partial_json: '{"file_path":"' },
        },
      }),
      stdout({
        type: 'stream_event',
        event: {
          type: 'content_block_delta',
          delta: { type: 'input_json_delta', partial_json: '/x"}' },
        },
      }),
    ])
    expect(events).toHaveLength(1)
    expect(events[0]).toMatchObject({
      kind: 'tool_use',
      name: 'Edit',
      input: '{"file_path":"/x"}',
    })
  })

  it('extracts tool_result preview from a user message and flags truncation', () => {
    const big = 'x'.repeat(1000)
    const events = reduceHealLog([
      stdout({
        type: 'user',
        message: {
          content: [
            { type: 'tool_result', tool_use_id: 't1', content: big },
          ],
        },
      }),
    ])
    expect(events).toHaveLength(1)
    expect(events[0].kind).toBe('tool_result')
    if (events[0].kind === 'tool_result') {
      expect(events[0].truncated).toBe(true)
      expect(events[0].preview.length).toBeLessThan(big.length)
    }
  })

  it('drops signature_delta (binary noise) silently', () => {
    const events = reduceHealLog([
      stdout({
        type: 'stream_event',
        event: {
          type: 'content_block_delta',
          delta: { type: 'signature_delta', signature: 'EpgCC...binary...' },
        },
      }),
    ])
    expect(events).toEqual([])
  })

  it('drops structural stream events (message_start, content_block_stop, etc.)', () => {
    const events = reduceHealLog([
      stdout({
        type: 'stream_event',
        event: { type: 'message_start', message: {} },
      }),
      stdout({
        type: 'stream_event',
        event: { type: 'content_block_stop', index: 0 },
      }),
      stdout({
        type: 'stream_event',
        event: { type: 'message_stop' },
      }),
      stdout({
        type: 'stream_event',
        event: { type: 'message_delta', delta: {} },
      }),
    ])
    expect(events).toEqual([])
  })

  it('falls back to a raw event for non-JSON stdout', () => {
    const events = reduceHealLog([
      { stream: 'stdout', line: 'this is not json' },
    ])
    expect(events).toHaveLength(1)
    expect(events[0]).toMatchObject({
      kind: 'raw',
      text: 'this is not json',
      stream: 'stdout',
    })
  })

  it('always renders stderr verbatim regardless of content', () => {
    const events = reduceHealLog([
      { stream: 'stderr', line: 'shim: claude error 27' },
    ])
    expect(events).toHaveLength(1)
    expect(events[0]).toMatchObject({
      kind: 'raw',
      text: 'shim: claude error 27',
      stream: 'stderr',
    })
  })

  it('skips empty stdout lines so a quiet stream is silent, not noisy', () => {
    const events = reduceHealLog([
      { stream: 'stdout', line: '' },
      { stream: 'stdout', line: '   ' },
    ])
    expect(events).toEqual([])
  })

  it('surfaces a neo_auto_repair line as an auto_repair card', () => {
    const events = reduceHealLog([
      stdout({
        type: 'neo_auto_repair',
        appliedCount: 5,
        residualCount: 2,
        summary: '3 edges, 1 kind fix, 0 position fixes, 1 layout entry, 2 residuals',
      }),
    ])
    expect(events).toHaveLength(1)
    expect(events[0]).toMatchObject({
      kind: 'auto_repair',
      appliedCount: 5,
      residualCount: 2,
    })
  })

  it('surfaces api_retry events with attempt count and delay', () => {
    const events = reduceHealLog([
      stdout({
        type: 'system',
        subtype: 'api_retry',
        attempt: 1,
        max_retries: 10,
        retry_delay_ms: 617.0886764108919,
        error_status: 529,
        error: 'rate_limit',
      }),
    ])
    expect(events).toHaveLength(1)
    expect(events[0]).toMatchObject({
      kind: 'api_retry',
      attempt: 1,
      maxRetries: 10,
      delayMs: 617.0886764108919,
      errorStatus: 529,
      error: 'rate_limit',
    })
  })

  it('collapses successive api_retry events into a single updating card', () => {
    const events = reduceHealLog([
      stdout({
        type: 'system',
        subtype: 'api_retry',
        attempt: 1,
        max_retries: 10,
        retry_delay_ms: 617,
        error_status: 529,
        error: 'rate_limit',
      }),
      stdout({
        type: 'system',
        subtype: 'api_retry',
        attempt: 2,
        max_retries: 10,
        retry_delay_ms: 1193,
        error_status: 529,
        error: 'rate_limit',
      }),
      stdout({
        type: 'system',
        subtype: 'api_retry',
        attempt: 3,
        max_retries: 10,
        retry_delay_ms: 2438,
        error_status: 529,
        error: 'rate_limit',
      }),
    ])
    expect(events).toHaveLength(1)
    expect(events[0]).toMatchObject({
      kind: 'api_retry',
      attempt: 3,
      delayMs: 2438,
    })
  })

  it('keeps a prior api_retry card when subsequent normal events arrive', () => {
    const events = reduceHealLog([
      stdout({
        type: 'system',
        subtype: 'api_retry',
        attempt: 1,
        max_retries: 10,
        retry_delay_ms: 617,
        error_status: 529,
        error: 'rate_limit',
      }),
      stdout({
        type: 'stream_event',
        event: {
          type: 'content_block_delta',
          delta: { type: 'text_delta', text: 'recovered.' },
        },
      }),
    ])
    expect(events.map((e) => e.kind)).toEqual(['api_retry', 'text'])
  })

  it('handles a realistic Read → result sequence in order', () => {
    const events = reduceHealLog([
      stdout({
        type: 'stream_event',
        event: {
          type: 'content_block_start',
          content_block: { type: 'tool_use', id: 't1', name: 'Read' },
        },
      }),
      stdout({
        type: 'stream_event',
        event: {
          type: 'content_block_delta',
          delta: {
            type: 'input_json_delta',
            partial_json: '{"file_path":"/tmp/event-model.json"}',
          },
        },
      }),
      stdout({
        type: 'user',
        message: {
          content: [
            { type: 'tool_result', tool_use_id: 't1', content: '1\tfile content' },
          ],
        },
      }),
    ])
    expect(events.map((e) => e.kind)).toEqual(['tool_use', 'tool_result'])
  })
})
