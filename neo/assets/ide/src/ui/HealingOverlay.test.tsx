import { describe, it, expect, vi } from 'vitest'
import { render, screen, fireEvent } from '../test/render'
import { HealingOverlay } from './HealingOverlay'

describe('HealingOverlay', () => {
  it('renders the default healing message and a spinner', () => {
    render(<HealingOverlay />)
    expect(screen.getByText(/healing event model/i)).toBeInTheDocument()
    expect(screen.getByTestId('heal-spinner')).toBeInTheDocument()
  })

  it('uses a custom message when supplied', () => {
    render(<HealingOverlay message="Custom status" />)
    expect(screen.getByText('Custom status')).toBeInTheDocument()
  })

  it('marks itself as a live status region for screen readers', () => {
    render(<HealingOverlay />)
    expect(screen.getByRole('status')).toBeInTheDocument()
  })

  it('shows the log scroller with a placeholder when no lines have arrived', () => {
    render(<HealingOverlay log={[]} />)
    const scroller = screen.getByTestId('heal-log')
    expect(scroller).toBeInTheDocument()
    expect(scroller.textContent).toMatch(/reasoning, tool calls, and results/i)
    expect(screen.getByText(/waiting for the agent/i)).toBeInTheDocument()
  })

  it('renders raw stderr lines verbatim', () => {
    render(
      <HealingOverlay
        log={[{ stream: 'stderr', line: 'INFO: starting' }]}
      />,
    )
    const raw = screen.getByTestId('heal-event-raw')
    expect(raw.textContent).toContain('INFO: starting')
  })

  it('renders thinking deltas as a "thinking" card with the streamed text', () => {
    render(
      <HealingOverlay
        log={[
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'stream_event',
              event: {
                type: 'content_block_delta',
                delta: { type: 'thinking_delta', thinking: 'Let me look at the file' },
              },
            }),
          },
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'stream_event',
              event: {
                type: 'content_block_delta',
                delta: { type: 'thinking_delta', thinking: ' and decide what to do.' },
              },
            }),
          },
        ]}
      />,
    )
    const thinking = screen.getByTestId('heal-event-thinking')
    // Fragmented deltas merged into one card.
    expect(thinking.textContent).toContain(
      'Let me look at the file and decide what to do.',
    )
  })

  it('renders a tool_use card with the tool name and accumulated JSON input', () => {
    render(
      <HealingOverlay
        log={[
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'stream_event',
              event: {
                type: 'content_block_start',
                content_block: { type: 'tool_use', id: 't1', name: 'Read' },
              },
            }),
          },
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'stream_event',
              event: {
                type: 'content_block_delta',
                delta: { type: 'input_json_delta', partial_json: '{"file_path":"' },
              },
            }),
          },
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'stream_event',
              event: {
                type: 'content_block_delta',
                delta: { type: 'input_json_delta', partial_json: '/tmp/x.json"}' },
              },
            }),
          },
        ]}
      />,
    )
    const tool = screen.getByTestId('heal-event-tool-use')
    expect(tool.textContent).toContain('Read')
    expect(tool.textContent).toContain('/tmp/x.json')
  })

  it('renders a tool_result card with a truncated preview', () => {
    render(
      <HealingOverlay
        log={[
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'user',
              message: {
                content: [
                  {
                    type: 'tool_result',
                    tool_use_id: 't1',
                    content: 'a'.repeat(800),
                  },
                ],
              },
            }),
          },
        ]}
      />,
    )
    const result = screen.getByTestId('heal-event-tool-result')
    expect(result.textContent).toMatch(/truncated/i)
  })

  it('skips the giant system/init payload and the rate_limit_event noise', () => {
    render(
      <HealingOverlay
        log={[
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'system',
              subtype: 'init',
              tools: Array.from({ length: 400 }, (_, i) => `tool_${i}`),
            }),
          },
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'rate_limit_event',
              rate_limit_info: { status: 'allowed' },
            }),
          },
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'stream_event',
              event: {
                type: 'content_block_delta',
                delta: { type: 'text_delta', text: 'visible reply' },
              },
            }),
          },
        ]}
      />,
    )
    // Only the visible reply renders; init + rate limit are dropped.
    expect(screen.queryByText(/tool_0/)).not.toBeInTheDocument()
    expect(screen.queryByText(/rate_limit_info/)).not.toBeInTheDocument()
    expect(screen.getByTestId('heal-event-text').textContent).toContain('visible reply')
  })

  it('falls back to a raw card when a line is not parseable JSON', () => {
    render(
      <HealingOverlay
        log={[{ stream: 'stdout', line: 'plain text from the subprocess' }]}
      />,
    )
    const raw = screen.getByTestId('heal-event-raw')
    expect(raw.textContent).toContain('plain text from the subprocess')
  })

  it('renders an auto_repair card showing applied count and residuals', () => {
    render(
      <HealingOverlay
        log={[
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'neo_auto_repair',
              appliedCount: 7,
              residualCount: 3,
              summary: '5 edges, 1 kind fix, 0 position fixes, 1 layout entry, 3 residuals',
            }),
          },
        ]}
      />,
    )
    const card = screen.getByTestId('heal-event-auto-repair')
    expect(card.textContent).toContain('Auto-repaired 7 items')
    expect(card.textContent).toContain('3 residuals')
  })

  it('renders an api_retry card with the overloaded reason, attempt count, and delay', () => {
    render(
      <HealingOverlay
        log={[
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'system',
              subtype: 'api_retry',
              attempt: 2,
              max_retries: 10,
              retry_delay_ms: 1193,
              error_status: 529,
              error: 'rate_limit',
            }),
          },
        ]}
      />,
    )
    const card = screen.getByTestId('heal-event-api-retry')
    expect(card.textContent).toMatch(/overloaded/i)
    expect(card.textContent).toContain('attempt 2/10')
    expect(card.textContent).toContain('1.2s')
  })

  it('renders a Cancel button only when onCancel is supplied', () => {
    const { rerender } = render(<HealingOverlay />)
    expect(screen.queryByTestId('heal-cancel')).not.toBeInTheDocument()
    rerender(<HealingOverlay onCancel={() => {}} />)
    expect(screen.getByTestId('heal-cancel')).toBeInTheDocument()
  })

  it('fires onCancel when the user clicks Cancel', () => {
    const onCancel = vi.fn()
    render(<HealingOverlay onCancel={onCancel} />)
    fireEvent.click(screen.getByTestId('heal-cancel'))
    expect(onCancel).toHaveBeenCalledTimes(1)
  })

  it('disables Cancel and shows "Cancelling…" while cancelling is true', () => {
    render(<HealingOverlay onCancel={() => {}} cancelling />)
    const btn = screen.getByTestId('heal-cancel') as HTMLButtonElement
    expect(btn.disabled).toBe(true)
    expect(btn.textContent).toMatch(/cancelling/i)
  })

  it('updates the step counter from "waiting" to the timeline length', () => {
    const { rerender } = render(<HealingOverlay log={[]} />)
    expect(screen.getByText(/waiting for the agent/i)).toBeInTheDocument()
    rerender(
      <HealingOverlay
        log={[
          {
            stream: 'stdout',
            line: JSON.stringify({
              type: 'stream_event',
              event: {
                type: 'content_block_delta',
                delta: { type: 'text_delta', text: 'hello' },
              },
            }),
          },
        ]}
      />,
    )
    expect(screen.queryByText(/waiting for the agent/i)).not.toBeInTheDocument()
    expect(screen.getByText(/1 step/)).toBeInTheDocument()
  })
})
