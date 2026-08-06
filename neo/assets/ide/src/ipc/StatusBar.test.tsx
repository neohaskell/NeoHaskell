import { render, screen } from '../test/render'
import userEvent from '@testing-library/user-event'
import { describe, it, expect, vi } from 'vitest'
import { StatusBar } from './StatusBar'

const base = {
  state: { status: 'open' as const },
  init: null,
  issueCounts: { error: 0, warning: 0, info: 0 },
  onToggleProblems: vi.fn(),
  onRetrySave: vi.fn(),
}

describe('StatusBar', () => {
  it('shows "All changes saved" when clean', () => {
    render(<StatusBar {...base} saveState={{ kind: 'saved' }} />)
    expect(screen.getByText(/all changes saved/i)).toBeInTheDocument()
  })

  it('shows "Saving…" while saving', () => {
    render(<StatusBar {...base} saveState={{ kind: 'saving' }} />)
    expect(screen.getByText(/saving…/i)).toBeInTheDocument()
  })

  it('shows offline copy when the connection is down', () => {
    render(<StatusBar {...base} saveState={{ kind: 'offline' }} />)
    expect(screen.getByText(/will save when reconnected/i)).toBeInTheDocument()
  })

  it('shows a Retry button on save failure', async () => {
    const user = userEvent.setup()
    const onRetrySave = vi.fn()
    render(<StatusBar {...base} onRetrySave={onRetrySave} saveState={{ kind: 'failed' }} />)
    expect(screen.getByText(/save failed/i)).toBeInTheDocument()
    await user.click(screen.getByTestId('retry-save'))
    expect(onRetrySave).toHaveBeenCalledOnce()
  })

  it('shows Valid and toggles the Problems panel on chip click', async () => {
    const user = userEvent.setup()
    const onToggleProblems = vi.fn()
    render(<StatusBar {...base} onToggleProblems={onToggleProblems} saveState={{ kind: 'saved' }} />)
    expect(screen.getByTestId('validation-chip')).toHaveTextContent('Valid')
    await user.click(screen.getByTestId('validation-chip'))
    expect(onToggleProblems).toHaveBeenCalledOnce()
  })

  it('shows error · warning counts when there are problems', () => {
    render(<StatusBar {...base} issueCounts={{ error: 1, warning: 3, info: 2 }} saveState={{ kind: 'saved' }} />)
    expect(screen.getByTestId('validation-chip')).toHaveTextContent('1 · 3')
  })
})
