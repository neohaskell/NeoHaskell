import { render, screen } from '../test/render'
import userEvent from '@testing-library/user-event'
import { describe, it, expect, vi } from 'vitest'
import { ProblemsPanel } from './ProblemsPanel'
import type { Issue } from '../model/validate'

const issues: Issue[] = [
  { id: 'dangling-edge', severity: 'error', edgeId: 'e1', message: 'Error — edge broken. Fix: delete it.' },
  { id: 'node-no-slice', severity: 'warning', nodeId: 'n1', message: 'Warning — no slice. Fix: drag it.' },
]

describe('ProblemsPanel', () => {
  it('renders nothing when closed', () => {
    render(
      <ProblemsPanel issues={issues} open={false} onClose={() => {}} onFocusIssue={() => {}} />,
    )
    expect(screen.queryByTestId('problems-panel')).not.toBeInTheDocument()
  })

  it('groups issues by severity and focuses the element on row click', async () => {
    const user = userEvent.setup()
    const onFocusIssue = vi.fn()
    render(<ProblemsPanel issues={issues} open onClose={() => {}} onFocusIssue={onFocusIssue} />)
    expect(screen.getByTestId('problems-group-error')).toBeInTheDocument()
    expect(screen.getByTestId('problems-group-warning')).toBeInTheDocument()
    await user.click(screen.getByTestId('problem-row-node-no-slice'))
    expect(onFocusIssue).toHaveBeenCalledWith(issues[1])
  })

  it('shows an empty state when there are no problems', () => {
    render(<ProblemsPanel issues={[]} open onClose={() => {}} onFocusIssue={() => {}} />)
    expect(screen.getByText(/no problems/i)).toBeInTheDocument()
  })
})
