import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { describe, it, expect, vi } from 'vitest'
import { EditableLabel } from './EditableLabel'

describe('EditableLabel', () => {
  it('displays the label text', () => {
    render(<EditableLabel label="OrderPlaced" onRename={vi.fn()} />)
    expect(screen.getByText('OrderPlaced')).toBeInTheDocument()
  })

  it('enters edit mode on double-click', async () => {
    const user = userEvent.setup()
    render(<EditableLabel label="OrderPlaced" onRename={vi.fn()} />)
    await user.dblClick(screen.getByText('OrderPlaced'))
    expect(screen.getByRole('textbox')).toBeInTheDocument()
    expect(screen.getByRole('textbox')).toHaveValue('OrderPlaced')
  })

  it('confirms rename on Enter', async () => {
    const user = userEvent.setup()
    const onRename = vi.fn()
    render(<EditableLabel label="Old" onRename={onRename} />)
    await user.dblClick(screen.getByText('Old'))
    const input = screen.getByRole('textbox')
    await user.clear(input)
    await user.type(input, 'New{Enter}')
    expect(onRename).toHaveBeenCalledWith('New')
    expect(screen.queryByRole('textbox')).not.toBeInTheDocument()
  })

  it('cancels rename on Escape', async () => {
    const user = userEvent.setup()
    const onRename = vi.fn()
    render(<EditableLabel label="Original" onRename={onRename} />)
    await user.dblClick(screen.getByText('Original'))
    const input = screen.getByRole('textbox')
    await user.clear(input)
    await user.type(input, 'Changed{Escape}')
    expect(onRename).not.toHaveBeenCalled()
    expect(screen.getByText('Original')).toBeInTheDocument()
  })

  it('confirms rename on blur', async () => {
    const user = userEvent.setup()
    const onRename = vi.fn()
    render(<EditableLabel label="Old" onRename={onRename} />)
    await user.dblClick(screen.getByText('Old'))
    const input = screen.getByRole('textbox')
    await user.clear(input)
    await user.type(input, 'New')
    await user.tab()
    expect(onRename).toHaveBeenCalledWith('New')
  })

  it('does not call onRename if value unchanged', async () => {
    const user = userEvent.setup()
    const onRename = vi.fn()
    render(<EditableLabel label="Same" onRename={onRename} />)
    await user.dblClick(screen.getByText('Same'))
    await user.keyboard('{Enter}')
    expect(onRename).not.toHaveBeenCalled()
  })
})
