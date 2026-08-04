import { describe, it, expect, vi } from 'vitest'
import { render, screen } from '../../test/render'
import userEvent from '@testing-library/user-event'
import { ActivityRail } from './ActivityRail'

describe('ActivityRail', () => {
  it('activity_rail_switches_lens', async () => {
    const user = userEvent.setup()
    const onChange = vi.fn()
    render(<ActivityRail lens="model" onChange={onChange} />)
    // All four lenses present; model is active.
    expect(screen.getByTestId('lens-model')).toHaveAttribute('data-active', 'true')
    expect(screen.getByTestId('lens-schema')).toHaveAttribute('data-active', 'false')
    await user.click(screen.getByTestId('lens-schema'))
    expect(onChange).toHaveBeenCalledWith('schema')
  })

  it('renders all four lenses', () => {
    render(<ActivityRail lens="model" onChange={() => {}} />)
    for (const id of ['model', 'schema', 'logs', 'emulate']) {
      expect(screen.getByTestId(`lens-${id}`)).toBeInTheDocument()
    }
  })
})
