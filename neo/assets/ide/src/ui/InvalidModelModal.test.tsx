import { describe, it, expect, vi } from 'vitest'
import { render, screen } from '../test/render'
import userEvent from '@testing-library/user-event'
import { InvalidModelModal } from './InvalidModelModal'
import type { ValidationError } from '../ipc/eventModel'

const fixtureErrors: ValidationError[] = [
  {
    pointer: '/nodes/3/type',
    message: '"frobnicate" is not one of the allowed node types',
    kind: 'schema',
  },
  {
    pointer: '/edges/1/sourceId',
    message: 'Edge `e1`: source node `missing` is not in nodes',
    kind: 'referentialIntegrity',
  },
]

describe('InvalidModelModal', () => {
  it('renders each error message', () => {
    render(
      <InvalidModelModal
        errors={fixtureErrors}
        onHeal={() => {}}
        onCancel={() => {}}
      />,
    )
    expect(
      screen.getByText(/frobnicate.*is not one of the allowed node types/i),
    ).toBeInTheDocument()
    expect(
      screen.getByText(/source node `missing` is not in nodes/i),
    ).toBeInTheDocument()
  })

  it('shows the JSON pointer for each error', () => {
    render(
      <InvalidModelModal
        errors={fixtureErrors}
        onHeal={() => {}}
        onCancel={() => {}}
      />,
    )
    expect(screen.getByText(/\/nodes\/3\/type/)).toBeInTheDocument()
    expect(screen.getByText(/\/edges\/1\/sourceId/)).toBeInTheDocument()
  })

  it('fires onHeal when the Heal button is clicked', async () => {
    const onHeal = vi.fn()
    render(
      <InvalidModelModal
        errors={fixtureErrors}
        onHeal={onHeal}
        onCancel={() => {}}
      />,
    )
    await userEvent.click(screen.getByRole('button', { name: /heal with ai/i }))
    expect(onHeal).toHaveBeenCalledOnce()
  })

  it('fires onCancel when the Cancel button is clicked', async () => {
    const onCancel = vi.fn()
    render(
      <InvalidModelModal
        errors={fixtureErrors}
        onHeal={() => {}}
        onCancel={onCancel}
      />,
    )
    await userEvent.click(screen.getByRole('button', { name: /cancel/i }))
    expect(onCancel).toHaveBeenCalledOnce()
  })

  it('uses preamble override when supplied', () => {
    render(
      <InvalidModelModal
        errors={fixtureErrors}
        preamble="The file is not valid JSON."
        onHeal={() => {}}
        onCancel={() => {}}
      />,
    )
    expect(screen.getByText('The file is not valid JSON.')).toBeInTheDocument()
  })
})
