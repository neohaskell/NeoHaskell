import { describe, it, expect } from 'vitest'
import { render, screen } from '../../test/render'
import { EmptyLens } from './EmptyLens'

describe('EmptyLens', () => {
  it('empty_lens_shows_coming_soon', () => {
    render(<EmptyLens lens="schema" />)
    expect(screen.getByTestId('empty-lens-schema')).toBeInTheDocument()
    expect(screen.getByText('Coming soon')).toBeInTheDocument()
    expect(screen.getByText(/fields of every command/i)).toBeInTheDocument()
  })

  it('renders the lens label as a heading', () => {
    render(<EmptyLens lens="emulate" />)
    expect(screen.getByRole('heading', { name: 'Emulate' })).toBeInTheDocument()
  })
})
