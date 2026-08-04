import { describe, it, expect } from 'vitest'
import { Button } from '@mantine/core'
import { render, screen } from './render'

describe('test render helper', () => {
  it('render_helper_wraps_in_mantine_provider', () => {
    // Without the MantineProvider wrapper this throws "@mantine/core: MantineProvider
    // was not found in component tree". If it renders, the wrapper works.
    render(<Button>Click me</Button>)
    expect(screen.getByRole('button', { name: /click me/i })).toBeInTheDocument()
  })
})
