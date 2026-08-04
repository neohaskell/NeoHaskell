import type { ReactNode } from 'react'
import { render as rtlRender, type RenderOptions } from '@testing-library/react'
import { MantineProvider } from '@mantine/core'
import { ModalsProvider } from '@mantine/modals'
import { Notifications } from '@mantine/notifications'
import { theme, cssVariablesResolver } from '../theme'

/**
 * Test render that wraps the tree in the real app MantineProvider/theme so
 * Mantine components resolve their styles + context. Use this everywhere a
 * `.tsx` test renders UI — direct `@testing-library/react` render throws
 * "MantineProvider was not found" for any Mantine component.
 *
 * `env="test"` disables transitions/portcorrectness quirks that make assertions
 * flaky in jsdom.
 */
function Wrapper({ children }: { children: ReactNode }) {
  return (
    <MantineProvider theme={theme} cssVariablesResolver={cssVariablesResolver} env="test">
      <Notifications />
      <ModalsProvider>{children}</ModalsProvider>
    </MantineProvider>
  )
}

export function render(ui: ReactNode, options?: Omit<RenderOptions, 'wrapper'>) {
  return rtlRender(ui, { wrapper: Wrapper, ...options })
}

// Re-export everything so tests import screen/within/etc. from here.
export * from '@testing-library/react'
