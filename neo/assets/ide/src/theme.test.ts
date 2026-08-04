import { describe, it, expect } from 'vitest'
import { mergeMantineTheme, DEFAULT_THEME } from '@mantine/core'
import { theme, cssVariablesResolver } from './theme'

// The resolver runs against a fully-resolved theme (Mantine merges defaults —
// including gray/dark — before calling it). Mirror that here.
const resolved = mergeMantineTheme(DEFAULT_THEME, theme)

describe('theme', () => {
  it('theme_exposes_event_modeling_palette', () => {
    for (const name of ['emEvent', 'emCommand', 'emQuery', 'emIntegration', 'emUI', 'emFeature']) {
      expect(theme.colors?.[name]).toBeDefined()
      expect(theme.colors?.[name]).toHaveLength(10)
    }
    expect(theme.primaryColor).toBe('emFeature')
  })

  it('css_variables_resolver_emits_canvas_tokens_per_scheme', () => {
    const vars = cssVariablesResolver(resolved)
    expect(vars.variables['--em-event']).toBeTruthy()
    expect(vars.variables['--em-command']).toBeTruthy()
    // Selection + canvas bg differ by scheme.
    expect(vars.light['--em-selection']).toBeTruthy()
    expect(vars.dark['--em-selection']).toBeTruthy()
    expect(vars.light['--em-canvas-bg']).not.toBe(vars.dark['--em-canvas-bg'])
  })
})
