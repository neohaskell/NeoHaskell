import {
  createTheme,
  type MantineColorsTuple,
  type CSSVariablesResolver,
} from '@mantine/core'

// ──────────────────────────────────────────────────────────────────────────
// Centralized theme — the SINGLE source of styling truth for the neo IDE.
// See assets/ide/CLAUDE.md: no in-place styling; everything flows from here.
//
// The event-modeling color grammar (orange events / blue commands / green
// queries / gray integrations / yellow UI / indigo features) is encoded as
// custom Mantine colors so node components and canvas CSS read the SAME tokens
// in both light and dark schemes.
// ──────────────────────────────────────────────────────────────────────────

// 10-shade tuples (0 = lightest … 9 = darkest). Index 5/6 is the "solid" shade
// used for filled node backgrounds.
const emEvent: MantineColorsTuple = [
  '#fff4e6', '#ffe8cc', '#ffd8a8', '#ffc078', '#ffa94d',
  '#ff922b', '#fd7e14', '#f76707', '#e8590c', '#d9480f',
]
const emCommand: MantineColorsTuple = [
  '#e7f5ff', '#d0ebff', '#a5d8ff', '#74c0fc', '#4dabf7',
  '#339af0', '#228be6', '#1c7ed6', '#1971c2', '#1864ab',
]
const emQuery: MantineColorsTuple = [
  '#ebfbee', '#d3f9d8', '#b2f2bb', '#8ce99a', '#69db7c',
  '#51cf66', '#40c057', '#37b24d', '#2f9e44', '#2b8a3e',
]
// Monotonic gray ramp; index 6 (#6b7280) is the solid integration-node fill —
// dark enough for legible white text via autoContrast.
const emIntegration: MantineColorsTuple = [
  '#f8f9fa', '#eceef0', '#d9dde0', '#c2c8cd', '#a7afb6',
  '#8a939b', '#6b7280', '#586068', '#454b52', '#33373d',
]
const emUI: MantineColorsTuple = [
  '#fff9db', '#fff3bf', '#ffec99', '#ffe066', '#ffd43b',
  '#fcc419', '#fab005', '#f59f00', '#f08c00', '#e67700',
]
const emFeature: MantineColorsTuple = [
  '#edf2ff', '#dbe4ff', '#bac8ff', '#91a7ff', '#748ffc',
  '#5c7cfa', '#4c6ef5', '#4263eb', '#3b5bdb', '#364fc7',
]

export const theme = createTheme({
  colors: { emEvent, emCommand, emQuery, emIntegration, emUI, emFeature },
  primaryColor: 'emFeature',
  primaryShade: { light: 6, dark: 5 },
  defaultRadius: 'md',
  autoContrast: true,
  fontFamily:
    '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif',
  fontFamilyMonospace:
    'ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, monospace',
  // Centralized per-component defaults & overrides. Components must NOT restate
  // these inline — change them here.
  components: {
    Button: { defaultProps: { variant: 'light', size: 'sm' } },
    ActionIcon: { defaultProps: { variant: 'subtle', size: 'md' } },
    Modal: { defaultProps: { centered: true, overlayProps: { blur: 2 } } },
    TextInput: { defaultProps: { size: 'sm' } },
    Select: { defaultProps: { size: 'sm', comboboxProps: { withinPortal: true } } },
    Tooltip: { defaultProps: { withArrow: true, openDelay: 300 } },
    Badge: { defaultProps: { variant: 'light' } },
    Tabs: { defaultProps: { keepMounted: false } },
  },
})

// Expose the event-modeling solid shades + selection/dim tokens as plain CSS
// variables so src/styles/canvas.css (React Flow internals) can reference the
// same palette. These resolve per color-scheme.
export const cssVariablesResolver: CSSVariablesResolver = (t) => ({
  variables: {
    '--em-event': t.colors.emEvent[6],
    '--em-command': t.colors.emCommand[6],
    '--em-query': t.colors.emQuery[6],
    '--em-integration': t.colors.emIntegration[6],
    '--em-ui': t.colors.emUI[6],
    '--em-feature': t.colors.emFeature[5],

    // ── Record-card node tokens ──────────────────────────────────────────
    // Each node is a two-zone card: a saturated COLOR HEADER (type name) over a
    // neutral BODY (field rows). The header hue is the event-modeling identity
    // color, but DARKENED toward black so white header text clears WCAG AA
    // (white needs header relative-luminance ≤ 0.183, i.e. ≥ 4.5:1). The mix %
    // is tuned per hue — green is brightest so it darkens most. The bright
    // identity color is kept for the thin left rail (--em-* directly), so the
    // hue still reads at full saturation somewhere on every card.
    '--node-header-event': `color-mix(in srgb, ${t.colors.emEvent[6]} 78%, black)`,
    '--node-header-command': `color-mix(in srgb, ${t.colors.emCommand[6]} 78%, black)`,
    '--node-header-query': `color-mix(in srgb, ${t.colors.emQuery[6]} 66%, black)`,
    '--node-header-integration': `color-mix(in srgb, ${t.colors.emIntegration[6]} 78%, black)`,
    '--node-header-text': t.white,
  },
  light: {
    '--em-selection': t.colors.emFeature[6],
    '--em-canvas-bg': t.colors.gray[0],
    '--em-grid-dot': t.colors.gray[3],
    // Default edges: a readable dark line on the light canvas; portal hops in indigo.
    '--em-edge': t.colors.dark[5],
    '--em-edge-portal': t.colors.emFeature[6],
    // Trace highlight (selected node's causal path).
    '--em-trace': t.colors.dark[8],
    '--em-trace-portal': t.colors.emFeature[7],
    // Node body zone (light scheme): the per-variant faint tint is mixed onto
    // this base in NodeShell.module.css.
    '--node-body-base': t.white,
    '--node-body-text': t.colors.dark[7],
    '--node-type-text': t.colors.gray[6],
    '--node-border': t.colors.gray[3],
    '--node-divider': t.colors.gray[2],
  },
  dark: {
    '--em-selection': t.colors.emFeature[4],
    '--em-canvas-bg': t.colors.dark[8],
    '--em-grid-dot': t.colors.dark[4],
    // On the dark canvas a black line vanishes — use a light line instead.
    '--em-edge': t.colors.gray[5],
    '--em-edge-portal': t.colors.emFeature[4],
    '--em-trace': t.colors.gray[2],
    '--em-trace-portal': t.colors.emFeature[4],
    // Node body zone (dark scheme). Field NAMES use gray[1] (≈15:1 on dark[7]);
    // field TYPES use dark[1] dimmed (≈6.6:1) — both clear AA.
    '--node-body-base': t.colors.dark[7],
    '--node-body-text': t.colors.gray[1],
    '--node-type-text': t.colors.dark[1],
    '--node-border': t.colors.dark[4],
    '--node-divider': t.colors.dark[5],
  },
})
