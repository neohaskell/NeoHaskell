import { describe, it, expect, vi, beforeEach } from 'vitest'
import type { ReactNode } from 'react'
import { ReactFlowProvider } from '@xyflow/react'
import { render, screen } from '../../test/render'
import { NodeShell } from './NodeShell'

// The node's level-of-detail reads the canvas zoom via useStore(s => s.transform[2]).
// Mock useStore to feed a controllable zoom while keeping the rest of React Flow
// real (Handle/ReactFlowProvider still work).
let mockZoom = 1
vi.mock('@xyflow/react', async (importOriginal) => {
  const actual = await importOriginal<typeof import('@xyflow/react')>()
  return {
    ...actual,
    useStore: (selector: (s: { transform: [number, number, number] }) => unknown) =>
      selector({ transform: [0, 0, mockZoom] }),
  }
})

const wrap = (ui: ReactNode) => render(<ReactFlowProvider>{ui}</ReactFlowProvider>)

const fields = [
  { name: 'orderId', type: 'UUID' },
  { name: 'total', type: 'Money' },
]

beforeEach(() => {
  mockZoom = 1
})

describe('NodeShell record card', () => {
  it('renders_header_and_body_zones', () => {
    const { container } = wrap(<NodeShell variant="event" label="OrderPlaced" fields={fields} />)
    expect(container.querySelector('[data-variant="event"]')).not.toBeNull()
    // header carries the type name; body carries the field rows.
    expect(screen.getByText('OrderPlaced')).toBeInTheDocument()
    expect(screen.getByText('orderId')).toBeInTheDocument()
    expect(screen.getByText('UUID')).toBeInTheDocument()
  })

  it('node_renders_fields_read_only', () => {
    // A node with fields and NO change-callback shows the field rows (name +
    // type) and renders NO text inputs / no FieldsEditor.
    const { container } = wrap(<NodeShell variant="event" label="E" fields={fields} />)
    expect(screen.getByText('orderId')).toBeInTheDocument()
    expect(screen.getByText('UUID')).toBeInTheDocument()
    expect(screen.getByText('total')).toBeInTheDocument()
    expect(screen.getByText('Money')).toBeInTheDocument()
    expect(screen.queryByTestId('fields-editor')).toBeNull()
    expect(container.querySelectorAll('input')).toHaveLength(0)
  })

  it('empty_node_shows_no_fields_state', () => {
    wrap(<NodeShell variant="command" label="Checkout" fields={[]} />)
    expect(screen.getByText('no fields')).toBeInTheDocument()
  })

  it('node_has_no_field_editor', () => {
    // Fields are strictly read-only: the editor is never mounted regardless of
    // selection or zoom level.
    for (const sel of [false, true]) {
      const { unmount } = wrap(
        <NodeShell variant="event" label="E" fields={fields} selected={sel} />,
      )
      expect(screen.queryByTestId('fields-editor')).toBeNull()
      expect(screen.getByText('orderId')).toBeInTheDocument()
      unmount()
    }
    // …and at the close ("edit") zoom level too.
    mockZoom = 2 // above the edit threshold
    const { container } = wrap(<NodeShell variant="event" label="E" fields={fields} selected />)
    expect(screen.queryByTestId('fields-editor')).toBeNull()
    expect(container.querySelectorAll('input')).toHaveLength(0)
  })

  it('header_only_when_zoomed_out', () => {
    mockZoom = 0.3 // below COLLAPSE_THRESHOLD
    const { container } = wrap(<NodeShell variant="event" label="OrderPlaced" fields={fields} />)
    expect(container.querySelector('[data-detail="header"]')).not.toBeNull()
    // type name still shows; field rows are gone (flow view).
    expect(screen.getByText('OrderPlaced')).toBeInTheDocument()
    expect(screen.queryByText('orderId')).toBeNull()
  })

  it('caps_visible_rows_with_more_indicator', () => {
    const many = Array.from({ length: 9 }, (_, i) => ({ name: `f${i}`, type: 'T' }))
    wrap(<NodeShell variant="event" label="Big" fields={many} />)
    expect(screen.getByText('+3 more')).toBeInTheDocument() // 9 - FIELD_CAP(6)
  })
})
