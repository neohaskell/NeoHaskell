import { render, screen, waitFor, within, fireEvent } from './test/render'
import userEvent from '@testing-library/user-event'
import { describe, it, expect, vi, beforeEach } from 'vitest'

// `ipc/client` is mocked module-wide so we can control the WS handshake
// and the responses to `initialize` / `readEventModel` / `healEventModel`.
// Tests configure behaviour via the `__configureIpc` helper exposed below.

interface IpcStubConfig {
  initialize: { ok: true; result: unknown } | { ok: false; error: { code: number; message: string } }
  readQueue: Array<
    | { ok: true; result: { content: string | null; validation: unknown } }
    | { ok: false; error: { code: number; message: string } }
  >
  healQueue: Array<
    | { ok: true; result: { outcome: { status: 'healed' } | { status: 'stillInvalid'; errors: unknown[] } } }
    | { ok: false; error: { code: number; message: string } }
  >
  healDelayMs?: number
}

const ipcState: {
  config: IpcStubConfig
  readCalls: number
  healCalls: number
  relayoutCalls: number
  callLog: string[]
} = {
  config: {
    initialize: {
      ok: true,
      result: {
        serverInfo: { name: 'neo', version: '0.0.0' },
        serverCapabilities: {},
        workspace: { id: 'ws', root: '/tmp/ws', project: null },
        sessionId: 'sess',
      },
    },
    readQueue: [],
    healQueue: [],
  },
  readCalls: 0,
  healCalls: 0,
  relayoutCalls: 0,
  callLog: [],
}

function nextResponse<T>(queue: T[], label: string): T {
  if (queue.length === 0) {
    throw new Error(`Test setup error: no queued ${label} response`)
  }
  return queue.shift()!
}

const notificationSubs = new Map<string, Set<(params: unknown) => void>>()
function emitNotification(method: string, params: unknown) {
  const subs = notificationSubs.get(method)
  if (!subs) return
  for (const sub of subs) sub(params)
}

vi.mock('./ipc/client', () => {
  class IdeClientStub {
    onState(listener: (s: unknown) => void) {
      listener({ status: 'open' })
      return () => {}
    }
    onNotification(method: string, listener: (params: unknown) => void) {
      let subs = notificationSubs.get(method)
      if (!subs) {
        subs = new Set()
        notificationSubs.set(method, subs)
      }
      subs.add(listener)
      return () => {
        subs!.delete(listener)
      }
    }
    close() {}
  }
  return { IdeClient: IdeClientStub, defaultWsUrl: () => 'ws://stub' }
})

vi.mock('./ipc/initialize', () => ({
  initialize: vi.fn(async () => ipcState.config.initialize),
}))

vi.mock('./ipc/eventModel', () => ({
  readEventModel: vi.fn(async () => {
    ipcState.readCalls += 1
    ipcState.callLog.push('read')
    return nextResponse(ipcState.config.readQueue, 'readEventModel')
  }),
  writeEventModel: vi.fn(async () => {
    ipcState.callLog.push('write')
    return { ok: true, result: { path: '/tmp/ws/event-model.json' } }
  }),
  relayoutEventModel: vi.fn(async () => {
    ipcState.relayoutCalls += 1
    ipcState.callLog.push('relayout')
    return { ok: true, result: { applied: 1 } }
  }),
  healEventModel: vi.fn(async () => {
    ipcState.healCalls += 1
    if (ipcState.config.healDelayMs) {
      await new Promise((r) => setTimeout(r, ipcState.config.healDelayMs))
    }
    return nextResponse(ipcState.config.healQueue, 'healEventModel')
  }),
}))

import App from './App'

function configureIpc(patch: Partial<IpcStubConfig>) {
  ipcState.config = {
    initialize: patch.initialize ?? ipcState.config.initialize,
    readQueue: patch.readQueue ?? [],
    healQueue: patch.healQueue ?? [],
    healDelayMs: patch.healDelayMs,
  }
  ipcState.readCalls = 0
  ipcState.healCalls = 0
  ipcState.relayoutCalls = 0
  ipcState.callLog = []
}

const VALID_MODEL_JSON = JSON.stringify({
  id: 'm1',
  name: 'Demo',
  chapters: [],
  entities: [],
  slices: [],
  nodes: [],
  edges: [],
  layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
})

beforeEach(() => {
  configureIpc({
    readQueue: [{ ok: true, result: { content: null, validation: { status: 'notFound' } } }],
  })
  localStorage.clear()
  notificationSubs.clear()
})

describe('App — base render', () => {
  it('renders without crashing', () => {
    render(<App />)
    // The shell chrome: header actions + the activity rail + canvas. Node
    // creation is gesture/palette-driven now (no toolbar buttons).
    expect(screen.getByRole('button', { name: /^new$/i })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: /heal with ai/i })).toBeInTheDocument()
    expect(screen.getByTestId('activity-rail')).toBeInTheDocument()
    expect(screen.getByTestId('canvas')).toBeInTheDocument()
    // No Save button — changes autosave.
    expect(screen.queryByRole('button', { name: /^save$/i })).not.toBeInTheDocument()
  })

  it('renders the canvas area', () => {
    render(<App />)
    expect(screen.getByTestId('canvas')).toBeInTheDocument()
  })

  it('has no node-creation toolbar (creation is gesture/palette-driven)', () => {
    render(<App />)
    // The old "+ Event" etc. toolbar buttons are gone.
    expect(screen.queryByRole('button', { name: /\+ event/i })).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: /\+ command/i })).not.toBeInTheDocument()
  })

  it('shows file/model actions in the header', () => {
    render(<App />)
    expect(screen.getByRole('button', { name: /^new$/i })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: /open/i })).toBeInTheDocument()
    expect(screen.getByRole('button', { name: /tidy by flow/i })).toBeInTheDocument()
    expect(screen.queryByRole('button', { name: /^save$/i })).not.toBeInTheDocument()
  })

  it('autosaves to disk after a model change (no Save button)', async () => {
    const user = userEvent.setup()
    render(<App />)
    // Any model mutation triggers the debounced autosave; use the feature
    // navigator's "+ Feature" (stable, in the model lens) as the trigger.
    await user.click(screen.getByTestId('add-feature'))
    await waitFor(() => expect(ipcState.callLog).toContain('write'), { timeout: 3000 })
  })
})

describe('App — event-model load + heal flow', () => {
  it('does not show modal when backend reports notFound', async () => {
    configureIpc({
      readQueue: [{ ok: true, result: { content: null, validation: { status: 'notFound' } } }],
    })
    render(<App />)
    // Give the mount effect a tick to complete.
    await waitFor(() => expect(ipcState.readCalls).toBe(1))
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument()
  })

  it('does not show modal when backend reports valid', async () => {
    configureIpc({
      readQueue: [
        { ok: true, result: { content: VALID_MODEL_JSON, validation: { status: 'valid' } } },
      ],
    })
    render(<App />)
    await waitFor(() => expect(ipcState.readCalls).toBe(1))
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument()
  })

  it('shows the invalid-model modal when backend reports invalid', async () => {
    configureIpc({
      readQueue: [
        {
          ok: true,
          result: {
            content: '{"missing": "id"}',
            validation: {
              status: 'invalid',
              errors: [{ pointer: '', message: 'missing required `id`', kind: 'schema' }],
            },
          },
        },
      ],
    })
    render(<App />)
    expect(await screen.findByRole('dialog')).toBeInTheDocument()
    expect(screen.getByText(/missing required `id`/)).toBeInTheDocument()
  })

  it('shows the malformed-JSON modal with a helpful preamble', async () => {
    configureIpc({
      readQueue: [
        {
          ok: true,
          result: {
            content: '{not json',
            validation: { status: 'malformedJson', parseError: 'expected `}` at line 1 column 9' },
          },
        },
      ],
    })
    render(<App />)
    expect(await screen.findByRole('dialog')).toBeInTheDocument()
    expect(
      screen.getByText(/event-model\.json on disk is not valid JSON/),
    ).toBeInTheDocument()
  })

  it('Cancel hides the modal without further RPC traffic', async () => {
    const user = userEvent.setup()
    configureIpc({
      readQueue: [
        {
          ok: true,
          result: {
            content: '{}',
            validation: {
              status: 'invalid',
              errors: [{ pointer: '', message: 'oops', kind: 'schema' }],
            },
          },
        },
      ],
    })
    render(<App />)
    await user.click(await screen.findByRole('button', { name: /cancel/i }))
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument()
    expect(ipcState.healCalls).toBe(0)
  })

  it('Heal happy path: heal → reload → modal closes', async () => {
    const user = userEvent.setup()
    configureIpc({
      readQueue: [
        {
          ok: true,
          result: {
            content: '{}',
            validation: {
              status: 'invalid',
              errors: [{ pointer: '', message: 'oops', kind: 'schema' }],
            },
          },
        },
        // Second call (post-heal reload) — file is now valid.
        { ok: true, result: { content: VALID_MODEL_JSON, validation: { status: 'valid' } } },
      ],
      healQueue: [{ ok: true, result: { outcome: { status: 'healed' } } }],
    })
    render(<App />)
    const modal = await screen.findByRole('dialog')
    await user.click(within(modal).getByRole('button', { name: /heal with ai/i }))
    await waitFor(() => expect(ipcState.healCalls).toBe(1))
    await waitFor(() => expect(ipcState.readCalls).toBe(2))
    await waitFor(() =>
      expect(screen.queryByRole('dialog')).not.toBeInTheDocument(),
    )
    expect(screen.queryByRole('status')).not.toBeInTheDocument()
  })

  it('Heal still-invalid: modal reappears with new errors', async () => {
    const user = userEvent.setup()
    configureIpc({
      readQueue: [
        {
          ok: true,
          result: {
            content: '{}',
            validation: {
              status: 'invalid',
              errors: [{ pointer: '', message: 'first error', kind: 'schema' }],
            },
          },
        },
      ],
      healQueue: [
        {
          ok: true,
          result: {
            outcome: {
              status: 'stillInvalid',
              errors: [{ pointer: '/foo', message: 'second error after heal', kind: 'schema' }],
            },
          },
        },
      ],
    })
    render(<App />)
    const modal = await screen.findByRole('dialog')
    await user.click(within(modal).getByRole('button', { name: /heal with ai/i }))
    await waitFor(() =>
      expect(screen.getByText(/second error after heal/)).toBeInTheDocument(),
    )
    expect(screen.getByText(/still invalid/i)).toBeInTheDocument()
  })

  it('Heal RPC failure shows a toast and closes the modal', async () => {
    const user = userEvent.setup()
    configureIpc({
      readQueue: [
        {
          ok: true,
          result: {
            content: '{}',
            validation: {
              status: 'invalid',
              errors: [{ pointer: '', message: 'oops', kind: 'schema' }],
            },
          },
        },
      ],
      healQueue: [{ ok: false, error: { code: -32000, message: 'claude not on PATH' } }],
    })
    render(<App />)
    const modal = await screen.findByRole('dialog')
    await user.click(within(modal).getByRole('button', { name: /heal with ai/i }))
    await waitFor(() =>
      expect(screen.getByText(/Healing failed: claude not on PATH/)).toBeInTheDocument(),
    )
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument()
  })

  it('Heal shows the spinner overlay while the RPC is in flight', async () => {
    const user = userEvent.setup()
    configureIpc({
      readQueue: [
        {
          ok: true,
          result: {
            content: '{}',
            validation: {
              status: 'invalid',
              errors: [{ pointer: '', message: 'oops', kind: 'schema' }],
            },
          },
        },
        { ok: true, result: { content: VALID_MODEL_JSON, validation: { status: 'valid' } } },
      ],
      healQueue: [{ ok: true, result: { outcome: { status: 'healed' } } }],
      healDelayMs: 50,
    })
    render(<App />)
    const modal = await screen.findByRole('dialog')
    await user.click(within(modal).getByRole('button', { name: /heal with ai/i }))
    // Overlay should appear while heal is pending.
    expect(await screen.findByRole('status')).toBeInTheDocument()
    expect(screen.getByText(/healing event model/i)).toBeInTheDocument()
    // And go away after heal + reload.
    await waitFor(() => expect(screen.queryByRole('status')).not.toBeInTheDocument())
  })

  it('streams $/progress log lines into the HealingOverlay during heal', async () => {
    const user = userEvent.setup()
    configureIpc({
      readQueue: [
        {
          ok: true,
          result: {
            content: '{}',
            validation: {
              status: 'invalid',
              errors: [{ pointer: '', message: 'oops', kind: 'schema' }],
            },
          },
        },
        { ok: true, result: { content: VALID_MODEL_JSON, validation: { status: 'valid' } } },
      ],
      healQueue: [{ ok: true, result: { outcome: { status: 'healed' } } }],
      healDelayMs: 200,
    })
    render(<App />)
    const modal = await screen.findByRole('dialog')
    await user.click(within(modal).getByRole('button', { name: /heal with ai/i }))

    // Overlay appears.
    expect(await screen.findByRole('status')).toBeInTheDocument()

    // Server pushes two progress notifications while the heal is in flight.
    emitNotification('$/progress', {
      token: 'healEventModel',
      value: { kind: 'log', stream: 'stdout', line: 'STREAMED_STDOUT_LINE' },
    })
    emitNotification('$/progress', {
      token: 'healEventModel',
      value: { kind: 'log', stream: 'stderr', line: 'STREAMED_STDERR_LINE' },
    })

    // Both lines must surface in the overlay's log scroller.
    await waitFor(() =>
      expect(screen.getByTestId('heal-log').textContent).toContain('STREAMED_STDOUT_LINE'),
    )
    expect(screen.getByTestId('heal-log').textContent).toContain('STREAMED_STDERR_LINE')

    // Notifications with a different token (or non-log kind) must NOT pollute
    // the log scroller — assert by sending an unrelated progress event and
    // confirming the next assertion still finds only the two lines above.
    emitNotification('$/progress', {
      token: 'unrelated',
      value: { kind: 'log', stream: 'stdout', line: 'WRONG_TOKEN_LINE' },
    })
    emitNotification('$/progress', {
      token: 'healEventModel',
      value: { kind: 'begin', title: 'noise' },
    })
    expect(screen.getByTestId('heal-log').textContent).not.toContain('WRONG_TOKEN_LINE')
    expect(screen.getByTestId('heal-log').textContent).not.toContain('noise')

    // Let the in-flight heal (healDelayMs) finish + reload so its async tail
    // doesn't leak into the next test's reset IPC queue.
    await waitFor(() => expect(screen.queryByRole('status')).not.toBeInTheDocument())
  })

  it('manual Heal button (FileMenu) triggers heal even when the file is valid', async () => {
    const user = userEvent.setup()
    configureIpc({
      readQueue: [
        // Mount load: file is already valid.
        { ok: true, result: { content: VALID_MODEL_JSON, validation: { status: 'valid' } } },
        // Post-heal reload.
        { ok: true, result: { content: VALID_MODEL_JSON, validation: { status: 'valid' } } },
      ],
      healQueue: [{ ok: true, result: { outcome: { status: 'healed' } } }],
    })
    render(<App />)
    // Wait for mount to settle.
    await waitFor(() => expect(ipcState.readCalls).toBe(1))
    expect(screen.queryByRole('dialog')).not.toBeInTheDocument()
    // Click the FileMenu's Heal button — distinct from the modal one because
    // the modal isn't open here.
    await user.click(screen.getByRole('button', { name: /heal with ai/i }))
    await waitFor(() => expect(ipcState.healCalls).toBe(1))
    // Reload fired after heal.
    await waitFor(() => expect(ipcState.readCalls).toBe(2))
  })
})

describe('App — chapter reorder', () => {
  const MODEL_WITH_CHAPTERS = JSON.stringify({
    id: 'm1',
    name: 'Demo',
    chapters: [
      { id: 'c1', name: 'First', order: 0 },
      { id: 'c2', name: 'Second', order: 1 },
    ],
    entities: [],
    slices: [],
    nodes: [],
    edges: [],
    layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
  })

  function dataTransfer(): DataTransfer {
    const store: Record<string, string> = {}
    return {
      effectAllowed: 'none',
      dropEffect: 'none',
      setData: (t: string, v: string) => {
        store[t] = v
      },
      getData: (t: string) => store[t] ?? '',
    } as unknown as DataTransfer
  }

  it('drag-reorder runs write → relayout → read (reload) in that order', async () => {
    configureIpc({
      readQueue: [
        // mount load
        { ok: true, result: { content: MODEL_WITH_CHAPTERS, validation: { status: 'valid' } } },
        // post-reorder relayout reload
        { ok: true, result: { content: MODEL_WITH_CHAPTERS, validation: { status: 'valid' } } },
      ],
    })
    render(<App />)
    const c1 = await screen.findByTestId('chapter-row-c1')
    const c2 = screen.getByTestId('chapter-row-c2')

    const dt = dataTransfer()
    fireEvent.dragStart(c1, { dataTransfer: dt })
    fireEvent.drop(c2, { dataTransfer: dt })

    await waitFor(() => expect(ipcState.relayoutCalls).toBe(1))
    await waitFor(() => expect(ipcState.readCalls).toBe(2))

    // The three reorder RPCs must fire in order: write the new chapter.order,
    // relayout (which now honours it), then reload the relaid-out file.
    const writeIdx = ipcState.callLog.indexOf('write')
    const relayoutIdx = ipcState.callLog.indexOf('relayout')
    const reloadIdx = ipcState.callLog.lastIndexOf('read')
    expect(writeIdx).toBeGreaterThanOrEqual(0)
    expect(relayoutIdx).toBeGreaterThan(writeIdx)
    expect(reloadIdx).toBeGreaterThan(relayoutIdx)
  })
})

describe('App — submodel band reflow on load', () => {
  // A submodel model whose saved node positions are NOT band-aligned. Both
  // nodes are in the submodel, so autoLayout leaves them untouched; the only
  // thing that can move them is the band reflow in applyReadResult.
  const MODEL_WITH_SUBMODEL = JSON.stringify({
    id: 'm',
    name: 'Demo',
    entities: [{ id: 'e1', name: 'User', order: 0 }],
    submodels: [{ id: 'sm1', name: 'Onboarding', order: 0 }],
    chapters: [{ id: 'ch1', name: 'Onb', order: 0, submodelId: 'sm1' }],
    slices: [{ id: 's1', name: 'Signup', chapterId: 'ch1', order: 0 }],
    nodes: [
      { id: 'c1', type: 'command', name: 'Signup', entityId: 'e1', sliceId: 's1' },
      { id: 'ev1', type: 'event', name: 'SignedUp', entityId: 'e1', sliceId: 's1' },
    ],
    edges: [],
    layout: {
      nodePositions: { c1: { x: 999, y: 999 }, ev1: { x: 999, y: 1200 } },
      viewport: { x: 0, y: 0, zoom: 1 },
    },
  })

  it('reload_with_submodels_applies_band_reflow', async () => {
    configureIpc({
      readQueue: [
        { ok: true, result: { content: MODEL_WITH_SUBMODEL, validation: { status: 'valid' } } },
      ],
    })
    render(<App />)
    await waitFor(() => expect(ipcState.readCalls).toBe(1))
    // applyReadResult re-bands the model on load; the band positions differ
    // from disk, so the model is dirty → autosave writes the reflowed model
    // back (the observable proof the reflow ran).
    await waitFor(() => expect(ipcState.callLog).toContain('write'), { timeout: 3000 })
  })
})

describe('App — $/eventModelChanged push reload', () => {
  // A model carrying a distinctly-named chapter so we can observe the store
  // replace in the DOM (chapter rows render with data-testid chapter-row-<id>).
  const MODEL_AFTER_SYNC = JSON.stringify({
    id: 'm1',
    name: 'Demo',
    chapters: [{ id: 'synced1', name: 'FromSourceSync', order: 0 }],
    entities: [],
    slices: [],
    nodes: [],
    edges: [],
    layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
  })

  it('ws_event_model_changed_triggers_reread', async () => {
    configureIpc({
      readQueue: [
        // Mount load: nothing on disk yet.
        { ok: true, result: { content: null, validation: { status: 'notFound' } } },
        // The re-read the notification handler must perform.
        { ok: true, result: { content: MODEL_AFTER_SYNC, validation: { status: 'valid' } } },
      ],
    })
    render(<App />)
    // Mount read settles; the synced chapter is NOT present yet.
    await waitFor(() => expect(ipcState.readCalls).toBe(1))
    expect(screen.queryByTestId('chapter-row-synced1')).not.toBeInTheDocument()

    // The Rust background sync rewrote event-model.json and pushed the
    // notification (no params needed — the handler just re-reads from disk).
    emitNotification('$/eventModelChanged', {})

    // Handler re-reads (readCalls → 2) and replaces the in-memory model, so the
    // newly-synced chapter row appears.
    await waitFor(() => expect(ipcState.readCalls).toBe(2))
    expect(await screen.findByTestId('chapter-row-synced1')).toBeInTheDocument()
  })
})
