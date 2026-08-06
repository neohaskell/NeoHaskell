// Minimal JSON-RPC 2.0 client over a single WebSocket.
//
// Pairs with `src/ide/server.rs` on the Rust side. Wire format defined by the
// `MUST` invariants captured in /Users/nick/.claude/projects/-Users-nick-repos-neo/memory/project_ide_jsonrpc_architecture.md.
//
// Intentionally tiny: no reconnect, no cancellation. Notification handling is
// supported (see `onNotification`) for server-pushed messages such as
// `$/progress` (heal logs) and `$/eventModelChanged` (background-sync reload).

export interface RpcSuccess<T> {
  ok: true
  result: T
}

export interface RpcFailure {
  ok: false
  error: {
    code: number
    message: string
    data?: unknown
  }
}

export type RpcResult<T> = RpcSuccess<T> | RpcFailure

export type ConnectionState =
  | { status: 'connecting' }
  | { status: 'open' }
  | { status: 'closed'; reason: string }
  | { status: 'error'; message: string }

type PendingResolver = (envelope: unknown) => void

interface PendingEnvelope {
  id: number | string
  result?: unknown
  error?: { code: number; message: string; data?: unknown }
}

interface NotificationEnvelope {
  method: string
  params: unknown
}

export type NotificationListener = (params: unknown) => void

/** Derive the same-origin WS URL from the current document. */
export function defaultWsUrl(): string {
  const proto = window.location.protocol === 'https:' ? 'wss:' : 'ws:'
  return `${proto}//${window.location.host}/ws`
}

export class IdeClient {
  private ws: WebSocket
  private nextId = 1
  private pending = new Map<number | string, PendingResolver>()
  private opened: Promise<void>
  private state: ConnectionState = { status: 'connecting' }
  private listeners = new Set<(s: ConnectionState) => void>()
  private notificationListeners = new Map<string, Set<NotificationListener>>()

  constructor(url: string = defaultWsUrl()) {
    this.ws = new WebSocket(url)
    // `opened` always RESOLVES, never rejects — callers check `getState()`
    // (or the synthetic RpcFailure from `request()`) for the failure case.
    // This keeps the contract simple: no `try/catch` at every call site, no
    // unhandled rejections in tests where jsdom synthesises a failed WS.
    this.opened = new Promise((resolve) => {
      this.ws.addEventListener('open', () => {
        this.setState({ status: 'open' })
        resolve()
      })
      this.ws.addEventListener('error', () => {
        // The browser does not expose the underlying network error.
        // `close` follows with a useful code; treat `error` as a heads-up.
        this.setState({ status: 'error', message: 'WebSocket error' })
        resolve()
      })
    })
    this.ws.addEventListener('close', (ev) => {
      this.setState({
        status: 'closed',
        reason: `code=${ev.code}${ev.reason ? ` ${ev.reason}` : ''}`,
      })
      // Resolve any in-flight requests as a synthetic failure so callers don't hang.
      for (const [id, cb] of this.pending) {
        cb({
          id,
          error: { code: -32603, message: 'connection closed before response' },
        } satisfies PendingEnvelope)
      }
      this.pending.clear()
    })
    this.ws.addEventListener('message', (ev) => {
      let parsed: PendingEnvelope & Partial<NotificationEnvelope>
      try {
        parsed = JSON.parse(ev.data as string) as PendingEnvelope &
          Partial<NotificationEnvelope>
      } catch {
        return
      }
      if (parsed.id === null || parsed.id === undefined) {
        // Server-pushed notification — route to subscribers by method.
        if (typeof parsed.method !== 'string') return
        const subs = this.notificationListeners.get(parsed.method)
        if (!subs) return
        for (const sub of subs) sub(parsed.params)
        return
      }
      const cb = this.pending.get(parsed.id)
      if (cb) {
        this.pending.delete(parsed.id)
        cb(parsed)
      }
    })
  }

  /** Resolves once the WS is open. Rejects if the open handshake fails. */
  ready(): Promise<void> {
    return this.opened
  }

  /** Current connection state. */
  getState(): ConnectionState {
    return this.state
  }

  /** Subscribe to state changes. Returns an unsubscribe function. */
  onState(listener: (s: ConnectionState) => void): () => void {
    this.listeners.add(listener)
    // Push current state immediately for ergonomic React useEffect setup.
    listener(this.state)
    return () => this.listeners.delete(listener)
  }

  private setState(next: ConnectionState) {
    this.state = next
    for (const l of this.listeners) l(next)
  }

  /**
   * Send a typed request. Resolves with `{ ok: true, result }` on success or
   * `{ ok: false, error }` on a JSON-RPC error response. Never rejects — any
   * transport failure (WS never opened, closed before response) surfaces as
   * an `RpcFailure` with code -32603.
   */
  async request<P, R>(method: string, params: P): Promise<RpcResult<R>> {
    await this.opened
    if (this.state.status !== 'open') {
      return {
        ok: false,
        error: {
          code: -32603,
          message: `connection not open (state: ${this.state.status})`,
        },
      }
    }
    const id = this.nextId++
    return new Promise((resolve) => {
      this.pending.set(id, (envelope) => {
        const env = envelope as PendingEnvelope
        if (env.error) {
          resolve({ ok: false, error: env.error })
        } else {
          resolve({ ok: true, result: env.result as R })
        }
      })
      try {
        this.ws.send(JSON.stringify({ jsonrpc: '2.0', id, method, params }))
      } catch (e) {
        this.pending.delete(id)
        resolve({
          ok: false,
          error: {
            code: -32603,
            message: `send failed: ${e instanceof Error ? e.message : 'unknown'}`,
          },
        })
      }
    })
  }

  /**
   * Subscribe to server-pushed notifications under `method`. Returns an
   * unsubscribe function. Multiple subscribers per method are fanned out
   * in subscription order.
   */
  onNotification(method: string, listener: NotificationListener): () => void {
    let subs = this.notificationListeners.get(method)
    if (!subs) {
      subs = new Set()
      this.notificationListeners.set(method, subs)
    }
    subs.add(listener)
    return () => {
      subs!.delete(listener)
      if (subs!.size === 0) this.notificationListeners.delete(method)
    }
  }

  close(): void {
    this.ws.close()
  }
}
