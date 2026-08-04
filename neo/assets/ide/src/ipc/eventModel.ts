// Typed wrappers for the workspace/eventModel JSON-RPC methods. Mirrors
// `src/ide/methods/{read,write,heal}_event_model.rs`. The Rust backend is
// the authoritative validator: when `readEventModel` returns a non-`valid`
// status, the frontend should branch on it rather than re-parsing.

import type { IdeClient, RpcResult } from './client'

export type ValidationErrorKind = 'schema' | 'referentialIntegrity'

export interface ValidationError {
  /** JSON Pointer (RFC 6901) to the offending location. Empty = whole doc. */
  pointer: string
  /** Human-readable message — written for the dumbest LLM. */
  message: string
  kind: ValidationErrorKind
}

export type ValidationStatus =
  | { status: 'notFound' }
  | { status: 'valid' }
  | { status: 'invalid'; errors: ValidationError[] }
  | { status: 'malformedJson'; parseError: string }

export interface ReadEventModelResult {
  /** Raw file contents. `null` only when status is `notFound`. */
  content: string | null
  validation: ValidationStatus
}

export interface WriteEventModelResult {
  /** Absolute path the file landed at. Useful for "Saved to <path>" toasts. */
  path: string
}

export type HealOutcome =
  | { status: 'healed' }
  | { status: 'stillInvalid'; errors: ValidationError[] }
  | { status: 'cancelled'; deterministicApplied: number }

export interface HealEventModelResult {
  outcome: HealOutcome
}

export interface CancelHealEventModelResult {
  /** `true` when a heal was in flight and the cancel signal fired.
   *  `false` when nothing was running (no-op cancel — safe to ignore). */
  cancelled: boolean
}

export function readEventModel(
  client: IdeClient,
): Promise<RpcResult<ReadEventModelResult>> {
  return client.request<Record<string, never>, ReadEventModelResult>(
    'workspace/readEventModel',
    {},
  )
}

export function writeEventModel(
  client: IdeClient,
  content: string,
): Promise<RpcResult<WriteEventModelResult>> {
  return client.request<{ content: string }, WriteEventModelResult>(
    'workspace/writeEventModel',
    { content },
  )
}

/** How aggressively the server should invoke the agent. Matches the Rust
 *  enum `HealMode`. `validate` is the default for the auto-triggered modal
 *  (only spawns `claude` if validation fails). `improve` is for the manual
 *  "Heal with AI" button (always spawns `claude` to refine layout/edges). */
export type HealMode = 'validate' | 'improve'

export function healEventModel(
  client: IdeClient,
  mode: HealMode = 'validate',
): Promise<RpcResult<HealEventModelResult>> {
  return client.request<{ mode: HealMode }, HealEventModelResult>(
    'workspace/healEventModel',
    { mode },
  )
}

/** Abort an in-flight `healEventModel` request. The server kills the
 *  `claude` subprocess (if running) and persists whatever the
 *  deterministic pre-pass already patched. The in-flight `healEventModel`
 *  promise will resolve with `outcome.status === 'cancelled'`. */
export function cancelHealEventModel(
  client: IdeClient,
): Promise<RpcResult<CancelHealEventModelResult>> {
  return client.request<Record<string, never>, CancelHealEventModelResult>(
    'workspace/cancelHealEventModel',
    {},
  )
}

export interface RelayoutEventModelResult {
  /** Number of layout fixes applied. 0 = file's layout was already canonical. */
  applied: number
  /** Short human-readable summary of what changed. */
  summary: string
}

/** Run the deterministic layout pass ONLY — chapter grouping, y-band
 *  fixes, slice-column rebalance, missing-position fills. Does NOT add
 *  any nodes/edges/entities and never spawns the AI agent. Use this when
 *  the user wants to clean up the canvas layout without touching the
 *  model's structure. */
export function relayoutEventModel(
  client: IdeClient,
): Promise<RpcResult<RelayoutEventModelResult>> {
  return client.request<Record<string, never>, RelayoutEventModelResult>(
    'workspace/relayoutEventModel',
    {},
  )
}
