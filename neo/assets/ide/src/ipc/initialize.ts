// Typed wrapper around the `initialize` JSON-RPC method.
//
// The shape mirrors `src/ide/methods/initialize.rs` on the Rust side. If a
// field is added there, add it here too — keeping the schema in two places
// is the v1 cost; the architecture memory flags `schemars`/automatic type
// generation as an explicit next slice.

import type { IdeClient, RpcResult } from './client'

export interface ClientInfo {
  name: string
  version: string
}

export interface ServerInfo {
  name: string
  version: string
}

export interface NeoProjectInfo {
  name: string
  version: string
  neoVersion: string
}

export interface WorkspaceInfo {
  id: string
  root: string
  project: NeoProjectInfo | null
}

// Open-shaped — `initialize` accepts and ignores it in v1.
export type ClientCapabilities = Record<string, unknown>

// Empty in v1; the field exists so future methods can announce.
export type ServerCapabilities = Record<string, unknown>

export interface InitializeParams {
  clientInfo: ClientInfo
  capabilities: ClientCapabilities
}

export interface InitializeResult {
  serverInfo: ServerInfo
  serverCapabilities: ServerCapabilities
  workspace: WorkspaceInfo
  sessionId: string
}

export function initialize(
  client: IdeClient,
  clientInfo: ClientInfo,
  capabilities: ClientCapabilities = {},
): Promise<RpcResult<InitializeResult>> {
  return client.request<InitializeParams, InitializeResult>('initialize', {
    clientInfo,
    capabilities,
  })
}
