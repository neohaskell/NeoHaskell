import { useState, useRef, useEffect, useCallback } from 'react'
import type { EventModel } from '../model/types'
import { modelToJson } from './fileOps'

// Idle-debounced autosave. git is the rollback layer, so there is no manual
// Save button and no "unsaved changes" anxiety — every change writes itself.
// The localStorage mirror (in App) is the crash-survival buffer; this hook
// owns only the debounced JSON-RPC write to disk, with single-flight
// coalescing (latest-wins), backoff-on-failure, and graceful offline.
//
// Deliberately uses ONE idle debounce for all change classes (the panel
// debated 0ms-for-structural vs 800ms-for-text; the difference is
// imperceptible for persistence since localStorage is synchronous, and a
// single path is far more robust). A maxWait ceiling was deferred — drags
// flush on drop, so a >800ms continuous edit is rare.

export type SaveState =
  | { kind: 'saved' }
  | { kind: 'saving' }
  | { kind: 'offline' }
  | { kind: 'failed' }

const DEBOUNCE_MS = 800
const BACKOFF_MS = [1000, 2000, 4000, 8000, 15000]

export interface UseAutosaveArgs {
  model: EventModel
  /** True when there are changes not yet written to disk. */
  dirty: boolean
  /** True when the JSON-RPC WebSocket is open. */
  connOpen: boolean
  /** Persist the serialized model. Resolves `true` on success. */
  write: (content: string) => Promise<boolean>
  /** Called after a successful write of the current model (clears `dirty`). */
  onSaved: () => void
}

export interface UseAutosaveResult {
  saveState: SaveState
  /** Cancel the debounce and write immediately (Cmd/Ctrl+S, or before New/Open/Heal). */
  flushNow: () => void
}

export function useAutosave({ model, dirty, connOpen, write, onSaved }: UseAutosaveArgs): UseAutosaveResult {
  const [saveState, setSaveState] = useState<SaveState>({ kind: 'saved' })

  const modelRef = useRef(model)
  modelRef.current = model
  const dirtyRef = useRef(dirty)
  dirtyRef.current = dirty
  const connRef = useRef(connOpen)
  connRef.current = connOpen
  const writeRef = useRef(write)
  writeRef.current = write
  const onSavedRef = useRef(onSaved)
  onSavedRef.current = onSaved

  const debounceTimer = useRef<ReturnType<typeof setTimeout> | null>(null)
  const retryTimer = useRef<ReturnType<typeof setTimeout> | null>(null)
  const writingRef = useRef(false)
  const retryIdxRef = useRef(0)

  const clearTimers = () => {
    if (debounceTimer.current) {
      clearTimeout(debounceTimer.current)
      debounceTimer.current = null
    }
    if (retryTimer.current) {
      clearTimeout(retryTimer.current)
      retryTimer.current = null
    }
  }

  const doWrite = useCallback(async () => {
    clearTimers()
    if (!dirtyRef.current) {
      setSaveState({ kind: 'saved' })
      return
    }
    if (!connRef.current) {
      setSaveState({ kind: 'offline' })
      return
    }
    if (writingRef.current) return // single-flight; the in-flight write will re-check
    writingRef.current = true
    setSaveState({ kind: 'saving' })
    const snapshot = modelToJson(modelRef.current)
    let ok = false
    try {
      ok = await writeRef.current(snapshot)
    } catch {
      ok = false
    }
    writingRef.current = false
    if (ok) {
      retryIdxRef.current = 0
      if (modelToJson(modelRef.current) === snapshot) {
        onSavedRef.current() // clears dirty
        setSaveState({ kind: 'saved' })
      } else {
        // Newer edits landed during the write — persist them too.
        void doWrite()
      }
    } else {
      setSaveState({ kind: 'failed' })
      const ms = BACKOFF_MS[Math.min(retryIdxRef.current, BACKOFF_MS.length - 1)]
      retryIdxRef.current += 1
      retryTimer.current = setTimeout(() => void doWrite(), ms)
    }
  }, [])

  const flushNow = useCallback(() => void doWrite(), [doWrite])

  // (Re)arm the idle debounce on every model change while dirty + connected.
  useEffect(() => {
    if (!dirty) {
      setSaveState({ kind: 'saved' })
      return
    }
    if (!connOpen) {
      setSaveState({ kind: 'offline' })
      return
    }
    if (debounceTimer.current) clearTimeout(debounceTimer.current)
    debounceTimer.current = setTimeout(() => void doWrite(), DEBOUNCE_MS)
    return () => {
      if (debounceTimer.current) {
        clearTimeout(debounceTimer.current)
        debounceTimer.current = null
      }
    }
  }, [model, dirty, connOpen, doWrite])

  // Warn on exit ONLY in the genuine-danger case (a write is failing or queued
  // offline) — never on a clean exit.
  useEffect(() => {
    const handler = (e: BeforeUnloadEvent) => {
      if (saveState.kind === 'failed' || (dirty && saveState.kind === 'offline')) {
        e.preventDefault()
        e.returnValue = ''
      }
    }
    window.addEventListener('beforeunload', handler)
    return () => window.removeEventListener('beforeunload', handler)
  }, [saveState, dirty])

  return { saveState, flushNow }
}
