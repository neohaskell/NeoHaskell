import { describe, it, expect, vi, afterEach } from 'vitest'
import { renderHook, act } from '@testing-library/react'
import { useAutosave, type UseAutosaveArgs } from './useAutosave'
import type { EventModel } from '../model/types'

function model(name = 'a'): EventModel {
  return {
    id: 'm',
    name,
    submodels: [],
    chapters: [],
    entities: [],
    slices: [],
    nodes: [],
    edges: [],
    layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
  }
}

function args(over: Partial<UseAutosaveArgs> = {}): UseAutosaveArgs {
  return {
    model: model('a'),
    dirty: true,
    connOpen: true,
    write: vi.fn().mockResolvedValue(true),
    onSaved: vi.fn(),
    ...over,
  }
}

afterEach(() => {
  vi.useRealTimers()
})

describe('useAutosave', () => {
  it('writes once after the debounce window when dirty', async () => {
    vi.useFakeTimers()
    const write = vi.fn().mockResolvedValue(true)
    const onSaved = vi.fn()
    renderHook((p: UseAutosaveArgs) => useAutosave(p), { initialProps: args({ write, onSaved }) })
    expect(write).not.toHaveBeenCalled()
    await act(async () => {
      await vi.advanceTimersByTimeAsync(800)
    })
    expect(write).toHaveBeenCalledTimes(1)
    expect(onSaved).toHaveBeenCalledTimes(1)
  })

  it('does not write when there are no unsaved changes', async () => {
    vi.useFakeTimers()
    const write = vi.fn().mockResolvedValue(true)
    renderHook((p: UseAutosaveArgs) => useAutosave(p), { initialProps: args({ dirty: false, write }) })
    await act(async () => {
      await vi.advanceTimersByTimeAsync(2000)
    })
    expect(write).not.toHaveBeenCalled()
  })

  it('coalesces rapid changes into a single write', async () => {
    vi.useFakeTimers()
    const write = vi.fn().mockResolvedValue(true)
    const { rerender } = renderHook((p: UseAutosaveArgs) => useAutosave(p), { initialProps: args({ write }) })
    await act(async () => {
      await vi.advanceTimersByTimeAsync(400)
    })
    rerender(args({ model: model('b'), write }))
    await act(async () => {
      await vi.advanceTimersByTimeAsync(400) // 400ms since last change — not yet
    })
    expect(write).not.toHaveBeenCalled()
    await act(async () => {
      await vi.advanceTimersByTimeAsync(400) // now 800ms idle
    })
    expect(write).toHaveBeenCalledTimes(1)
  })

  it('stays offline (no write) until reconnect, then flushes', async () => {
    vi.useFakeTimers()
    const write = vi.fn().mockResolvedValue(true)
    const { result, rerender } = renderHook((p: UseAutosaveArgs) => useAutosave(p), {
      initialProps: args({ connOpen: false, write }),
    })
    await act(async () => {
      await vi.advanceTimersByTimeAsync(1000)
    })
    expect(write).not.toHaveBeenCalled()
    expect(result.current.saveState.kind).toBe('offline')
    rerender(args({ connOpen: true, write }))
    await act(async () => {
      await vi.advanceTimersByTimeAsync(800)
    })
    expect(write).toHaveBeenCalledTimes(1)
  })

  it('marks failed and retries with backoff', async () => {
    vi.useFakeTimers()
    const write = vi.fn().mockResolvedValueOnce(false).mockResolvedValue(true)
    const { result } = renderHook((p: UseAutosaveArgs) => useAutosave(p), { initialProps: args({ write }) })
    await act(async () => {
      await vi.advanceTimersByTimeAsync(800)
    })
    expect(write).toHaveBeenCalledTimes(1)
    expect(result.current.saveState.kind).toBe('failed')
    await act(async () => {
      await vi.advanceTimersByTimeAsync(1000) // first backoff
    })
    expect(write).toHaveBeenCalledTimes(2)
  })

  it('flushNow writes immediately, cancelling the debounce', async () => {
    vi.useFakeTimers()
    const write = vi.fn().mockResolvedValue(true)
    const { result } = renderHook((p: UseAutosaveArgs) => useAutosave(p), { initialProps: args({ write }) })
    await act(async () => {
      await result.current.flushNow()
    })
    expect(write).toHaveBeenCalledTimes(1)
  })
})
