import { describe, it, expect } from 'vitest'
import { isConnectionValid } from './connectionRules'
import type { NodeType } from '../model/types'

const allTypes: NodeType[] = ['event', 'command', 'query', 'integration', 'uiPlaceholder']

const validPairs: [NodeType, NodeType][] = [
  ['uiPlaceholder', 'command'],
  ['command', 'event'],
  ['event', 'query'],
  ['event', 'integration'],
  ['integration', 'command'],
  ['query', 'uiPlaceholder'],
]

describe('isConnectionValid', () => {
  for (const [source, target] of validPairs) {
    it(`allows ${source} → ${target}`, () => {
      expect(isConnectionValid(source, target)).toBe(true)
    })
  }

  it('rejects all invalid combinations', () => {
    let invalidCount = 0
    for (const source of allTypes) {
      for (const target of allTypes) {
        const isValid = validPairs.some(
          ([s, t]) => s === source && t === target,
        )
        if (!isValid) {
          expect(
            isConnectionValid(source, target),
            `${source} → ${target} should be invalid`,
          ).toBe(false)
          invalidCount++
        }
      }
    }
    // 25 total - 6 valid = 19 invalid
    expect(invalidCount).toBe(19)
  })
})
