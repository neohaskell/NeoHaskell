import { describe, it, expect } from 'vitest'
import {
  estimateNodeDimensions,
  visibleRowCount,
  MIN_NODE_WIDTH,
  MAX_NODE_WIDTH,
  HEADER_H,
  ROW_H,
  FIELD_CAP,
} from './nodeDimensions'

const field = (name: string, type = 'String') => ({ name, type })

describe('estimateNodeDimensions', () => {
  it('clamps width to MIN_NODE_WIDTH for short labels', () => {
    const { width } = estimateNodeDimensions('A')
    expect(width).toBe(MIN_NODE_WIDTH)
  })

  it('clamps width to MAX_NODE_WIDTH for very long labels', () => {
    const { width } = estimateNodeDimensions('x'.repeat(200))
    expect(width).toBe(MAX_NODE_WIDTH)
  })

  it('scales width with label length up to the cap', () => {
    const short = estimateNodeDimensions('Pay')
    const medium = estimateNodeDimensions('PayBankRoute')
    const long = estimateNodeDimensions('PaymentFormPreparation')
    expect(short.width).toBeLessThanOrEqual(medium.width)
    expect(medium.width).toBeLessThanOrEqual(long.width)
  })

  it('width_fits_widest_field_row — a wide field row widens the card beyond the label', () => {
    const narrow = estimateNodeDimensions('E', [field('a', 'B')])
    const wide = estimateNodeDimensions('E', [
      field('aVeryLongFieldName', 'AnEquallyLongTypeName'),
    ])
    expect(wide.width).toBeGreaterThan(narrow.width)
  })

  it('height_grows_with_field_count', () => {
    const zero = estimateNodeDimensions('E', [])
    const one = estimateNodeDimensions('E', [field('a')])
    const three = estimateNodeDimensions('E', [field('a'), field('b'), field('c')])
    expect(one.height).toBeGreaterThan(zero.height - 1) // 0-field row ≈ 1 row
    expect(three.height).toBe(one.height + 2 * ROW_H)
    expect(three.height).toBe(2 + HEADER_H + 12 + 3 * ROW_H)
  })

  it('zero_fields_reports_header_plus_one_row', () => {
    const { lines, height } = estimateNodeDimensions('E', [])
    expect(lines).toBe(1)
    expect(height).toBe(2 + HEADER_H + 12 + ROW_H)
  })

  it('caps_visible_rows_at_field_cap', () => {
    const many = Array.from({ length: FIELD_CAP + 4 }, (_, i) => field(`f${i}`))
    const { lines } = estimateNodeDimensions('E', many)
    expect(lines).toBe(FIELD_CAP + 1) // capped rows + one "+N more" row
    expect(visibleRowCount(FIELD_CAP + 4)).toBe(FIELD_CAP + 1)
    expect(visibleRowCount(FIELD_CAP)).toBe(FIELD_CAP)
    expect(visibleRowCount(0)).toBe(1)
  })

  it('is deterministic — same input produces same output', () => {
    const fields = [field('a'), field('b')]
    const a = estimateNodeDimensions('AnyLabel', fields)
    const b = estimateNodeDimensions('AnyLabel', fields)
    expect(a).toEqual(b)
  })
})
