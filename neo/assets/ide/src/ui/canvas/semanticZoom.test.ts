import { describe, it, expect } from 'vitest'
import {
  shouldShowFields,
  nodeDetailLevel,
  SEMANTIC_ZOOM_THRESHOLD,
  COLLAPSE_THRESHOLD,
} from './semanticZoom'

describe('semantic zoom', () => {
  it('semantic_zoom_reveals_fields_above_threshold', () => {
    expect(shouldShowFields(SEMANTIC_ZOOM_THRESHOLD)).toBe(true)
    expect(shouldShowFields(SEMANTIC_ZOOM_THRESHOLD + 0.5)).toBe(true)
    expect(shouldShowFields(SEMANTIC_ZOOM_THRESHOLD - 0.01)).toBe(false)
    expect(shouldShowFields(1)).toBe(false)
  })

  it('header_level_below_collapse_threshold', () => {
    expect(nodeDetailLevel(COLLAPSE_THRESHOLD - 0.01)).toBe('header')
    expect(nodeDetailLevel(0.1)).toBe('header')
  })

  it('card_level_in_mid_band', () => {
    expect(nodeDetailLevel(COLLAPSE_THRESHOLD)).toBe('card')
    expect(nodeDetailLevel(1)).toBe('card')
    expect(nodeDetailLevel(SEMANTIC_ZOOM_THRESHOLD - 0.01)).toBe('card')
  })

  it('edit_level_at_or_above_edit_threshold', () => {
    expect(nodeDetailLevel(SEMANTIC_ZOOM_THRESHOLD)).toBe('edit')
    expect(nodeDetailLevel(2)).toBe('edit')
  })

  it('boundaries_inclusive — collapse is exclusive low, edit is inclusive', () => {
    // exactly at COLLAPSE_THRESHOLD → already a full card (not header).
    expect(nodeDetailLevel(COLLAPSE_THRESHOLD)).toBe('card')
    // exactly at SEMANTIC_ZOOM_THRESHOLD → edit.
    expect(nodeDetailLevel(SEMANTIC_ZOOM_THRESHOLD)).toBe('edit')
  })
})
