import { describe, it, expect, vi } from 'vitest'
import { render, screen, fireEvent } from '../test/render'
import { FeatureNavigator } from './FeatureNavigator'
import { UNGROUPED_FEATURE } from './featurePages'
import type { Chapter, Slice, Submodel } from '../model/types'

const submodels: Submodel[] = [{ id: 'smA', name: 'Checkout', order: 0 }]
const chapters: Chapter[] = [
  { id: 'c1', name: 'One', order: 0, submodelId: 'smA' },
  { id: 'c2', name: 'Two', order: 1, submodelId: 'smA' },
  { id: 'cU', name: 'Free', order: 2, submodelId: null },
]
const slices: Slice[] = [
  { id: 's1', name: 'Slice 1', chapterId: 'c1', order: 0 },
  { id: 's2', name: 'Slice 2', chapterId: 'c1', order: 1 },
  { id: 's3', name: 'Slice 3', chapterId: 'c2', order: 2 },
]

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

function setup(overrides: Partial<React.ComponentProps<typeof FeatureNavigator>> = {}) {
  const props = {
    chapters,
    slices,
    submodels,
    activeFeatureId: 'smA',
    hasUngrouped: true,
    busy: false,
    onSelectFeature: vi.fn(),
    onReorder: vi.fn(),
    onMoveSlice: vi.fn(),
    onCreateFeatureFromChapters: vi.fn(),
    onAddFeature: vi.fn(),
    onRenameFeature: vi.fn(),
    onDeleteFeature: vi.fn(),
    onAddChapter: vi.fn(),
    onDeleteChapter: vi.fn(),
    onRenameChapter: vi.fn(),
    ...overrides,
  }
  render(<FeatureNavigator {...props} />)
  return props
}

describe('FeatureNavigator', () => {
  it('renders a row per submodel plus the Ungrouped pseudo-feature', () => {
    setup()
    expect(screen.getByTestId('feature-row-smA')).toBeInTheDocument()
    expect(screen.getByTestId(`feature-row-${UNGROUPED_FEATURE}`)).toBeInTheDocument()
  })

  it('expands only the active feature, listing its chapters', () => {
    setup({ activeFeatureId: 'smA' })
    expect(screen.getByTestId('chapter-row-c1')).toBeInTheDocument()
    expect(screen.getByTestId('chapter-row-c2')).toBeInTheDocument()
    // The Ungrouped chapter belongs to a collapsed feature → not shown.
    expect(screen.queryByTestId('chapter-row-cU')).not.toBeInTheDocument()
  })

  it('clicking a feature row switches to it', () => {
    const props = setup()
    fireEvent.click(screen.getByTestId(`feature-row-${UNGROUPED_FEATURE}`))
    expect(props.onSelectFeature).toHaveBeenCalledWith(UNGROUPED_FEATURE)
  })

  it('selecting chapters reveals New Feature, which creates from the selection', () => {
    const props = setup({ activeFeatureId: 'smA' })
    // No action bar until something is selected.
    expect(screen.queryByTestId('new-feature-from-selection')).not.toBeInTheDocument()
    fireEvent.click(screen.getByTestId('chapter-row-c1'))
    fireEvent.click(screen.getByTestId('chapter-row-c2'))
    fireEvent.click(screen.getByTestId('new-feature-from-selection'))
    expect(props.onCreateFeatureFromChapters).toHaveBeenCalledWith(['c1', 'c2'])
  })

  it('toggling a chapter off removes it from the selection', () => {
    const props = setup({ activeFeatureId: 'smA' })
    fireEvent.click(screen.getByTestId('chapter-row-c1'))
    fireEvent.click(screen.getByTestId('chapter-row-c1')) // toggle off
    expect(screen.queryByTestId('new-feature-from-selection')).not.toBeInTheDocument()
    expect(props.onCreateFeatureFromChapters).not.toHaveBeenCalled()
  })

  it('Add Feature button creates a feature', () => {
    const props = setup()
    fireEvent.click(screen.getByTestId('add-feature'))
    expect(props.onAddFeature).toHaveBeenCalledOnce()
  })

  it('a real feature can be deleted; Ungrouped cannot', () => {
    const props = setup()
    expect(screen.queryByTestId(`delete-feature-${UNGROUPED_FEATURE}`)).not.toBeInTheDocument()
    fireEvent.click(screen.getByTestId('delete-feature-smA'))
    expect(props.onDeleteFeature).toHaveBeenCalledWith('smA')
    // Deleting must not also fire a feature switch (stopPropagation).
    expect(props.onSelectFeature).not.toHaveBeenCalled()
  })

  it('drag-reorders chapters within the active feature (global order)', () => {
    const props = setup({ activeFeatureId: 'smA' })
    const dt = dataTransfer()
    fireEvent.dragStart(screen.getByTestId('chapter-row-c1'), { dataTransfer: dt })
    fireEvent.dragOver(screen.getByTestId('chapter-row-c2'), { dataTransfer: dt })
    fireEvent.drop(screen.getByTestId('chapter-row-c2'), { dataTransfer: dt })
    // Dragging One onto Two → [c2, c1, cU] (cU keeps its global tail position).
    expect(props.onReorder).toHaveBeenCalledWith(['c2', 'c1', 'cU'])
  })

  it('lists slices under their chapter and reorders within a chapter', () => {
    const props = setup({ activeFeatureId: 'smA' })
    expect(screen.getByTestId('slice-row-s1')).toBeInTheDocument()
    expect(screen.getByTestId('slice-row-s2')).toBeInTheDocument()
    const dt = dataTransfer()
    fireEvent.dragStart(screen.getByTestId('slice-row-s1'), { dataTransfer: dt })
    fireEvent.dragOver(screen.getByTestId('slice-row-s2'), { dataTransfer: dt })
    fireEvent.drop(screen.getByTestId('slice-row-s2'), { dataTransfer: dt })
    // s1 dropped onto s2 (same chapter c1) → reordered within c1.
    expect(props.onMoveSlice).toHaveBeenCalledWith('s1', 'c1', ['s2', 's1', 's3'])
  })

  it('moves a slice to another chapter by dropping onto that chapter’s slice', () => {
    const props = setup({ activeFeatureId: 'smA' })
    const dt = dataTransfer()
    fireEvent.dragStart(screen.getByTestId('slice-row-s1'), { dataTransfer: dt })
    fireEvent.dragOver(screen.getByTestId('slice-row-s3'), { dataTransfer: dt })
    fireEvent.drop(screen.getByTestId('slice-row-s3'), { dataTransfer: dt })
    // s1 → c2 (s3's chapter), taking s3's slot.
    expect(props.onMoveSlice).toHaveBeenCalledWith('s1', 'c2', ['s2', 's3', 's1'])
  })

  it('moves a slice into a chapter by dropping onto the chapter row', () => {
    const props = setup({ activeFeatureId: 'smA' })
    const dt = dataTransfer()
    fireEvent.dragStart(screen.getByTestId('slice-row-s1'), { dataTransfer: dt })
    fireEvent.drop(screen.getByTestId('chapter-row-c2'), { dataTransfer: dt })
    // s1 appended into c2 after its existing slice s3.
    expect(props.onMoveSlice).toHaveBeenCalledWith('s1', 'c2', ['s2', 's3', 's1'])
  })

  it('does not drag while busy', () => {
    const props = setup({ busy: true })
    const row = screen.getByTestId('chapter-row-c1')
    expect(row).toHaveAttribute('draggable', 'false')
    const dt = dataTransfer()
    fireEvent.dragStart(row, { dataTransfer: dt })
    fireEvent.drop(screen.getByTestId('chapter-row-c2'), { dataTransfer: dt })
    expect(props.onReorder).not.toHaveBeenCalled()
  })

  it('shows an empty state when there are no features', () => {
    setup({ submodels: [], chapters: [], hasUngrouped: false })
    expect(screen.getByText(/No features yet/i)).toBeInTheDocument()
  })

  it('adds a chapter to the active feature (submodel id passed through)', () => {
    const props = setup({ activeFeatureId: 'smA' })
    fireEvent.click(screen.getByTestId('add-chapter-smA'))
    expect(props.onAddChapter).toHaveBeenCalledWith('smA')
  })

  it('adds a chapter to the Ungrouped feature with a null submodel id', () => {
    const props = setup({ activeFeatureId: UNGROUPED_FEATURE })
    fireEvent.click(screen.getByTestId(`add-chapter-${UNGROUPED_FEATURE}`))
    expect(props.onAddChapter).toHaveBeenCalledWith(null)
  })

  it('deletes a chapter without also selecting it (stopPropagation)', () => {
    const props = setup({ activeFeatureId: 'smA' })
    fireEvent.click(screen.getByTestId('delete-chapter-c1'))
    expect(props.onDeleteChapter).toHaveBeenCalledWith('c1')
    // Must not toggle selection, so the New Feature action bar stays hidden.
    expect(screen.queryByTestId('new-feature-from-selection')).not.toBeInTheDocument()
  })

  it('does not delete a chapter while busy', () => {
    const props = setup({ busy: true })
    expect(screen.getByTestId('delete-chapter-c1')).toBeDisabled()
    fireEvent.click(screen.getByTestId('delete-chapter-c1'))
    expect(props.onDeleteChapter).not.toHaveBeenCalled()
  })
})
