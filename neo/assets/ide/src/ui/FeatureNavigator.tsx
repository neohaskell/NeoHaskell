import { useState } from 'react'
import { Text, ActionIcon, Button } from '@mantine/core'
import { IconGripVertical, IconX, IconCheck, IconPlus } from '@tabler/icons-react'
import type { Chapter, Slice, Submodel } from '../model/types'
import { UNGROUPED_FEATURE } from './featurePages'
import { EditableLabel } from './nodes/EditableLabel'
import classes from './FeatureNavigator.module.css'

interface FeatureNavigatorProps {
  chapters: readonly Chapter[]
  slices: readonly Slice[]
  submodels: readonly Submodel[]
  /** Currently shown feature (submodel id or UNGROUPED_FEATURE). */
  activeFeatureId: string
  /** Whether any ungrouped content exists (drives the Ungrouped pseudo-feature). */
  hasUngrouped: boolean
  /** Disable interactions while a relayout/heal round-trip is in flight. */
  busy: boolean
  onSelectFeature: (featureId: string) => void
  /** Drop with the FULL chapter id list in its new order (global reorder). */
  onReorder: (orderedChapterIds: string[]) => void
  /** Move a slice into `chapterId` (or reorder within it) — full slice id list
   *  in its new order. Drop onto a slice = join its chapter; onto a chapter row
   *  = move into that chapter. */
  onMoveSlice: (sliceId: string, chapterId: string | null, orderedSliceIds: string[]) => void
  onCreateFeatureFromChapters: (chapterIds: string[]) => void
  onAddFeature: () => void
  onRenameFeature: (submodelId: string, name: string) => void
  onDeleteFeature: (submodelId: string) => void
  /** Create a new chapter inside `submodelId` (null = the Ungrouped feature). */
  onAddChapter: (submodelId: string | null) => void
  /** Delete a chapter. Its slices are detached (moved to Ungrouped), not removed. */
  onDeleteChapter: (chapterId: string) => void
  onRenameChapter: (chapterId: string, name: string) => void
}

const CHAPTER_MIME = 'application/x-neo-chapter'
const SLICE_MIME = 'application/x-neo-slice'

/**
 * Left-docked navigator for the "Features as pages" view. Each submodel is a
 * FEATURE (page); clicking one switches the whole canvas to it. The active
 * feature lists its chapters, each chapter lists its slices. Chapters and slices
 * drag-to-reorder (global order; never reassigning a feature). Selecting
 * chapters (click) + "New Feature" promotes them. A permanent, non-deletable
 * "Ungrouped" pseudo-feature collects chapters with no submodel.
 */
export function FeatureNavigator({
  chapters,
  slices,
  submodels,
  activeFeatureId,
  hasUngrouped,
  busy,
  onSelectFeature,
  onReorder,
  onMoveSlice,
  onCreateFeatureFromChapters,
  onAddFeature,
  onRenameFeature,
  onDeleteFeature,
  onAddChapter,
  onDeleteChapter,
  onRenameChapter,
}: FeatureNavigatorProps) {
  const [dragId, setDragId] = useState<string | null>(null)
  const [overId, setOverId] = useState<string | null>(null)
  const [dragSliceId, setDragSliceId] = useState<string | null>(null)
  const [overSliceId, setOverSliceId] = useState<string | null>(null)
  const [selected, setSelected] = useState<Set<string>>(new Set())

  const ordered = [...chapters].sort((a, b) => a.order - b.order)
  const orderedSlices = [...slices].sort((a, b) => a.order - b.order)
  const submodelIds = new Set(submodels.map((s) => s.id))

  function move(fromId: string, toId: string) {
    if (fromId === toId) return
    const ids = ordered.map((c) => c.id)
    const from = ids.indexOf(fromId)
    const to = ids.indexOf(toId)
    if (from === -1 || to === -1) return
    ids.splice(from, 1)
    ids.splice(to, 0, fromId)
    onReorder(ids)
  }

  function moveSlice(fromId: string, toId: string) {
    if (fromId === toId) return
    const target = orderedSlices.find((s) => s.id === toId)
    if (!target) return
    const ids = orderedSlices.map((s) => s.id)
    const fromIdx = ids.indexOf(fromId)
    const toIdx = ids.indexOf(toId)
    if (fromIdx === -1 || toIdx === -1) return
    ids.splice(fromIdx, 1)
    ids.splice(toIdx, 0, fromId)
    onMoveSlice(fromId, target.chapterId, ids)
  }

  function dropSliceOnChapter(sliceId: string, chapterId: string) {
    const ids = orderedSlices.map((s) => s.id).filter((id) => id !== sliceId)
    const chapterSliceIds = orderedSlices
      .filter((s) => s.chapterId === chapterId && s.id !== sliceId)
      .map((s) => s.id)
    const insertAt =
      chapterSliceIds.length > 0
        ? ids.indexOf(chapterSliceIds[chapterSliceIds.length - 1]) + 1
        : ids.length
    ids.splice(insertAt, 0, sliceId)
    onMoveSlice(sliceId, chapterId, ids)
  }

  function toggleSelect(id: string) {
    setSelected((prev) => {
      const next = new Set(prev)
      if (next.has(id)) next.delete(id)
      else next.add(id)
      return next
    })
  }

  const features: { id: string; name: string; deletable: boolean }[] = [
    ...[...submodels]
      .sort((a, b) => a.order - b.order)
      .map((s) => ({ id: s.id, name: s.name, deletable: true })),
    ...(hasUngrouped ? [{ id: UNGROUPED_FEATURE, name: 'Ungrouped', deletable: false }] : []),
  ]

  const chaptersOf = (featureId: string): Chapter[] =>
    featureId === UNGROUPED_FEATURE
      ? ordered.filter((c) => !c.submodelId || !submodelIds.has(c.submodelId))
      : ordered.filter((c) => c.submodelId === featureId)

  const slicesOf = (chapterId: string): Slice[] =>
    orderedSlices.filter((s) => s.chapterId === chapterId)

  const renderSliceRow = (s: Slice) => {
    const cls = [
      classes.sliceRow,
      dragSliceId === s.id ? classes.dragging : '',
      overSliceId === s.id && dragSliceId !== s.id ? classes.sliceDropBefore : '',
    ].join(' ')
    return (
      <div
        key={s.id}
        data-testid={`slice-row-${s.id}`}
        draggable={!busy}
        onDragStart={(e) => {
          if (busy) return
          setDragSliceId(s.id)
          e.dataTransfer.effectAllowed = 'move'
          e.dataTransfer.setData(SLICE_MIME, s.id)
        }}
        onDragOver={(e) => {
          if (busy || dragSliceId === null) return
          e.preventDefault()
          e.dataTransfer.dropEffect = 'move'
          setOverSliceId(s.id)
        }}
        onDrop={(e) => {
          if (busy || dragSliceId === null) return
          e.preventDefault()
          moveSlice(dragSliceId, s.id)
          setDragSliceId(null)
          setOverSliceId(null)
        }}
        onDragEnd={() => {
          setDragSliceId(null)
          setOverSliceId(null)
        }}
        className={cls}
      >
        <IconGripVertical size={12} className={classes.grip} />
        <Text component="span" size="xs" c="dimmed" className={classes.rowLabel}>{s.name}</Text>
      </div>
    )
  }

  const renderChapterRow = (c: Chapter) => {
    const isSelected = selected.has(c.id)
    const cls = [
      classes.chapterRow,
      dragId === c.id ? classes.dragging : '',
      isSelected ? classes.chapterSelected : '',
      overId === c.id && dragId !== c.id ? classes.dropBefore : '',
    ].join(' ')
    return (
      <div
        data-testid={`chapter-row-${c.id}`}
        draggable={!busy}
        onClick={() => toggleSelect(c.id)}
        onDragStart={(e) => {
          if (busy) return
          setDragId(c.id)
          e.dataTransfer.effectAllowed = 'move'
          e.dataTransfer.setData(CHAPTER_MIME, c.id)
        }}
        onDragOver={(e) => {
          if (busy) return
          if (dragSliceId !== null) {
            e.preventDefault()
            e.dataTransfer.dropEffect = 'move'
            setOverId(c.id)
            return
          }
          if (dragId === null) return
          e.preventDefault()
          e.dataTransfer.dropEffect = 'move'
          setOverId(c.id)
        }}
        onDrop={(e) => {
          if (busy) return
          if (dragSliceId !== null) {
            e.preventDefault()
            dropSliceOnChapter(dragSliceId, c.id)
            setDragSliceId(null)
            setOverSliceId(null)
            setOverId(null)
            return
          }
          if (dragId === null) return
          e.preventDefault()
          move(dragId, c.id)
          setDragId(null)
          setOverId(null)
        }}
        onDragEnd={() => {
          setDragId(null)
          setOverId(null)
        }}
        className={cls}
      >
        <IconGripVertical size={12} className={classes.grip} />
        <Text component="span" size="sm" className={classes.rowLabel} inherit>
          <EditableLabel label={c.name} onRename={(name) => onRenameChapter(c.id, name)} />
        </Text>
        {isSelected && <IconCheck size={14} color="var(--em-feature)" />}
        <ActionIcon
          size="xs"
          variant="subtle"
          color="red"
          data-testid={`delete-chapter-${c.id}`}
          disabled={busy}
          onClick={(e) => {
            e.stopPropagation()
            onDeleteChapter(c.id)
          }}
          title="Delete chapter (keeps its slices, moves them to Ungrouped)"
          aria-label="Delete chapter"
        >
          <IconX size={12} />
        </ActionIcon>
      </div>
    )
  }

  return (
    <aside className={classes.panel} data-testid="features-panel">
      <div className={classes.header}>
        <Text size="sm" fw={600}>Features</Text>
      </div>

      <div className={`${classes.list} ${busy ? classes.busy : ''}`}>
        <button
          type="button"
          data-testid="add-feature"
          className={classes.addRow}
          disabled={busy}
          onClick={onAddFeature}
        >
          <IconPlus size={14} />
          <span>New feature</span>
        </button>

        {features.length === 0 ? (
          <Text px="xs" py="sm" size="sm" c="dimmed">
            No features yet — add one, or group slices into chapters first.
          </Text>
        ) : (
          features.map((f) => {
            const isActive = f.id === activeFeatureId
            const featureChapters = chaptersOf(f.id)
            return (
              <div key={f.id} data-testid={`feature-section-${f.id}`}>
                <div
                  data-testid={`feature-row-${f.id}`}
                  data-active={isActive ? 'true' : 'false'}
                  onClick={() => onSelectFeature(f.id)}
                  className={`${classes.featureRow} ${isActive ? classes.featureRowActive : ''}`}
                >
                  <Text component="span" className={classes.rowLabel} inherit>
                    {f.deletable ? (
                      <EditableLabel label={f.name} onRename={(name) => onRenameFeature(f.id, name)} />
                    ) : (
                      f.name
                    )}
                  </Text>
                  <Text component="span" size="xs" c={isActive ? undefined : 'dimmed'} opacity={isActive ? 0.8 : 1}>
                    {featureChapters.length}
                  </Text>
                  {f.deletable && (
                    <ActionIcon
                      size="xs"
                      variant="subtle"
                      color={isActive ? 'gray' : 'red'}
                      data-testid={`delete-feature-${f.id}`}
                      disabled={busy}
                      onClick={(e) => {
                        e.stopPropagation()
                        onDeleteFeature(f.id)
                      }}
                      title="Delete feature (keeps its chapters, moves them to Ungrouped)"
                      aria-label="Delete feature"
                    >
                      <IconX size={12} />
                    </ActionIcon>
                  )}
                </div>

                {isActive && (
                  <div className={classes.children}>
                    {featureChapters.length === 0 ? (
                      <Text px={4} py={4} size="xs" c="dimmed">No chapters in this feature.</Text>
                    ) : (
                      featureChapters.map((c) => {
                        const chapterSlices = slicesOf(c.id)
                        return (
                          <div key={c.id}>
                            {renderChapterRow(c)}
                            {chapterSlices.length > 0 && (
                              <div className={classes.sliceChildren}>
                                {chapterSlices.map(renderSliceRow)}
                              </div>
                            )}
                          </div>
                        )
                      })
                    )}
                    <button
                      type="button"
                      data-testid={`add-chapter-${f.id}`}
                      className={classes.addChapterRow}
                      disabled={busy}
                      onClick={() => onAddChapter(f.id === UNGROUPED_FEATURE ? null : f.id)}
                    >
                      <IconPlus size={12} />
                      <span>New chapter</span>
                    </button>
                  </div>
                )}
              </div>
            )
          })
        )}
      </div>

      {selected.size > 0 && (
        <div className={classes.selectionBar}>
          <Button
            size="xs"
            color="emFeature"
            style={{ flex: 1 }}
            data-testid="new-feature-from-selection"
            disabled={busy}
            onClick={() => {
              onCreateFeatureFromChapters([...selected])
              setSelected(new Set())
            }}
          >
            New Feature ({selected.size})
          </Button>
          <Button
            size="xs"
            variant="default"
            data-testid="clear-selection"
            onClick={() => setSelected(new Set())}
          >
            Clear
          </Button>
        </div>
      )}
    </aside>
  )
}
