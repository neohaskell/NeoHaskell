import { useMemo } from 'react'
import { Spotlight, type SpotlightActionData } from '@mantine/spotlight'
import {
  IconFilePlus,
  IconFolderOpen,
  IconWand,
  IconSparkles,
  IconPlus,
  IconLayoutBoardSplit,
} from '@tabler/icons-react'
import { LENSES, type Lens } from './lenses/lenses'

export interface CommandPaletteHandlers {
  onNew: () => void
  onOpen: () => void
  onRelayout: () => void
  onHeal: () => void
  onAddEvent: () => void
  onAddCommand: () => void
  onAddQuery: () => void
  onAddIntegration: () => void
  onAddUIPlaceholder: () => void
  onAddEntity: () => void
  onAddSlice: () => void
  onAddChapter: () => void
  onSelectLens: (lens: Lens) => void
}

/**
 * ⌘K command palette — the keyboard-first home for every action now that the
 * toolbars are gone. Registers the global mod+K shortcut. The same handlers
 * back the header buttons and canvas menus, so there is one action surface.
 */
export function CommandPalette(h: CommandPaletteHandlers) {
  const actions = useMemo<SpotlightActionData[]>(
    () => [
      { id: 'new', label: 'New model', onClick: h.onNew, leftSection: <IconFilePlus size={18} /> },
      { id: 'open', label: 'Open model', onClick: h.onOpen, leftSection: <IconFolderOpen size={18} /> },
      { id: 'tidy', label: 'Tidy by flow', description: 'Order slices/chapters by the event-modeling wave', onClick: h.onRelayout, leftSection: <IconWand size={18} /> },
      { id: 'heal', label: 'Heal with AI', description: 'Ask Claude to improve the model', onClick: h.onHeal, leftSection: <IconSparkles size={18} /> },
      { id: 'add-event', label: 'Add Event', group: 'Add', onClick: h.onAddEvent, leftSection: <IconPlus size={18} /> },
      { id: 'add-command', label: 'Add Command', group: 'Add', onClick: h.onAddCommand, leftSection: <IconPlus size={18} /> },
      { id: 'add-query', label: 'Add Query', group: 'Add', onClick: h.onAddQuery, leftSection: <IconPlus size={18} /> },
      { id: 'add-integration', label: 'Add Integration', group: 'Add', onClick: h.onAddIntegration, leftSection: <IconPlus size={18} /> },
      { id: 'add-ui', label: 'Add UI Placeholder', group: 'Add', onClick: h.onAddUIPlaceholder, leftSection: <IconPlus size={18} /> },
      { id: 'add-entity', label: 'Add Entity', group: 'Add', onClick: h.onAddEntity, leftSection: <IconPlus size={18} /> },
      { id: 'add-slice', label: 'Add Slice', group: 'Add', onClick: h.onAddSlice, leftSection: <IconPlus size={18} /> },
      { id: 'add-chapter', label: 'Add Chapter', group: 'Add', onClick: h.onAddChapter, leftSection: <IconPlus size={18} /> },
      ...LENSES.map((l) => ({
        id: `lens-${l.id}`,
        label: `Show ${l.label}`,
        group: 'Lens',
        onClick: () => h.onSelectLens(l.id),
        leftSection: <IconLayoutBoardSplit size={18} />,
      })),
    ],
    [h],
  )

  return (
    <Spotlight
      actions={actions}
      shortcut="mod+K"
      nothingFound="Nothing found"
      searchProps={{ placeholder: 'Search actions…' }}
    />
  )
}
