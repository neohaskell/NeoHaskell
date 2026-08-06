import { Stack, Group, TextInput, ActionIcon, Button } from '@mantine/core'
import { IconX, IconPlus } from '@tabler/icons-react'
import type { Field } from '../../model/types'

interface FieldsEditorProps {
  fields: readonly Field[]
  onChange: (fields: Field[]) => void
}

/**
 * Compact name:type field editor. NOT mounted by NodeShell anymore — node
 * fields are strictly READ-ONLY in the canvas (source code authors the schema
 * via the Rust background sync). Kept here, importable but currently
 * unreferenced, for the future editable Schema/Inspector lens. Carries `nodrag`
 * so editing inside a React Flow node doesn't drag it; React Flow itself ignores
 * key events from inputs (so Backspace edits text rather than deleting the node).
 */
export function FieldsEditor({ fields, onChange }: FieldsEditorProps) {
  const update = (i: number, patch: Partial<Field>) =>
    onChange(fields.map((f, idx) => (idx === i ? { ...f, ...patch } : f)))
  const remove = (i: number) => onChange(fields.filter((_, idx) => idx !== i))
  const add = () => onChange([...fields, { name: 'field', type: 'String' }])

  return (
    <Stack
      gap={4}
      data-testid="fields-editor"
      className="nodrag"
      onClick={(e) => e.stopPropagation()}
      onDoubleClick={(e) => e.stopPropagation()}
    >
      {fields.map((f, i) => (
        <Group gap={4} wrap="nowrap" key={i}>
          <TextInput
            size="xs"
            value={f.name}
            placeholder="name"
            aria-label="field name"
            onChange={(e) => update(i, { name: e.currentTarget.value })}
          />
          <TextInput
            size="xs"
            value={f.type}
            placeholder="type"
            aria-label="field type"
            onChange={(e) => update(i, { type: e.currentTarget.value })}
          />
          <ActionIcon
            size="sm"
            variant="subtle"
            color="red"
            aria-label="remove field"
            onClick={() => remove(i)}
          >
            <IconX size={12} />
          </ActionIcon>
        </Group>
      ))}
      <Button
        size="compact-xs"
        variant="subtle"
        leftSection={<IconPlus size={12} />}
        data-testid="add-field"
        onClick={add}
      >
        field
      </Button>
    </Stack>
  )
}
