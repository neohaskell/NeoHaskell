import { Modal, Text, Stack, Box, Group, Button, ScrollArea } from '@mantine/core'
import type { ValidationError } from '../ipc/eventModel'
import classes from './InvalidModelModal.module.css'

interface InvalidModelModalProps {
  errors: ValidationError[]
  /** Optional message above the error list (e.g. for malformed-JSON). */
  preamble?: string
  onHeal: () => void
  onCancel: () => void
}

export function InvalidModelModal({ errors, preamble, onHeal, onCancel }: InvalidModelModalProps) {
  return (
    <Modal
      opened
      onClose={onCancel}
      size="xl"
      title={<Text fw={600} size="lg">event-model.json is invalid</Text>}
    >
      <Stack gap="md">
        <Text size="sm" c="dimmed">
          {preamble ??
            'The file on disk does not match the event-model schema. You can ask an AI agent to heal it, or cancel and keep your local copy.'}
        </Text>
        <ScrollArea.Autosize mah="50vh">
          <Stack gap="xs">
            {errors.map((e, i) => (
              <Box key={`${e.pointer}-${i}`} pl="sm" className={classes.errorItem}>
                <Text size="xs" c="dimmed" ff="monospace">
                  {e.pointer === '' ? '(whole document)' : e.pointer}{' '}
                  <Text span c="dimmed" opacity={0.7}>[{e.kind}]</Text>
                </Text>
                <Text size="sm" ff="monospace">{e.message}</Text>
              </Box>
            ))}
          </Stack>
        </ScrollArea.Autosize>
        <Group justify="flex-end" gap="xs">
          <Button variant="default" onClick={onCancel}>Cancel</Button>
          <Button color="emFeature" variant="filled" onClick={onHeal}>Heal with AI</Button>
        </Group>
      </Stack>
    </Modal>
  )
}
