import { Center, Stack, ThemeIcon, Title, Text, Badge } from '@mantine/core'
import { LENS_BY_ID, type Lens } from './lenses'

/** Polished placeholder for a roadmap lens (schema / logs / emulate). */
export function EmptyLens({ lens }: { lens: Lens }) {
  const meta = LENS_BY_ID[lens]
  return (
    <Center h="100%" p="xl" data-testid={`empty-lens-${lens}`}>
      <Stack align="center" gap="md" maw={420}>
        <ThemeIcon size={64} radius="xl" variant="light" color="emFeature">
          {meta.icon}
        </ThemeIcon>
        <Title order={3}>{meta.label}</Title>
        <Badge variant="light" color="emFeature">Coming soon</Badge>
        <Text c="dimmed" ta="center">{meta.blurb}</Text>
      </Stack>
    </Center>
  )
}
