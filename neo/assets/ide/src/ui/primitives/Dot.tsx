import { Box, type MantineColor } from '@mantine/core'

interface DotProps {
  color: MantineColor
  size?: number
}

/** A small themed status dot. Used for connection state and issue severity so
 *  the colored-circle pattern lives in exactly one place. */
export function Dot({ color, size = 8 }: DotProps) {
  return <Box w={size} h={size} bg={color} style={{ borderRadius: '50%', flexShrink: 0 }} />
}

/** Issue severity → Mantine color, shared by StatusBar + ProblemsPanel. */
export const SEVERITY_COLOR = {
  error: 'red',
  warning: 'yellow',
  info: 'blue',
} as const
