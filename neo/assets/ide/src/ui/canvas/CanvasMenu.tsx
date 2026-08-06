import type { ReactNode } from 'react'
import { Menu } from '@mantine/core'

interface CanvasMenuProps {
  opened: boolean
  /** Viewport coordinates (clientX/clientY) to anchor the menu at. */
  x: number
  y: number
  onClose: () => void
  children: ReactNode
}

/**
 * A Mantine Menu anchored at arbitrary viewport coordinates — the home for the
 * canvas right-click / double-click create menus. The target is a zero-size
 * fixed element placed at the cursor; the dropdown opens from there.
 */
export function CanvasMenu({ opened, x, y, onClose, children }: CanvasMenuProps) {
  return (
    <Menu opened={opened} onClose={onClose} position="bottom-start" withinPortal shadow="md" width={220}>
      <Menu.Target>
        <div style={{ position: 'fixed', left: x, top: y, width: 0, height: 0 }} />
      </Menu.Target>
      <Menu.Dropdown>{children}</Menu.Dropdown>
    </Menu>
  )
}
