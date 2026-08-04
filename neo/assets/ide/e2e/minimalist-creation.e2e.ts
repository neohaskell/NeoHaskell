import { test, expect, type Page } from '@playwright/test'

const STORAGE_KEY = 'neoide:model'

// Flat model (no submodels) with one slice so created nodes always render.
const MODEL = JSON.stringify({
  id: 'm',
  name: 'Shop',
  submodels: [],
  chapters: [{ id: 'c1', name: 'Cart', order: 0 }],
  entities: [{ id: 'e1', name: 'Cart', order: 0 }],
  slices: [{ id: 's1', name: 'Add', chapterId: 'c1', order: 0 }],
  nodes: [{ id: 'cmd1', type: 'command', name: 'AddItem', entityId: 'e1', sliceId: 's1' }],
  edges: [],
  layout: { nodePositions: { cmd1: { x: 200, y: 200 } }, viewport: { x: 0, y: 0, zoom: 1 } },
})

async function seed(page: Page) {
  await page.addInitScript(([k, v]) => localStorage.setItem(k, v), [STORAGE_KEY, MODEL] as const)
}

test('no node-creation toolbar exists', async ({ page }) => {
  await seed(page)
  await page.goto('/')
  await expect(page.getByRole('button', { name: /^\+ event$/i })).toHaveCount(0)
  await expect(page.getByRole('button', { name: /^\+ command$/i })).toHaveCount(0)
})

test('double-click the pane opens the add menu and creates a node', async ({ page }) => {
  await seed(page)
  await page.goto('/')
  await expect(page.getByText('AddItem')).toBeVisible()

  // Double-click an empty area of the pane (bottom-right, away from nodes).
  const pane = page.locator('.react-flow__pane')
  const box = await pane.boundingBox()
  expect(box).toBeTruthy()
  await page.mouse.dblclick(box!.x + box!.width * 0.7, box!.y + box!.height * 0.7)

  // The add menu appears; create an Event.
  await page.getByTestId('pane-add-event').click()
  await expect(page.getByText('New Event')).toBeVisible()
})

test('right-click a node offers a valid successor and creates it + an edge', async ({ page }) => {
  await seed(page)
  await page.goto('/')
  const cmd = page.locator('.react-flow__node', { hasText: 'AddItem' })
  await expect(cmd).toBeVisible()
  await cmd.click({ button: 'right' })

  // command → event is the only valid successor.
  await page.getByTestId('node-add-successor-event').click()
  await expect(page.getByText('New Event')).toBeVisible()
  // A produced-event edge now exists on the canvas.
  await expect(page.locator('.react-flow__edge')).toHaveCount(1)
})

test('⌘K opens the command palette', async ({ page }) => {
  await seed(page)
  await page.goto('/')
  await page.keyboard.press('ControlOrMeta+k')
  await expect(page.getByPlaceholder(/search actions/i)).toBeVisible()
  await page.keyboard.press('Escape')
  await expect(page.getByPlaceholder(/search actions/i)).toHaveCount(0)
})
