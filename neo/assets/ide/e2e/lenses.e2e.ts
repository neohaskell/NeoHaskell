import { test, expect, type Page } from '@playwright/test'

const STORAGE_KEY = 'neoide:model'

const MODEL = JSON.stringify({
  id: 'm',
  name: 'Shop',
  submodels: [],
  chapters: [{ id: 'c1', name: 'Cart', order: 0 }],
  entities: [{ id: 'e1', name: 'Cart', order: 0 }],
  slices: [{ id: 's1', name: 'Add', chapterId: 'c1', order: 0 }],
  nodes: [{ id: 'ev1', type: 'event', name: 'ItemAdded', entityId: 'e1', sliceId: 's1' }],
  edges: [],
  layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
})

async function seed(page: Page) {
  await page.addInitScript(([k, v]) => localStorage.setItem(k, v), [STORAGE_KEY, MODEL] as const)
}

test('activity rail switches lenses and back to model', async ({ page }) => {
  await seed(page)
  await page.goto('/')
  await expect(page.getByTestId('canvas')).toBeVisible()

  // Switch to Schema → placeholder; canvas hidden.
  await page.getByTestId('lens-schema').click()
  await expect(page.getByTestId('empty-lens-schema')).toBeVisible()
  await expect(page.getByText('Coming soon')).toBeVisible()
  await expect(page.getByTestId('canvas')).toHaveCount(0)

  // Logs + Emulate placeholders too.
  await page.getByTestId('lens-logs').click()
  await expect(page.getByTestId('empty-lens-logs')).toBeVisible()
  await page.getByTestId('lens-emulate').click()
  await expect(page.getByTestId('empty-lens-emulate')).toBeVisible()

  // Tidy/Heal are disabled off the model lens.
  await expect(page.getByRole('button', { name: /tidy by flow/i })).toBeDisabled()
  await expect(page.getByRole('button', { name: /heal with ai/i })).toBeDisabled()

  // Back to Model → canvas returns.
  await page.getByTestId('lens-model').click()
  await expect(page.getByTestId('canvas')).toBeVisible()
})
