import { test, expect, type Page } from '@playwright/test'

const STORAGE_KEY = 'neoide:model'

// A feature model so the band frame + its add (+) buttons render.
const MODEL = JSON.stringify({
  id: 'm',
  name: 'Shop',
  submodels: [{ id: 'smA', name: 'Checkout', order: 0 }],
  chapters: [{ id: 'cA', name: 'Cart', order: 0, submodelId: 'smA' }],
  entities: [{ id: 'eX', name: 'Order', order: 0 }],
  slices: [{ id: 's1', name: 'Place', chapterId: 'cA', order: 0 }],
  nodes: [
    { id: 'cmd', type: 'command', name: 'PlaceOrder', entityId: 'eX', sliceId: 's1' },
    { id: 'ev', type: 'event', name: 'OrderPlaced', entityId: 'eX', sliceId: 's1' },
  ],
  edges: [{ id: 'e1', type: 'commandProducesEvent', sourceId: 'cmd', targetId: 'ev' }],
  layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
})

async function seed(page: Page) {
  await page.addInitScript(([k, v]) => localStorage.setItem(k, v), [STORAGE_KEY, MODEL] as const)
}

test('sidebar uses an add-row, not a header + Feature button', async ({ page }) => {
  await seed(page)
  await page.goto('/')
  // The add affordance is the dashed row carrying the add-feature testid.
  const addRow = page.getByTestId('add-feature')
  await expect(addRow).toBeVisible()
  await expect(addRow).toContainText(/new feature/i)
  // Clicking it creates + switches into a new feature.
  await addRow.click()
  const active = page.locator('[data-testid^="feature-row-"][data-active="true"]')
  await expect(active).toContainText(/new feature/i)
})

test('the + slice button adds a slice column inside the feature', async ({ page }) => {
  await seed(page)
  await page.goto('/')
  await expect(page.getByText('PlaceOrder')).toBeVisible()
  await expect(page.getByTestId('add-slice-button')).toBeVisible()

  // Wired properly: the new slice attaches to the feature's last chapter, so a
  // "New Slice" column appears on the page (not an invisible ungrouped slice).
  await page.getByTestId('add-slice-button').click({ force: true })
  await expect(page.getByText('New Slice')).toBeVisible()
})

test('the + entity button adds an entity lane inside the feature', async ({ page }) => {
  await seed(page)
  await page.goto('/')
  await expect(page.getByTestId('add-entity-button')).toBeVisible()

  // Wired properly: a freshly-added (eventless) entity renders as an empty lane.
  await page.getByTestId('add-entity-button').click({ force: true })
  await expect(page.getByText('New Entity')).toBeVisible()
})

test('the lens rail sits on the right edge of the window', async ({ page }) => {
  await seed(page)
  await page.goto('/')
  const rail = page.getByTestId('activity-rail')
  await expect(rail).toBeVisible()
  const railBox = await rail.boundingBox()
  const viewport = page.viewportSize()!
  // Right edge of the rail is flush with (near) the right edge of the window.
  expect(railBox!.x + railBox!.width).toBeGreaterThan(viewport.width - 4)
})
