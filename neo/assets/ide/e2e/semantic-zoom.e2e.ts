import { test, expect, type Page } from '@playwright/test'

const STORAGE_KEY = 'neoide:model'

// A command carrying schema fields, plus a second node so fit-to-view doesn't
// start zoomed past the threshold.
const MODEL = JSON.stringify({
  id: 'm',
  name: 'Shop',
  submodels: [],
  chapters: [{ id: 'c1', name: 'Cart', order: 0 }],
  entities: [{ id: 'e1', name: 'Order', order: 0 }],
  slices: [{ id: 's1', name: 'Place', chapterId: 'c1', order: 0 }],
  nodes: [
    { id: 'cmd1', type: 'command', name: 'PlaceOrder', entityId: 'e1', sliceId: 's1', fields: [{ name: 'orderId', type: 'UUID' }] },
    { id: 'ev1', type: 'event', name: 'OrderPlaced', entityId: 'e1', sliceId: 's1' },
  ],
  edges: [],
  layout: {
    nodePositions: { cmd1: { x: 0, y: 0 }, ev1: { x: 900, y: 600 } },
    viewport: { x: 0, y: 0, zoom: 1 },
  },
})

async function seed(page: Page) {
  // Seed only if absent so a reload doesn't clobber edits made during the test
  // (localStorage is the crash-survival buffer the app reloads from).
  await page.addInitScript(
    ([k, v]) => {
      if (!localStorage.getItem(k)) localStorage.setItem(k, v)
    },
    [STORAGE_KEY, MODEL] as const,
  )
}

// Zoom the canvas with the wheel (React Flow zooms on scroll, centered on the
// cursor). Robust against content overlapping the zoom controls at high zoom.
async function wheelZoom(page: Page, deltaY: number, steps: number) {
  const box = await page.getByTestId('canvas').boundingBox()
  await page.mouse.move(box!.x + box!.width / 2, box!.y + box!.height / 2)
  for (let i = 0; i < steps; i++) {
    await page.mouse.wheel(0, deltaY)
    await page.waitForTimeout(50)
  }
}

test('zooming in past the threshold reveals a node\'s schema fields', async ({ page }) => {
  await seed(page)
  await page.goto('/')
  await expect(page.getByText('PlaceOrder')).toBeVisible()
  await expect(page.getByTestId('fields-editor')).toHaveCount(0)

  await wheelZoom(page, -200, 12) // zoom in
  await expect(page.getByTestId('fields-editor').first()).toBeVisible()
  await expect(page.getByLabel('field name').first()).toHaveValue('orderId')

  await wheelZoom(page, 200, 16) // zoom back out
  await expect(page.getByTestId('fields-editor')).toHaveCount(0)
})

// Field add/edit/remove + persistence are covered robustly at the unit layer
// (FieldsEditor.test.tsx, serialization.test.ts `serialization_roundtrips_fields`,
// and App autosave→localStorage). Driving the in-node editor through React
// Flow's zoom transform in a browser is flaky, so it's intentionally not an
// e2e — the reveal-on-zoom behaviour above is the e2e-worthy part.
