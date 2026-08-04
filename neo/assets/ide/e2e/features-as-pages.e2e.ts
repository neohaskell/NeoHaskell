import { test, expect, type Page } from '@playwright/test'

const STORAGE_KEY = 'neoide:model'

// Two features with a cross-feature edge: Checkout(OrderPlaced) feeds
// Billing(InvoiceView). The cross edge MUST surface as a boundary portal on
// each feature — never silently dropped.
const MODEL_TWO_FEATURES = JSON.stringify({
  id: 'm',
  name: 'Demo',
  submodels: [
    { id: 'smA', name: 'Checkout', order: 0 },
    { id: 'smB', name: 'Billing', order: 1 },
  ],
  chapters: [
    { id: 'cA', name: 'Order', order: 0, submodelId: 'smA' },
    { id: 'cB', name: 'Invoice', order: 1, submodelId: 'smB' },
  ],
  entities: [{ id: 'eX', name: 'Order', order: 0 }],
  slices: [
    { id: 'sA', name: 'Place', chapterId: 'cA', order: 0 },
    { id: 'sB', name: 'Bill', chapterId: 'cB', order: 1 },
  ],
  nodes: [
    { id: 'cmdA', type: 'command', name: 'PlaceOrder', entityId: 'eX', sliceId: 'sA' },
    { id: 'evA', type: 'event', name: 'OrderPlaced', entityId: 'eX', sliceId: 'sA' },
    { id: 'qB', type: 'query', name: 'InvoiceView', sliceId: 'sB' },
  ],
  edges: [
    { id: 'e1', type: 'commandProducesEvent', sourceId: 'cmdA', targetId: 'evA' },
    { id: 'e2', type: 'eventFeedsQuery', sourceId: 'evA', targetId: 'qB' },
  ],
  layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
})

// Flat model (no submodels) for the create-from-selection flow.
const MODEL_FLAT = JSON.stringify({
  id: 'm2',
  name: 'Flat',
  submodels: [],
  chapters: [
    { id: 'c1', name: 'Cart', order: 0 },
    { id: 'c2', name: 'Pay', order: 1 },
  ],
  entities: [{ id: 'eX', name: 'Order', order: 0 }],
  slices: [
    { id: 's1', name: 'AddItem', chapterId: 'c1', order: 0 },
    { id: 's2', name: 'Checkout', chapterId: 'c2', order: 1 },
  ],
  nodes: [
    { id: 'ev1', type: 'event', name: 'ItemAdded', entityId: 'eX', sliceId: 's1' },
    { id: 'ev2', type: 'event', name: 'Paid', entityId: 'eX', sliceId: 's2' },
  ],
  edges: [],
  layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
})

async function seed(page: Page, model: string) {
  await page.addInitScript(
    ([key, value]) => localStorage.setItem(key, value),
    [STORAGE_KEY, model] as const,
  )
}

test('switches features and renders cross-feature edges as boundary portals', async ({ page }) => {
  await seed(page, MODEL_TWO_FEATURES)
  await page.goto('/')

  // Navigator lists both features; the first (Checkout) is active by default.
  await expect(page.getByTestId('feature-row-smA')).toHaveAttribute('data-active', 'true')
  await expect(page.getByTestId('feature-row-smB')).toBeVisible()

  // Checkout screen shows ONLY its own nodes.
  await expect(page.getByText('PlaceOrder')).toBeVisible()
  await expect(page.getByText('OrderPlaced')).toBeVisible()

  // The cross-feature edge to Billing renders as a boundary portal (not dropped).
  const portalA = page.getByTestId('boundary-portal').first()
  await expect(portalA).toContainText('InvoiceView')
  await expect(portalA).toContainText('Billing')
  await page.screenshot({ path: 'e2e-out/feature-checkout.png', fullPage: true })

  // Switch to Billing.
  await page.getByTestId('feature-row-smB').click()
  await expect(page.getByTestId('feature-row-smB')).toHaveAttribute('data-active', 'true')
  await expect(page.getByText('InvoiceView')).toBeVisible()

  // The same edge appears here as an INCOMING portal from Checkout.
  const portalB = page.getByTestId('boundary-portal').first()
  await expect(portalB).toContainText('OrderPlaced')
  await expect(portalB).toContainText('Checkout')
  await page.screenshot({ path: 'e2e-out/feature-billing.png', fullPage: true })

  // Clicking the portal navigates back to Checkout.
  await portalB.click()
  await expect(page.getByTestId('feature-row-smA')).toHaveAttribute('data-active', 'true')
})

test('creates a feature from selected chapters and switches into it', async ({ page }) => {
  await seed(page, MODEL_FLAT)
  await page.goto('/')

  // Flat model → Ungrouped pseudo-feature, active, listing its chapters.
  await expect(page.getByTestId('feature-row-__ungrouped__')).toBeVisible()
  await expect(page.getByTestId('chapter-row-c1')).toBeVisible()
  await expect(page.getByTestId('chapter-row-c2')).toBeVisible()

  // Select both chapters and create a feature from them.
  await page.getByTestId('chapter-row-c1').click()
  await page.getByTestId('chapter-row-c2').click()
  await page.getByTestId('new-feature-from-selection').click()

  // Selection bar gone; a new feature is active and named "New Feature".
  await expect(page.getByTestId('new-feature-from-selection')).toHaveCount(0)
  const active = page.locator('[data-testid^="feature-row-"][data-active="true"]')
  await expect(active).toContainText('New Feature')

  // The canvas still shows the chapters' nodes, now in feature mode.
  await expect(page.getByText('ItemAdded')).toBeVisible()
  await page.screenshot({ path: 'e2e-out/feature-created.png', fullPage: true })
})
