import { test, type Page } from '@playwright/test'

// Screenshot harness for visual/UX critique of the neo IDE main screen.
// Captures the *real* rendered app (not an empty canvas) so a critique judges
// what a user actually sees. Driven by the visual-critique skill.
//
//   NEO_SHOT_MODEL  path to a JSON event-model to seed (defaults to the demo below)
//   NEO_SHOT_OUT    output PNG path (default e2e-out/critique-main.png)
//   NEO_SHOT_FULL   "1" for fullPage capture (default: viewport only)
//
// Runs against `vite preview` (see playwright.config.ts). There is no Rust
// server at that origin, so the status bar shows "disconnected" by design —
// the model is seeded into localStorage, which is the canvas's source of truth.

import { readFileSync } from 'node:fs'

const STORAGE_KEY = 'neoide:model'

// A realistic e-commerce event model: two features, multiple chapters/slices,
// commands→events→queries with an integration and a UI placeholder. Enough
// density to judge the real working screen.
const DEMO_MODEL = JSON.stringify({
  id: 'm',
  name: 'Shop',
  submodels: [
    { id: 'smA', name: 'Checkout', order: 0 },
    { id: 'smB', name: 'Fulfilment', order: 1 },
  ],
  chapters: [
    { id: 'cA', name: 'Cart', order: 0, submodelId: 'smA' },
    { id: 'cB', name: 'Payment', order: 1, submodelId: 'smA' },
    { id: 'cC', name: 'Shipping', order: 2, submodelId: 'smB' },
  ],
  entities: [
    { id: 'eCart', name: 'Cart', order: 0 },
    { id: 'eOrder', name: 'Order', order: 1 },
    { id: 'eShip', name: 'Shipment', order: 2 },
  ],
  slices: [
    { id: 's1', name: 'Add Item', chapterId: 'cA', order: 0 },
    { id: 's2', name: 'View Cart', chapterId: 'cA', order: 1 },
    { id: 's3', name: 'Pay', chapterId: 'cB', order: 2 },
    { id: 's4', name: 'Ship Order', chapterId: 'cC', order: 3 },
  ],
  nodes: [
    { id: 'ui1', type: 'uiPlaceholder', name: 'Product Page', sliceId: 's1' },
    {
      id: 'cmd1', type: 'command', name: 'AddItemToCart', entityId: 'eCart', sliceId: 's1',
      fields: [
        { name: 'productId', type: 'UUID' },
        { name: 'quantity', type: 'Int' },
      ],
    },
    {
      id: 'ev1', type: 'event', name: 'ItemAdded', entityId: 'eCart', sliceId: 's1',
      fields: [
        { name: 'productId', type: 'UUID' },
        { name: 'quantity', type: 'Int' },
        { name: 'addedAt', type: 'Timestamp' },
      ],
    },
    {
      id: 'q1', type: 'query', name: 'CartView', sliceId: 's2',
      fields: [
        { name: 'items', type: '[CartLine]' },
        { name: 'subtotal', type: 'Money' },
      ],
    },
    {
      id: 'cmd2', type: 'command', name: 'Checkout', entityId: 'eOrder', sliceId: 's3',
      fields: [
        { name: 'cartId', type: 'UUID' },
        { name: 'paymentMethod', type: 'PaymentMethod' },
      ],
    },
    {
      id: 'ev2', type: 'event', name: 'OrderPlaced', entityId: 'eOrder', sliceId: 's3',
      fields: [
        { name: 'orderId', type: 'UUID' },
        { name: 'total', type: 'Money' },
        { name: 'placedAt', type: 'Timestamp' },
      ],
    },
    {
      id: 'int1', type: 'integration', name: 'Stripe', kind: 'outbound', sliceId: 's3',
      fields: [{ name: 'apiKey', type: 'Secret' }],
    },
    {
      id: 'ev3', type: 'event', name: 'PaymentCaptured', entityId: 'eOrder', sliceId: 's3',
      fields: [
        { name: 'orderId', type: 'UUID' },
        { name: 'amount', type: 'Money' },
        { name: 'capturedAt', type: 'Timestamp' },
      ],
    },
  ],
  edges: [
    { id: 'e1', type: 'commandProducesEvent', sourceId: 'cmd1', targetId: 'ev1' },
    { id: 'e2', type: 'eventFeedsQuery', sourceId: 'ev1', targetId: 'q1' },
    { id: 'e3', type: 'commandProducesEvent', sourceId: 'cmd2', targetId: 'ev2' },
    { id: 'e4', type: 'commandProducesEvent', sourceId: 'cmd2', targetId: 'ev3' },
  ],
  layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
})

const model = process.env.NEO_SHOT_MODEL
  ? readFileSync(process.env.NEO_SHOT_MODEL, 'utf8')
  : DEMO_MODEL
const out = process.env.NEO_SHOT_OUT ?? 'e2e-out/critique-main.png'
const fullPage = process.env.NEO_SHOT_FULL === '1'
// NEO_SHOT_ZOOMOUT: number of zoom-out clicks before the shot. Lets a critique
// capture the zoomed-out "flow" level-of-detail (nodes collapse to header-only
// below COLLAPSE_THRESHOLD). 0 = default fit-to-view (full record cards).
const zoomOutClicks = Number(process.env.NEO_SHOT_ZOOMOUT ?? '0')

async function seed(page: Page, value: string) {
  await page.addInitScript(
    ([key, v]) => localStorage.setItem(key, v),
    [STORAGE_KEY, value] as const,
  )
}

test('capture main screen', async ({ page }) => {
  await seed(page, model)
  await page.goto('/')
  await page.waitForTimeout(1500)
  for (let i = 0; i < zoomOutClicks; i++) {
    await page.locator('.react-flow__controls-zoomout').click()
    await page.waitForTimeout(200)
  }
  if (zoomOutClicks > 0) await page.waitForTimeout(400)
  await page.screenshot({ path: out, fullPage })
})
