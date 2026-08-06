import { test, expect, type Page } from '@playwright/test'

const STORAGE_KEY = 'neoide:model'

async function seed(page: Page, model: string) {
  await page.addInitScript(
    ([key, value]) => localStorage.setItem(key, value),
    [STORAGE_KEY, model] as const,
  )
}

// A command with no slice and no produced event → 2 warnings.
const MODEL_ORPHAN = JSON.stringify({
  id: 'm',
  name: 'x',
  submodels: [],
  chapters: [],
  entities: [],
  slices: [],
  nodes: [{ id: 'orphan', type: 'command', name: 'Orphan', entityId: null, sliceId: null }],
  edges: [],
  layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
})

// One feature, a connected command→event pair, plus an unrelated command.
const MODEL_TRACE = JSON.stringify({
  id: 'm',
  name: 'x',
  submodels: [{ id: 'smA', name: 'Checkout', order: 0 }],
  chapters: [{ id: 'cA', name: 'Order', order: 0, submodelId: 'smA' }],
  entities: [{ id: 'eX', name: 'Order', order: 0 }],
  slices: [{ id: 'sA', name: 'Place', chapterId: 'cA', order: 0 }],
  nodes: [
    { id: 'cmd', type: 'command', name: 'PlaceOrder', entityId: 'eX', sliceId: 'sA' },
    { id: 'ev', type: 'event', name: 'OrderPlaced', entityId: 'eX', sliceId: 'sA' },
    { id: 'other', type: 'command', name: 'Unrelated', entityId: 'eX', sliceId: 'sA' },
  ],
  edges: [{ id: 'e1', type: 'commandProducesEvent', sourceId: 'cmd', targetId: 'ev' }],
  layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
})

// One feature, one chapter spanning two slices that each hold an event.
const MODEL_CHAPTERS = JSON.stringify({
  id: 'm',
  name: 'x',
  submodels: [{ id: 'smA', name: 'Checkout', order: 0 }],
  chapters: [{ id: 'cA', name: 'Ordering', order: 0, submodelId: 'smA' }],
  entities: [{ id: 'eX', name: 'Cart', order: 0 }],
  slices: [
    { id: 's1', name: 'AddItem', chapterId: 'cA', order: 0 },
    { id: 's2', name: 'Submit', chapterId: 'cA', order: 1 },
  ],
  nodes: [
    { id: 'ev1', type: 'event', name: 'ItemAdded', entityId: 'eX', sliceId: 's1' },
    { id: 'ev2', type: 'event', name: 'OrderSubmitted', entityId: 'eX', sliceId: 's2' },
  ],
  edges: [],
  layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
})

// One slice holding a command, a query, and an integration together.
const MODEL_CQI = JSON.stringify({
  id: 'm',
  name: 'x',
  submodels: [{ id: 'smA', name: 'Checkout', order: 0 }],
  chapters: [{ id: 'cA', name: 'Ordering', order: 0, submodelId: 'smA' }],
  entities: [{ id: 'eX', name: 'Order', order: 0 }],
  slices: [{ id: 's1', name: 'Place', chapterId: 'cA', order: 0 }],
  nodes: [
    { id: 'cmd', type: 'command', name: 'PlaceOrder', entityId: 'eX', sliceId: 's1' },
    { id: 'q', type: 'query', name: 'OrderView', sliceId: 's1' },
    { id: 'i', type: 'integration', name: 'Notify', kind: 'outbound', sliceId: 's1' },
  ],
  edges: [],
  layout: { nodePositions: {}, viewport: { x: 0, y: 0, zoom: 1 } },
})

test('a slice hosts command, query and integration on one level (wider column)', async ({ page }) => {
  await seed(page, MODEL_CQI)
  await page.goto('/')
  await expect(page.getByText('PlaceOrder')).toBeVisible()
  await expect(page.getByText('OrderView')).toBeVisible()
  await expect(page.getByText('Notify')).toBeVisible()

  const cmd = await page.getByText('PlaceOrder').boundingBox()
  const q = await page.getByText('OrderView').boundingBox()
  const i = await page.getByText('Notify').boundingBox()
  expect(cmd && q && i).toBeTruthy()
  // Same vertical level (within tolerance), distinct x (side by side).
  expect(Math.abs(cmd!.y - q!.y)).toBeLessThan(24)
  expect(Math.abs(q!.y - i!.y)).toBeLessThan(24)
  expect(Math.abs(cmd!.x - q!.x)).toBeGreaterThan(40)
  expect(Math.abs(q!.x - i!.x)).toBeGreaterThan(40)
  await page.screenshot({ path: 'e2e-out/cqi-same-level.png', fullPage: true })
})

test('nodes in a slice can be dragged freely and stay put (no snap-back)', async ({ page }) => {
  await seed(page, MODEL_CQI)
  await page.goto('/')
  await expect(page.getByText('Notify')).toBeVisible()

  // React Flow nodes drag via mouse events (not HTML5 DnD), so Playwright can
  // drive them. Drag the integration well to the left of where the grid put it.
  const node = page.locator('.react-flow__node', { hasText: 'Notify' })
  const before = await node.boundingBox()
  expect(before).toBeTruthy()

  await page.mouse.move(before!.x + before!.width / 2, before!.y + before!.height / 2)
  await page.mouse.down()
  await page.mouse.move(before!.x - 250, before!.y + before!.height / 2, { steps: 12 })
  await page.mouse.up()

  // After the model update + re-render, it must NOT snap back to the grid slot.
  await page.waitForTimeout(500)
  const after = await node.boundingBox()
  expect(after).toBeTruthy()
  expect(after!.x).toBeLessThan(before!.x - 120)
  await page.screenshot({ path: 'e2e-out/free-move.png', fullPage: true })
})

test('chapters render on the feature canvas and slices are listed for reordering', async ({ page }) => {
  await seed(page, MODEL_CHAPTERS)
  await page.goto('/')

  // The chapter arrow is drawn on the canvas (a React Flow node), labeled.
  const chapterArrow = page.locator('.react-flow__node[data-id="__chapter-arrow-cA"]')
  await expect(chapterArrow).toBeVisible()
  await expect(chapterArrow).toContainText('Ordering')

  // The active feature's chapter lists its slices (drag handles for reorder).
  await expect(page.getByTestId('slice-row-s1')).toBeVisible()
  await expect(page.getByTestId('slice-row-s2')).toBeVisible()
  await page.screenshot({ path: 'e2e-out/chapters-and-slices.png', fullPage: true })
})

test('autosaves without a Save button — change survives a reload', async ({ page }) => {
  // No seed: start empty so the reload reflects only what we add.
  await page.goto('/')
  // There is no Save button anywhere.
  await expect(page.getByRole('button', { name: /^save$/i })).toHaveCount(0)

  // Creation is gesture/palette-driven now (no toolbar). Add an Event via ⌘K.
  await page.keyboard.press('ControlOrMeta+k')
  const search = page.getByPlaceholder(/search actions/i)
  await expect(search).toBeVisible()
  await search.fill('Add Event')
  await page.getByText('Add Event', { exact: true }).click()
  await expect(page.getByText('New Event')).toBeVisible()

  // Reload — the change persisted (localStorage is the crash-survival buffer;
  // no manual Save was ever clicked).
  await page.reload()
  await expect(page.getByText('New Event')).toBeVisible()
})

test('live validation surfaces issues in the status chip and Problems panel', async ({ page }) => {
  await seed(page, MODEL_ORPHAN)
  await page.goto('/')

  const chip = page.getByTestId('validation-chip')
  // Debounced validation turns the chip from "Valid" into a problem count.
  await expect(chip).toContainText('2', { timeout: 3000 })

  await chip.click()
  const panel = page.getByTestId('problems-panel')
  await expect(panel).toBeVisible()
  await expect(panel).toContainText(/no slice/i)
  await expect(panel).toContainText(/produces no event/i)
  await page.screenshot({ path: 'e2e-out/validation-problems.png', fullPage: true })
})

test('selecting a node highlights its connected edges and dims the rest', async ({ page }) => {
  await seed(page, MODEL_TRACE)
  await page.goto('/')
  await expect(page.getByText('PlaceOrder')).toBeVisible()
  await expect(page.getByText('Unrelated')).toBeVisible()

  // Select the command; its produced-event edge should thicken to the trace width.
  await page.getByText('PlaceOrder').click()

  await expect
    .poll(
      async () =>
        page
          .locator('.react-flow__edge-path')
          .evaluateAll((els) => els.some((e) => getComputedStyle(e).strokeWidth === '4.5px')),
      { timeout: 3000 },
    )
    .toBe(true)

  // The unrelated node is dimmed (focus + context).
  await expect
    .poll(async () =>
      page
        .locator('.react-flow__node')
        .evaluateAll((els) => els.some((e) => parseFloat(getComputedStyle(e).opacity) <= 0.3)),
    )
    .toBe(true)
  await page.screenshot({ path: 'e2e-out/trace.png', fullPage: true })

  // Clicking empty canvas clears the trace (nothing dimmed any more).
  await page.locator('.react-flow__pane').click({ position: { x: 5, y: 5 } })
  await expect
    .poll(async () =>
      page
        .locator('.react-flow__node')
        .evaluateAll((els) => els.every((e) => parseFloat(getComputedStyle(e).opacity || '1') > 0.3)),
    )
    .toBe(true)
})
