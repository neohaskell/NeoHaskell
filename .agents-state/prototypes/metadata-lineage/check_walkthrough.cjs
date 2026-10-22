const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { chromium } = require(process.env.LINEAGE_PLAYWRIGHT || 'playwright');

async function main() {
  const directory = path.resolve(__dirname, '../../control-ui/metadata-lineage');
  fs.mkdirSync(directory, { recursive: true });
  const browser = await chromium.launch({ headless: true });
  try {
    const page = await browser.newPage({ viewport: { width: 1120, height: 900 } });
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto('file://' + path.join(__dirname, 'lineage.html'));
    await page.screenshot({ path: path.join(directory, 'initial.png'), fullPage: true });
    const expected = {
      chain: [['E1', 'user-42', 'R1', 'Absent', 'R1'], ['E2', 'user-42', 'R1', 'E1', 'R2'], ['E3', 'user-42', 'R1', 'E2', 'R3']],
      fanout: [['E1', 'user-42', 'R1', 'Absent', 'R1'], ['E2', 'user-42', 'R1', 'E1', 'R2'], ['E3', 'user-42', 'R1', 'E1', 'R3']],
      historical: [['E-old', 'user-7', 'Absent', 'Absent', 'historical'], ['E1', 'user-7', 'E-old', 'E-old', 'R2'], ['E2', 'user-7', 'E-old', 'E-old', 'R3']],
      timer: [['E1', 'Absent', 'R2', 'Absent', 'R2'], ['E2', 'Absent', 'R3', 'Absent', 'R3']],
      retry: [['E1', 'user-42', 'R1', 'Absent', 'R1'], ['E2', 'user-42', 'R1', 'E1', 'R2']],
    };
    for (const [name, rows] of Object.entries(expected)) {
      await page.locator('[data-scenario=' + name + ']').click();
      while (await page.locator('#next').isEnabled()) await page.locator('#next').click();
      const observed = await page.locator('#events tr').evaluateAll(elements => elements.map(row => Array.from(row.cells, cell => cell.textContent)));
      assert.deepEqual(observed, rows, name);
      const user = await page.locator('#authority dd').first().textContent();
      assert.equal(user, 'Absent', name + ' does not reconstruct verified user');
      await page.screenshot({ path: path.join(directory, name + '.png'), fullPage: true });
    }
    await page.locator('#owner').selectOption('envelope');
    assert.match(await page.locator('#ownerDetail').textContent(), /explicit argument/);
    await page.locator('#owner').selectOption('request');
    assert.match(await page.locator('#ownerDetail').textContent(), /existing EventMetadata/);
    await page.setViewportSize({ width: 390, height: 844 });
    assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth), true);
    await page.screenshot({ path: path.join(directory, 'mobile.png'), fullPage: true });
    assert.deepEqual(errors, []);
    console.log('PASS 5 walkthroughs, 2 context selectors, no restored claims, mobile width, no page errors');
  } finally {
    await browser.close();
  }
}

main().catch(error => { console.error(error); process.exitCode = 1; });
