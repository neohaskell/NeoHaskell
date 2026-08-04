import { defineConfig } from '@playwright/test'
import { existsSync } from 'node:fs'

// Drive Brave (Chromium-based) via executablePath. Brave is not a Playwright
// "channel", so we point at its binary directly. Override with BRAVE_PATH.
const BRAVE_CANDIDATES = [
  process.env.BRAVE_PATH,
  '/tmp/brave-app/Brave Browser.app/Contents/MacOS/Brave Browser',
  '/Applications/Brave Browser.app/Contents/MacOS/Brave Browser',
].filter((p): p is string => Boolean(p))

const bravePath = BRAVE_CANDIDATES.find((p) => existsSync(p))
if (!bravePath) {
  console.warn(
    '[playwright] Brave not found — falling back to bundled Chromium. Set BRAVE_PATH to use Brave.',
  )
}

export default defineConfig({
  testDir: './e2e',
  testMatch: '**/*.e2e.ts',
  timeout: 30_000,
  expect: { timeout: 10_000 },
  fullyParallel: false,
  workers: 1,
  reporter: [['list']],
  use: {
    baseURL: 'http://localhost:5174',
    headless: true,
    viewport: { width: 1400, height: 900 },
    screenshot: 'only-on-failure',
    trace: 'retain-on-failure',
    browserName: 'chromium',
    launchOptions: bravePath ? { executablePath: bravePath } : {},
  },
  webServer: {
    command: 'npm run preview -- --port 5174 --strictPort',
    port: 5174,
    reuseExistingServer: true,
    timeout: 60_000,
  },
})
