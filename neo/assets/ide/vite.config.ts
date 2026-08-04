/// <reference types="vitest" />
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// Styling is centralized in Mantine (see src/theme.ts and assets/ide/CLAUDE.md).
// Tailwind was removed; the PostCSS pipeline (postcss.config.cjs) handles
// Mantine's CSS. No Tailwind plugin here by design.
export default defineConfig({
  plugins: [react()],
  test: {
    globals: true,
    environment: 'jsdom',
    setupFiles: './src/test/setup.ts',
    css: true,
  },
})
