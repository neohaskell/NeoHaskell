// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';
import starlightLlmsTxt from 'starlight-llms-txt';

// https://astro.build/config
export default defineConfig({
	site: 'https://neohaskell.org',
	integrations: [
		starlight({
			title: 'NeoHaskell',
			plugins: [
				starlightLlmsTxt({
					projectName: 'NeoHaskell',
					description:
						'NeoHaskell is a dialect of Haskell designed for beginners and pragmatists. It features compiler-enforced event sourcing — events, entities, commands, queries, services, and integrations are language primitives, not library patterns. Incorrect state transitions do not compile.',
					details: [
						'## Key concepts for LLMs working with NeoHaskell code',
						'',
						'NeoHaskell uses its own terminology that differs from standard DDD/event-sourcing literature:',
						'- **Entity** (not "aggregate") — the stateful domain object that folds events into current state',
						'- **Update** (not "projection") — the pure function that folds an event into entity state',
						'- **Query** (not "read model" or "ReadModel") — uses the `QueryOf` class to derive read-optimized views from events',
						'- **Service** (not "bounded context") — a self-contained domain boundary defined with `withService`',
						'- **Integration** (not "process manager" or "saga") — cross-service coordination defined with `withOutbound`',
						'',
						'NeoHaskell uses do-notation by default. Do not suggest standard Haskell idioms (like monad transformers, lens, or Template Haskell) — NeoHaskell intentionally avoids these.',
						'',
						'Event versioning is not supported. If a schema needs to change, create a new event type. Events are immutable facts.',
					].join('\n'),
					promote: ['index*', 'concepts/why-neohaskell', 'getting-started/**', 'tutorial/**'],
					exclude: ['adrs/**'],
				}),
			],
			defaultLocale: 'root',
			locales: {
				root: { label: 'English', lang: 'en' },
				// Non-English locales are temporarily disabled during the docs revamp.
				// They will be re-enabled once the translation pipeline rebuilds
				// translations from the new English source.
			},
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/neohaskell/neohaskell' }],
			sidebar: [
				{ slug: 'concepts/why-neohaskell', label: 'Why NeoHaskell?' },
				{
					label: 'Getting Started',
					items: [
						'getting-started/installation',
						'getting-started/first-events',
						'getting-started/reading-neohaskell',
						'getting-started/syntax-warmup',
						'getting-started/using-ai',
						'getting-started/cheat-sheet',
					],
				},
				{
					label: 'Tutorial: Build NeoBank',
					items: [
						{ slug: 'tutorial', label: 'Introduction' },
						'tutorial/01-first-transaction',
						'tutorial/02-account-rules',
						'tutorial/03-transaction-history',
						'tutorial/04-multiple-accounts',
						'tutorial/05-transfers',
						'tutorial/06-audit-everything',
						'tutorial/whats-next',
					],
				},
				{
					label: 'Core Concepts',
					collapsed: true,
					autogenerate: { directory: 'concepts' },
				},
				{
					label: 'Guides',
					collapsed: true,
					autogenerate: { directory: 'guides' },
				},
				{
					label: 'Coming From...',
					collapsed: true,
					autogenerate: { directory: 'coming-from' },
				},
				{
					label: 'Reference',
					collapsed: true,
					autogenerate: { directory: 'reference' },
				},
				{
					label: 'ADRs',
					collapsed: true,
					autogenerate: { directory: 'adrs' },
				},
			],
		}),
	],
});
