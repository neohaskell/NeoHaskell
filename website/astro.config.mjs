// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

// https://astro.build/config
export default defineConfig({
	site: 'https://neohaskell.org',
	integrations: [
		starlight({
			title: 'NeoHaskell',
			description:
				'Build understandable applications with events, a visual model, and your coding agent.',
			social: [
				{
					icon: 'github',
					label: 'GitHub',
					href: 'https://github.com/neohaskell/NeoHaskell',
				},
			],
			// English is the source locale (content lives at the docs root); the
			// non-English locales are kept in sync by .github/workflows/translate.yml.
			// Pages missing a translation fall back to the English source — which is
			// why the canonical-English ADR records render everywhere without being
			// duplicated per locale.
			defaultLocale: 'root',
			locales: {
				root: { label: 'English', lang: 'en' },
				es: { label: 'Español', lang: 'es' },
				fr: { label: 'Français', lang: 'fr' },
				hy: { label: 'Հայերեն', lang: 'hy' },
				ja: { label: '日本語', lang: 'ja' },
				ru: { label: 'Русский', lang: 'ru' },
			},
			sidebar: [
                { label: 'Start and evaluate', items: [{ autogenerate: { directory: 'start' } }] },
                { label: 'Get started', items: [{ autogenerate: { directory: 'getting-started' } }] },
                { label: 'Build applications', collapsed: true, items: [{ autogenerate: { directory: 'build' } }] },
                { label: 'Connect systems', collapsed: true, items: [{ autogenerate: { directory: 'connect' } }] },
                { label: 'Operate and evolve', collapsed: true, items: [{ autogenerate: { directory: 'operate' } }] },
                { label: 'Reference', collapsed: true, items: [{ autogenerate: { directory: 'reference' } }] },
				{
					label: 'Architecture Decision Records',
					// Collapsible group, folded by default — the 70 records shouldn't
					// crowd the sidebar until the reader opens the section. Starlight
					// still auto-expands the group when the current page is one of the
					// ADRs, so a record is never hidden from its own page.
					collapsed: true,
					translations: {
						es: 'Registros de Decisiones Arquitectónicas',
						fr: 'Dossiers de Décisions Architecturales',
						hy: 'Կառուցվածքային Որոշումների Ձայնագրություններ',
						ja: 'アーキテクチャ決定記録',
						ru: 'Записи об архитектурных решениях',
					},
					// Every page under src/content/docs/adrs/ is surfaced as a nested
					// entry by autogenerate. Nothing is listed by hand: adding an ADR to
					// docs/decisions/ regenerates its page before dev/build/check and it
					// appears here automatically.
					items: [{ autogenerate: { directory: 'adrs' } }],
				},
			],
		}),
	],
});
