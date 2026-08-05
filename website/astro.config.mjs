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
				'A newcomer-friendly dialect of Haskell with compiler-enforced event sourcing.',
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
				{
					label: 'Getting Started',
					link: '/getting-started/',
					translations: {
						es: 'Comenzando',
						fr: 'Commencer',
						hy: 'Սկսել',
						ja: 'はじめに',
						ru: 'Начало работы',
					},
				},
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
