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
			sidebar: [
				{ label: 'Getting Started', link: '/getting-started/' },
				{ label: 'Architecture Decision Records', link: '/adrs/' },
			],
		}),
	],
});
