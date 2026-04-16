import { defineCollection } from 'astro:content';
import { glob } from 'astro/loaders';
import { docsSchema } from '@astrojs/starlight/schema';

export const collections = {
	docs: defineCollection({
		// Stale auto-translated locale directories are temporarily excluded.
		// They will be regenerated from the new English content by the
		// translation pipeline after the docs revamp lands.
		loader: glob({
			base: './src/content/docs',
			pattern: ['**/*.{md,mdx}', '!{es,fr,hy,ja,ru}/**/*'],
		}),
		schema: docsSchema(),
	}),
};
