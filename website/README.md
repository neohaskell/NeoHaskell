# NeoHaskell Documentation

Documentation website for [NeoHaskell](https://github.com/neohaskell/neohaskell), built with [Astro Starlight](https://starlight.astro.build/).

## Development

```bash
pnpm install
pnpm run dev
```

Open [localhost:4321](http://localhost:4321) to view the site.

## Build

```bash
pnpm run build
pnpm run preview
```

## Content

Documentation source files live in `src/content/docs/`. Only edit the English source files — translations are auto-generated into locale subdirectories (e.g., `es/`, `ru/`).

### Sections

| Directory | Content |
|-----------|---------|
| `getting-started/` | Setup and first steps |
| `concepts/` | Core NeoHaskell concepts |
| `guides/` | Step-by-step tutorials |
| `reference/` | API reference |
| `adrs/` | Architecture Decision Records |

## Translations

Translations to Spanish, Russian, Armenian, French, and Japanese are handled automatically by a GitHub Action on push to `main`. Do not edit translation files manually.

## License

See the [NeoHaskell repository](https://github.com/neohaskell/neohaskell) for license information.
