# ADR-0078: Enlarge documentation images in a Starlight dialog

## Status

Accepted

## Context

Diagrams and IDE screenshots carry much of the documentation's explanation.
Opening the image file directly leaves the lesson and its reading position.
The maintainer requested an Astro Starlight plugin that enlarges images in place.

## Decision

Use [starlight-image-zoom](https://github.com/HiDeoo/starlight-image-zoom), pinned
to 0.15.0, whose declared peer range includes the website's Starlight 0.41.3.
Version 0.16.0 requires Starlight 0.42.0 or newer; upgrading the documentation
framework is separate work. The package and its resolved dependencies live in
the existing pnpm lockfile.

Configure Astro's supported Unified Markdown processor through a direct, pinned
`@astrojs/markdown-remark` 7.2.1 dependency (already resolved by the existing
Starlight installation). Image Zoom 0.15 uses rehype and cannot process the default
Sätteri pipeline. This keeps the existing Astro/Starlight versions while using the
plugin's supported integration. The full site build and rendered-link checks cover
the renderer change; future upgrades can reconsider Sätteri when all versions align.

Register the plugin in Starlight's configuration. Render illustrations as ordinary
Markdown images, since linked images are intentionally excluded by the plugin.
Keep editable Draw.io downloads as separate links. Use the plugin's dialog,
keyboard triggers, captions, Escape dismissal, and focus restoration rather than
maintaining our own lightbox implementation. Without JavaScript, the ordinary
inline image and its alternative text remain available.

## Consequences

- Clicking an illustration enlarges it without navigating away from the lesson.
- The plugin owns dialog behaviour; CSS preserves readable diagram backgrounds.
- Dependency updates must respect the installed Starlight version.
- Built-page checks require a zoom controller and accessible trigger for every
  documentation diagram and screenshot. Browser review checks opening, dismissal,
  keyboard use, and preservation of the current page and scroll position.
- No application runtime or public API changes.
