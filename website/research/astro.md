# Astro Documentation Analysis

## Navigation Structure

Astro's docs use Starlight's multi-level sidebar with **tabbed top-level navigation** and **collapsible sections**:

### Top-level tabs (visible on every page):
- **Start** (getting started, tutorials, installation)
- **Guides and recipes** (how-to content)
- **Reference** (API docs, configuration)
- **Integrations** (framework adapters, official integrations)
- **Third-party services** (deployment, CMS, backend)

### Sidebar hierarchy within tabs:
Each tab contains collapsible sections. Example from "Start" tab:
- **Welcome, world!** (Why Astro?, Islands architecture, Tutorial, Courses)
- **Start a new project** (Installation, Project structure, Develop and build)
- **Configuration** (Configuration overview, Editor setup, TypeScript, Environment variables, Build with AI, Dev toolbar)
- **Migrate to Astro** (Site migration overview, then 15+ migration guides from specific platforms)

### Starlight features in use:
- **Collapsible sidebar groups** (all major sections collapse/expand)
- **Multi-tab navigation** (5 top-level tabs organize hundreds of pages)
- **Breadcrumb navigation** (visible on all pages)
- **Language selector** (12+ languages: English, Deutsch, Português do Brasil, Español, 简体中文, 正體中文, Français, हिन्दी, العربية, 日本語, 한국어, Polski, Русский, Italiano)
- **Theme toggle** (Dark/Light/Auto)
- **Search** (prominent search bar in header)
- **GitHub/Discord links** (persistent in header)
- **On this page** (right sidebar with page section links)

## Page Types

Astro's docs have **5 distinct page types**, each with specific structure:

### 1. Tutorial pages
- **Tutorial Tracker** component (left sidebar showing progress: "Unit 0 (0/2 Complete)")
- Checkbox-based progression (users can mark lessons complete)
- Sequential navigation (Previous/Next links)
- Multi-unit structure (6 units, each with 4-6 lessons)
- Example: `/en/tutorial/0-introduction/`

### 2. Guide pages
- Standard prose with code examples
- Tabbed code blocks (npm/pnpm/Yarn alternatives)
- Aside boxes for notes, tips, warnings
- "On this page" navigation (right sidebar)
- Example: `/en/guides/deploy/`

### 3. Reference pages
- Dense, structured API documentation
- Extensive use of anchor links (every config option is linkable)
- Type signatures and defaults clearly marked
- Nested sections (e.g., "Build Options" contains 9 sub-options)
- Example: `/en/reference/configuration-reference/`

### 4. Landing/overview pages
- Card-based layouts (deployment guides show 30+ cards with logos)
- Visual hierarchy (icons, badges like "On demand" and "Static")
- Multiple entry points (Website UI vs CLI Deployment)
- Example: `/en/guides/deploy/`

### 5. Concept pages
- Explanatory content with diagrams
- Less code-heavy than guides
- Focus on "why" rather than "how"
- Example: `/en/concepts/why-astro/`

## Content Patterns

Astro uses **specific Starlight components** throughout their docs:

### Aside boxes (callouts)
Three types visible in the content:
- **Note** (blue, informational)
- **Tip** (green, helpful suggestions)
- **Caution** (yellow/orange, warnings)

Example from tutorial page:
```markdown
Note

If you would rather start exploring Astro with a pre-built Astro site, you can visit https://astro.new and choose a starter template to open and edit in an online editor.
```

### Tabbed code blocks
Used extensively for package manager alternatives:
```markdown
- [npm](#tab-panel-2783)
- [pnpm](#tab-panel-2784)
- [Yarn](#tab-panel-2785)

Terminal window
npm install --global netlify-cli

Terminal window
pnpm add --global netlify-cli

Terminal window
yarn global add netlify-cli
```

### Code blocks with labels
Every code block has a label showing the file path or context:
- `astro.config.mjs`
- `Terminal window`
- `src/content.config.ts`
- `src/pages/blog.astro`

### Inline code highlighting
Type signatures and config options use inline code:
- **Type:** `'static' | 'server'`
- **Default:** `'static'`
- **Added in:** `astro@2.0.0`

### Card grids
Deployment guides page uses a card grid with:
- Logo images (`/logos/netlify.svg`)
- Service name
- Badges ("On demand", "Static")
- Link to guide

### Checklists
Tutorial uses interactive checklists:
```markdown
## Checklist
- [ ] Looks great! I'm ready to get started!
```

### Sponsored content
Persistent sponsor logos at top and bottom of pages (Netlify, Webflow, Cloudflare, Mux)

### Learn/course promotions
Sidebar ads for courses:
- Scrimba course with James Q Quick
- "Learn Astro with Coding in Public" (150+ video lessons)
- Discount codes included

## Progression Model

### Tutorial structure (6 units, 28 lessons total):
**Unit 0: Welcome, world!** (2 lessons)
- Build your first Astro Blog
- About this Tutorial

**Unit 1: Create and deploy your first Astro site** (6 lessons)
- Check in: Unit 1 - Setup
- Prepare your dev environment
- Create your first Astro project
- Write your first line of Astro
- Store your repository online
- Deploy your site to the web

**Unit 2: Add, style and link to pages on your site** (6 lessons)
- Check in: Unit 2 - Pages
- Create your first Astro page
- Write your first Markdown blog post
- Add dynamic content about you
- Style your About page
- Add site-wide styling

**Unit 3: Build and design with Astro UI components** (5 lessons)
- Check in: Unit 3 - Components
- Make a reusable Navigation component
- Create a social media footer
- Build it yourself - Header
- Send your first script to the browser

**Unit 4: Save time and energy with reusable page layouts** (4 lessons)
- Check in: Unit 4 - Layouts
- Build your first layout
- Create and pass data to a custom blog layout
- Combine layouts to get the best of both worlds

**Unit 5: Beef up your blog** (5 lessons)
- Check in: Unit 5 - Astro API
- Create a blog post archive
- Generate tag pages
- Build a tag index page
- Add an RSS feed

**Unit 6: Set sail for Astro islands** (5 lessons)
- Check in: Unit 6 - Astro Islands
- Build your first Astro island
- Back on dry land. Take your blog from day to night, no island required!
- Congratulations!
- Optional: Make a content collection

### Progression principles:
1. **Incremental complexity** (start with static pages, end with islands/interactivity)
2. **Check-in pages** (every unit starts with a summary/checkpoint)
3. **Hands-on building** (every lesson builds the same blog project)
4. **Optional advanced topics** (Unit 6 ends with optional content collections lesson)
5. **Completion tracking** (visual progress: "Unit 0 (0/2 Complete)")

### Guide organization:
Guides are organized by **task category**, not by difficulty:
- **Routing and navigation** (7 guides)
- **Build your UI** (7 guides)
- **Add content to your site** (5 guides)
- **Server rendering** (4 guides)
- **Upgrade** (6 guides, including version-specific migration guides)
- **How-to recipes** (20+ short, focused recipes)

## Standout Features

### Starlight-specific features Astro leverages:

1. **Multi-language support (i18n)**
   - 12+ languages available
   - Language selector in header
   - URL structure: `/en/`, `/es/`, `/ru/`, etc.
   - Automatic language detection

2. **Built-in search**
   - Prominent search bar in header
   - Keyboard shortcut support
   - Full-text search across all pages

3. **Theme switching**
   - Dark/Light/Auto modes
   - Persistent across sessions
   - Accessible via header toggle

4. **Responsive sidebar**
   - Collapsible on mobile
   - Persistent scroll position
   - Active page highlighting

5. **Tabbed navigation**
   - 5 top-level tabs organize content
   - Each tab has independent sidebar
   - Reduces cognitive load (only see relevant sections)

6. **Edit page links**
   - Every page has "Edit page" link to GitHub
   - "Translate this page" link to contribution guide
   - Encourages community contributions

7. **Breadcrumb navigation**
   - Shows current location in hierarchy
   - Clickable path back to parent pages

8. **Social links**
   - GitHub and Discord links in header
   - Persistent across all pages
   - Community engagement focus

9. **Sponsor integration**
   - Sponsor logos at top and bottom
   - Non-intrusive placement
   - Supports project sustainability

10. **Course/learning promotions**
    - Sidebar ads for official courses
    - Discount codes included
    - Monetization without disrupting docs

### Content-specific features:

1. **Tutorial tracker component**
   - Custom Starlight component
   - Shows progress across all units
   - Checkbox-based completion
   - Persistent state (likely localStorage)

2. **Deployment guide cards**
   - Visual grid of 30+ deployment options
   - Logo images for each service
   - Badges showing capabilities ("On demand", "Static")
   - Filterable/searchable

3. **Migration guides**
   - 15+ platform-specific migration guides
   - Consistent structure across all guides
   - Helps users switch from competitors

4. **Version-specific upgrade guides**
   - Separate guide for each major version (v1.0 through v5.0)
   - Breaking changes clearly documented
   - Migration steps provided

5. **Recipes section**
   - 20+ short, focused how-to guides
   - Single-purpose solutions
   - Quick reference for common tasks

## Applicable to NeoHaskell

**CRITICAL: Astro's docs are built with Starlight, the SAME platform NeoHaskell uses.** Every feature, component, and organizational pattern is directly applicable.

### Immediate actions for NeoHaskell:

1. **Use Starlight's tabbed navigation**
   - Organize docs into: Start, Guides, Reference, Ecosystem
   - Reduces sidebar clutter
   - Improves discoverability

2. **Leverage Starlight components**
   - **Aside boxes** for notes, tips, cautions (already available)
   - **Tabbed code blocks** for Stack/Cabal alternatives
   - **Card grids** for showcasing libraries, tools, examples
   - **Code block labels** showing file paths

3. **Enable i18n support**
   - Starlight has built-in i18n
   - Configure languages in `astro.config.mjs`
   - Use Astro's translation workflow

4. **Create a tutorial with progress tracking**
   - Build a "Build your first NeoHaskell app" tutorial
   - Use Starlight's custom components for progress tracking
   - 4-6 units, 20-30 lessons total

5. **Add deployment guides**
   - Create card grid of deployment options
   - Include logos and badges
   - Cover Fly.io, Railway, Heroku, Docker, etc.

6. **Organize guides by task category**
   - Not by difficulty or alphabetically
   - Categories: "Build your app", "Add features", "Deploy", "Integrate with services"

7. **Use consistent code block labeling**
   - Show file paths: `src/Main.hs`, `package.yaml`
   - Show context: `Terminal window`, `REPL session`

8. **Add "Edit page" and "Translate this page" links**
   - Starlight provides these out of the box
   - Configure GitHub repo in `astro.config.mjs`

9. **Create a recipes section**
   - Short, focused how-to guides
   - "Add authentication", "Connect to PostgreSQL", "Deploy to Fly.io"
   - Single-purpose solutions

10. **Use Starlight's search**
    - Already enabled by default
    - Configure search options in `astro.config.mjs`
    - No additional setup needed

### Starlight configuration examples for NeoHaskell:

```javascript
// astro.config.mjs
export default defineConfig({
  integrations: [
    starlight({
      title: 'NeoHaskell Docs',
      social: {
        github: 'https://github.com/neohaskell/neohaskell',
        discord: 'https://discord.gg/neohaskell',
      },
      sidebar: [
        {
          label: 'Start',
          items: [
            { label: 'Why NeoHaskell?', link: '/start/why-neohaskell/' },
            { label: 'Installation', link: '/start/installation/' },
            { label: 'Tutorial', link: '/start/tutorial/' },
          ],
        },
        {
          label: 'Guides',
          items: [
            { label: 'Build your app', link: '/guides/build/' },
            { label: 'Add features', link: '/guides/features/' },
            { label: 'Deploy', link: '/guides/deploy/' },
          ],
        },
        {
          label: 'Reference',
          items: [
            { label: 'API Reference', link: '/reference/api/' },
            { label: 'Configuration', link: '/reference/config/' },
          ],
        },
      ],
      locales: {
        root: {
          label: 'English',
          lang: 'en',
        },
        es: {
          label: 'Español',
          lang: 'es',
        },
      },
      editLink: {
        baseUrl: 'https://github.com/neohaskell/website/edit/main/',
      },
    }),
  ],
});
```

### Content patterns to adopt:

1. **Aside boxes** (Starlight component):
```markdown
:::note
NeoHaskell uses Stack by default, but you can also use Cabal.
:::

:::tip
Use `neo new` to scaffold a new project with best practices.
:::

:::caution
This feature is experimental and may change in future versions.
:::
```

2. **Tabbed code blocks** (Starlight component):
```markdown
<Tabs>
  <TabItem label="Stack">
    ```bash
    stack build
    ```
  </TabItem>
  <TabItem label="Cabal">
    ```bash
    cabal build
    ```
  </TabItem>
</Tabs>
```

3. **Card grids** (Starlight component):
```markdown
<CardGrid>
  <Card title="Web Framework" icon="rocket">
    Build web apps with NeoHaskell's batteries-included framework.
  </Card>
  <Card title="CLI Tools" icon="terminal">
    Create command-line tools with ease.
  </Card>
</CardGrid>
```

4. **Code block labels**:
```markdown
```haskell title="src/Main.hs"
module Main where

main :: IO ()
main = putStrLn "Hello, NeoHaskell!"
```
```

### Organizational patterns to copy:

1. **Tutorial structure**:
   - Unit 0: Welcome (2 lessons)
   - Unit 1: Setup (4-6 lessons)
   - Unit 2: Core concepts (4-6 lessons)
   - Unit 3: Building features (4-6 lessons)
   - Unit 4: Advanced topics (4-6 lessons)

2. **Guide categories**:
   - Getting started
   - Core concepts
   - Building features
   - Deployment
   - Integrations
   - Recipes

3. **Reference structure**:
   - API reference (auto-generated from Haddock)
   - Configuration reference
   - CLI reference
   - Error reference

4. **Migration guides**:
   - From vanilla Haskell
   - From Yesod
   - From Servant
   - From Scotty

### What NOT to copy:

1. **Sponsor logos** (unless NeoHaskell has sponsors)
2. **Course promotions** (unless there are official courses)
3. **Excessive migration guides** (only create if there's demand)
4. **Version-specific upgrade guides** (NeoHaskell is pre-1.0, not needed yet)

### Key takeaway:

**Astro's docs prove that Starlight can handle large, complex documentation sites with hundreds of pages.** NeoHaskell should leverage Starlight's built-in features (tabs, i18n, search, theme switching, aside boxes, card grids) rather than building custom solutions. The platform is already powerful enough to support world-class documentation.
