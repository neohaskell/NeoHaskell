# Laravel Documentation Analysis

## Navigation Structure

Laravel uses a **three-level sidebar hierarchy** that's become the industry standard for framework documentation:

**Top Level (12 sections):**
1. Prologue (3 pages: Release Notes, Upgrade Guide, Contribution Guide)
2. Getting Started (7 pages: Installation through Deployment)
3. Architecture Concepts (4 pages: Request Lifecycle, Service Container, Service Providers, Facades)
4. The Basics (14 pages: Routing through Logging)
5. Digging Deeper (21 pages: Artisan through Task Scheduling)
6. Security (6 pages: Authentication through Password Reset)
7. Database (7 pages: Getting Started through MongoDB)
8. Eloquent ORM (7 pages: Getting Started through Factories)
9. AI (3 pages: AI SDK, MCP, Boost) — **new in v12**
10. Testing (6 pages: Getting Started through Mocking)
11. Packages (23 first-party packages, each with dedicated page)
12. API Documentation (external link to api.laravel.com)

**Version Selector:** Dropdown at top with 14 versions (Master, 12.x down to 4.2). Each version has its own complete documentation tree. Version 11.x shows a warning banner: "You're browsing the documentation for an old version of Laravel. Consider upgrading your project to Laravel 12.x."

**Depth:** Maximum 3 levels. Example: `Eloquent ORM > Getting Started > [page sections]`. Sidebar never goes deeper than this.

**Persistent Elements:**
- Search (⌘K shortcut)
- Version switcher
- "On this page" right sidebar with anchor links to H2/H3 headings
- Breadcrumb trail at top of content

## Page Types

**1. Installation/Getting Started Pages**
- Multiple installation paths (Herd, Sail/Docker, manual PHP)
- Platform-specific tabs (macOS, Windows, Linux)
- Code blocks with copy buttons
- "Next Steps" section linking to two primary use cases (full-stack vs API backend)
- Example: `/docs/12.x/installation` has tabs for macOS/Windows/Linux install commands

**2. Feature Reference Pages**
- Comprehensive single-page coverage of one feature
- Deep table of contents in right sidebar (10-20 anchor links)
- Code examples every 2-3 paragraphs
- Callout boxes for warnings, tips, notes
- Example: `/docs/11.x/eloquent` covers models, conventions, retrieval, CRUD, events, observers in one scrollable page

**3. Architecture Concept Pages**
- Shorter, focused on "why" not just "how"
- Diagrams or conceptual explanations
- Links to related feature pages
- Example: "Request Lifecycle" explains the flow, then links to routing, middleware, controllers

**4. Package Pages**
- Dedicated page per first-party package (Cashier, Horizon, Telescope, etc.)
- Installation, configuration, usage
- Often link to separate package-specific docs sites
- Example: Jetstream links to jetstream.laravel.com

**5. Upgrade Guides**
- Version-specific breaking changes
- Estimated upgrade time
- Step-by-step migration instructions
- Organized by impact level (high, medium, low)

## Content Patterns

**Code Example Density:** Every 2-3 paragraphs. Laravel shows both the code AND the result/output when relevant.

**Tabbed Code Blocks:** Used for platform differences (macOS/Windows/Linux) or alternative approaches. Example from installation page:
```
macOS | Windows PowerShell | Linux
[different install commands for each]
```

**Callout Boxes:**
- **Warning** (red): Breaking changes, security concerns
- **Note** (blue): Important context, gotchas
- **Tip** (green): Best practices, performance hints
- Example: "You should not define closures in your configuration files. They cannot be serialized correctly when users execute the config:cache Artisan command."

**Version Notices:** Inline badges for feature availability. Example: "New in Laravel 11" or "Deprecated in Laravel 12."

**Progressive Disclosure:** Start simple, then show advanced usage. Eloquent page starts with `Flight::all()`, then query builder methods, then chunking, cursors, lazy collections.

**Artisan Command Examples:** Always show the full command with output when relevant:
```bash
php artisan make:model Flight --migration
# Output shown or explained
```

**Real-World Context:** Installation page mentions Herd, Valet, Sail by name with links. Doesn't hide the ecosystem.

**Handling Massive Scope:** Laravel has 100+ features. Their approach:
1. **Grouping by concern** (Security, Database, Testing)
2. **One feature = one page** (Eloquent Relationships gets its own page, separate from Eloquent Getting Started)
3. **Cross-linking aggressively** (every page links to 5-10 related pages)
4. **"Digging Deeper" section** for advanced/less-common features

**AI Integration (v12):** New "Agentic Development" page in Getting Started. Laravel Boost package gets dedicated coverage. Shows framework adapting to AI-assisted development as a first-class concern.

## Progression Model

**Not strictly linear.** Laravel offers **two entry paths** based on use case:

**Path 1: Full-Stack Framework**
1. Installation
2. Configuration
3. Directory Structure
4. Routing → Views → Blade → Controllers
5. Database → Eloquent
6. Authentication (via starter kits)
7. Deployment

**Path 2: API Backend**
1. Installation
2. Routing
3. Eloquent
4. API Resources
5. Sanctum (authentication)
6. Deployment

**"Getting Started" section** is linear (Installation → Configuration → Directory Structure → Frontend → Starter Kits → Deployment).

**After that:** Docs assume you'll jump around based on what you're building. The sidebar groupings (The Basics, Digging Deeper, Security, Database) are organized by **concern**, not learning order.

**Breadth Handling:** Laravel doesn't try to teach everything upfront. Instead:
- **Starter Kits** (Breeze, Jetstream) give you a working app with auth, then you explore from there
- **Laravel Bootcamp** (external, linked prominently) is the tutorial-first path
- Docs are reference-first, tutorial-second

**Search-Driven Discovery:** The ⌘K search is prominent. Assumption: developers will search for "validation" or "queues" when they need it, not read docs cover-to-cover.

## Standout Features

**1. Version Switcher That Actually Works**
- 14 versions maintained simultaneously
- Each version has complete docs (not just a changelog)
- URL structure: `/docs/{version}/{page}` makes linking to specific versions trivial
- Warning banners on old versions nudge upgrades without blocking access

**2. Multi-Path Installation**
- Herd (native macOS/Windows app)
- Sail (Docker)
- Manual PHP install
- Each path gets equal treatment, not "recommended" vs "advanced"
- Platform-specific tabs (macOS/Windows/Linux) in code blocks

**3. Code Quality in Examples**
- Real variable names (`$flight`, not `$foo`)
- Realistic use cases (Flight model, not User/Post)
- Shows both simple and advanced usage on same page
- Copy buttons on every code block

**4. Aggressive Cross-Linking**
- Every page links to 5-10 related pages
- "See also" sections
- Inline links mid-paragraph to related concepts
- External links to ecosystem packages (Livewire, Inertia) treated as first-class

**5. First-Party Package Integration**
- 23 official packages, each with dedicated page
- Packages feel like core features (Horizon, Telescope, Sanctum)
- Clear distinction: some packages link to external docs sites, others are fully documented inline

**6. Search Experience**
- ⌘K shortcut (industry standard now, Laravel was early)
- Instant results
- Searches across all versions (can filter by version)
- Shows page title + section heading in results

**7. "On This Page" Sidebar**
- Right sidebar with anchor links to all H2/H3 headings
- Sticky scroll
- Current section highlighted
- Makes long reference pages (like Eloquent) navigable

**8. Upgrade Guides as First-Class Content**
- Not buried in release notes
- Organized by impact level
- Estimated time to upgrade
- Step-by-step instructions with code diffs

**9. AI-First Mindset (v12)**
- "Agentic Development" page in Getting Started
- Laravel Boost package for AI agents
- Framework conventions designed for AI code generation
- Quote: "Laravel's opinionated conventions and well-defined structure make it an ideal framework for AI assisted development"

**10. Ecosystem Transparency**
- Doesn't hide third-party tools (Livewire, Inertia, Pest)
- Links to Laracasts (video tutorials)
- Links to Laravel Bootcamp (separate tutorial site)
- "Community Framework" messaging

## Applicable to NeoHaskell

**1. Version Switcher (Critical for NeoHaskell)**
- Astro Starlight supports versioning out of the box
- Implement `/docs/{version}/{page}` URL structure from day one
- Even pre-1.0, use version tags (0.1, 0.2, etc.) so users can reference stable docs
- Add warning banners on old versions: "You're viewing docs for NeoHaskell 0.3. Consider upgrading to 0.5."

**2. Sidebar Structure (Adopt Laravel's Model)**
```
Getting Started (linear)
  - Installation
  - First Steps
  - Your First App

Core Concepts (non-linear, by concern)
  - Type System
  - Effects
  - Modules

Building Apps (feature reference)
  - Web Servers
  - Database Access
  - JSON Handling

Advanced (less common features)
  - Metaprogramming
  - FFI
  - Performance Tuning

Ecosystem (packages, tools)
  - Package List
  - Editor Setup
  - Deployment
```

**3. Multi-Path Installation**
- NeoHaskell should offer:
  - **Haskell developers:** "Install via GHCup, add NeoHaskell"
  - **New developers:** "Download NeoHaskell installer (includes GHC)"
  - **Docker:** "Use official NeoHaskell image"
- Use tabbed code blocks for platform differences (macOS/Linux/Windows)

**4. One Feature = One Page**
- Don't combine "Effects" and "Effect Handlers" on one page
- Each major concept gets its own URL
- Deep table of contents in right sidebar (Starlight supports this)

**5. Code Example Density**
- Every 2-3 paragraphs, show code
- Use realistic examples (not `foo`/`bar`)
- Show both simple and advanced usage on same page
- Example: "Type System" page starts with `Int`, ends with GADTs

**6. Search-First Docs**
- Assume developers will search, not read linearly
- Optimize page titles and headings for search
- Example: "How do I parse JSON?" should find "JSON Parsing" page

**7. Callout Boxes**
- **Warning:** "NeoHaskell's type inference can't handle this pattern. Add a type signature."
- **Note:** "This feature requires GHC 9.6 or higher."
- **Tip:** "Use `deriving` instead of manual instances for better performance."

**8. Ecosystem Transparency**
- Link to Haskell packages (aeson, servant, etc.) as first-class
- Don't hide that NeoHaskell builds on GHC
- Show how to use existing Haskell libraries

**9. Progressive Disclosure**
- Start with "Hello World" web server
- Then routing
- Then JSON
- Then database
- Then authentication
- Don't front-load type theory

**10. "Next Steps" Sections**
- End of installation page: "Build a web API" vs "Build a CLI tool"
- Two paths based on use case, like Laravel's full-stack vs API split

**What NOT to Take from Laravel:**
- **Don't copy the breadth.** Laravel has 100+ features because it's 15 years old. NeoHaskell should start focused.
- **Don't over-version early.** Wait until 0.5 or 1.0 to maintain multiple doc versions.
- **Don't assume PHP knowledge.** Laravel assumes you know PHP. NeoHaskell can't assume Haskell knowledge.

**Immediate Action Items:**
1. Set up Starlight versioning (even if only one version exists)
2. Create sidebar structure with Getting Started (linear) + Core Concepts (non-linear)
3. Add "On this page" right sidebar to all reference pages
4. Write installation page with multiple paths (GHCup, installer, Docker)
5. Add search (Starlight includes this by default)
6. Create "Your First NeoHaskell App" tutorial page (like Laravel Bootcamp)
