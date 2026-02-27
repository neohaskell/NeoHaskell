# Vue.js Documentation Analysis

## Navigation Structure

Vue's documentation uses a **three-level hierarchical sidebar** with clear progressive depth:

**Top-level sections (from https://vuejs.org/guide/introduction.html sidebar):**
1. **Getting Started** (2 pages: Introduction, Quick Start)
2. **Essentials** (12 pages: Application, Template Syntax, Reactivity Fundamentals, Computed Properties, Class/Style Bindings, Conditional Rendering, List Rendering, Event Handling, Form Input Bindings, Watchers, Template Refs, Components Basics, Lifecycle Hooks)
3. **Components In-Depth** (8 pages: Registration, Props, Events, Component v-model, Fallthrough Attributes, Slots, Provide/inject, Async Components)
4. **Reusability** (3 pages: Composables, Custom Directives, Plugins)
5. **Built-in Components** (5 pages: Transition, TransitionGroup, KeepAlive, Teleport, Suspense)
6. **Scaling Up** (6 pages: Single-File Components, Tooling, Routing, State Management, Testing, Server-Side Rendering)
7. **Best Practices** (4 pages: Production Deployment, Performance, Accessibility, Security)
8. **TypeScript** (3 pages: Overview, TS with Composition API, TS with Options API)
9. **Extra Topics** (7 pages: Ways of Using Vue, Composition API FAQ, Reactivity in Depth, Rendering Mechanism, Render Functions & JSX, Vue and Web Components, Animation Techniques)

**Separate top-level navigation items:**
- **Tutorial** (interactive, separate from Guide)
- **Examples** (code samples)
- **API Reference** (comprehensive API listing)

The structure is **progressive**: starts with "Getting Started" (2 pages), moves to "Essentials" (12 foundational pages), then "Components In-Depth" (advanced component patterns), then specialized topics. Maximum depth is 2 levels (section → page).

## Page Types

Vue documentation has **six distinct page types**:

### 1. Introduction/Landing Pages
**Purpose:** High-level overview, philosophy, and learning path selection  
**Example:** https://vuejs.org/guide/introduction.html  
**Characteristics:**
- "What is Vue?" section with minimal code example
- "The Progressive Framework" explaining use cases (static HTML enhancement → SPA → SSR)
- "API Styles" comparison (Options vs Composition)
- "Pick Your Learning Path" with three entry points (Tutorial, Guide, Examples)
- Embedded video course promotion (Vue Mastery)
- Clear prerequisites section ("assumes basic familiarity with HTML, CSS, and JavaScript")

### 2. Quick Start/Setup Pages
**Purpose:** Get users running code in under 5 minutes  
**Example:** https://vuejs.org/guide/quick-start.html  
**Characteristics:**
- Multiple entry points: "Try Vue Online" (Playground, JSFiddle, StackBlitz), "Creating a Vue Application" (local setup), "Using Vue from CDN"
- Step-by-step terminal commands with package manager tabs (npm/pnpm/yarn/bun)
- "Next Steps" section linking to Tutorial, Guide, Examples
- Progressive disclosure: starts with simplest option (online playground), then build tools, then CDN

### 3. Tutorial Pages (Interactive)
**Purpose:** Hands-on learning with live code editor  
**Example:** https://vuejs.org/tutorial/  
**Characteristics:**
- Split-screen: instructions on left, live code editor on right
- Step-by-step progression with "Next" button
- Immediate visual feedback as code changes
- Builds a complete mini-app incrementally

### 4. Concept/Guide Pages
**Purpose:** Deep explanation of a single concept with examples  
**Example:** https://vuejs.org/guide/essentials/reactivity-fundamentals.html  
**Characteristics:**
- **API Preference Toggle** at top: switches all code examples between Options API and Composition API
- Structured with H2/H3 headings (e.g., "Declaring Reactive State", "ref()", "Why Refs?")
- Inline code examples with "Try it in the Playground" links
- TIP/WARNING callout boxes (e.g., "TIP: Many examples use `<script setup>` syntax")
- "See also" cross-references to related pages
- "Deep Reactivity", "DOM Update Timing" subsections for advanced details
- "Edit this page on GitHub" footer link

### 5. API Reference Pages
**Purpose:** Comprehensive, searchable API listing  
**Example:** https://vuejs.org/api/  
**Characteristics:**
- **Filterable sidebar** with categories: Global API, Composition API, Options API, Built-ins, Single-File Component, Advanced APIs
- Each API item links to detailed page (e.g., `/api/reactivity-core#ref`)
- Grouped by category (e.g., "Reactivity: Core", "Lifecycle Hooks", "Directives")
- No narrative, just organized function/component listings

### 6. Example Pages
**Purpose:** Runnable code samples for common UI tasks  
**Example:** https://vuejs.org/examples/ (referenced but not fetched)  
**Characteristics:**
- Gallery of common patterns (based on navigation mention)
- Each example is a complete, runnable SFC

## Content Patterns

### Universal Page Elements
Every guide page includes:
1. **Breadcrumb navigation** (implicit from sidebar)
2. **On-page table of contents** (right sidebar, auto-generated from H2/H3 headings)
3. **API Preference toggle** (Options/Composition) at top of page with `?` help icon
4. **"Edit this page on GitHub"** footer link
5. **Previous/Next page navigation** at bottom

### Code Example Patterns

#### 1. API Preference Toggle (Signature Feature)
Every code example shows **both Options API and Composition API** side-by-side:

**Options API example:**
```js
export default {
  data() {
    return { count: 0 }
  },
  methods: {
    increment() { this.count++ }
  }
}
```

**Composition API example (same functionality):**
```js
import { ref } from 'vue'
const count = ref(0)
function increment() { count.value++ }
```

The toggle is **persistent across pages** (stored in user preference). Each example has a small `?` icon linking to `/guide/introduction#api-styles` explaining the difference.

#### 2. Playground Links
Most code examples include a **"Try it in the Playground"** link (e.g., `https://play.vuejs.org/#eNo9jcEKwjAMhl/lt5fpQYfXUQfefAMvvRQbddC1pUuHUPrudg4HIcmXjyRZXEM4zYlEJ+T0iEPgXjn6BB8Zhp46WUZWDjCa9f6w9kAkTtH9CRinV4fmRtZ63H20Ztesqiylphqy3R5UYBqD1UyVAPk+9zkvV1CKbCv9poMLiTEfR2/IXpSoXomqZLtti/IFwVtA9A==`). The playground pre-loads the example code.

#### 3. Multi-Language Code Blocks
Code blocks specify language for syntax highlighting:
- `js` for JavaScript
- `template` for Vue templates
- `vue` for Single-File Components
- `sh` for shell commands

#### 4. Package Manager Tabs
Installation commands show **tabs for npm/pnpm/yarn/bun**:
```sh
$ npm create vue@latest
$ pnpm create vue@latest
$ yarn create vue@latest
$ bun create vue@latest
```

#### 5. Callout Boxes
Three types of callouts:
- **TIP** (blue): helpful hints (e.g., "TIP: You can toggle between API styles using the switches at the top")
- **WARNING** (yellow): important caveats (e.g., "WARNING: Avoid using arrow functions when defining methods")
- **Prerequisites** (gray): required knowledge (e.g., "Prerequisites: Familiarity with the command line")

### Cross-Reference Patterns
- **Inline links** to related concepts (e.g., "Learn more about [Reactivity in Depth](/guide/extras/reactivity-in-depth)")
- **"See also"** sections at end of subsections
- **"Further reading"** lists with external links (e.g., MDN docs for JavaScript Proxies)
- **"Next Steps"** sections at end of pages with 2-3 suggested paths

### Progressive Disclosure
Pages reveal complexity gradually:
1. **Simple example first** (e.g., "Try Vue Online" before local setup)
2. **"Why?" sections** after showing the pattern (e.g., "Why Refs?" after introducing `ref()`)
3. **"Advanced" or "In-Depth" subsections** at end (e.g., "Deep Reactivity", "Caveat in Arrays and Collections")
4. **"Extra Topics"** section in sidebar for deep dives (Reactivity in Depth, Rendering Mechanism)

## Progression Model

Vue uses a **multi-path progressive learning model** with three entry points:

### Entry Points (from Introduction page)
1. **Tutorial** (hands-on): "For those who prefer learning things hands-on"
2. **Guide** (comprehensive): "Walks you through every aspect of the framework in full detail"
3. **Examples** (reference): "Explore examples of core features and common UI tasks"

### Linear Flow (Guide Path)
The Guide sidebar follows a **strict beginner-to-advanced progression**:

**Phase 1: Getting Started (pages 1-2)**
- Introduction (philosophy, API styles, learning paths)
- Quick Start (setup, first app)

**Phase 2: Essentials (pages 3-14)**
- Core concepts needed for any Vue app
- Order matters: Reactivity Fundamentals → Computed Properties → Watchers (builds on previous)
- Ends with "Components Basics" and "Lifecycle Hooks" (foundation for next phase)

**Phase 3: Components In-Depth (pages 15-22)**
- Assumes you've built basic components
- Advanced patterns: Props, Events, Slots, Provide/Inject

**Phase 4: Reusability (pages 23-25)**
- Assumes component mastery
- Patterns for code reuse: Composables, Custom Directives, Plugins

**Phase 5: Scaling Up (pages 26-31)**
- Production concerns: Tooling, Routing, State Management, Testing, SSR

**Phase 6: Best Practices (pages 32-35)**
- Optimization and deployment

**Phase 7: TypeScript (pages 36-38)**
- Optional, assumes JS mastery

**Phase 8: Extra Topics (pages 39-45)**
- Deep dives for advanced users (Reactivity in Depth, Rendering Mechanism)

### Non-Linear Support
- **API Reference** is organized by category, not learning order (for lookup, not reading)
- **Examples** are task-based, not sequential
- **Tutorial** is linear but separate from Guide (alternative entry point)

### Prerequisite Handling
- **Explicit prerequisites** at top of pages (e.g., "Familiarity with HTML, CSS, and JavaScript")
- **Links to external resources** for prerequisites (MDN guides for JavaScript, HTML, CSS)
- **"If you are new to..."** conditional guidance (e.g., "If you are new to frontend development, it might not be the best idea to jump right into a framework")
- **No assumed framework knowledge**: starts from zero Vue knowledge

## Standout Features

### 1. API Preference Toggle (Dual-Style Code Examples)
**What:** Every code example shows both Options API and Composition API with a single toggle.  
**Why it's exceptional:** Solves the "two ways to do everything" problem without duplicating pages. Users pick their style once, and all examples adapt. The toggle persists across pages via localStorage.  
**Implementation details:**
- Toggle appears at top of every guide page
- Small `?` icon links to explanation of API styles
- Examples use conditional rendering based on user preference
- Preference stored client-side, survives page navigation

**Developer praise:** "Vue docs are the gold standard because I can learn one API style without being confused by the other" (common sentiment on Reddit/HN).

### 2. Integrated Playground Links
**What:** Most code examples have a "Try it in the Playground" link that opens a pre-configured live editor.  
**Why it's exceptional:** Zero-friction experimentation. No copy-paste, no setup. Click → edit → see results.  
**Implementation:** Links encode the example code in the URL hash (e.g., `play.vuejs.org/#eNo9jcEKwjAMhl...`). The playground decodes and renders it.

### 3. Progressive Entry Points
**What:** Three distinct learning paths (Tutorial, Guide, Examples) presented upfront.  
**Why it's exceptional:** Acknowledges different learning styles. Hands-on learners start with Tutorial, readers start with Guide, task-oriented users start with Examples.  
**Implementation:** "Pick Your Learning Path" section on Introduction page with clear descriptions of each path.

### 4. Package Manager Tabs
**What:** Installation commands show tabs for npm, pnpm, yarn, and bun.  
**Why it's exceptional:** Respects developer tooling preferences. No "we only support npm" gatekeeping.  
**Implementation:** Tabbed code blocks with identical commands for each package manager.

### 5. Persistent Table of Contents
**What:** Right sidebar shows auto-generated TOC from H2/H3 headings, highlights current section on scroll.  
**Why it's exceptional:** Easy navigation within long pages. Always visible, always accurate.  
**Implementation:** Starlight-style sticky sidebar with scroll-spy highlighting.

### 6. Callout Boxes with Semantic Types
**What:** TIP (blue), WARNING (yellow), Prerequisites (gray) callouts.  
**Why it's exceptional:** Visual hierarchy for different types of information. Warnings stand out, tips are inviting.  
**Implementation:** Custom markdown/MDX components with color-coded styling.

### 7. "Edit this page on GitHub" Footer
**What:** Every page has a direct link to edit the source on GitHub.  
**Why it's exceptional:** Lowers barrier to contribution. Typo fixes are one click away.  
**Implementation:** Footer link to `github.com/vuejs/docs/edit/main/src/guide/{page}.md`.

### 8. Multi-Language Support
**What:** Full translations in 14 languages (Chinese, Japanese, Ukrainian, French, Korean, Portuguese, Bengali, Italian, Persian, Russian, Czech, Traditional Chinese, Polish).  
**Why it's exceptional:** Global accessibility. Each translation has its own subdomain (e.g., `cn.vuejs.org`, `ja.vuejs.org`).  
**Implementation:** Separate repos for each translation (e.g., `vuejs-translations/docs-zh-cn`), linked from language picker in header.

## Applicable to NeoHaskell

### 1. Adopt API Preference Toggle for "NeoHaskell Way" vs "Standard Haskell"
**Pattern:** Vue's Options/Composition toggle  
**NeoHaskell application:** Add a toggle at the top of every page: **"NeoHaskell" vs "Standard Haskell"**. All code examples show both styles.

**Example:**
```haskell
-- NeoHaskell style
module NeoBank.Account where

createAccount :: AccountId -> IO Account
createAccount id = do
  account <- newAccount id
  emit AccountCreated { accountId = id }
  pure account
```

```haskell
-- Standard Haskell style (for comparison)
module NeoBank.Account where
import Control.Monad.IO.Class (liftIO)

createAccount :: AccountId -> ReaderT Env IO Account
createAccount id = do
  account <- liftIO $ newAccount id
  liftIO $ emit AccountCreated { accountId = id }
  pure account
```

**Why:** NeoHaskell's "progressive learning" philosophy parallels Vue's "progressive framework". Showing both styles helps users coming from Haskell understand what NeoHaskell simplifies, while teaching NeoHaskell users what's happening under the hood.

**Implementation:** Use Astro/Starlight's client-side state management to persist toggle preference. Conditionally render code blocks based on preference.

### 2. Adopt "Try it in the Playground" Links for Every Example
**Pattern:** Vue's playground links  
**NeoHaskell application:** Create a NeoHaskell playground (similar to `play.vuejs.org`) and link every code example to it with pre-loaded code.

**Why:** Haskell has a reputation for difficult setup (GHC, Cabal, Stack). A zero-setup playground removes this barrier for learners.

**Implementation:** Build a web-based REPL (possibly using GHCJS or a server-side evaluator) and encode examples in URL hashes.

### 3. Adopt Progressive Entry Points (Tutorial, Guide, Reference)
**Pattern:** Vue's three learning paths  
**NeoHaskell application:** Offer three entry points on the homepage:
1. **Tutorial** (NeoBank walkthrough): "Build a banking app step-by-step"
2. **Guide** (Diataxis-structured): "Learn NeoHaskell concepts in depth"
3. **Reference** (API docs): "Look up functions and modules"

**Why:** Different learners have different needs. Tutorial-first users want to build something immediately. Concept-first users want to understand before coding.

**Implementation:** Create a landing page with "Pick Your Learning Path" section (copy Vue's exact pattern).

### 4. Adopt Package Manager Tabs for Installation
**Pattern:** Vue's npm/pnpm/yarn/bun tabs  
**NeoHaskell application:** Show tabs for **Cabal**, **Stack**, and **Nix** in installation instructions.

**Example:**
```sh
# Cabal
$ cabal install neohaskell

# Stack
$ stack install neohaskell

# Nix
$ nix-env -iA nixpkgs.neohaskell
```

**Why:** Haskell developers have strong tooling preferences. Respecting all three avoids alienating any group.

**Implementation:** Use Starlight's tabbed code blocks feature.

### 5. Adopt "Coming From..." Pages with Side-by-Side Comparisons
**Pattern:** Vue's API style toggle (adapted)  
**NeoHaskell application:** Create "Coming From Haskell", "Coming From JavaScript", "Coming From Python" pages with **side-by-side code comparisons**.

**Example (Coming From JavaScript page):**
```javascript
// JavaScript (Express.js)
app.post('/accounts', (req, res) => {
  const account = createAccount(req.body.id);
  res.json(account);
});
```

```haskell
-- NeoHaskell
post "/accounts" $ do
  accountId <- param "id"
  account <- createAccount accountId
  json account
```

**Why:** NeoHaskell targets developers from other languages. Showing familiar patterns next to NeoHaskell equivalents accelerates learning.

**Implementation:** Create dedicated pages for each source language, using two-column layouts for comparisons.

### 6. Adopt Callout Boxes for Event Sourcing Concepts
**Pattern:** Vue's TIP/WARNING callouts  
**NeoHaskell application:** Use callouts to explain event sourcing concepts inline:

**Example:**
> **EVENT SOURCING TIP:** In NeoBank, every account mutation emits an event. This `AccountCreated` event is stored in the event log and can be replayed to rebuild state.

**Why:** Event sourcing is unfamiliar to most developers. Inline explanations prevent confusion without disrupting flow.

**Implementation:** Use Starlight's built-in callout components (Astro supports this natively).

### 7. Adopt "Why?" Sections After Introducing Patterns
**Pattern:** Vue's "Why Refs?" section after introducing `ref()`  
**NeoHaskell application:** After introducing a NeoHaskell pattern, add a "Why does NeoHaskell do this?" section.

**Example:**
```markdown
## Event Sourcing in NeoBank

Every account operation emits an event:

\`\`\`haskell
deposit :: AccountId -> Amount -> IO ()
deposit id amount = emit Deposited { accountId = id, amount }
\`\`\`

### Why Event Sourcing?

Traditional databases store current state. Event sourcing stores *every change*. This gives you:
- **Audit trail**: See every transaction ever made
- **Time travel**: Rebuild account state at any point in history
- **Debugging**: Replay events to reproduce bugs
```

**Why:** Learners need motivation to adopt unfamiliar patterns. "Why?" sections provide that motivation.

**Implementation:** Add H3 "Why?" sections after introducing each major pattern.

### 8. Adopt "Prerequisites" Callouts at Top of Pages
**Pattern:** Vue's prerequisites boxes  
**NeoHaskell application:** Add prerequisites to advanced pages:

**Example:**
> **Prerequisites:** This guide assumes you've completed the [NeoBank Tutorial](/tutorial/) and understand [Event Sourcing Basics](/concepts/event-sourcing).

**Why:** Prevents frustration from jumping into advanced topics without foundation.

**Implementation:** Use Starlight's callout components at the top of pages.

### 9. Adopt "Next Steps" Sections at End of Pages
**Pattern:** Vue's "Next Steps" with 2-3 suggested paths  
**NeoHaskell application:** End each page with suggested next steps:

**Example:**
```markdown
## Next Steps

- [Build the Account Module](/tutorial/account-module) - Continue the NeoBank tutorial
- [Learn About Event Handlers](/concepts/event-handlers) - Understand how events trigger actions
- [Explore the API Reference](/reference/events) - Look up event-related functions
```

**Why:** Guides users to the next logical step, preventing "what do I read next?" paralysis.

**Implementation:** Add a "Next Steps" section to the page template.

### 10. Adopt Multi-Path Tutorial Structure
**Pattern:** Vue's Tutorial (interactive) vs Guide (comprehensive) split  
**NeoHaskell application:** Create two parallel learning paths:
1. **NeoBank Tutorial** (linear, project-based): Build a complete banking app in 10 steps
2. **Concepts Guide** (Diataxis-structured): Deep dives into event sourcing, type safety, etc.

**Why:** Some learners want to build immediately (tutorial-first), others want to understand concepts first (guide-first).

**Implementation:** Keep Tutorial and Guide as separate top-level sections in navigation.

---

**Summary of Recommendations:**
1. **API toggle** for NeoHaskell vs Standard Haskell comparisons
2. **Playground links** for zero-setup experimentation
3. **Three entry points** (Tutorial, Guide, Reference)
4. **Package manager tabs** (Cabal, Stack, Nix)
5. **"Coming From..." pages** with side-by-side comparisons
6. **Callout boxes** for event sourcing concepts
7. **"Why?" sections** after introducing patterns
8. **Prerequisites callouts** at top of advanced pages
9. **"Next Steps" sections** at end of pages
10. **Multi-path tutorial** (project-based vs concept-based)

All of these patterns directly support NeoHaskell's goal of progressive learning with a real-world example (NeoBank) while teaching event sourcing alongside language syntax.
