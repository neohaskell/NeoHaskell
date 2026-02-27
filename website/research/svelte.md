# Svelte Documentation Analysis

## Navigation Structure

Svelte uses a **three-level hierarchical sidebar** with clear visual grouping:

**Level 1: Product separation**
- Svelte (core framework)
- SvelteKit (application framework)
- CLI
- MCP

**Level 2: Section headers** (within Svelte docs)
- Introduction (4 pages)
- Runes (7 pages)
- Template syntax (18 pages)
- Styling (4 pages)
- Special elements (7 pages)
- Runtime (5 pages)
- Misc (6 pages)
- Reference (18 pages)
- Legacy APIs (12 pages)

**Level 3: Individual pages** under each section

The sidebar is **always visible** and uses **collapsible sections** with clear visual hierarchy. Each section header is bold, and pages are indented beneath. The current page is highlighted.

**Key observation**: The sidebar groups by **concept type** (runes, template syntax, styling) rather than by learning progression. This works because the tutorial is completely separate.

## Page Types

Svelte maintains **strict separation** between tutorial and reference:

### 1. Tutorial Pages (learn.svelte.dev/tutorial)
- **Interactive code editor** with live preview (split-pane layout)
- **Solve button** that reveals the solution
- **Minimal explanatory text** (2-3 paragraphs max)
- **Single concept per page** with immediate practice
- **Sequential progression** with prev/next navigation
- Example: `/tutorial/svelte/state` teaches `$state` rune with editable code

### 2. Reference Pages (/docs/svelte/*)
- **Comprehensive API documentation** with all details
- **Multiple code examples** (read-only, syntax-highlighted)
- **Deep technical explanations** including edge cases
- **Cross-references** to related concepts
- **"See also" boxes** linking to tutorial pages
- Example: `/docs/svelte/$state` covers deep state, classes, raw state, snapshot, eager, module sharing

### 3. Getting Started Page (/docs/svelte/getting-started)
- **Installation instructions** (SvelteKit recommended)
- **Alternative setups** (Vite standalone)
- **Editor tooling** links
- **Help resources** (Discord, Stack Overflow)
- No tutorial content, just setup

### 4. Overview Page (/docs/svelte/overview)
- **Single code example** showing component structure
- **One-paragraph pitch** ("compiler that turns declarative components...")
- **Links to tutorial and playground**
- Explicitly directs newcomers to tutorial first

**Critical distinction**: Tutorial pages teach through doing. Reference pages document through explaining. There's no hybrid "guide" type that tries to do both.

## Content Patterns

### Code Examples

**Tutorial pages**: 
- **Editable code** in left pane, **live preview** in right pane
- **File tree** showing component structure (src/App.svelte, etc.)
- **Solve button** reveals complete solution
- **Minimal boilerplate** (just the relevant code)

**Reference pages**:
- **Multiple static examples** per page (5-10 examples typical)
- **TypeScript and JavaScript tabs** for same example
- **Inline type annotations** in code blocks
- **Progressive complexity** (simple example first, edge cases later)

Example from `/docs/svelte/$state`:
```svelte
<script>
  let count = $state(0);
</script>

<button onclick={() => count++}>
  clicks: {count}
</button>
```

Then shows deep state, classes, raw state, snapshot, etc. Each with its own example.

### Cross-References

**Bidirectional linking**:
- Reference pages have **"See also" boxes** linking to tutorial pages
- Tutorial pages have **"Docs" links** in top navigation
- Example: `/docs/svelte/$state` has "See also: Tutorial > Basic Svelte > Reactivity > State"

**Inline links**:
- Reference pages link to **related runes** (`$derived`, `$effect`)
- Reference pages link to **MDN** for JavaScript concepts (Proxy, get/set)
- Tutorial pages have **minimal links** to avoid distraction

### Page Structure

**Tutorial pages** (consistent template):
1. Brief introduction (1-2 sentences)
2. Task description ("Make the count declaration reactive...")
3. Code editor with TODO comments
4. Solve button

**Reference pages** (flexible structure):
1. Brief description
2. Basic example
3. Subsections for variants (Deep state, Classes, etc.)
4. Edge cases and gotchas
5. Cross-references

**No table of contents** on tutorial pages (too short). **Automatic TOC** on reference pages (right sidebar).

## Progression Model

### Tutorial: Strictly Linear

The tutorial at `/tutorial` enforces **sequential progression**:

**Basic Svelte** (48 lessons across 12 sections):
1. Introduction (6 lessons: Welcome, First component, Dynamic attributes, Styling, Nested components, HTML tags)
2. Reactivity (6 lessons: State, Deep state, Derived state, Inspecting state, Effects, Universal reactivity)
3. Props (3 lessons)
4. Logic (6 lessons: If blocks, Else blocks, Each blocks, etc.)
5. Events (5 lessons)
6. Bindings (7 lessons)
7. Classes and styles (3 lessons)
8. Attachments (2 lessons)
9. Transitions (8 lessons)

**Advanced Svelte** (separate section, 20+ lessons)

**Basic SvelteKit** (separate section, 20+ lessons)

**Advanced SvelteKit** (separate section, 20+ lessons)

**Key characteristics**:
- **No skipping ahead** (each lesson builds on previous)
- **Immediate practice** (every concept has an exercise)
- **Tiny increments** (one concept per lesson)
- **Clear milestones** (Basic → Advanced → SvelteKit)

### Reference: Non-Linear

The reference docs at `/docs` support **random access**:

- **Grouped by concept type** (Runes, Template syntax, etc.)
- **No prerequisites stated** (assumes tutorial completion)
- **Comprehensive on each page** (doesn't assume you read others)
- **Heavy cross-linking** (easy to jump between related topics)

**Critical insight**: Svelte doesn't try to make the reference docs teachable. They assume you learned from the tutorial and now need to look things up.

## Standout Features

### 1. Interactive Tutorial (learn.svelte.dev/tutorial)

**Split-pane editor**:
- Left: Editable code with file tree
- Right: Live preview that updates as you type
- Bottom: Instructions and task description

**Solve button**:
- Reveals complete solution
- Disabled on intro pages (no exercise)
- Encourages trying first ("Try not to rely on it too much")

**Vim mode toggle** (top right) for power users.

**No accounts required** (runs entirely in browser).

### 2. Playground Integration

**Seamless transitions**:
- Overview page links to `/playground`
- Tutorial pages can open in playground
- Reference examples are **not** interactive (deliberate choice to keep docs fast)

**External playground**:
- StackBlitz integration mentioned for full environment
- Playground is for quick experiments, not learning

### 3. Dual Tutorial Tracks

**Svelte tutorial** (framework basics):
- 48 lessons on components, reactivity, styling
- Standalone, no server concepts

**SvelteKit tutorial** (application framework):
- 40+ lessons on routing, data loading, forms
- Separate track, starts with "What is SvelteKit?"

**No mixing**: You complete Svelte basics before touching SvelteKit. Clean separation prevents confusion.

### 4. TypeScript/JavaScript Parity

**Every code example** has both versions:
- Tab switcher at top of code block
- Same example in both languages
- Type annotations shown in TS version
- No "TS is advanced" framing (equal citizens)

### 5. Migration Guides as First-Class Docs

**Legacy APIs section** (12 pages):
- Documents Svelte 4 patterns
- Shows equivalent Svelte 5 code
- Separate migration guides for v4 and v5
- Treats legacy as valid (not deprecated)

**Why this matters**: Users with existing codebases can find old patterns documented, not just "upgrade now" pressure.

### 6. Compiler Errors as Documentation

**Reference section includes**:
- Compiler errors (dedicated page)
- Compiler warnings (dedicated page)
- Runtime errors (dedicated page)
- Runtime warnings (dedicated page)

**Each error** gets an explanation and fix. Turns error messages into learning opportunities.

## Applicable to NeoHaskell

### 1. Tutorial/Reference Separation (CRITICAL)

**Adopt Svelte's model**:
- **Tutorial** (`/tutorial` or `/learn`): Interactive NeoBank project, linear progression, minimal text, immediate practice
- **Reference** (`/docs`): Comprehensive API docs, grouped by concept, random access

**Don't mix them**: NeoHaskell's current plan to have "tutorial layer" in docs pages risks creating hybrid pages that fail at both teaching and reference.

**Concrete recommendation**: 
- Move NeoBank tutorial to separate `/tutorial` path
- Keep `/docs` for reference only
- Link bidirectionally (tutorial → docs for deep dives, docs → tutorial for learning)

### 2. Interactive Code Examples (HIGH PRIORITY)

**Svelte's tutorial editor** is the killer feature. NeoHaskell needs equivalent:

**Phase 1** (MVP):
- Embedded code editor with syntax highlighting
- "Run" button that compiles and shows output
- Pre-populated exercises with TODO comments
- Solve button reveals solution

**Phase 2** (future):
- Live preview (like Svelte's right pane)
- File tree for multi-file projects
- Persistent state across lessons

**NeoBank context**: Each tutorial lesson could be a small NeoBank feature (create account, deposit money, check balance). Code editor shows the implementation, preview shows the result.

### 3. Tiny Incremental Lessons

**Svelte's lesson size**: 1 concept, 1 exercise, 2-3 paragraphs of explanation.

**NeoHaskell should match this**:
- Don't try to teach "event sourcing" in one lesson
- Break it down: (1) What is an event? (2) Storing events (3) Replaying events (4) Building state from events
- Each lesson adds **one small feature** to NeoBank

**Current risk**: NeoHaskell's "New Syntax" asides could become too dense. Keep them to 1-2 syntax items per lesson.

### 4. Separate Advanced Track

**Svelte's model**:
- Basic Svelte (48 lessons)
- Advanced Svelte (20 lessons)
- Clear boundary between them

**NeoHaskell equivalent**:
- **Basic NeoHaskell**: Build NeoBank (accounts, transactions, balance)
- **Advanced NeoHaskell**: Event sourcing internals, performance, testing, deployment
- **NeoHaskell + Web**: Separate track for web apps (like SvelteKit)

**Don't interleave**: Finish basic track before introducing advanced concepts.

### 5. Compiler-as-Teacher Integration

**Svelte documents compiler errors**. NeoHaskell should go further:

**Compiler errors as tutorial content**:
- Tutorial lessons could **intentionally trigger errors** to teach debugging
- "Break It" exercises show error messages and ask students to fix
- Error messages link to relevant docs/tutorial pages

**Example lesson**: "Let's see what happens when we forget to handle an event type. Click Run... See that error? Here's what it means and how to fix it."

**Svelte doesn't do this** (their tutorial avoids errors). NeoHaskell's compiler-as-teacher could be a differentiator.

### 6. Playground as Separate Tool

**Svelte's approach**:
- Tutorial is for learning (structured, guided)
- Playground is for experimenting (unstructured, free-form)
- They don't try to make playground teachable

**NeoHaskell should**:
- Build tutorial first (structured learning)
- Add playground later (free experimentation)
- Don't try to make playground replace tutorial

**Current plan**: NeoHaskell mentions "interactive elements planned for future phases." Good. Tutorial comes first.

### 7. Sidebar Organization

**Svelte groups by concept type** (Runes, Template syntax, Styling). This works for reference docs.

**NeoHaskell's sidebar** (from AGENTS.md):
- Getting Started
- Core Concepts
- Guides
- API Reference
- ADRs

**Recommendation**: This is good for reference docs. But tutorial should have **separate sidebar** organized by progression:
- Tutorial sidebar: Lesson 1, Lesson 2, Lesson 3... (linear)
- Docs sidebar: Concepts, Guides, API Reference (grouped)

**Don't share sidebar** between tutorial and docs. Different mental models.

### 8. TypeScript/JavaScript Parity → NeoHaskell/Haskell Parity?

**Svelte shows both languages** for every example. Could NeoHaskell show:
- NeoHaskell syntax (primary)
- Equivalent Haskell syntax (for Haskell users)

**Probably not needed**: NeoHaskell is the point. But could be useful in migration guides or "Coming from Haskell" section.

### 9. No Prerequisites Stated

**Svelte's tutorial** says "You'll need basic familiarity with HTML, CSS and JavaScript" but doesn't test it. Just starts teaching.

**NeoHaskell should**:
- State "No programming experience required" (if true)
- Or "Basic programming concepts helpful" (if not)
- Don't gate-keep with "you must know X first"

**Current plan**: Tutorial-first approach suggests no prerequisites. Good.

### 10. Solve Button Psychology

**Svelte's solve button** says "Try not to rely on it too much; you will learn faster by figuring out where to put each suggested code block."

**NeoHaskell should**:
- Include solve button (reduces frustration)
- Add similar encouragement to try first
- Maybe track solve button usage and suggest slowing down if overused?

**Compiler-as-teacher tie-in**: Solve button could show **why** the solution works, not just the code.

### Summary: Concrete Actions for NeoHaskell

1. **Separate tutorial from reference** (different URLs, different sidebars, different mental models)
2. **Build interactive code editor** for tutorial (even basic version beats static examples)
3. **Keep lessons tiny** (1 concept, 1 exercise, 2-3 paragraphs)
4. **Create advanced track** separate from basic (don't interleave)
5. **Document compiler errors** as learning opportunities (Svelte does this, NeoHaskell can go further)
6. **Playground is separate** from tutorial (don't conflate learning and experimenting)
7. **Bidirectional linking** between tutorial and reference (easy to go deep or come back)
8. **No hybrid pages** (each page is either tutorial or reference, not both)
9. **Linear tutorial progression** (can't skip ahead, each lesson builds on previous)
10. **Random-access reference** (can jump to any page, comprehensive on its own)

**Biggest takeaway**: Svelte's tutorial/reference separation is the key insight. Don't try to make docs teachable. Make tutorial teach, make docs document, link between them.
