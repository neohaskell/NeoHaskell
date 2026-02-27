---

# Review of the NeoHaskell Documentation Plan

**Reviewer: Sarah Drasner**
**Date: February 2026**

---

## Preface

I've spent the better part of my career thinking about what makes developers stay on a page, what makes them leave, and what makes them come back. From building developer experience at Netlify to leading engineering at Google, from writing about visual learning to contributing to Vue's ecosystem — the through-line has always been the same question: *does this serve the person using it, or does it serve the person who built it?*

This documentation plan is clearly written by people who care deeply about teaching. The ambition is real, the audience model is thoughtful, and the banking domain is a genuinely clever pedagogical choice. But caring about teaching and designing a great *experience* are related but distinct skills. A brilliant curriculum in a confusing building is still a confusing building.

Steve Klabnik's review — which I've read carefully — focuses largely on pedagogical structure: layers, taxonomy, progression pacing, tutorial content. His feedback is sharp and I agree with much of it. My review is going to pull on different threads. I'm going to focus on what happens *around* the content: the surfaces the developer touches, the navigation decisions they make, the moments of confusion the plan doesn't design for, and the Starlight features sitting right there that the plan barely acknowledges.

Let's get into it.

---

## I. The Plan Designs Content but Not the Experience of Encountering Content

**Responding to: The plan as a whole — specifically the gap between "what to write" and "how it feels to use"**

The DOCS-PLAN.md is ~790 lines long. It contains detailed specifications for tutorial structure, exercise design, concept page templates, code verification, audience models, and mental model journeys. What it does *not* contain is any specification for:

- What the landing page looks like and how it routes the three audiences
- What the sidebar looks like at each stage of the reader's journey
- How navigation between doc types (tutorial → concept → guide → reference) actually works in the UI
- What happens when a developer searches for something and doesn't find it
- What happens on mobile
- What visual hierarchy differentiates page types
- How the reader *knows* where they are in the documentation structure at any moment

This matters because **documentation is an interface, not just content.** The plan treats documentation as a writing problem. It's also — and I'd argue primarily, for the first impression — a *design* problem.

**Why I feel strongly about this:**

When I was at Netlify, we spent as much time on the information architecture and navigation design of our docs as we did on the actual prose. We had a map — a literal visual map — of every page, every link between pages, every entry point, and every exit point. When we changed the sidebar, we tested it with real users before shipping. The content was excellent, but the content doesn't matter if people can't find it or don't know where they are.

Vue's documentation — which I know intimately from my time on the core team — succeeds not just because of the writing quality, but because of the *experience design.* The API preference toggle (Options vs. Composition) isn't a content decision. It's a UX decision. It says: "We see you. We know you have a preference. We'll adapt to you." The three entry points on the introduction page (Tutorial, Guide, Examples) aren't a structural decision. They're a *wayfinding* decision. The visitor self-selects, and the docs respect their choice.

The NeoHaskell plan has three audiences (Developer, Architect, Decision-Maker) and describes their needs beautifully. But it never designs the *moment* where they arrive at the site and self-select. It never designs the *surface* that routes them.

**My specific recommendation:**

Add a "Landing Page Experience" section to the plan with these requirements:

1. **The landing page must route all three audiences within 5 seconds.** Three clearly differentiated paths: "I want to try this" → Quick Start / Tutorial. "I want to evaluate this" → Core Concepts / Architecture. "I want to understand the value" → Why NeoHaskell / Case Studies. These aren't sidebar links — they're the *hero content* of the page.

2. **Each path should have a visual identity.** Starlight supports `<CardGrid>` and `<Card>` components with icons. Use them. The Developer path gets a rocket icon, the Architect path gets a blueprint icon, the Decision-Maker path gets a chart icon. This isn't decorative — it's wayfinding. The reader's eyes should be drawn to their path before they read a word.

3. **The sidebar should be designed for each section, not just listed.** The plan's Site Structure section (lines 291-339) lists sidebar sections with labels and purposes. But it doesn't specify: are these collapsible? How deep is the nesting? What's visible by default? The Astro research (astro.md) documents that Astro uses multi-tab top-level navigation with collapsible sidebar groups. The plan should specify which Starlight navigation pattern it's using and why.

---

## II. Starlight Is an Incredible Platform and the Plan Barely Uses It

**Responding to: "Interactive Elements (Future, But Design For Them Now)" (lines 567-583) and the general absence of Starlight component specifications**

The plan's Phase 1 interactive elements are listed as:

> - Copy buttons on all code blocks (Starlight default)
> - Syntax highlighting (Starlight default)
> - Search (Starlight default)
> - Dark mode (Starlight default)

These are not "interactive elements." These are the bare minimum that Starlight gives you *for free by doing nothing.* Listing them as Phase 1 features is like listing "the website will load in a browser" as a feature. It signals that the team hasn't deeply explored what Starlight actually offers.

The Astro research document (astro.md) — which was literally written for this project — catalogs Starlight features in detail. Here's what Starlight provides that the plan never mentions using:

1. **Tabbed content blocks** (`<Tabs>` / `<TabItem>`) — The plan mentions OS-specific installation in Open Questions but never specifies using Starlight's tab component for it. The Vue research (vue.md) identifies package manager tabs as a standout feature. This is directly applicable to the "Coming From..." pages (show NeoHaskell alongside the reader's language), installation (Nix vs. alternatives), and the Rosetta Stone table (which could be an interactive comparison instead of a static table).

2. **Aside components** (`:::note`, `:::tip`, `:::caution`, `:::danger`) — The plan's concept page template (lines 393-418) defines sections like "The One-Sentence Version" and "How NeoHaskell Enforces This" but never specifies using Starlight's semantic callout types. The synthesis research (Pattern 10) found that 5/12 sites use inline callouts to great effect. The event sourcing vocabulary — "command," "event," "aggregate," "projection" — should be introduced with `:::tip` callouts the first time each term appears.

3. **Card grids** (`<CardGrid>` / `<Card>`) — Perfect for the "Coming From..." section landing page, the Guides index, and the "Choose Your Path" landing page I recommended above. The Astro docs use card grids for deployment guides (30+ cards with logos). The NeoHaskell "Coming From..." section should present 6 entry points as cards with language logos.

4. **Code block labels** (the `title` attribute on fenced code blocks) — Every code block in the tutorial should be labeled with the file being edited: `src/NeoBank/Account.nh`, `src/NeoBank/Events.nh`. This seems small but it's enormous for the reader's mental model. "Where does this code go?" is one of the top frustrations in any tutorial. Labeled code blocks answer it before the question is asked. The Astro research specifically calls this out as a content pattern (astro.md, lines 104-108).

5. **Badge components** — Starlight supports badges for marking pages or sections. These could indicate: "Beginner," "Requires Tutorial Part 3," "Advanced," or "Coming Soon." The plan's mental model stages (0-5) could be surfaced as badges on each page, making the progression visible in the sidebar.

6. **Steps component** — Starlight has a `<Steps>` component for numbered step-by-step instructions. The tutorial's checkpoint sections should use this rather than raw markdown, because `<Steps>` provides visual differentiation that tells the reader "this is an instruction sequence, not an explanation."

**Why this matters beyond aesthetics:**

Every Starlight component I've listed serves a *cognitive* purpose. Tabs reduce choice paralysis. Callouts create visual landmarks in long pages. Card grids enable scanning instead of reading. Code labels reduce "where am I?" confusion. Steps components differentiate instruction from explanation. These aren't decorations — they're the difference between documentation that *works* and documentation that merely *exists.*

**My specific recommendation:**

Add a "Starlight Component Strategy" section to the plan that maps each Starlight component to its purpose:

| Component | Where to use | Why |
|-----------|-------------|-----|
| `<Tabs>` | Installation, Coming From, Rosetta Stone | Multiple paths without duplication |
| `:::note/tip/caution` | Tutorial (first-use vocabulary), Concept pages (gotchas) | Visual hierarchy, scannable landmarks |
| `<CardGrid>` | Landing page, Coming From index, Guides index | Self-selection, wayfinding |
| Code block `title` | Every tutorial code block | "Where does this go?" answered immediately |
| `<Steps>` | Tutorial checkpoints, installation | Visual instruction sequence |
| `<Badge>` | Sidebar labels, page headers | Stage/difficulty indication |

This isn't Phase 2. This is Phase 0. These components should be in the content from the first page.

---

## III. The "First 5 Minutes" Experience Has a Critical Gap

**Responding to: "Quick Start" section definition (lines 295-298) and Phase 1 tasks (lines 607-618)**

The plan defines the Quick Start as:

> "Zero to running code in 10 minutes"
> Stage: 0 → 1

And the Success Metrics (line 773) say:

> **Time to first deposit**: A new developer goes from zero to depositing $100 and seeing their balance derived from events — under 10 minutes.

This is the right goal. But the plan designs the Quick Start *content* without designing the Quick Start *experience.* Let me walk through what a developer actually encounters:

1. They arrive at the site. (The plan doesn't specify what the landing page looks like.)
2. They find "Quick Start" in the sidebar. (The plan doesn't specify how prominent it is.)
3. They click to the Getting Started section, which has: `index.mdx`, `installation.mdx`, `reading-neohaskell.mdx`, `cheat-sheet.mdx`, `first-events.mdx`.
4. They read `installation.mdx` and run `nix develop`.

Stop. Right here. This is where developers leave.

**The Nix problem:**

Open Question #1 asks: "Is Nix the only installation path?" Open Question #16 says: "I need Docker/Kubernetes to run this" is a misconception — "`nix develop` gives you everything."

If Nix is the only path, this is the single highest-friction point in the entire developer experience, and the plan treats it as an open question rather than a design challenge. Let me be direct: **most developers have never used Nix.** A JavaScript developer arriving from React, a Python developer arriving from Django — they don't have Nix installed, they don't know what it is, and "just use Nix" feels like "just learn a new build system before you can try the thing you came to try."

The synthesis research identifies this problem. Under "Getting Started best practices" (Synthesis lines 376-399), it recommends "Multiple entry paths" (from Laravel, Deno, Astro) — GHCup, installer, Docker. It also recommends "No configuration required" (from FastAPI, Svelte, Astro).

The plan's own Guiding Principle #3 says: "Ship value early, iterate constantly." But requiring Nix installation before any value is delivered violates this principle.

**My specific recommendation:**

The Quick Start experience needs to be designed around the *worst case*, not the best case:

1. **Offer a zero-install path.** If there is any way to run NeoHaskell in a browser (playground, Gitpod, Codespaces, StackBlitz), that should be the FIRST option on the installation page. Not the last. Not "future work." The first thing the reader sees. Vue does this with "Try Vue Online" linking to the playground, JSFiddle, and StackBlitz. FastAPI does this with "try it in the browser." The synthesis calls this out explicitly.

2. **If Nix is required, design the Nix experience as if it's the product.** Don't just say "run `nix develop`." Explain what Nix is in one sentence. Show what it does in one screenshot. Provide a copy-paste install command. Show the expected terminal output at every step. Use Starlight's `:::caution` aside for "If you see this error, try this fix." The installation page is not boilerplate — it's the most important page on the entire site, because it's the page where most people give up.

3. **Add installation time estimates.** "This step takes ~2 minutes" or "This download is ~500MB." The uncertainty of "how long will this take?" is a leading cause of mid-installation abandonment. I've seen this pattern across every developer tool I've worked on.

4. **Use Starlight's `<Tabs>` component** to present installation paths: macOS / Linux / Windows / Docker / Online Playground. Even if the commands are similar, the *tab labels* signal platform support. The Astro research (astro.md) specifically recommends this.

---

## IV. The Architect and Decision-Maker Audiences Are Acknowledged but Not Served

**Responding to: Audience Model (lines 22-46) and Site Structure (lines 271-339)**

The audience model describes three personas with distinct needs. The Developer gets: Quick Start, Tutorial, Guides, Reference — four entire sections of the docs. The Architect gets: Core Concepts (shared with everyone) and "Coming From..." (also shared). The Decision-Maker gets: effectively nothing purpose-built.

The synthesis research identifies this gap indirectly. Under "Multi-Language/Multi-Path Support" (Pattern 8), it notes that 10/12 sites provide multiple entry paths that respect different user contexts. But those paths are usually for different *technical* contexts (npm vs. yarn, Options API vs. Composition API), not different *role* contexts.

**Why this matters:**

The Architect persona is described as:

> Comes from: enterprise systems, microservices, existing event sourcing with Kafka/EventStoreDB
> Wants: technical depth, tradeoff analysis, transaction guarantees, comparison with existing event sourcing infrastructure

Nothing in the plan's file structure serves this person directly. There's no "Architecture" section. There's no "Why Event Sourcing in a Language?" page. There's no comparison with EventStoreDB/Kafka/Axon. The Architect is expected to find what they need in Core Concepts, which is designed primarily for learners, not evaluators.

The Decision-Maker persona is described as:

> Wants: the "why" without the "how", proof points, risk assessment

There is literally no page in the file structure designed for this person. No "Executive Summary." No "Why NeoHaskell for Your Team." No "Case Studies." No "ROI of Event Sourcing." The Decision-Maker who arrives at this documentation site has nowhere to go except the tutorial (too detailed) or the concepts (too technical).

**My specific recommendation:**

1. **Add a "Why NeoHaskell?" page** to the Getting Started section. This is common across best-in-class docs (Vue has "Why Vue?", Astro has "Why Astro?"). It serves both Architects and Decision-Makers. Structure it as:
   - For Developers: "What you can build" (with screenshots/demos)
   - For Architects: "How it compares" (comparison table with EventStoreDB, Kafka, Axon — honest, not defensive)
   - For Decision-Makers: "Why it matters for your team" (velocity, correctness, audit compliance)

2. **Add an "Architecture" section or page** to Core Concepts that's explicitly for evaluators. Content: system architecture diagrams, deployment topology options, performance characteristics, scalability story, integration points. This is what an Architect evaluating any technology expects to find. Its absence signals immaturity.

3. **Consider a short "For Teams" or "Adopt NeoHaskell" page** that serves the Decision-Maker. It doesn't need to be long — 500 words about adoption strategy, risk mitigation, and how to run a pilot. Link it from the landing page. The synthesis doesn't identify this as a common pattern because most docs sites don't target Decision-Makers, but the plan *explicitly* identifies them as an audience. If they're an audience, they need a door.

---

## V. The Emotional Journey Is Designed for the Tutorial but Not for Anything Else

**Responding to: The Mental Model Journey (lines 49-66) and the plan's overall emotional design**

The plan's mental model journey is well-designed for the tutorial path:

```
Stage 0: "I store data in rows" → Stage 5: "I can model any domain this way"
```

Each tutorial part moves the reader forward, and the "wow" moments are explicitly designed. This is good emotional design for the learning path.

But what about the emotional journey of:

- **The developer who gets stuck on installation.** The plan doesn't design for frustration recovery. There's no "Troubleshooting Installation" section. There's no "Having trouble? Join our Discord" callout. When a developer hits a wall in the first 5 minutes, they need an escape hatch — not to the next tutorial page, but to *help.*

- **The developer who arrives mid-docs via search.** Most traffic to documentation sites comes from search engines, not the landing page. A developer Googles "neohaskell event sourcing example," lands on a concept page, and has no idea what stage of the mental model journey they're in. The plan's internal metadata (entry stage, exit stage) is invisible to the reader. There's no visible orientation for search-driven visitors.

- **The Haskell developer who feels patronized.** The plan's tone is optimized for CRUD developers learning event sourcing. But a Haskell developer who sees "No one reads a grammar chapter before cooking dinner" (Principle #7) might feel like the docs assume they're a beginner. The "Coming From Haskell" page helps, but the *rest of the docs* still assumes a CRUD-to-events journey. A Haskell dev who already knows algebraic data types and immutability will find the concepts section redundant unless there's a fast-track path.

- **The developer who finishes the tutorial and doesn't know what to do next.** The plan describes Tutorial Part 6 ending with "I accidentally built a compliance engine." Great moment. Then what? The plan's file structure shows `guides/` and `reference/`, but there's no designed transition from "tutorial complete" to "independent developer." No "Where to Go From Here" page. No "Build Your Own Project" guide. The emotional cliff at the end of a tutorial — "the hand-holding is over, now what?" — is one of the most common reasons developers abandon a technology after completing docs.

**My specific recommendation:**

1. **Add a "Troubleshooting" callout pattern** to every Getting Started and Tutorial page. Use Starlight's `:::caution` or `:::danger` aside. Format:

   ```
   :::caution[Stuck?]
   If you're seeing errors, check the [Common Errors](/guides/common-errors) page or ask in [Discord](link).
   :::
   ```

   This is an emotional safety net. It costs almost nothing to add and it dramatically reduces "I'm stuck and alone" anxiety.

2. **Add breadcrumb context for search-driven visitors.** Starlight provides breadcrumbs by default, but the plan should specify that every page includes a visible "Prerequisites" or "Before You Read This" callout for readers who didn't arrive linearly. The Vue research identifies "Prerequisites callouts" as a standout feature (vue.md, line 401).

3. **Design a "What's Next" page** after Tutorial Part 6. Content:
   - "You've built NeoBank. Here's what you can build next." (3 project ideas with scaffolding)
   - "Go deeper." (Links to advanced concepts, the logistics example domain)
   - "Join the community." (Discord, contributing, etc.)
   - "Build something and show us." (Encouragement + gallery/showcase)

   This transition page is the difference between "I completed a tutorial" and "I'm a NeoHaskell developer." It's an identity-formation moment. Don't leave it to chance.

---

## VI. The "Coming From..." Section Is the Highest-Leverage Content and Is Underspecified

**Responding to: "Coming From..." Section (lines 430-488)**

The plan correctly notes: "These become the highest-traffic pages." That's been my experience at every company I've worked at. Migration and comparison content gets the most organic search traffic, the most time-on-page, and drives the most conversion from "evaluator" to "adopter."

Yet the plan specifies 6 "Coming From..." pages at a high level and only expands on one (Haskell). The others get a table row and a template.

**My critique has three parts:**

**First: The "Coming from CRUD" page is arguably the most important page on the entire site, and it gets one table row.**

The plan's entire thesis is: "Move developers from CRUD thinking to event thinking." The Developer audience "brings: CRUD mental model, ORM habits, REST/GraphQL assumptions." The "Coming from CRUD" page is where this audience *arrives.* It's the page that should be the most carefully designed, the most visually rich, and the most emotionally supportive.

Instead, the plan gives it this:

| Coming from CRUD | REST endpoints, SQL updates | `UPDATE balance` → record `MoneyDeposited`; `SELECT balance` → fold over transactions; REST endpoint → command handler |

That's the entire specification. The key translations are shown as inline code fragments. For the audience that represents the majority of potential NeoHaskell users, this is drastically insufficient.

**Second: The template pattern (lines 446-456) is code-first when it should be concept-first.**

```
## In [your language], you'd write...
[familiar code]

## In NeoHaskell, the equivalent is...
[NeoHaskell code]

## But here's what's actually different...
[the mental model shift]
```

This structure shows code comparison before mental model shift. But for "Coming from CRUD" — which isn't a language migration, it's a *paradigm* migration — the mental model shift should come first. The reader needs to understand *why* things are different before seeing *how* they're different. The synthesis research supports this: Pattern 5 says "teach it by building something familiar, letting the concept emerge naturally." Pattern 7 says "start reference pages with scannable tables." For the CRUD page specifically, a comparison *table* (not code blocks) should come first:

| What you do in CRUD | What you do in NeoHaskell | Why it's different |
|---------------------|---------------------------|-------------------|
| `UPDATE accounts SET balance = 150` | Emit `MoneyDeposited { amount: 50 }` | You don't lose history |
| `SELECT balance FROM accounts` | `fold events` → current balance | Balance is *calculated*, not stored |
| `DELETE FROM accounts WHERE id = 1` | (You don't) | Events are facts — you can't un-happen something |

**Third: The "Coming from Event Sourcing" page is a missed opportunity for the Architect audience.**

This page (described in one table row) should be the Architect's primary entry point. An architect who already knows EventStoreDB or Kafka doesn't need the tutorial — they need to know: "How is NeoHaskell's approach different from what I already use? What do I gain? What do I lose?" This is the "How it compares" page I mentioned in Section IV, and it should be substantial.

**My specific recommendation:**

1. **Expand the "Coming from CRUD" specification** to match the "Coming from Haskell" specification. It needs: an opening section that reframes CRUD vs. event sourcing using the banking analogy, a visual comparison table (not just code), a side-by-side "Same App, Two Ways" walkthrough showing a REST API with SQL vs. the NeoHaskell equivalent, and an emotional reassurance section ("Your SQL knowledge isn't wasted — read models use queries you already know").

2. **Redesign the "Coming From..." template** to have two variants:
   - **Language migration** (JS, Python, Go, Haskell): Code comparison first, mental model shift second.
   - **Paradigm migration** (CRUD, Event Sourcing): Mental model shift first, code comparison second.

3. **Make the "Coming from Event Sourcing" page** a technical comparison document with a table: NeoHaskell vs. EventStoreDB vs. Kafka vs. Axon. Cover: event storage, schema evolution, projections/read models, deployment model, language integration. This is the page an Architect sends to their team as part of an evaluation.

---

## VII. Mobile Experience Is Completely Unaddressed

**Responding to: The plan as a whole**

The word "mobile" does not appear in DOCS-PLAN.md. Not once.

This is a problem. Analytics from every documentation site I've worked on show that 15-30% of documentation traffic comes from mobile devices. Developers read docs on their phones during commutes, on tablets during meetings, and on mobile when they're pair programming and using their laptop for code.

Starlight is responsive by default — the sidebar collapses into a hamburger menu on mobile. But responsive layout and good mobile *experience* are different things. Specific concerns:

1. **Code blocks on mobile.** NeoHaskell code blocks will have lines that exceed mobile screen width. Starlight provides horizontal scrolling for code blocks, but long lines are still harder to read on mobile. The plan's code examples should be designed with line-length awareness. The tutorial code should aim for lines under 60 characters where possible.

2. **The three-layer tutorial system (even if simplified per Steve's recommendation) on mobile.** Collapsible asides, inline links, and concept references all compete for attention on a small screen. On desktop, progressive disclosure works because you have spatial separation. On mobile, everything is stacked vertically, and expandable sections push content below the fold.

3. **The Rosetta Stone table (lines 138-146) and the Coming from Haskell table (lines 468-482) on mobile.** Wide tables with 4+ columns are unreadable on mobile. Starlight renders tables with horizontal scroll, but scrolling a 12-row, 4-column table on a phone is a miserable experience.

4. **The cheat sheet (cheat-sheet.mdx) is described as "printable, fits on 2 pages."** That's a desktop/print assumption. On mobile, a cheat sheet should be designed as a vertical scroll with clear section headers, not a two-column table layout.

**My specific recommendation:**

1. **Add a "Mobile Considerations" section** to the plan's design constraints. Specify: maximum code block line length (60 chars for tutorial, 80 chars for reference), table column limits (3 columns max, or use Starlight's responsive card layout instead of tables for comparison content), and progressive disclosure design for small screens.

2. **Redesign wide comparison tables as stacked cards on mobile.** Instead of a 4-column table, use a card-per-row layout where each card shows: the concept, the CRUD version, the NeoHaskell version, and the explanation. Starlight's `<CardGrid>` component is responsive by default.

3. **Test the tutorial on a phone** before shipping. Add this to the Phase 1 testing protocol: "Can someone follow Tutorial Part 1 on an iPhone SE?" If the answer is no, the code examples or layout need adjustment.

---

## VIII. Search Experience Is Treated as a Default, Not a Designed Experience

**Responding to: The plan's implicit reliance on Starlight's search and Success Metric #5 (line 777)**

The plan's Success Metric #5 is:

> **Search satisfaction**: People who use site search find what they need (low "search → bounce" rate).

But the plan never designs for this. Starlight's built-in search (Pagefind) is good — it's fast, it's local, it indexes all content. But search *satisfaction* depends on more than indexing. It depends on:

1. **Content being written with searchable terms.** If a developer searches "how to check account balance" and the page is titled "Projections" with no mention of "check balance" in the first paragraph, they won't find it. The plan should specify that every guide and concept page includes a "What this page covers" section with plain-language descriptions that match how developers think, not just how the framework names things.

2. **Headings being scannable and descriptive.** The concept page template (lines 393-418) uses section names like "The One-Sentence Version" and "In NeoBank Terms." These are cute but not searchable. A developer searching for "projections" wants to see a heading that says "What Is a Projection?" not "The One-Sentence Version." The template headings should use the concept name, not a meta-description of the section's function.

3. **Synonyms and aliases.** Event sourcing has competing vocabulary. "Read model" = "projection" = "view" = "query side." "Command" = "action" = "intent" = "request." The plan's Rosetta Stone (lines 138-146) maps these terms, but the *content* should use all common synonyms at least once per page so search catches them.

**My specific recommendation:**

1. **Revise the concept page template** to use concept-name headings instead of meta-names. Replace "The One-Sentence Version" with "What Is [Concept]?" Replace "In NeoBank Terms" with "[Concept] in Practice." Replace "How NeoHaskell Enforces This" with "Type Safety for [Concept]." These headings serve both readers (who scan headings) and search (which indexes them).

2. **Add keyword metadata to frontmatter.** Starlight supports `description` in frontmatter, which is used for search. The plan's concept page template should include a `keywords` or `synonyms` field that lists alternative terms. Even if Starlight doesn't index this field natively, having it in the template forces writers to think about searchability.

3. **Design a "search landing" experience.** When a developer searches and finds a concept page, the first thing they see should orient them: what is this concept, which tutorial part introduced it, and what stage of the mental model journey it belongs to. The `:::note` callout is perfect for this:

   ```
   :::note[Context]
   Projections are introduced in [Tutorial Part 3: Transaction History](/tutorial/03-transaction-history). 
   This page goes deeper into how and why they work.
   :::
   ```

---

## IX. Cross-Referencing Is Mentioned but Not Systematically Designed

**Responding to: Tutorial Layer System Layer 3 (lines 207-219), and the general absence of navigation design between doc types**

Steve's review identifies the missing "See Also" sections, and I agree with his recommendation. But I want to push further on a deeper issue: **the plan doesn't design the web of connections between its four documentation modes.**

The Diataxis framework (which the plan correctly adopts) defines four modes: Tutorials, How-To Guides, Explanation (Concepts), and Reference. The power of Diataxis isn't the four modes — it's the *connections* between them. A tutorial should link to explanations ("want to know why?"), guides ("want to do something specific?"), and reference ("want the full API?"). An explanation should link back to the tutorial ("see this in practice"), forward to guides ("apply this knowledge"), and sideways to reference ("look up the details").

The plan's Layer 3 links handle one direction: tutorial → concept. But what about:

- **Concept → tutorial?** "This concept was introduced in Tutorial Part 2. Go back and try it."
- **Guide → concept?** "This guide assumes you understand projections. See [What Is a Projection?](/concepts/projections)."
- **Reference → guide?** "For a complete example of using this function, see the [Testing Guide](/guides/testing)."
- **Concept → guide?** "Ready to apply this? See [Building Custom Projections](/guides/custom-projections)."

The synthesis research identifies cross-reference density as universal (Pattern 11, all 12 sites). Laravel is cited as having "aggressive cross-linking (every page links to 5-10 related pages)." The plan doesn't specify this density or systematize it.

**My specific recommendation:**

Add a "Cross-Reference Matrix" to the plan that specifies, for each page type, which other page types it links to and where:

| Page type | Links to tutorials | Links to concepts | Links to guides | Links to reference |
|-----------|-------------------|-------------------|-----------------|-------------------|
| Tutorial | Previous/Next | Layer 3 (inline) | "See Also" (bottom) | "See Also" (bottom) |
| Concept | "Introduced in" (top callout) | Related concepts (bottom) | "Apply this" (bottom) | Related APIs (bottom) |
| Guide | "Prerequisite tutorial" (top) | "Background reading" (top) | Related guides (bottom) | API details (inline) |
| Reference | "Tutorial example" (inline) | "Concept explanation" (inline) | "Usage guide" (inline) | Related functions (sidebar) |

Then add this to the authoring checklist: every page must have at least 3 outbound links to other documentation modes before it ships.

---

## X. The Exercise Strategy Doesn't Design for the Reader Who Gets Stuck

**Responding to: Exercise Strategy (lines 491-523) and Bloom's Taxonomy Progression (lines 510-523)**

Steve's review argues convincingly that Bloom's taxonomy should be a validation tool, not a design constraint. I agree. But I want to raise a different problem with the exercise strategy: **it designs for success but not for failure.**

Every exercise in the plan assumes the reader will attempt it, potentially struggle, and then succeed. But what about the reader who:

- **Doesn't know where to start** on an "Extend" exercise?
- **Gets a compiler error** they don't understand during a "Modify" exercise?
- **Gets the wrong output** on a "Predict" exercise and doesn't know why?
- **Completes the exercise** but isn't sure if their solution is correct?

The best exercise systems I've seen (and designed) include *graduated hints.* Not the answer — that teaches nothing. But a progression from "here's a nudge" to "here's a bigger nudge" to "here's the approach, try to implement it" to "here's the full solution."

Vue's interactive tutorial does this with a "Show me!" button that reveals the solution. React's tutorial includes expandable "Show solution" sections. The Svelte tutorial has a "Solve" button. The Astro tutorial provides "Check in" pages that recap the expected state of the project.

The NeoHaskell plan has *none* of this. Exercises are described, but the plan doesn't specify what happens when the reader can't complete one.

**My specific recommendation:**

1. **Add a "Hints" system to the exercise template.** Each exercise should have 2-3 progressive hints stored in Starlight `<details>` components:

   ```markdown
   <details>
   <summary>Hint 1</summary>
   Think about which event type you need to add first.
   </details>

   <details>
   <summary>Hint 2</summary>
   You'll need a new constructor in your `Event` type: `DailyWithdrawalLimitSet Amount`.
   </details>

   <details>
   <summary>Show solution</summary>
   [Complete code solution with explanation]
   </details>
   ```

2. **Add solution verification.** After each exercise, include a "Check your work" section with the expected output or a diff against the expected code. The reader needs to know: "Did I do this right?" This is distinct from the checkpoint sections — checkpoints verify tutorial progress, solution verification checks exercise completion.

3. **Design the "I'm stuck" escape hatch.** Every exercise should end with: "Stuck? Join the [#tutorial channel on Discord](link) or check the [solution code on GitHub](link)." This isn't a failure state — it's a support system.

---

## XI. The "What We're NOT Doing" Section Contains a DX Anti-Pattern

**Responding to: "Beginner / Intermediate / Advanced language tiers" (line 765)**

The plan rejects beginner/intermediate/advanced labels:

> Artificial divisions that create anxiety ("Am I intermediate yet?"). Instead, the tutorial progression naturally moves from simple to complex syntax.

I partially agree — labeling *readers* as beginner/intermediate/advanced is problematic. But labeling *content* with difficulty indicators is a universal DX pattern, and rejecting it entirely is a mistake.

The synthesis identifies "Complexity Ratings" as Pattern 15 (from Stripe and Vue) and "What You'll Learn upfront" as Pattern 16 (from Stripe, React, Astro, FastAPI). Both are forms of difficulty signaling that don't label the *reader* — they label the *page.*

**Why this matters:**

A developer browsing the Guides section sees:
- Testing event-sourced systems
- Deployment
- PostgreSQL event store
- Error handling
- Integrating existing systems

Which of these is a 10-minute read and which is a 45-minute deep dive? Which requires Tutorial Part 4 as a prerequisite and which can be read standalone? Without difficulty indicators, the reader has to click into each page and discover this information. That's unnecessary friction.

**My specific recommendation:**

Don't label readers. DO label content. Add to each guide and concept page's frontmatter:

```yaml
---
title: Testing Event-Sourced Systems
description: How to test commands, events, and projections
reading_time: 15 minutes
prerequisites: [Tutorial Part 2, Core Concepts: Events]
---
```

Display `reading_time` and `prerequisites` on the page and optionally in the sidebar. This gives readers the information they need to self-select without creating anxiety about their "level."

---

## XII. The Plan's Visual Design Language Is Completely Absent

**Responding to: The plan as a whole — the complete absence of visual design specification**

This is perhaps the gap that's most in my wheelhouse and most absent from the plan. There is no specification for:

- **Color coding by section.** Should tutorial pages look different from concept pages? Many docs sites use subtle color accents to differentiate sections. Astro's docs use different tab highlight colors for each top-level section.

- **Typography hierarchy.** How do code blocks, prose, callouts, and exercises visually differentiate? The plan describes different content *types* (narrative, exercises, checkpoints, asides) but doesn't specify how they look different on the page.

- **Iconography.** The plan mentions "wow" moments and emotional design but uses no visual language to reinforce them. A checkmark icon next to completed checkpoints. A warning icon next to "Break It" exercises. A rocket icon next to "you just built" moments. These small visual cues create emotional punctuation in the reading experience.

- **The event flow diagram design.** Steve's review correctly identifies the need for architectural diagrams. But what should they look like? SVG? Mermaid? Hand-drawn (ala Julia Evans)? Pixel-perfect technical diagrams? The visual style of diagrams sets the tone for the entire docs site. Hand-drawn feels approachable. Technical diagrams feel authoritative. The choice should be deliberate.

**My specific recommendation:**

1. **Adopt a visual design system** before writing content. This doesn't need to be complex — 5 decisions:
   - Section accent colors (e.g., tutorial = blue, concepts = purple, guides = green, reference = gray)
   - Exercise visual pattern (e.g., yellow background, pencil icon)
   - Checkpoint visual pattern (e.g., green background, checkmark icon)
   - Diagram style (I'd recommend clean SVG with the approachable, slightly rounded aesthetic of modern dev tools — not academic, not hand-drawn)
   - "Wow" moment visual pattern (e.g., a subtle celebration callout with a sparkle icon after a significant achievement)

2. **Create a custom Starlight component for exercises.** Rather than using raw `<details>` tags, create an `<Exercise>` component that consistently renders with the exercise visual pattern. This ensures every exercise looks the same across the entire docs, which builds reader recognition: "yellow box = I need to do something."

3. **Create custom Starlight components for checkpoints.** An `<VerifyProgress>` component that renders with the checkpoint visual pattern. Again, consistency builds recognition.

---

## XIII. The "Using AI" Guide Is Excellent but Shouldn't Be Buried in Guides

**Responding to: Phase 1 tasks (lines 612-613) and Guides file structure (line 726)**

The plan includes `guides/using-ai.mdx` — a guide for using AI tools with NeoHaskell, covering the 5 most common AI mistakes and how to spot wrong patterns. This is genuinely forward-thinking and I haven't seen another language documentation plan that addresses this explicitly.

**My critique:** It's buried in the Guides section. In 2026, AI-assisted coding is the default for a huge portion of developers. A developer trying NeoHaskell will immediately paste code into Claude, Copilot, or ChatGPT. If the AI generates `pure` instead of `Task.yield`, the developer will think NeoHaskell is broken, not the AI.

This isn't a guide — it's a *critical path* concern. It belongs in Getting Started, not in Guides.

**My specific recommendation:**

1. **Move "Using AI with NeoHaskell" to the Getting Started section**, between `installation.mdx` and `first-events.mdx`. Make it a short, prominent page with:
   - A copy-paste system prompt for AI tools
   - The top 5 mistakes in a scannable table
   - A `:::caution` callout: "AI tools don't know NeoHaskell's conventions. Always check generated code against the patterns in this tutorial."

2. **Add an AI-related `:::caution` callout to Tutorial Part 1:**

   ```
   :::caution[Using AI?]
   If you're using Copilot or ChatGPT, it may suggest `pure` instead of `Task.yield` 
   or `$` instead of `|>`. These are valid Haskell but not idiomatic NeoHaskell. 
   See [Using AI with NeoHaskell](/getting-started/using-ai) for the full list.
   :::
   ```

3. **Reference the AI guide in the "Coming from Haskell" page's AI mistakes section** (already planned, good) AND in every "Coming From..." page. A JavaScript developer using Copilot will get different wrong patterns than a Haskell developer using Copilot.

---

## XIV. Progress Indicators Are Missing from the Tutorial Design

**Responding to: The tutorial design as a whole, and Astro's tutorial tracker (astro.md, lines 36-39)**

The Astro research explicitly documents Astro's **Tutorial Tracker** component:

> - Tutorial Tracker component (left sidebar showing progress: "Unit 0 (0/2 Complete)")
> - Checkbox-based progression (users can mark lessons complete)
> - Sequential navigation (Previous/Next links)

The Astro docs use this on the *same platform* NeoHaskell is building on. The component exists. It's proven. The NeoHaskell plan never mentions progress tracking.

This is a DX gap because tutorial completion rates are one of the plan's own Success Metrics (line 774):

> **Tutorial completion rate**: >50% of people who start tutorial part 1 finish part 4.

You cannot improve what you don't measure, and you cannot encourage completion without making progress *visible.* A 6-part tutorial without a progress indicator feels open-ended. "How much more is there?" is a question that, left unanswered, leads to abandonment. A visible "Part 3 of 6" indicator transforms the same content from "indefinite commitment" to "I'm halfway there."

**My specific recommendation:**

1. **Implement a tutorial progress tracker** in Phase 0, not Phase 4. If Astro's tutorial tracker component is available in Starlight, use it directly. If not, build a minimal version: a sidebar element showing "Part X of 6" with checkmarks for completed parts. Use `localStorage` to persist state.

2. **Add estimated reading time** to each tutorial part. "Part 3: Transaction History (~20 minutes)." This manages expectations and reduces "how long will this take?" anxiety.

3. **Add "Check-in" pages between tutorial parts**, as Astro does ("Check in: Unit 2 - Pages"). These serve three purposes: they recap what was learned, they verify the reader's project state matches expectations, and they create natural stopping points. A reader who needs to stop mid-tutorial should stop at a check-in page, not mid-section.

---

## XV. i18n Is Already Set Up but the Plan Doesn't Leverage It Strategically

**Responding to: AGENTS.md (which notes auto-generated translations) and the plan's silence on internationalization**

The AGENTS.md file reveals that NeoHaskell's website already has auto-generated translations for Spanish, Russian, Armenian, French, and Japanese. The plan never mentions internationalization at all.

This is relevant because the Astro research (astro.md) and Vue research (vue.md) both identify multi-language support as standout features. Vue has 14 translations. Astro has 12+.

But auto-generated translations need verification, and the plan should address this:

**My specific recommendation:**

1. **Add a "Translation Quality" item to Phase 4.** Auto-generated translations are a starting point, not a solution. Identify which languages have the most users (probably Japanese and Spanish, based on typical developer demographics) and prioritize human review for those.

2. **Add language-specific content considerations.** The NeoBank tutorial uses English-language concepts (deposit, withdrawal, balance) that translate well. But the "Coming From..." pages reference language-specific concepts (JavaScript's `let`, Python's ORM) that may need cultural adaptation, not just translation.

3. **Use Starlight's i18n sidebar labels** (documented in astro.md) to present the sidebar in each language. This is a low-effort, high-impact DX improvement for non-English readers.

---

## Summary of Recommendations (Ranked by Impact on Developer Experience)

1. **Design the landing page experience** with three audience-specific paths. This is where most developers will decide to stay or leave. (Section I)

2. **Adopt Starlight components systematically** — Tabs, Asides, CardGrids, code labels, Steps, Badges — from Phase 0, not as future work. (Section II)

3. **Solve the installation friction problem** with a zero-install path (playground/Codespaces) as the primary option. (Section III)

4. **Serve the Architect and Decision-Maker audiences** with purpose-built content, not just spillover from developer docs. (Section IV)

5. **Add progress indicators and time estimates** to the tutorial. You can't improve completion rates without visibility. (Section XIV)

6. **Design the "Coming from CRUD" page** as a first-class, richly specified document equal to "Coming from Haskell." (Section VI)

7. **Add graduated hints and solutions to exercises** so stuck readers have somewhere to go besides giving up. (Section X)

8. **Design cross-referencing systematically** with a matrix specifying link types between all four Diataxis modes. (Section IX)

9. **Move the "Using AI" guide to Getting Started** and add AI-related callouts throughout the tutorial. (Section XIII)

10. **Address mobile experience** with line-length limits, responsive table alternatives, and phone testing. (Section VII)

11. **Design for search** with concept-name headings, synonym coverage, and context callouts for search-driven visitors. (Section VIII)

12. **Adopt a visual design system** with section colors, exercise components, checkpoint components, and diagram style before writing content. (Section XII)

13. **Design the emotional journey beyond the tutorial** — troubleshooting callouts, "What's Next" transitions, and the "I'm stuck" escape hatch. (Section V)

14. **Add difficulty indicators to content** (reading time, prerequisites) without labeling readers. (Section XI)

15. **Address i18n quality** for auto-generated translations, starting with highest-traffic languages. (Section XV)

---

## Where I Agree with Steve — and Where I Diverge

Steve's review is excellent and I agree with most of it. A few points where I want to build on or nuance his recommendations:

**Layer 2 (syntax sidebar):** Steve says drop it entirely and integrate explanations inline. I mostly agree, but I'd go one step further: the issue isn't just the sidebar — it's that the plan doesn't leverage Starlight's `:::tip` asides, which are the *right* mechanism for this. A `:::tip[New syntax]` aside inline in the tutorial flow is less disruptive than a collapsible sidebar, more visible than a parenthetical, and it uses the platform's native component. It's progressive disclosure done right.

**Alex the character:** Steve says let Alex fade by Part 2. I'd refine this: Alex should be a *framing device* for Part 1's opening paragraph, not a persistent character. Don't use Alex's name in section headings or exercise prompts. Use "you" everywhere. Alex's purpose is to establish "you're a developer building a bank" — once that's established, Alex has served their purpose.

**Visual aids:** Steve correctly identifies the gap and recommends SVG diagrams. I want to push harder: the event flow diagram should be a *recurring visual motif* that appears on the landing page, in Tutorial Part 1, on every concept page, and in the architecture evaluation content. It should be the visual identity of NeoHaskell's documentation — the way Rust's crab is their mascot, the event flow diagram should be NeoHaskell's signature image. Make it beautiful. Make it memorable. Put it everywhere.

**The research contradiction:** Steve points out that the "What We're NOT Doing" section contradicts the research that was actually done. I agree this should be fixed, but I'd frame it positively: the research was clearly valuable. Own it. The "What We're NOT Doing" section should be about genuinely rejected approaches, not about dismissing work that was actually done and useful.

---

## Closing Thought

The best documentation I've ever used — Vue, Stripe, Tailwind, the modern React docs — share a quality that's hard to name but easy to feel: *they respect your time and your intelligence simultaneously.* They don't talk down to you. They don't waste your time. They anticipate your confusion and address it before you feel it. They make you feel like the person who wrote them was thinking about *you specifically.*

This plan has the foundation for that quality. The audience model is empathetic. The banking domain is clever. The "implicit-first" teaching philosophy is exactly right. The misconceptions list is rigorous.

What's missing is the *experience layer* — the surfaces, the navigation, the visual language, the emotional design, the Starlight components, the mobile consideration, the search optimization, the cross-referencing system, the progress indicators. The plan designs great content. It doesn't yet design a great *product.* And documentation, at its best, is a product.

Build the experience first. Then fill it with your excellent content.

— Sarah Drasner