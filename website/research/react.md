# React Documentation Analysis

## Navigation Structure

React.dev uses a two-tier top-level navigation split:

1. **Learn** (https://react.dev/learn) - Tutorial and concept-driven content
   - Quick Start (80% of daily React concepts in one page)
   - Tutorial: Tic-Tac-Toe (hands-on project)
   - Thinking in React (mental model building)
   - Core concepts (State, Components, Hooks, etc.)
   - Deep dives (nested within concept pages)

2. **Reference** (https://react.dev/reference) - API documentation
   - React APIs (useState, useEffect, etc.)
   - React DOM APIs
   - Organized by import path and function signature

The sidebar is deeply nested (3-4 levels) but the Learn section flows linearly while Reference is organized alphabetically by API surface. Each Learn page links forward to the next logical concept.

## Page Types

React.dev has five distinct page types:

1. **Quick Start** (https://react.dev/learn) - Single-page crash course covering 80% of daily React usage. Includes inline sandboxes for every concept. Designed for "I need to start building today" developers.

2. **Tutorial** (https://react.dev/learn/thinking-in-react) - Step-by-step project walkthroughs. "Thinking in React" teaches the five-step design process (mockup → component hierarchy → static version → minimal state → data flow). Each step builds on the previous with live code.

3. **Concept Pages** (https://react.dev/learn/state-a-components-memory) - Deep explanations of a single idea (state, props, effects). Structure: problem statement → solution → interactive examples → pitfalls → deep dives (expandable sections).

4. **API Reference** (https://react.dev/reference/react/useState) - Exhaustive function documentation. Format: signature → parameters → returns → caveats → usage examples → troubleshooting. Every parameter and return value is documented with types.

5. **Deep Dive Sections** - Expandable `<DeepDive>` components embedded in concept pages. Example: "How does React know which state to return?" inside the useState concept page. These are optional rabbit holes for curious readers.

## Content Patterns

Every React.dev page follows a strict component-based structure:

**Universal Elements:**
- `<Intro>` block at the top (2-3 sentences, sets expectations)
- `<YouWillLearn>` bulleted list (concrete outcomes, not vague goals)
- Inline `<Sandpack>` components (editable, runnable code in every example)
- `<Pitfall>` warnings (red boxes for common mistakes)
- `<DeepDive>` expandable sections (optional advanced content)
- `<Note>` callouts (conventions, best practices)

**Code Examples:**
- Every code snippet is runnable via Sandpack (in-browser React runtime)
- Code includes line highlighting (`{5}` syntax) to draw attention to key lines
- Examples progress from minimal to realistic (button → form → todo app)
- No "foo/bar" placeholders - all examples use real-world domains (sculptures, products, todos)

**Cross-References:**
- Inline links to related concepts (`[state](/learn/state-a-components-memory)`)
- "Read X to learn more" at the end of sections
- API reference pages link back to concept explanations
- External links to MDN for JavaScript fundamentals

**Interactive Elements:**
- Sandpack editors on every page (not just tutorials)
- Diagrams with step-by-step annotations (component trees, data flow)
- Before/after comparisons (wrong code → correct code)

## Progression Model

React.dev uses a **hub-and-spoke** learning model:

**Hub: Quick Start**
- The Quick Start page (https://react.dev/learn) is the entry point for all learners
- Covers components, JSX, props, state, events, hooks, and data flow in one scrollable page
- Each section has a "Next Steps" link to deeper content
- Designed to get developers productive in 20 minutes

**Spoke 1: Tutorial (Tic-Tac-Toe)**
- Hands-on project for kinesthetic learners
- Builds a complete game from scratch
- Introduces concepts just-in-time (state when you need to track moves, props when you need to pass data)
- Ends with "Where to go from here" links to concept pages

**Spoke 2: Thinking in React**
- Mental model for designing React apps
- Five-step process: mockup → hierarchy → static → state → data flow
- Uses a realistic example (product table with search/filter)
- Teaches the "React way" of breaking down problems

**Spoke 3: Concept Deep Dives**
- Each concept page (state, effects, refs) is self-contained
- Assumes you've read Quick Start but not other concept pages
- Progressive disclosure via `<DeepDive>` sections
- Links to related concepts at decision points ("If you want to reset state, see [Preserving and Resetting State](/learn/preserving-and-resetting-state)")

**Reference: Non-Linear**
- API docs are not meant to be read sequentially
- Organized for lookup, not learning
- Every API page links back to the concept that explains when to use it

**Prerequisite Signaling:**
- No explicit "beginner/intermediate/advanced" labels
- Prerequisites are contextual: "If you're not familiar with X, read [link] first"
- Quick Start assumes JavaScript knowledge but links to MDN for unfamiliar syntax
- Concept pages assume Quick Start knowledge

**Learn vs. Reference Split:**
- Learn = "why and when" (mental models, decision-making)
- Reference = "what and how" (exact syntax, parameters, edge cases)
- Every Learn page links to relevant Reference pages
- Every Reference page links back to the Learn page that explains the concept

## Standout Features

**1. Sandpack (Inline Runnable Code)**
- Every code example is editable and runs in the browser
- No "copy to CodeSandbox" friction - just edit and see results
- Examples include CSS and multiple files when needed
- Sandpack is React.dev's killer feature - it eliminates the "does this actually work?" doubt

**2. `<Pitfall>` Components**
- Red warning boxes for common mistakes
- Appear exactly where developers make the mistake (not buried in a "Common Mistakes" appendix)
- Example from useState page: "Calling the `set` function does not change state in the already executing code" with a code snippet showing the wrong mental model

**3. Recipes Pattern**
- `<Recipes>` component groups related examples (Counter, Text Field, Checkbox, Form)
- Each recipe is a complete, runnable example
- Lets readers compare approaches side-by-side
- Example: "Passing the updater function" vs. "Passing the next state directly" recipes on useState page

**4. Diagrams with Annotations**
- Component tree diagrams show parent/child relationships
- Data flow diagrams animate state changes
- Diagrams are not decorative - they're teaching tools with step-by-step captions
- Example: "Thinking in React" has a mockup with colored boxes showing component boundaries

**5. Troubleshooting Sections**
- Every API reference page ends with a "Troubleshooting" section
- Organized by error message or symptom ("I've updated the state, but logging gives me the old value")
- Explains the mental model error, not just the fix
- Example: useState troubleshooting explains React's snapshot model to fix the "stale state" confusion

**6. Progressive Disclosure via `<DeepDive>`**
- Advanced content is hidden by default
- Readers can skip deep dives without losing the main thread
- Deep dives answer "how does this work internally?" questions
- Example: "How does React know which state to return?" deep dive explains the Hook call order mechanism

**7. Real-World Examples**
- No "foo/bar" or "example1/example2" - every example uses a real domain
- Sculpture gallery, product table, todo list, image carousel
- Examples are complex enough to be realistic but simple enough to understand in 30 seconds

**8. Explicit Mental Models**
- React.dev teaches how to think, not just what to type
- "State as a Snapshot" page explains why `console.log(count)` after `setCount(count + 1)` shows the old value
- "Thinking in React" teaches the five-step design process
- Mental models are named and reinforced across pages

## Applicable to NeoHaskell

**1. Adopt the Hub-and-Spoke Model for NeoBank Tutorial**
- Create a "Quick Start" equivalent that builds a minimal NeoBank feature (create account, deposit, check balance) in one page
- Use this as the entry point, then link to deep dives on event sourcing, types, and syntax
- NeoHaskell's challenge: teaching event sourcing + Haskell syntax simultaneously. React solved "teaching hooks to class-component devs" with Quick Start → deep dives.

**2. Implement Tutorial Layer System with Sandpack-Style Interactivity**
- NeoHaskell's "Tutorial Layer System" (narrative + expandable asides) maps directly to React's `<Intro>` + `<DeepDive>` pattern
- Make "New Syntax" asides expandable like React's `<DeepDive>` components
- Add runnable code examples for every NeoBank feature (if tooling allows browser-based Haskell execution, use it; otherwise, link to a REPL)

**3. Use `<Pitfall>` Pattern for "Break It" Exercises**
- React's `<Pitfall>` components appear exactly where mistakes happen
- NeoHaskell's "Break It" exercises (Bloom's taxonomy) should be inline, not in a separate section
- Example: After showing event sourcing, add a `<BreakIt>` exercise: "What happens if you apply events out of order?" with a runnable example that fails

**4. Create "Thinking in Event Sourcing" Page**
- Mirror React's "Thinking in React" five-step process
- NeoHaskell equivalent: Design → Events → Commands → Projections → Queries
- Use NeoBank as the example (same as React uses product table)
- Teach the mental model shift from CRUD to event sourcing

**5. Adopt Recipes Pattern for Syntax Variations**
- React uses `<Recipes>` to show "updater function vs. direct state" side-by-side
- NeoHaskell can use this for "do notation vs. applicative style" or "record syntax variations"
- Helps readers compare approaches without flipping between pages

**6. Split Learn vs. Reference Early**
- React's Learn/Reference split is crucial for NeoHaskell
- Learn = NeoBank tutorial, event sourcing concepts, when to use X
- Reference = Type signatures, function catalog, compiler flags
- Don't mix them - readers in "learning mode" don't want exhaustive API docs

**7. Use Real-World Domain (NeoBank) Everywhere**
- React never uses "foo/bar" - every example is sculptures, products, todos
- NeoHaskell should use NeoBank (accounts, transactions, balances) in every example
- Consistency helps readers build a mental model of the domain alongside the language

**8. Add Troubleshooting Sections for Compiler Errors**
- React's troubleshooting sections are organized by symptom ("I'm getting an error: 'Too many re-renders'")
- NeoHaskell should do this for common compiler errors ("Could not deduce (Monad m) from the context")
- Explain the mental model error, not just the fix (like React explains snapshot model for stale state)

**9. Implement Progressive Disclosure for Advanced Topics**
- NeoHaskell's Tutorial Layer System already has this (expandable asides)
- Go further: hide type theory, performance optimization, and advanced patterns by default
- Let readers opt-in to complexity (like React's `<DeepDive>` sections)

**10. Create a "Quick Start" Page That Gets Developers Productive in 20 Minutes**
- React's Quick Start covers 80% of daily usage in one page
- NeoHaskell equivalent: Create account, deposit, withdraw, check balance (event sourcing + basic syntax)
- This is the "I need to start building today" page - optimize for time-to-first-working-code

**11. Use Inline Sandboxes (If Possible)**
- React's Sandpack is transformative - it eliminates "does this work?" doubt
- If NeoHaskell can run in the browser (via GHCJS or similar), use inline editors
- If not, link to a hosted REPL with pre-loaded NeoBank examples

**12. Teach Mental Models Explicitly**
- React names its mental models: "State as a Snapshot", "One-Way Data Flow", "Lifting State Up"
- NeoHaskell should name its models: "Events as Facts", "Projections as Views", "Commands as Intentions"
- Reinforce these names across pages to build a shared vocabulary
