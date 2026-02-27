# Go Documentation Analysis

## Navigation Structure

Go's documentation is organized across multiple interconnected domains:

**Primary documentation hub (go.dev/doc/):**
- **Getting Started** (7 tutorials): Installing Go → Tutorial: Getting started → Create a module → Multi-module workspaces → RESTful API with Gin → Generics → Fuzzing → Writing Web Applications
- **Using and understanding Go** (9 guides): Effective Go, FAQ, Editor plugins, Diagnostics, Garbage Collector Guide, Managing dependencies, Fuzzing, Coverage, Profile-guided optimization
- **References** (7 docs): Package Documentation, Command Documentation, Language Specification, Go Modules Reference, go.mod file reference, Memory Model, Contribution Guide, Release History
- **Accessing databases** (9 specialized guides): Tutorial → Opening a handle → Executing SQL → Querying → Prepared statements → Transactions → Canceling operations → Managing connections → SQL injection
- **Developing modules** (7 guides): Developing and publishing → Release workflow → Managing source → Organizing a module → Major version updates → Publishing → Version numbering
- **Talks** (4 featured): Video Tour, Code that grows with grace, Go Concurrency Patterns, Advanced Concurrency
- **Codewalks** (3 interactive): First-Class Functions, Markov chain, Share Memory by Communicating
- **From the Blog** (organized by topic): Language (7 posts), Packages (4 posts), Modules (4 posts), Tools (9 posts)

**Separate but integrated resources:**
- **A Tour of Go** (go.dev/tour/): Interactive browser-based tutorial with runnable code
- **Effective Go** (go.dev/doc/effective_go): Standalone comprehensive style guide
- **Package documentation** (pkg.go.dev/std): Separate site for standard library reference
- **Language Specification** (go.dev/ref/spec): Formal language definition

The hierarchy is flat at the top level but deeply nested within sections. The main docs page acts as a curated index, not a tree navigation.

## Page Types

Go employs distinct documentation formats for different purposes:

**1. Interactive Tutorial (Tour of Go)**
- Browser-based, no installation required
- Four sections: Basics → Methods/Interfaces → Generics → Concurrency
- Each lesson has runnable code editor with immediate execution
- Exercises at section ends
- Can also be installed locally via `go install`

**2. Comprehensive Style Guide (Effective Go)**
- Long-form prose document (single page, ~50 sections)
- Organized by topic: Formatting → Commentary → Names → Semicolons → Control structures → Functions → Data → Initialization → Methods → Interfaces → Embedding → Concurrency → Errors → Web server
- Code examples throughout, but not runnable
- Explicitly positioned as "must read after tour and spec"
- Includes a disclaimer (added 2022) that it hasn't been updated since 2009 and doesn't cover modern features like modules, generics, or build system changes

**3. Task-Oriented Tutorials**
- Step-by-step guides with concrete outcomes ("Tutorial: Getting started", "Tutorial: RESTful API with Gin")
- Include copy-paste commands with visual copy button
- Show expected output
- Link to next steps

**4. Conceptual Guides**
- Explain "how things work" (Garbage Collector Guide, Memory Model)
- Mix prose with diagrams and code snippets
- Often link to related reference docs

**5. Reference Documentation**
- Package docs (pkg.go.dev): Auto-generated from source comments, includes examples that are runnable in browser
- Language Specification: Formal grammar and semantics
- Command docs: CLI tool reference

**6. Codewalks**
- Annotated source code walkthroughs
- Show real implementation with explanatory commentary
- Three examples on main page (functions, Markov chains, concurrency)

**7. Blog Posts**
- Organized by topic on main docs page
- Deep dives into specific features or patterns
- Often written when features are introduced

## Content Patterns

**Code Examples:**
- Inline code blocks use syntax highlighting
- Many examples are runnable in browser (Tour, package docs)
- Copy-paste button appears on hover for command-line snippets
- Examples in package docs are actual test files that run in CI
- Code examples show both the code and expected output
- No explicit "source file path" references (code is self-contained or from standard library)

**Cross-References:**
- Heavy linking between related docs (e.g., Effective Go links to Tour, Spec, and package docs)
- Package docs link to related packages
- Blog posts link to relevant reference docs
- Main docs page curates links to blog posts by topic

**Page Structure:**
- Most pages have no sidebar navigation (single-page scroll)
- Tour has left sidebar with lesson list
- Package docs have package index in left sidebar
- Top navigation bar is consistent across all go.dev pages

**Metadata and Context:**
- Effective Go includes a prominent disclaimer about its age and scope
- Tutorials show prerequisites ("read tour and spec first")
- Package docs show Go version, import path, and index

**Visual Elements:**
- Minimal use of diagrams
- Code is the primary visual element
- Consistent typography and spacing
- Copy-paste buttons for commands
- Runnable code editors in Tour

## Progression Model

Go's learning path is explicitly structured but not enforced:

**Explicit Sequencing:**
- Effective Go states: "augments the tour and the language specification, both of which should be read first"
- Getting Started section lists tutorials in order: Install → Getting started → Create a module → ...
- Tour is divided into four progressive sections with exercises

**Implicit Progression:**
- Main docs page organizes content by experience level (Getting Started → Using and understanding → References)
- Database guides progress from "Tutorial: Accessing a database" to advanced topics like transactions and connection pooling
- Module guides progress from "Developing and publishing" to "Major version updates"

**No Gatekeeping:**
- All docs are accessible from any entry point
- No "you must complete X before viewing Y"
- Links allow jumping directly to advanced topics

**Multiple Entry Points:**
- Newcomers: Tour → Tutorials → Effective Go
- Experienced developers: Jump to specific guides or reference docs
- Problem-solvers: Search package docs or blog posts

**Skill Signaling:**
- Section headers indicate audience ("Getting Started" vs "References")
- Effective Go's disclaimer helps readers assess relevance
- Tutorial titles indicate scope ("Getting started" vs "Getting started with fuzzing")

**Feedback Loops:**
- Tour includes exercises with solutions
- Tutorials show expected output
- Package docs include runnable examples
- No interactive quizzes or progress tracking

## Standout Features

**1. The Tour of Go (go.dev/tour/)**
- Runnable code in browser without any setup
- Progressive structure (basics → interfaces → generics → concurrency)
- Can be installed locally for offline use
- Influenced many subsequent language tutorials (Rust, Elm, etc.)

**2. Effective Go as "How to Think in Go"**
- Not just style rules, but philosophy and idioms
- Explains *why* Go does things differently (e.g., "Java programs are written in Java, not Go")
- Covers naming, formatting, control structures, concurrency patterns
- Single authoritative document for idiomatic Go
- Honest about its limitations (2022 disclaimer about age)

**3. Runnable Examples in Package Docs**
- Examples are actual test files (`example_test.go`)
- Run in CI to ensure they stay correct
- Executable in browser via Go Playground integration
- Show both code and output
- Linked from package docs with "Example" links

**4. Simplicity-First Philosophy Reflected in Docs**
- No overwhelming navigation trees
- Curated, not comprehensive (main docs page is ~200 lines, not 2000)
- Code examples are minimal and focused
- Prose is direct and concise

**5. Separation of Concerns**
- Tour (interactive learning) is separate from Effective Go (style guide) is separate from Spec (formal definition)
- Each resource has a clear purpose
- pkg.go.dev is a separate site for package browsing

**6. Codewalks**
- Unique format: annotated source code walkthroughs
- Show real implementations, not toy examples
- Three examples on main page (functions, Markov chains, concurrency)

**7. Blog Posts Organized by Topic**
- Main docs page curates blog posts into categories (Language, Packages, Modules, Tools)
- Provides narrative context for features
- Often written by feature authors

**8. Explicit Versioning and Stability**
- Package docs show Go version
- Release notes are prominent
- Effective Go's disclaimer about age is honest and helpful

## Applicable to NeoHaskell

**1. Adopt "Effective NeoHaskell" as Core Conceptual Guide**
- Model after Effective Go: single authoritative document for idioms and philosophy
- Explain "how to think in events" (parallel to Go's "how to think in goroutines")
- Cover naming conventions, readability rules, event-driven patterns
- Position as "must read after tutorial"
- Be honest about scope and maintenance (like Effective Go's 2022 disclaimer)

**2. Create Interactive "Tour of NeoHaskell"**
- Browser-based runnable code examples (like Tour of Go)
- Progressive structure: Basics → Events → Effects → Real-world patterns
- Exercises at section ends
- No installation required for first experience
- Consider making it installable for offline use

**3. Separate Tutorial from Style Guide from Reference**
- Tutorial: Step-by-step getting started (like "Tutorial: Getting started")
- Style Guide: "Effective NeoHaskell" (like Effective Go)
- Reference: API docs, language spec (like pkg.go.dev and go.dev/ref/spec)
- Don't mix these concerns in a single document

**4. Use Runnable Examples in Documentation**
- Make code examples executable (Go Playground model)
- Show expected output alongside code
- Ensure examples are tested in CI (like Go's `example_test.go`)
- Add "Run" button to code blocks where possible

**5. Create "Reading NeoHaskell" as a Codewalk**
- Model after Go's codewalks: annotated source walkthrough
- 5-minute guided tour of a real NeoHaskell program
- Explain idioms in context (qualified imports, explicit lambdas, event patterns)
- Show how readability rules manifest in real code

**6. Organize Docs by Task, Not by Feature**
- Database section: "Accessing databases" with 9 task-oriented guides
- Module section: "Developing modules" with 7 workflow guides
- For NeoHaskell: "Building web apps", "Handling events", "Managing state"

**7. Curate Blog Posts by Topic on Main Docs Page**
- Don't hide blog posts in a separate section
- Organize by topic (Language, Patterns, Tools)
- Link from main docs page as supplementary reading
- Use blog posts to explain "why" and provide narrative context

**8. Keep Main Docs Page Curated, Not Comprehensive**
- Go's main docs page is ~200 lines of organized links
- Not a tree navigation, but a curated index
- Group by audience (Getting Started → Using → References)
- Don't try to list every page

**9. Add Copy-Paste Buttons to Command Examples**
- Go shows copy button on hover for CLI commands
- Reduces friction for following tutorials
- Small UX detail with big impact

**10. Be Explicit About Prerequisites and Sequencing**
- "Read X before Y" (like Effective Go's opening)
- Section headers indicate audience level
- Tutorial titles indicate scope
- Don't hide advanced topics, but signal them clearly

**11. Provide Multiple Entry Points**
- Newcomers: Tour → Tutorial → Effective NeoHaskell
- Experienced Haskellers: Jump to "Differences from Haskell" or API reference
- Problem-solvers: Search guides or examples
- Don't force linear progression

**12. Create "Cheat Sheet" as Task-Oriented Reference**
- Model after Go's task-oriented database guides
- "I want to... / Write this..." format
- Quick lookup for common patterns
- Link to deeper explanations

**13. Use Honest Disclaimers**
- Effective Go's 2022 disclaimer is refreshingly honest
- If docs are incomplete or outdated, say so
- Helps readers assess relevance and set expectations

**14. Separate Package Docs from Conceptual Docs**
- Go uses pkg.go.dev for API reference, go.dev/doc for guides
- NeoHaskell could use similar separation
- API docs are auto-generated, guides are hand-written
- Different audiences, different needs
