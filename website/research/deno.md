# Deno Documentation Analysis

## Navigation Structure

Deno's documentation uses a clear three-tier hierarchy:

**Primary Navigation (Top Level)**
- Docs (Runtime documentation)
- API reference
- Examples
- Services (Deploy, Subhosting)

**Runtime Docs Sidebar Structure**
1. **Getting started** (5 pages)
   - Welcome to Deno
   - Installation
   - First project
   - Setup your environment
   - Command line interface

2. **Fundamentals** (14 pages)
   - TypeScript
   - **Node** (migration/compatibility)
   - Security
   - Modules and dependencies
   - Configuration
   - Web development
   - Testing
   - Debugging
   - Workspaces
   - Linting and formatting
   - HTTP Server
   - FFI
   - OpenTelemetry
   - Stability and releases

3. **Reference guides**
   - CLI (30+ subcommands, each with its own page)
   - Standard library (30+ modules)
   - Migration guide (Deno 1.x to 2.x)
   - Continuous integration
   - Environment variables
   - VS Code integration
   - JSX and React
   - Bundling
   - WebAssembly
   - Docker
   - LSP integration

4. **Contributing and support**
   - Contributing overview
   - Architecture
   - Profiling
   - Release schedule
   - Style guide
   - Documentation
   - Examples
   - Help

**API Reference Structure**
- Organized by namespace (Deno, Web, Node)
- Further categorized by domain (Cloud, Errors, Fetch, FFI, File System, GPU, HTTP Server, I/O, Network, Permissions, Runtime, Subprocess, Testing, WebSockets)
- "All symbols" index for each namespace

**Examples Structure**
- 15 categories (Basics, Modules, Web APIs, Frameworks, Testing, Deployment, Databases, etc.)
- 200+ individual examples
- Filterable by type (Examples, Tutorials, Videos)

## Page Types

Deno documentation employs distinct page types for different purposes:

**1. Landing Pages**
- "Welcome to Deno" with "Why Deno?" section
- Quick install commands
- "First steps" links
- Bullet-point value propositions

**2. Getting Started Guides**
- Step-by-step tutorials
- Code examples with shell commands
- "Run this" patterns
- Progressive complexity

**3. Fundamentals Pages**
- Concept explanation
- Practical examples
- Code snippets with syntax highlighting
- Links to related topics
- "Quick start" sections at the top

**4. Migration/Compatibility Pages**
- **Node.js compatibility page** (critical for NeoHaskell parallel)
  - "Quick start" section with three immediate examples
  - Side-by-side comparisons
  - "Node to Deno Cheatsheet" table
  - Detailed migration instructions
  - Troubleshooting sections
- **Migration guide** (Deno 1.x to 2.x)
  - Organized by change type (Config, CLI, API)
  - Before/after code diffs
  - Rationale for changes

**5. Reference Pages**
- Exhaustive API documentation
- Parameter tables
- Return value descriptions
- Code examples for each function
- Cross-references

**6. Examples/Tutorials**
- Problem statement
- Complete working code
- Step-by-step explanation
- "Run it" command
- Related examples links

## Content Patterns

**Code Example Structure**
Every code example follows this pattern:
```
[Optional: Filename label]
```language
code here
```
[Shell command to run it]
```

**Migration Content Pattern** (Node.js page)
1. **Immediate value**: "Quick start" section at the top with three runnable examples
2. **Compatibility statement**: "Deno is Node-compatible. Most Node projects will run in Deno with little or no change!"
3. **Simple import pattern**: `import { Hono } from "npm:hono";` (no install step needed)
4. **Detailed sections**: Built-in modules, npm packages, globals, CommonJS support
5. **Cheatsheet**: Direct command comparisons (node → deno)
6. **Troubleshooting**: "Hints and suggestions" sections throughout

**TypeScript-First Approach**
- All examples default to TypeScript
- No separate "TypeScript version" sections
- Type annotations shown naturally
- "Built-in TypeScript support" as a core feature

**Progressive Disclosure**
- Start with simplest case
- Add complexity incrementally
- "See also" links to advanced topics
- Collapsible sections for edge cases

**Inline Hints and Warnings**
- `Note:` callouts for important details
- `Caution:` for potential pitfalls
- `See [link] for details` cross-references
- Helpful error messages quoted directly

## Progression Model

**Learning Path Architecture**

1. **Immediate Start** (5 minutes)
   - Install command
   - Hello World
   - Run a script
   - No configuration needed

2. **First Project** (15 minutes)
   - `deno init` scaffolding
   - Project structure
   - Running tests
   - Basic imports

3. **Fundamentals** (Ordered by frequency of need)
   - TypeScript (it just works)
   - **Node compatibility** (second item in Fundamentals!)
   - Security model
   - Modules and dependencies
   - Testing
   - HTTP servers

4. **Migration** (For existing developers)
   - Node.js compatibility page (in Fundamentals)
   - Migration guide (in Reference)
   - Framework-specific guides (Next.js, Express, etc.)

5. **Deep Dive** (Reference material)
   - CLI reference
   - API documentation
   - Standard library

**Node.js Migration Flow** (Critical for NeoHaskell)
The Node.js compatibility page is structured to minimize friction:

1. **Reassurance**: "Deno is Node-compatible" (first sentence)
2. **Proof**: Three working examples in "Quick start"
3. **Key insight**: `npm:` specifier is all you need
4. **Detailed explanation**: How it works under the hood
5. **Edge cases**: CommonJS, globals, private registries
6. **Cheatsheet**: Command-by-command translation
7. **Troubleshooting**: Common issues and solutions

This progression assumes developers are coming from Node.js and need to be convinced it's worth switching.

## Standout Features

**1. Migration-First Mindset**
- Node.js compatibility is the **second item** in Fundamentals (after TypeScript)
- Dedicated "Migration guide" in Reference section
- "Node to Deno Cheatsheet" table for quick reference
- Framework compatibility list (Next.js, Remix, Svelte, etc.)
- Private registry support documented

**2. Cheatsheet Tables**
The Node to Deno cheatsheet is brilliant:
```
| Node.js              | Deno                    |
|----------------------|-------------------------|
| node file.js         | deno file.js            |
| ts-node file.ts      | deno file.ts            |
| npm install          | deno install            |
| npm run              | deno task               |
```
Simple, scannable, actionable.

**3. "It Just Works" Messaging**
- "TypeScript-ready out of the box. Zero config or additional steps necessary."
- "No `npm install` is necessary before the `deno run` command"
- "Deno understands `package.json` in your project"

**4. Inline Troubleshooting**
Error messages are shown directly in docs:
```
$ deno run main.mjs
error: Relative import path "os" not prefixed with / or ./ or ../
  hint: If you want to use a built-in Node module, add a "node:" prefix
```
This teaches the fix before users encounter the error.

**5. Comprehensive Examples Section**
- 200+ examples organized by category
- Filterable by type (example, tutorial, video)
- Each example is runnable
- Links to related examples

**6. API Organization by Use Case**
API reference is organized by **what you're trying to do** (File System, Network, HTTP Server) rather than alphabetically. This is user-centric design.

**7. Version Migration Guide**
The Deno 1.x to 2.x migration guide is exemplary:
- Organized by change type (Config, CLI, API)
- Every change has before/after code
- Rationale explained
- Links to GitHub issues for context

## Applicable to NeoHaskell

### Direct Parallels

**1. "Coming from Haskell" = "Node and npm Compatibility"**
Deno's Node.js page is the perfect template for NeoHaskell's "Coming from Haskell" page:

- **Start with reassurance**: "NeoHaskell is Haskell-compatible. Most Haskell concepts translate directly!"
- **Quick start section**: Three immediate examples showing familiar Haskell patterns in NeoHaskell
- **Key differences table**: Side-by-side comparison of syntax/concepts
- **Detailed sections**: Type classes → Interfaces, Lazy evaluation → Strict by default, etc.
- **Cheatsheet**: Haskell → NeoHaskell command/syntax translation
- **Troubleshooting**: "If you see X error, it's because Y changed"

**2. Cheatsheet Format**
Create a "Haskell to NeoHaskell Cheatsheet":
```
| Haskell                    | NeoHaskell                |
|----------------------------|---------------------------|
| data Maybe a = ...         | type Maybe a = ...        |
| class Functor f where ...  | interface Functor f ...   |
| do notation                | do notation (same!)       |
| lazy by default            | strict by default         |
```

**3. Migration Guide Structure**
If NeoHaskell has breaking changes from Haskell, use Deno's migration guide format:
- Organize by category (Syntax, Type System, Standard Library)
- Show before/after code for every change
- Explain **why** the change was made (not defensive, just factual)
- Link to ADRs for detailed rationale

**4. "It Just Works" Messaging**
Deno emphasizes zero-config, batteries-included. NeoHaskell should do the same:
- "Type inference just works. No language extensions needed."
- "Built-in formatter and linter. No hlint or brittany configuration."
- "Standard library included. No Stackage snapshots to manage."

**5. Inline Error Messages**
Show what errors look like and how to fix them:
```
$ neohaskell run main.nh
error: Lazy evaluation not supported
  hint: NeoHaskell is strict by default. Use `lazy` keyword for deferred evaluation.
```

**6. Progressive Disclosure**
Structure "Coming from Haskell" page like Deno's Node page:
1. **Quick start**: Three examples that work immediately
2. **Core concepts**: What's the same, what's different
3. **Detailed sections**: Deep dive into each difference
4. **Cheatsheet**: Quick reference
5. **Troubleshooting**: Common migration issues

### Specific Recommendations

**1. Create a "Rosetta Stone" Page**
Deno has a cheatsheet. NeoHaskell needs a comprehensive Rosetta Stone for Haskell developers:
- Syntax translations
- Type system mappings
- Standard library equivalents
- Common patterns (Maybe/Either, Functor/Monad, etc.)
- Build tool commands

**2. Explain Divergences Without Defensiveness**
Deno's migration guide explains changes factually:
> "The `deno bundle` command has been removed. We recommend using `esbuild`..."

NeoHaskell should do the same:
> "Lazy evaluation is not default in NeoHaskell. Strict evaluation provides better performance and predictability for web applications. Use the `lazy` keyword when deferred evaluation is needed."

**3. Show the "Why Switch" Value Prop**
Deno's landing page has bullet points:
- TypeScript-ready out of the box
- Secure by default
- Robust built-in toolchain
- Fully compatible with Node and npm

NeoHaskell's "Coming from Haskell" should have similar bullets:
- Simpler type system (no extensions needed)
- Faster compile times
- Built-in tooling (formatter, linter, test runner)
- Web-first standard library
- Familiar syntax for Haskell developers

**4. Use Tables for Quick Scanning**
Deno's cheatsheet table is instantly scannable. Use tables for:
- Command translations
- Syntax comparisons
- Type system mappings
- Standard library equivalents

**5. Provide Migration Paths**
Deno shows how to migrate incrementally:
> "You can use `deno install` on a Node.js project to install dependencies, run `deno fmt` to format code without needing Prettier..."

NeoHaskell should show incremental adoption:
> "You can start by converting pure functions to NeoHaskell while keeping IO code in Haskell. Use the FFI to call between them."

**6. Document the Compatibility Layer**
Deno documents exactly how Node.js compatibility works (npm: specifier, node: prefix, etc.). NeoHaskell should document:
- How Haskell libraries can be used
- What works out of the box
- What requires adaptation
- Performance implications

**7. Create a "Common Pitfalls" Section**
Deno has "Hints and suggestions" throughout. NeoHaskell needs:
- "If you're used to lazy evaluation..."
- "If you expect type class resolution to..."
- "If you're looking for the Prelude..."

**8. Organize by User Journey**
Deno puts Node compatibility **second** in Fundamentals because that's what users need. NeoHaskell should put "Coming from Haskell" early and prominently.

### Content Structure for "Coming from Haskell"

Based on Deno's Node.js page, here's a recommended structure:

```markdown
# Coming from Haskell

NeoHaskell is designed for Haskell developers. Most Haskell concepts translate directly, with a few intentional simplifications for web development.

## Quick Start

### Familiar Syntax
[Show a simple function that looks identical]

### Type Inference
[Show type inference working the same way]

### Different Default
[Show one key difference, like strict evaluation]

## What's the Same

- Pure functions
- Algebraic data types
- Pattern matching
- Type inference
- do notation
- [etc.]

## What's Different

### Strict by Default
[Explanation + code example]

### Simplified Type Classes
[Explanation + code example]

### No Language Extensions
[Explanation + code example]

## Haskell to NeoHaskell Cheatsheet

| Haskell | NeoHaskell |
|---------|------------|
| ...     | ...        |

## Migration Guide

### Converting Pure Functions
[Step-by-step]

### Handling IO
[Step-by-step]

### Using Haskell Libraries
[Step-by-step]

## Common Questions

### Why is lazy evaluation not default?
[Factual explanation]

### How do I use my favorite Haskell library?
[Practical answer]

### What about Monad transformers?
[Practical answer]
```

This structure mirrors Deno's approach: reassure, demonstrate, explain, provide tools, answer questions.
