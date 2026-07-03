# Onklaud 5 — A Source-Verified Teardown (JavaScript Flows)

**Status:** Research notes (v1 — for iteration)
**Date:** 2026-07-03
**Subject:** [KorroAi/onklaud-5](https://github.com/KorroAi/onklaud-5) @ `a7dbf3f`
**Scope:** the JavaScript/TypeScript-handling code paths inside Onklaud 5's harness (the harness itself is 100% Python).
**Method:** each section was extracted from the cloned source, then independently re-checked against it by a second, adversarial agent — every cited `file:line` re-read and every quantitative claim (hit rates, counts, pass/fail) re-run. **35 corrections were applied during verification.** Numbers here are observed, not quoted from the project's marketing.

> These are research notes studying Onklaud 5 as **prior art** for NeoHaskell's own
> skill/code-generation direction. They describe what Onklaud *does*, grounded in its
> code — not a proposal for NeoHaskell. Marketing claims are reported only where the
> code contradicts them.

---

## What Onklaud 5 is, and where JavaScript lives in it

Onklaud 5 v3.2 markets itself as "not a model" but a "fusion pipeline that orchestrates multiple models through a structured council process" (`README.md:27-28`; the subtitle and abstract call it a "multi-model fusion pipeline," `README.md:4`, `README.md:409`). Concretely, the repository is a small collection of Python CLI scripts that (a) try to short-circuit a coding task with deterministic offline pattern-matching, and (b) if that fails, fan the task out to two hosted LLMs (Kimi K2.7 + GLM 5.2 via OpenRouter) for generation, cross-review, and arbitration.

### The repo is 100% Python

Despite the marketing table advertising projects "Built With Onklaud 5" in TypeScript/React/Next.js/Three.js (`README.md:277-282`), **the tool itself contains no JavaScript or TypeScript source of any kind.** A recursive search for `*.js`, `*.ts`, `*.jsx`, `*.tsx`, `*.mjs`, `*.cjs`, `*.vue`, `*.svelte` (excluding `.git`) returns zero files. The tracked (non-`.git`) file inventory is 16 `.py`, 3 `.md`, 2 `.pdf`, and one each of `.yaml`, `.png`, `.mp4`, `.html`, `.gif`, `.gitignore`, `.example`, plus `LICENSE` — see the `measured` field. (The `.json`/`.jsonl`/`.pyc` files present on disk are gitignored, generated at runtime.) Every string of JavaScript in the repo lives *inside* Python: as pattern-dictionary values, as `subprocess` argv lists, or as prose in PDF/HTML generators.

So "JavaScript flows" in this teardown means **the JS/TS-handling code paths inside the Python harness** — the places where Python decides something is a JS/TS task, emits a JS snippet, or shells out to a Node toolchain.

### The 6-stage pipeline

The advertised pipeline (`README.md:70-115`; mirrored in `design-spec.md:9-33` and `nadirclaw/config.yaml:43-73`) is:

| Stage | Component | Cost | Where |
|---|---|---|---|
| 0 | **Ponytail Ladder** — stdlib → native → existing-dep → shortest | $0, offline | `ponytail_ladder.py` |
| 1 | **GLM 5.2 pre-design** — architecture sketch (touchpoint 1) | paid API | `council.py:407` (`do_glm_pre_design`) |
| 2 | **Kimi K2.7 generate** — primary code implementation | paid API | `council.py` / `config.yaml:54-57` |
| 3 | **Dual review** — Kimi + GLM both review, scores averaged (touchpoint 2) | paid API | `council.py:431` (`do_dual_review`) |
| 4 | **GLM 5.2 arbitration** — final synthesis (touchpoint 3) | paid API | `council.py:486` (`do_glm_arbitrate`) / `config.yaml:64-67` |
| 5 | **Quality gate 10/10 + verify** — 7-dimension scoring, then type-check + tests | $0, offline | `quality_gate.py`, `verify.py` |

The whole design turns on a **deterministic-offline ($0) vs paid-API split.** `config.yaml:121` sets `strategy: ladder_first`, and `run_ladder` in `ponytail_ladder.py:213-243` is the gatekeeper: if it returns `found: true` (exit 0), stages 1-4 are meant to be skipped entirely — the README's worked example says "Step 1-4 skipped — task resolved at Step 0" (`README.md:159`). Stages 0 and 5 make *zero* API calls (`config.yaml:47`, `design-spec.md:42-45`); stages 1-4 are the only ones that spend money, quoted at ~$0.003–0.025 per task (`README.md:257-263`).

### The four JS-touching flows this doc covers

Only four Python code paths actually reason about JavaScript/TypeScript. Everything else in the "JS" story is prose:

1. **Ponytail Ladder — JS knowledge base + JS detection + `package.json` dep-matching** (`ponytail_ladder.py`). The `STDLIB_PATTERNS["js"]` dict (`ponytail_ladder.py:56-71`) maps task phrases to Node one-liners; `detect_language` routes anything mentioning `javascript/typescript/node/react/...` (`ponytail_ladder.py:101-103`) or a `package.json` (`ponytail_ladder.py:108`) to the `js` branch; and `check_existing_dep` parses `package.json` `dependencies`/`devDependencies` against a `dep_mappings` table (`ponytail_ladder.py:152-180`). This is the only stage where a JS "solution" is *produced*.

2. **`fast_gate.py` — Node syntax gate.** `JS_EXTS = {".js", ".ts", ".jsx", ".tsx", ".mjs", ".cjs"}` (`fast_gate.py:11`); for those extensions `check_syntax` shells out to `node --check <file>` (`fast_gate.py:18-20`). This is the JS side of the tool the README calls the "Syntax Gate" — though the README's 100% (14/14) figure is measured via `py_compile` on the Python source, not via `node` (`README.md:225`).

3. **`verify.py` — Node/TS build-and-test verifier (stage 5).** It reads `package.json` to classify a project as `typescript` vs `javascript` (`verify.py:71-101`), runs `npx tsc --noEmit` for TS (`verify.py:130-149`) or walks the tree running `node --check` for JS (`verify.py:151-174`), detects the package manager from lockfiles (`verify.py:35-41`), and runs `package.json` scripts (`verify.py:43-62`).

4. **`pre_check.py` — immune-memory scan that accepts JS/TS files.** Its usage advertises `--file src/api.ts` (`pre_check.py:8`); the scan itself is language-agnostic — a category-keyword table (`pre_check.py:25-34`) plus a fuzzy word-overlap matcher (`pre_check.py:44-52`) run against stored failure patterns — but it is the fourth entry point that ingests a `.ts` path.

### Subtleties and defects worth flagging up front

- **Marketing/code mismatch on JS pattern count.** `generate_paper_pdf.py:387` (and the shipped paper) claim "20+ JavaScript patterns," but `STDLIB_PATTERNS["js"]` has only **14** entries (measured). Python has 23 (the same generator claims "50+"), native HTML/CSS has 16.
- **The JS hit-rate number is not reproducible.** The README's Ponytail breakdown claims JavaScript = **2/10 (20.0%)** (`README.md:235`). Re-running the ladder against the exact 10 JS benchmark tasks in `benchmark_full.py:36-45` yields **3/10 (30.0%)** — see `measured`. The extra hit is a **subset false-positive**: matching is `pattern_words.issubset(task_words)` on whitespace-split words (`ponytail_ladder.py:123-128`), so the pattern `read file` matches the task "Read all lines from a file" and returns `readFileSync` — a semantically wrong answer counted as a hit.
- **The README's worked example does not resolve.** The example claims Ponytail resolves "Build an HTTP client with retry logic" at Step 0 ($0) with a Python `requests.Session` one-liner (`README.md:151-166`), but running the ladder on that exact task returns `found: false` (it auto-detects `python` and still misses) — see `measured`.
- **A dead JS pattern.** `STDLIB_PATTERNS["js"]` includes the key `"sleep/delay"` (`ponytail_ladder.py:64`). Because matching splits on whitespace, `sleep/delay` is a single token; a natural task like "sleep delay in node" never matches it (only the literal string `sleep/delay` does). It is effectively unreachable from real task phrasing.
- **Duplicate dict key.** `dep_mappings` defines `"zod"` twice (`ponytail_ladder.py:171` shadowing `:165`) — harmless (second wins) but sloppy.
- **A referenced data file is missing as-cloned.** The benchmark-data file `research_benchmarks.json` that the PDF/HTML report generators load at import time (`generate_paper_pdf.py:9`, `generate_html_report.py:9`) is gitignored and **not present in the repo**, so those generators raise `FileNotFoundError` even on import as-cloned. (By contrast, `config.yaml`'s references to `immune_memory.json` storage (`config.yaml:118`) and a `verify.py --type-only` command (`config.yaml:73`) are *real*: `--type-only` is an implemented flag (`verify.py:300-301`, dispatched at `verify.py:113`), and `immune_memory.json` is generated runtime data present on disk that `pre_check.py` reads.)
- **Gated paths.** `verify.py` skips the TS type-check entirely if there is no `tsconfig.json` (`verify.py:132-135`), and its `_smoke_test` only runs under `mode == "smoke"` against an already-running server (`verify.py:120-122`).

---

## Flow A — JS resolution: the Ponytail Ladder JS branch

The "Ponytail Ladder" (`ponytail_ladder.py`) is Onklaud 5's zero-API-call gate that tries to answer a coding task from three progressively-wider rungs — stdlib, native HTML/CSS, and existing project dependency — before any model is invoked. Its own module docstring frames it as "checks if a task can be solved with stdlib/native/existing-dep. Returns the shortest solution that works. Zero API calls. Instant." (`ponytail_ladder.py:3-4`). For JS/TS tasks the entire mechanism is a set-membership test over a tiny hand-maintained keyword table. Read closely, the JS branch resolves only 3 of the 10 JS benchmark tasks, and one of those three is a *wrong* answer.

### `detect_language()` — JS triggers and the Python-default trap

`detect_language(task, project_dir=None)` (`ponytail_ladder.py:94`) lowercases the task and runs two keyword sweeps followed by a project-file probe:

```python
if any(kw in task_lower for kw in ["python", ".py", "django", "flask", "fastapi"]):
    return "python"
if any(kw in task_lower for kw in ["javascript", "typescript", "node", "react", "next", "vue", "angular",
                                    ".js", ".ts", ".jsx", ".tsx", "npm", "vite", "express"]):
    return "js"
```
(`ponytail_ladder.py:99-103`)

The JS trigger set is 14 substrings (`ponytail_ladder.py:101-102`). Note these are raw `in` substring tests, not word matches — so `"next"` fires on any task containing the substring "next" and `"react"` on "reaction", etc. If no keyword hits and a `project_dir` is supplied, it falls back to file sniffing: a `package.json` returns `"js"`, while `requirements.txt`/`pyproject.toml`/`setup.py` return `"python"` (`ponytail_ladder.py:106-111`). If nothing matches, the function unconditionally returns `"python"`:

```python
    # Default
    return "python"
```
(`ponytail_ladder.py:113-114`)

This default is the reason the benchmark must pass `--lang js` explicitly. None of the 10 JS benchmark task strings (`benchmark_full.py:36-45`) contain a JS keyword — e.g. "Read a file synchronously", "Deep clone an object" — so with no project dir they auto-detect as **python** (verified: `detect_language('Read a file synchronously') -> python`, `detect_language('Deep clone an object') -> python`). The benchmark harness works around this by appending `--lang js` for any task whose language column isn't `native`/`""` (`benchmark_full.py:68-69`), which is exactly the invocation this section reproduces.

### `STDLIB_PATTERNS["js"]` — the 14-key table

The JS knowledge base is a flat `dict` of 14 `pattern -> solution` string pairs (`ponytail_ladder.py:56-71`). Enumerated in declaration order, the keys are:

| # | Pattern key | Solution (abbrev.) | Modern `node:`? |
|---|---|---|---|
| 1 | `read file` | `readFileSync(path,'utf-8')` from `node:fs` | yes (`node:fs`) |
| 2 | `write file` | `writeFileSync(path,data)` from `node:fs` | yes (`node:fs`) |
| 3 | `read json` | `JSON.parse(readFileSync(...))` from `node:fs` | yes (`node:fs`) |
| 4 | `parse url` | `new URL(input)` (WHATWG global) | n/a (global) |
| 5 | `http get` | `await fetch(url).then(r=>r.json())` | n/a (global) |
| 6 | `generate uuid` | `randomUUID()` from `node:crypto` | yes (`node:crypto`) |
| 7 | `hash string` | `createHash('sha256')...` from `node:crypto` | yes (`node:crypto`) |
| 8 | `sleep/delay` | `await new Promise(r=>setTimeout(r,ms))` | n/a (global) |
| 9 | `environment variable` | `process.env.KEY \|\| 'default'` | n/a (global) |
| 10 | `path join` | `join(dir,file)` from `node:path` | yes (`node:path`) |
| 11 | `temp dir` | `mkdtempSync(...)` from `node:fs`+`node:os` | yes (`node:fs`,`node:os`) |
| 12 | `parse date` | `new Date(isoString)` | n/a (global) |
| 13 | `regex match` | `/pattern/.exec(text)` | n/a (literal) |
| 14 | `base64 encode` | `Buffer.from(data).toString('base64')` | n/a (global) |

All seven solutions that need a runtime import use the modern `node:` protocol prefix — `node:fs` (keys 1,2,3,11), `node:crypto` (6,7), `node:path` (10), `node:os` (11) — quoted directly, e.g. `import { readFileSync } from 'node:fs'` (`ponytail_ladder.py:57`) and `import { randomUUID } from 'node:crypto'` (`ponytail_ladder.py:62`). The remaining seven solutions rely on runtime globals (`URL`, `fetch`, `process.env`, `Buffer`, `Date`, `setTimeout`) or a regex literal and need no import. So the table is stylistically modern; the problem is not the *solutions* but the *keys* used to reach them.

### `NATIVE_PATTERNS` — the shared CSS/HTML rung

The second rung is `NATIVE_PATTERNS` (`ponytail_ladder.py:75-92`), a 16-key table of HTML5/CSS answers that require no code — e.g. `"date picker": '<input type="date"> - native date picker, no JS needed'` (`ponytail_ladder.py:76`), `"dark mode"` → `@media (prefers-color-scheme: dark)` (`ponytail_ladder.py:84`), `"dialog/modal"` → `<dialog>` (`ponytail_ladder.py:91`). This rung is **language-agnostic**: `check_native()` (`ponytail_ladder.py:133`) takes no `lang` argument and is consulted for every task regardless of `--lang`. It is the CSS/HTML fallback shared by the Python and JS ladders alike. None of the 10 JS benchmark tasks touch this rung (they're all IO/data/async), but note `"dialog/modal"` carries the token `dialog/modal` — the same slash-token construction that neuters `sleep/delay` (below).

### `check_stdlib()` — the word-subset matching algorithm

The matcher (`ponytail_ladder.py:116-131`) is the heart of the flow, and its algorithm is crude:

```python
patterns = STDLIB_PATTERNS[lang]
task_lower = task.lower()
task_words = set(task_lower.split())

for pattern, solution in patterns.items():
    pattern_words = set(pattern.split())
    # All pattern words must appear in task words
    if pattern_words.issubset(task_words):
        return {"level": "stdlib", "solution": solution, "language": lang, "pattern_matched": pattern}
```
(`ponytail_ladder.py:121-129`)

Both the task and each pattern key are lowercased and split on whitespace into `set`s, and a pattern matches iff **every whitespace-delimited token in the pattern key is present somewhere in the task's token set** (`pattern_words.issubset(task_words)`, `ponytail_ladder.py:128`). It is an unordered, whitespace-tokenized, exact-token bag-of-words test. Consequences:

- **No stemming / no substring match.** "directory" ≠ "dir", so the `temp dir` key (`ponytail_ladder.py:67`) cannot match "Create a temporary directory" (verified: `set('temp dir'.split()).issubset({'directory','temporary','a','create'}) == False`). "ISO string" doesn't contain "parse", so `parse date` misses "Format a date as ISO string".
- **First match wins, in insertion order.** The loop returns on the first subset hit; Python 3.7+ dicts preserve insertion order, so `read file` (key #1) is tested before everything else.
- **Iteration order = declaration order,** which is why the false positive below resolves to `readFileSync` rather than any better key — `read file` is simply first.

`check_native()` (`ponytail_ladder.py:133-142`) uses the identical `issubset` algorithm on `NATIVE_PATTERNS`.

### `check_existing_dep()` — the JS half and the duplicate `zod` key

`check_existing_dep(task, project_dir=None)` (`ponytail_ladder.py:144`) is the third rung and is **gated on a project dir** — it returns `None` immediately if `project_dir` is falsy or not a directory (`ponytail_ladder.py:146-147`). For JS it parses `package.json`, merges `dependencies` and `devDependencies` into one `deps` dict (`ponytail_ladder.py:156-158`), and then keyword-matches the task against a `dep_mappings` table (`ponytail_ladder.py:161-172`):

```python
dep_mappings = {
    "lodash": ["filter", "map", "debounce", "throttle", "merge", "clone", "group"],
    "axios": ["http", "api call", "fetch", "request"],
    "date-fns": ["date format", "date parse", "date diff"],
    "zod": ["validate", "schema", "type check"],
    "express": ["server", "route", "middleware"],
    "react": ["component", "hook", "state"],
    "next": ["page", "route", "ssr"],
    "tailwind": ["style", "css", "layout"],
    "prisma": ["database", "query", "orm", "migrate"],
    "zod": ["validate", "schema", "type check"],
}
```
(`ponytail_ladder.py:161-172`)

The matching here is different from `check_stdlib`: it's a substring `any(kw in task_lower ...)` test, and the dep only counts if the package is actually present in `deps` (`ponytail_ladder.py:174-175`). Two things stand out:

1. **`"zod"` is declared twice** — at `ponytail_ladder.py:165` and again at `ponytail_ladder.py:171`, with identical value `["validate", "schema", "type check"]`. In a Python dict literal a repeated key is not an error; the later binding silently overwrites the earlier one, so the table collapses to **9 effective entries, not 10**. Because both values are identical it's a harmless no-op today, but it is dead source and a latent footgun (if someone edits one copy they'll be surprised the other wins). Verified: constructing the dict yields a single `zod` key.
2. This rung never fires in the benchmark at all — every benchmark invocation runs with **no** `--project-dir` (`benchmark_full.py:67`), so `check_existing_dep` short-circuits to `None` on line 146 before ever reading a `package.json`. `lodash`'s `clone` keyword (`ponytail_ladder.py:162`) *would* rescue "Deep clone an object" — but only in a repo that lists lodash, which the benchmark never provides.

The Python half (`py_dep_mappings`, `ponytail_ladder.py:191-200`) is analogous, matching against `requirements.txt` text.

### `run_ladder()` — rung order

`run_ladder(task, project_dir=None, lang=None)` (`ponytail_ladder.py:213`) fixes the climb order:

1. If no `lang`, call `detect_language` (`ponytail_ladder.py:215-216`).
2. **Stdlib** — `check_stdlib` (`ponytail_ladder.py:219-222`); on hit, set `found=True`, exit code 0.
3. **Native** — `check_native` (`ponytail_ladder.py:225-228`); on hit, exit 0.
4. **Existing dependency** — only `if project_dir` (`ponytail_ladder.py:231-235`).
5. **Not found** — returns `{"found": False, "reason": "Task requires custom implementation", ...}` and exit code 1 (`ponytail_ladder.py:238-243`).

So stdlib is tried before native before deps; the language-agnostic native rung sits *under* the language-specific stdlib rung, and the dep rung is unreachable without a project dir.

### The false positive: "Read all lines from a file" → whole-file `readFileSync`

Benchmark JS task #9 is "Read all lines from a file" (`benchmark_full.py:44`). The *intent* is line-wise reading (e.g. `readFileSync(path,'utf-8').split('\n')` or a `readline` stream). But the tokenized task is `{'all','from','lines','file','a','read'}`, and the pattern key `read file` tokenizes to `{'file','read'}`, which **is** a subset (verified: `subset? True`). The ladder therefore returns key #1's solution — `readFileSync(path, 'utf-8')` (`ponytail_ladder.py:57`) — which reads the **entire file as one string**, not its lines. The gate reports `found: true, level: stdlib, pattern_matched: "read file"` with full confidence and exit code 0, silently shipping a wrong answer. This is a direct artifact of the bag-of-words `issubset` design (`ponytail_ladder.py:128`): the extra tokens "all", "lines", "from" that change the semantics are simply ignored because supersets always satisfy `issubset`.

### The (practically) dead pattern: `sleep/delay`

Key #8 is `"sleep/delay"` (`ponytail_ladder.py:64`). Because the matcher tokenizes on whitespace via `.split()` (`ponytail_ladder.py:123,126`), `"sleep/delay".split()` produces the **single token** `['sleep/delay']` — the slash is not a delimiter (verified: `set('sleep/delay'.split()) == {'sleep/delay'}`). For this pattern to match, the task's whitespace-split token set must literally contain the token `sleep/delay` **with the embedded slash**. No natural-language task phrases it that way. The benchmark's async task, "Set a timeout that can be cancelled" (`benchmark_full.py:43`), contains neither "sleep" nor "delay" as tokens and misses entirely. I confirmed the pattern *can* fire only with an artificial slash-token input: `--task "sleep/delay execution"` matches (`pattern_matched: "sleep/delay"`, exit 0), whereas `--task "sleep delay execution for N seconds"` (slash replaced by space) **misses** — the two separate tokens `sleep`, `delay` are never a superset of the single token `sleep/delay`. So `sleep/delay` is not literally unmatchable, but it is a dead pattern for any realistic task phrasing: the only way to hit it is to type the slash. The same defect afflicts native key `dialog/modal` (`ponytail_ladder.py:91`).

### Net effect on the JS benchmark

Of the 10 JS benchmark tasks (`benchmark_full.py:36-45`), only three tokenize to a superset of a `STDLIB_PATTERNS["js"]` key: "Read a file synchronously" (→ `read file`), "Parse a URL and extract query parameters" (→ `parse url`), and — spuriously — "Read all lines from a file" (→ `read file` again, wrong solution). The other seven miss because the required token isn't present verbatim: the JS table has no `deep clone`/`random`/`http server`/date-`format`/path-`exists` keys (`http get` exists but needs both "http" and "get", and task #3 lacks "get"), `temp dir` loses to "directory", and `sleep/delay` needs the slash token. That yields a measured **30.0% JS hit rate — and only 20% *correct*, since one of the three "hits" is the readFileSync false positive.**

---

## Flow B — JS syntax gate (`fast_gate.py`)

`fast_gate.py` is Onklaud 5's "free, 0-API-cost" per-file check — the README lists it under *"Free Operations (0 API cost)"* alongside `ponytail_ladder.py` and `pre_check.py` (`README.md:309-315`). Its own docstring pitches it as *"Per-file code quality check - syntax (instant) + optional Kimi review (API)"* (`fast_gate.py:3`). In practice, only the syntax half of that promise works: the Kimi half is dead on arrival for any real (API-key-present) run, and even the syntax half is a fragile shell-out to `node --check` that produces guaranteed false positives on three of the extensions it advertises support for (`.ts`, `.tsx`, `.jsx`).

### The extension map

The file recognizes two families of source by extension (`fast_gate.py:11-12`):

```python
JS_EXTS = {".js", ".ts", ".jsx", ".tsx", ".mjs", ".cjs"}
PY_EXTS = {".py", ".pyw"}
```

Anything outside those two sets is silently waved through — `check_syntax` returns `True` with a comment that says exactly that: `return True, ""  # Unknown format, skip syntax check` (`fast_gate.py:24-25`). Verified: passing a `.txt` file prints `[SYNTAX] thing.txt: OK` and does not affect the exit code (see *measured*).

### `check_syntax()` — shelling to `node --check` / `py_compile`

The core is a `subprocess.run` dispatch on the lowercased extension (`fast_gate.py:16-27`):

```python
if ext in JS_EXTS:
    r = subprocess.run(["node", "--check", filepath], capture_output=True, text=True, timeout=10)
    return r.returncode == 0, r.stderr.strip() if r.stderr else ""
elif ext in PY_EXTS:
    r = subprocess.run(["python", "-m", "py_compile", filepath], capture_output=True, text=True, timeout=10)
    return r.returncode == 0, r.stderr.strip() if r.stderr else ""
else:
    return True, ""  # Unknown format, skip syntax check
```

- **JS family → `node --check <file>`** (`fast_gate.py:19`) with a 10s timeout. `node --check` parses the file and exits non-zero on a *parse* error only. The gate treats `returncode == 0` as "OK".
- **PY family → `python -m py_compile <file>`** (`fast_gate.py:22`), also 10s.
- Any exception (e.g. `node` not installed, timeout) is caught by the bare `except Exception as e:` at `fast_gate.py:26-27`, which returns `(False, str(e))` — i.e. an infrastructure failure is reported as a *syntax failure*.

#### What `node --check` actually catches — and its false positives

`node --check` only validates that the file is parseable JavaScript. Three consequences, all verified against the checked-out source with Node v24.15.0:

1. **Type/logic errors pass.** A `.js` file containing `const r = n * undefined;` and `u.foo()` on an undefined variable is perfectly valid *syntax*; it prints `[SYNTAX] logicbug.js: OK` and the gate exits 0. The gate catches nothing semantic — no type errors, no undefined references, no unused vars, no security issues.

2. **TypeScript type-annotation syntax is a hard false positive.** `.ts` is in `JS_EXTS`, but `node --check` parses `.ts` as CommonJS JavaScript. A perfectly valid TypeScript file `let s: string = "hi";` fails with `SyntaxError: Unexpected token ':'`. So *every* TS file that uses a type annotation is reported `FAIL`, regardless of correctness.

3. **`.jsx` and `.tsx` never even reach the parser.** In modern Node, `node --check` on those extensions throws `TypeError [ERR_UNKNOWN_FILE_EXTENSION]: Unknown file extension ".jsx"` before parsing. That non-zero exit is surfaced as a syntax `FAIL`. So the gate guarantees a false negative for two of the six extensions it claims to support.

Net: of the six `JS_EXTS`, only `.js`, `.mjs`, and `.cjs` behave sanely; `.ts`/`.jsx`/`.tsx` are broken by construction.

#### Hardcoded interpreter names (portability footgun)

Both branches invoke a bare literal — `"node"` (`fast_gate.py:19`) and `"python"` (`fast_gate.py:22`) — resolved via `PATH`, never `sys.executable`. Two failure modes, both verified:

- **`node` missing from `PATH`** → `FileNotFoundError` caught at `fast_gate.py:26` → `[SYNTAX] good.js: FAIL - [Errno 2] No such file or directory: 'node'`, exit 1. The tool has a hard, undocumented dependency on `node` being on `PATH`.
- **Only `python3` on `PATH`** (common on modern Linux/macOS) → *Python* files fail with `[Errno 2] No such file or directory: 'python'`, exit 1, even though the file is fine.

### `kimi_review()` — shelling into `council.py`, and why it never works

When not in syntax-only mode, `main()` calls `kimi_review()` (`fast_gate.py:29-46`), which shells into the sibling `council.py`:

```python
r = subprocess.run(
    ["python", COUNCIL, "review", "--type", "code", "--prompt", prompt, "--draft-file", filepath],
    capture_output=True, text=True, timeout=120, cwd=MY_DIR
)
# council review outputs JSON to stdout on success
try:
    result = json.loads(r.stdout)
    ...
except json.JSONDecodeError:
    return 0, False, [r.stdout[:300]], ""
```

The comment on `fast_gate.py:36` asserts *"council review outputs JSON to stdout on success"* — **this is false for the way `fast_gate` invokes it.** `council.py`'s `cmd_review` prints `format_trace(result, args.type, args.json_output)` (`council.py:608`). `format_trace` only emits JSON when `json_output` is true (`council.py:579-582`); otherwise it returns the emoji *trace line* (`council.py:594`):

```
[🎠→⚡K(9/10)→🔮G→gate(0/10)] = 9/10 PASS
```

`fast_gate` never passes `--json`, and the `review` subparser defaults `json_output=False` (`council.py:920`, `council.py:941`). So on a **successful** review, council prints that trace line, `json.loads` raises `JSONDecodeError`, and `kimi_review` hits the fallback at `fast_gate.py:44-46` → `return 0, False, [r.stdout[:300]], ""`. `main()` then prints `[KIMI] <file>: 0/10 FAIL` and sets `all_passed = False` (`fast_gate.py:88-97`) — **the file is failed even though the model passed it 9/10.** I reproduced the exact trace line (by calling `council.format_trace` directly) and the resulting fallback (see *measured*). The Kimi path is broken for every API-key-present run.

There is a single narrow branch where `json.loads` *succeeds*: if `OPENROUTER_API_KEY` is unset, `council.py` short-circuits and prints `{"error": "OPENROUTER_API_KEY not set in .env", "passed": false, "score": 0}` to stdout (`council.py:952`), which is valid JSON. `kimi_review` parses it to `score=0, passed=False, issues=[], critique=""` — so the *no-key* path yields a clean `0/10 FAIL` via the happy branch, while the *with-key* path yields a `0/10 FAIL` via the exception branch. Either way the answer is always FAIL. Verified both branches (see *measured*).

### `main()` — arg loop, modes, and exit codes

`main()` hand-rolls its argument parsing in a `while` loop (`fast_gate.py:54-67`) rather than using `argparse`. Flags recognized:

- `--syntax-only` → sets `syntax_only` (`fast_gate.py:58-59`)
- `--skip-kimi` → sets `skip_kimi` (`fast_gate.py:60-61`) — **undocumented**: it appears in neither the module docstring/usage (`fast_gate.py:4`) nor the `main()` usage string (`fast_gate.py:70`), and no caller or doc anywhere in the tree references it (only `fast_gate.py:50,60,61,84` mention it).
- `--prompt <val>` → consumes the next arg *only if* `i + 1 < len(args)` (`fast_gate.py:62-64`)
- anything else → appended to `files` (`fast_gate.py:65-66`)

Two arg-loop subtleties, both verified:

- **A dangling `--prompt` (last arg, no value)** fails the `i + 1 < len(args)` guard, falls through to the `else`, and is appended to `files`. It is then "syntax-checked" as a filename with empty extension (`os.path.splitext("--prompt")[1] == ""`) → unknown format → `[SYNTAX] --prompt: OK`, exit 0. A malformed invocation silently passes.
- **No files at all** → prints the usage string to stderr and `sys.exit(1)` (`fast_gate.py:69-71`).

The per-file loop (`fast_gate.py:74-97`):

1. Always runs `check_syntax`. On failure: `[SYNTAX] <bn>: FAIL - <err[:200]>`, sets `all_passed = False`, and `continue`s — the Kimi step is skipped for files that fail syntax (`fast_gate.py:77-81`).
2. On syntax OK, if `syntax_only or skip_kimi` → `continue` (`fast_gate.py:84-85`).
3. Otherwise run `kimi_review` and print `[KIMI] <bn>: <score>/10 <PASS|FAIL>` plus up to 5 issues; a non-passing review sets `all_passed = False` (`fast_gate.py:88-97`).

### Exit codes and callers

The single gating signal is the final line `sys.exit(0 if all_passed else 1)` (`fast_gate.py:99`): **0 = all files clean, 1 = any file failed** (syntax or Kimi). That exit code is the entire contract a caller can rely on. Verified: mixed good/bad fixtures → exit 1; all-good → exit 0.

Nothing in the repo wires `fast_gate.py` as a git hook or pre-commit gate — and the file itself makes no such claim. Every caller invokes it only in `--syntax-only` mode: the README documents solely `fast_gate.py ... --syntax-only` (`README.md:314`, `README.md:345`); the benchmark harnesses call it with `--syntax-only` (`research_paper_benchmark.py:124`, `benchmark_full.py:109`); and `test_pipeline.py`'s `test_fast_gate` only ever exercises `--syntax-only` (`test_pipeline.py:144-154`). The Kimi review path (`kimi_review`) has **zero callers and zero test coverage** anywhere in the tree — no code ever reaches the non-syntax-only branch — making it an unshipped, non-functional appendage.

### Summary of what this gate does NOT catch

- Any semantic defect in valid-syntax code: type errors, undefined references, null/undefined deref, logic bugs, security issues — all pass (empirically: `n * undefined; u.foo()` → OK).
- TypeScript entirely: type-annotated `.ts` is a false-positive FAIL; `.jsx`/`.tsx` are `ERR_UNKNOWN_FILE_EXTENSION` FAILs.
- It requires `node` on `PATH` (JS) and a binary literally named `python` on `PATH` (Py); absent either, clean files are reported as failures.
- The advertised Kimi review is a no-op-to-broken: it always resolves to `0/10 FAIL` because `fast_gate` reads `council.py` stdout as JSON while `council.py review` emits a human trace line by default.

---

*Measured — all runs on the checked-out `/tmp/onklaud-5` with Node v24.15.0, Python 3.13.13, a `python` on `PATH`, `OPENROUTER_API_KEY` present in env, no `.env` file:*

1. `python fast_gate.py good.js bad.js typeerr.ts good.py bad.py thing.txt --syntax-only`: `good.js: OK`; `bad.js: FAIL - SyntaxError: Unexpected token ';'`; `typeerr.ts: FAIL - SyntaxError: Unexpected token ':'`; `good.py: OK`; `bad.py: FAIL - SyntaxError: invalid syntax`; `thing.txt: OK`. Exit 1. (TS type annotation is a false-positive FAIL; unknown `.txt` passes.)
2. `logicbug.js` (`const n="not a number"; const r=n*undefined; let u; u.foo();`) → `OK`, exit 0. (Type/logic errors pass.)
3. `good.js good.py thing.txt --syntax-only` → all OK, exit 0.
4. `m.mjs` OK, `broken.cjs` FAIL (`Unexpected token ';'`), exit 1. (`.mjs`/`.cjs` handled by `node --check`.)
5. `comp.jsx --syntax-only` → FAIL, `ERR_UNKNOWN_FILE_EXTENSION` for `.jsx`, exit 1. Direct `node --check comp.tsx` → same error (rc 1). Direct `node --check plain.ts` → `SyntaxError: Unexpected token ':'` (rc 1).
6. `node` absent from `PATH` (PATH limited to a dir containing only `python`, fast_gate launched via absolute python): `good.js: FAIL - [Errno 2] No such file or directory: 'node'`, exit 1.
7. Only `python3` on `PATH` (no `python`): `good.py: FAIL - [Errno 2] No such file or directory: 'python'`, exit 1.
8. `env -u OPENROUTER_API_KEY -u OPENROUTER_API_KEY_BACKUP python council.py review --type code --prompt test --draft-file good.js` → stdout `{"error": "OPENROUTER_API_KEY not set in .env", "passed": false, "score": 0}`, exit 1. `fast_gate`'s `json.loads` parses it → score=0, passed=False.
9. Key-present path (a passing 9/10 review, `council.format_trace` invoked directly): council stdout = `[🎠→⚡K(9/10)→🔮G→gate(0/10)] = 9/10 PASS`. `json.loads(that)` → `JSONDecodeError` → fast_gate fallback returns `(0, False, [trace[:300]], "")` → prints `[KIMI] good.js: 0/10 FAIL` and sets `all_passed=False` even though the model PASSED. (`format_trace(..., json_output=True)` would emit valid JSON, but `fast_gate` never passes `--json`.)
10. `python fast_gate.py --syntax-only --prompt` (dangling `--prompt`) → `[SYNTAX] --prompt: OK`, exit 0 (treated as a filename with unknown extension).

---

## Flow C — JS/TS verification (`verify.py`)

`verify.py` is Onklaud 5's runtime-verification stage — the pipeline's "enforcement" step. Its docstring frames it as what makes the tool credible: *"Fable 5 beats Opus 4.8 because it ACTUALLY TESTS what it builds. This is the enforcement."* (`verify.py:5-6`). In practice, for the JavaScript path it can report a clean pass having executed no meaningful check at all.

Four modes are parsed in `main()`'s arg loop (`verify.py:293-312`): `--type-only`, `--full`, `--smoke`, and default `auto`. Exit codes are documented as `0 = pass, 1 = fail, 2 = partial (tests unavailable but type-check OK)` (`verify.py:17`) — but `2` is never returned (see below).

### Platform / package-manager plumbing

`_npm_cmd(*args)` (`verify.py:28-33`) just appends `.cmd` to the first arg on Windows:

```python
cmd = list(args)
if IS_WIN:
    cmd[0] = cmd[0] + ".cmd"
return cmd
```

`_detect_pm(project_dir)` (`verify.py:35-41`) picks a package manager purely from lockfile presence, in fixed priority — pnpm, then yarn, else npm:

```python
if os.path.exists(os.path.join(project_dir, "pnpm-lock.yaml")):
    return "pnpm"
if os.path.exists(os.path.join(project_dir, "yarn.lock")):
    return "yarn"
return "npm"
```

`_run_script(pm, script, cwd, timeout=120)` (`verify.py:43-62`) contains a real bug:

```python
if script.startswith(pm) or script.startswith("npm") or script.startswith("yarn") or \
   "&&" in script or "||" in script or ";" in script:
    return subprocess.run(script, cwd=cwd, ... shell=True)
...
return subprocess.run(f'{pm_cmd} run {script}', cwd=cwd, ... shell=True)
```

`script` is the *value* of the package.json test script, but the fallback branch treats it as a *script name* and prepends `npm run`. So `"test": "jest"` becomes `npm run jest` → npm looks for a script literally named `jest`, fails with `Missing script`. Only values beginning with `npm`/`yarn`/`pnpm` or containing `&&`/`||`/`;` run verbatim through the shell. Confirmed by capturing the constructed command: `'echo ok'` → `npm run echo ok`, `'jest'` → `npm run jest`, `'npm run test:unit'` → `npm run test:unit`, `'vitest run && tsc'` → `vitest run && tsc`, `'yarn jest'` → `yarn jest` (all `shell=True`). Running `--full` on a fixture whose `"test"` is `"echo ok"` produced a real false failure: `npm error Missing script: "echo"`, exit 1. This mangling breaks the most common test-script shape (verified in Measured).

### `detect_project_type()` — classification from package.json + tsconfig

`detect_project_type()` (`verify.py:71-101`) reads `package.json`, merges `dependencies`+`devDependencies`, and derives flags:

```python
has_ts = os.path.exists(os.path.join(self.project_dir, "tsconfig.json"))
has_react = "react" in deps
has_express = "express" in deps
has_vite = "vite" in deps
...
return {
    "lang": "typescript" if has_ts else "javascript",
    "is_ui": has_react or has_vite,
    "is_server": has_express,
    "is_fullstack": has_react and has_express,
    ...
    "dev_cmd": scripts.get("dev", None) or scripts.get("start", None),
    ...
}
```

Subtleties:
- **`lang` is decided by `tsconfig.json` presence, not file extensions or a `typescript` dependency** (`verify.py:83,91`). A project with `typescript` in devDependencies but no tsconfig is classified `javascript` (Measured: a react+express+vite fixture with no tsconfig detected as `"lang":"javascript"`) and routed into the whole-tree `node --check` path, not `tsc`.
- Detection is dependency-name membership only: `"react" in deps`, `"express" in deps`, `"vite" in deps` (`verify.py:84-86`). Non-Express servers (Fastify, Koa, Nest, raw `http`) → `is_server=False`; non-React/Vite UIs (Vue, Angular, Svelte, Next) → `is_ui=False`.
- `dev_cmd`/`test_cmd`/`build_cmd` hold script **values** (e.g. `"vite --port 4321"` — verified), not names. This mismatch breaks both `_run_script` and `_smoke_ui`.
- No `package.json` → `{"lang": "unknown"}` (`verify.py:101`), which skips every phase and still passes (Measured: `--project /tmp` → `Detected: unknown`, `TypeCheck=skipped`, exit 0).

### `_type_check()` — two radically different paths

`_type_check(pt)` (`verify.py:126-178`) branches on `lang`.

**TypeScript path** (`verify.py:130-149`):

```python
if lang == "typescript":
    tsconfig = os.path.join(self.project_dir, "tsconfig.json")
    if not os.path.exists(tsconfig):
        print("[VERIFY]   No tsconfig.json - skipping type-check")
        self.results["type_check"] = "skipped"
        return
    r = subprocess.run(_npm_cmd("npx", "tsc", "--noEmit"), ... timeout=60)
```

Because `lang == "typescript"` is set *only* when tsconfig exists (`verify.py:91`), the `if not os.path.exists(tsconfig)` guard at `verify.py:132` is **structurally unreachable** — the "skipped" branch here is dead code. Real TS projects do run `npx tsc --noEmit` (60s timeout), a genuine type check.

**JavaScript path** (`verify.py:151-174`): a whole-tree walk running `node --check` on every JS/TS-ish file, one subprocess per file:

```python
for root, _, files in os.walk(self.project_dir):
    if "node_modules" in root or ".git" in root:
        continue
    for f in files:
        if f.endswith((".js", ".ts", ".jsx", ".tsx", ".mjs")):
            fp = os.path.join(root, f)
            r = subprocess.run(["node", "--check", fp], ... timeout=5)
            if r.returncode != 0:
                errors_found += 1
```

- **O(files) subprocess spawns** — one `node --check` per source file.
- **Syntax-only.** `node --check` parses; it does not run, resolve imports, or type-check. It does catch true syntax errors (Measured: a file `const x = ;` → `syntax error`, `TypeCheck=fail`, exit 1). It also runs on `.ts`/`.tsx`, where TypeScript type syntax makes `node --check` fail on otherwise-valid TS (verified: `node --check` on `const x: number = 1;` returns 1) — but this branch is only reached when there is no tsconfig.
- **The exclusion is a substring test on `root`** (`verify.py:155`); `os.walk` still descends into `node_modules` (no `dirs` pruning), and any path containing the substring `node_modules`/`.git` anywhere is skipped.
- **Empty-tree false pass:** `errors_found` starts at 0; if the walk matches zero files the loop never runs, so `errors_found == 0` → prints `node --check: OK`, sets `type_check = "pass"` (`verify.py:168-170`). Checking nothing reports as a passing check.

### `_run_tests()`

`_run_tests(pt)` (`verify.py:180-202`) is gated by `has_tests` and mode `auto`/`full` (`verify.py:117`). It reads `test_cmd` and calls `_run_script`, inheriting the `npm run <value>` mangling bug. On non-zero exit it concatenates stdout+stderr, ASCII-sanitizes for Windows cp1252 (`verify.py:198`), truncates, and records `"fail"`.

### `_smoke_test()` / `_smoke_server()` / `_smoke_ui()` — probing already-running servers

Smoke testing fires only in `--smoke` mode (`verify.py:121`) and never starts anything — it curls **already-running** servers on hardcoded ports.

`_smoke_server(pt)` (`verify.py:211-229`) probes `[8790, 3000, 8080, 8000]` at `/api/health`, accepting `200/204/302`, else records `smoke = "skip"`.

`_smoke_ui(pt)` (`verify.py:237-262`) is meant to honor an explicit `--port` via `_extract_port` (`verify.py:231-235`, regex `--port\s+(\d+)`), but is wired wrong:

```python
dev_value = scripts.get(pt.get("dev_cmd", "dev"), "")
explicit_port = self._extract_port(dev_value)
ports = [explicit_port] if explicit_port else [8800, 5173, 3000, 3001]
```

`pt["dev_cmd"]` is the script *value* (`"vite --port 4321"`), so `scripts.get("vite --port 4321", "")` returns `""`, `explicit_port` is `None`, and it always falls back to hardcoded `[8800, 5173, 3000, 3001]`. Verified: `_extract_port("vite --port 4321")` alone returns `4321`, but the value actually fed to it is `""`, so the declared port `4321` is never used — the `--port` regex extraction is **effectively dead** for real projects (Measured). On no match: `smoke = "skip"` (Measured: `--smoke` with nothing running → `Smoke=skip`, `ALL PASS: TypeCheck=pass | Smoke=skip`, exit 0).

### `_summarize()` — where "skip" (and "nothing") is a pass

`_summarize()` (`verify.py:264-290`):

```python
failures = [v for v in [tc, tests, smoke] if v == "fail"]
skips = [v for v in [tc, tests, smoke] if v == "skip"]
if not failures:
    self.results["passed"] = True
    ...
    return 0
else:
    ...
    return 1
```

Only the exact string `"fail"` fails a run. `None` (phase never ran), `"skip"`, and `"skipped"` are all non-failing. Verified truth table (driving `results` directly): `('skipped', None, 'skip')`, `('pass', None, 'skip')`, `('pass', 'pass', None)`, `(None, None, None)`, and `(None, None, 'skip')` all → exit 0 / `passed=True`; only `('fail', None, None)` → exit 1 / `passed=False`. Two wrinkles:
- `skips` is computed but never used, and matches only `"skip"` (`verify.py:279`) — whereas the type-check phase writes `"skipped"` (`verify.py:134,178`), so skipped type-checks aren't even counted as skips.
- **Exit code 2 is never returned.** The docstring promises "2 = partial" (`verify.py:17`), but `_summarize` returns only 0/1 (`verify.py:284,290`) and `main` does `sys.exit(verifier.run())` (`verify.py:312`). A grep confirms only `return 0` and `return 1` exist — no `return 2` anywhere — so the documented partial contract is never honored.

### The headline failure: a JS project that passes having checked nothing

A directory with a `package.json` (no `tsconfig.json`, empty/absent `scripts`, no test script, no Express/React, and no `.js`/`.ts` files — or files only under `node_modules`) under default `auto` mode:
1. `detect_project_type` → `lang="javascript"`, `has_tests=False`, `is_server=False`, `is_ui=False` (verified).
2. `_type_check` JS branch walks the tree, matches zero files, `errors_found==0` → `type_check="pass"`.
3. Tests skipped (`has_tests` false); smoke skipped (not `--smoke`).
4. `_summarize`: no `"fail"` → `passed=True`, prints `ALL PASS: TypeCheck=pass`, exit 0.

Nothing was compiled, parsed, executed, or requested. Verify's "enforcement" reports a green pass. Reproduced end-to-end against a fixture (package.json only, zero JS files): under both `auto` and `--type-only`, `node --check: OK`, `ALL PASS: TypeCheck=pass`, exit 0 (Measured, Fixture A).

---

## Flow D — a JS task that misses the ladder: the model council

When the Ponytail ladder returns `found: false` for a JavaScript task (Flow D's entry condition), control passes to `council.py`. The critical thing to understand up front: **the council never looks at the draft as JavaScript.** It routes a `--type code` job to Kimi, splices the draft into a text prompt, and scores whatever comes back with regex/substring greps that don't know or care that the payload is JS. There is no JS parser, no AST, no `node --check`, nothing language-aware anywhere in `council.py`. "Code" is just a routing keyword and a domain label for a prose grader.

### How a JS task reaches the council

The ladder itself is pure keyword set-subset matching (`ponytail_ladder.py:128`, inside `check_stdlib`: `if pattern_words.issubset(task_words)`), so any JS task whose words don't exactly cover a canned pattern like `"read file"` falls straight through to `found: false` (`ponytail_ladder.py:238`) with exit code 1. A realistic JS coding request — debouncing a React search input, an Express rate-limiter — misses every pattern and exits 1 (measured). That exit-1 is Flow D's trigger to invoke the council in `loop` mode.

The handoff is not automatic inside `council.py`; the draft arrives via `--draft`, `--draft-file`, stdin, or the temp file `/tmp/onklaud-draft.txt` (`council.py:66–68`, `_read_draft` at `council.py:821`). The council is handed a *draft to review*, not a task to *generate* from — a point the config and docstring obscure (see "Unshipped / mislabeled" below).

### `do_glm_pre_design()` — GLM touchpoint 1 (reasoning effort medium)

`cmd_loop` runs this first (`council.py:730`). It formats `GLM_PRE_DESIGN_PROMPT` (`council.py:125–132`), which asks GLM for a JSON sketch `{"approach", "files_to_touch", "risks", "alternatives", "complexity", "stdlib_check"}`, and calls the API at **reasoning effort medium** — the one place a non-default effort is set:

```python
raw = call_openrouter(GLM_MODEL, full_prompt, max_tokens=16000, reasoning={"effort": "medium"})
```
(`council.py:414`)

The draft is truncated to 8000 chars for pre-design (`council.py:410`), versus 60000 for review. There is nothing JS-aware here — the request/draft are pasted into a generic architecture prompt.

**Bug — the pre-design output is silently discarded.** Line 419 runs `parse_review(raw)` on GLM's response, but `parse_review` (`council.py:200`) only accepts JSON that contains **both** `"score"` and `"passed"` (`council.py:208`, `:219`). The pre-design schema has neither field, so even a perfectly-formed pre-design JSON falls through to the failure branch and returns `{"passed": False, "score": 0, "critique": "Failed to parse review response", "issues": [raw[:300]]}` (`council.py:254–255`) — the raw JSON, truncated to 300 chars, dumped into `issues[0]`. Measured. Consequences:

- The `approach` / `risks` / `stdlib_check` that GLM actually produced are thrown away — they survive only as a truncated string in `issues[0]` and are never parsed back out.
- In `cmd_loop`, line 732 prints `pre_design.get('critique', ...)` — i.e. it logs the string `"Failed to parse review response"` as the "GLM pre-design" line.
- Line 758 threads `pre_design.get("critique","")` into `all_critiques[...]["glm_pre_design"]`, so the arbiter later receives the literal words "Failed to parse review response" as the pre-design contribution, not any design content.

So GLM touchpoint 1 fires and costs tokens, but its architectural output is structurally unreadable to the code that consumes it.

### Kimi's role: reviewer, not generator

For a `code` task the only Kimi call is a **review** call. `do_review` (`council.py:372`) picks the model purely by the type string:

```python
model = KIMI_MODEL if review_type == "code" else GLM_MODEL
tmpl  = REVIEW_PROMPT_CODE if review_type == "code" else REVIEW_PROMPT_ARCH
```
(`council.py:374–375`; `KIMI_MODEL = "moonshotai/kimi-k2.7-code"`, `council.py:92`)

`REVIEW_PROMPT_CODE` (`council.py:97–104`) tells the model to emit `{"passed", "score", "critique", "issues"}`. The prompt lists things to look for (bugs, injection, race conditions), but the draft is inserted with a bare `DRAFT: {draft}` — no language tag, no fencing, no syntactic pre-processing. Whether the draft is JS, Python, or English prose, the exact same code path runs and the same JSON contract is expected back.

### `do_review()` and `do_dual_review()`

In `loop` mode Kimi never reviews alone — `cmd_loop` calls `do_dual_review` (`council.py:735`). That runs Kimi's code review (`do_review(..., "code")`, `council.py:436`) **and** a second, hand-rolled GLM review using the *same* `REVIEW_PROMPT_CODE` template (`council.py:439–446`) — this is GLM touchpoint 2. The two scores are then arithmetically averaged and rounded:

```python
avg_score = round((kimi_score + glm_score) / 2)
```
(`council.py:454`)

Note the combined `passed` uses `and` (`council.py:467`) while the numeric score is the mean — so a 9-and-a-fail can still average to a high number that later gets diluted again by the gate. Issues from both models are concatenated (`council.py:457`). GLM here reviews *code* with a *code* prompt despite the config framing GLM as the "architecture" model — the review stage's `architecture_model: glm-5.2` split (`config.yaml:95`) is bypassed in the dual path.

### The gate greps prose — the core of Flow D

After dual review, `cmd_loop` runs the quality gate on the **draft** (`council.py:744`) with `domain` set to the raw `review_type`. For a `code` task, `domain = review_type if review_type in ("code", "architecture") else "general"` (`council.py:703`) evaluates to `"code"` — **not** `"coding"`. This matters: **`quality_gate.py` registers every coding gate under `domain="coding"`** (`quality_gate.py:28,35,42,71,87`). A gate is applicable only if `g.domain in (domain, "all")` (`quality_gate.py:133`). With `domain="code"`, none of the `"coding"` gates match, so a `code`-typed loop draft is graded by only the two domain-agnostic gates — `ExcellenceThreshold` and `Clarity` — exactly like generic text (measured: `code` and `general` both resolve to `['ExcellenceThreshold', 'Clarity']`). The five coding-domain checks (ErrorHandling, TypeSafety, EdgeCases, DRY, DeadCode) never run in the loop's own gate step.

Even when the coding gates *do* run (the legacy `full` path also passes `review_type` verbatim, `council.py:637`, so `code`→`code` there too — the `"coding"` gates are only reachable via the `gate` subcommand with an explicit `--domain coding`), every gate is a substring/keyword grep over the raw text:

- `chk_error_handling` (`quality_gate.py:28`): flags if `"async"` appears but none of `"try"/"catch"/"except"` do — pure substring test.
- `chk_type_safety` (`quality_gate.py:35`): appends a `"Contains 'any' type"` issue if the substring `"any"` appears anywhere (so `"many"`, `"company"`, `"anything"` all trigger the issue text) — but the gate returns `len(issues) <= 1`, and there is only ever one such issue, so **TypeSafety can never actually fail**. The "any" check is a no-op: it adds an advisory string with zero effect on the score (measured — a draft containing `many`/`company`/`any` still scores 10/10 with `TypeSafety.passed = true`).
- `chk_edge_cases` (`quality_gate.py:42`): counts occurrences of `null/undefined/empty/invalid/edge/boundary` as substrings; passes if ≥1 found — so a comment naming those words satisfies it while real edge-handling code that doesn't spell them out fails.
- `chk_dry` (`quality_gate.py:71`): a coding gate the draft's other summaries omit — flags naive line duplication.
- `chk_dead_code` (`quality_gate.py:87`): greps for the literals `"TODO"`, `"console.log"`, `"debugger"`, `"print("`.
- `chk_clarity` (`quality_gate.py:120`, domain `all`): checks ` ``` ` fence parity and greps for `"http://"`.

None of these parse JS. They reward drafts that *talk about* correctness and punish terse correct code. Under `domain="coding"`, a working JS `debounce` with a real `await fetch` fails `ErrorHandling` and `EdgeCases` and scores 6 (measured), while the same logic reaches 10/10 the moment you paste the keywords `null undefined empty invalid edge boundary` plus a `try/catch` into comments (measured). Conversely, pure keyword-stuffed English prose with no code at all scores a perfect 10/10 PASS under `domain="general"` (measured) — indistinguishable from a real JS draft under that same domain.

The gate's pass bar is `score >= 10` (`quality_gate.py:148`), i.e. **every applicable gate must pass** — the "10/10 non-negotiable" of the config. Because the score is a weighted ratio ×10 (blocker 3, critical 2, warning 1; `quality_gate.py:129,146`), a single failed blocker drops a JS draft well below 10 under the `coding` domain. But in the loop's own draft-gate the domain is `"code"`, so only the two generic gates apply and any draft ≥100 chars with ≥3 newlines and balanced fences trivially clears 10/10.

### `cmd_loop()` — the round machine

State lives in two temp files: `ROUND_FILE = /tmp/onklaud-round.txt` and `ISSUES_FILE = /tmp/onklaud-issues.json` (`council.py:66–67`). Each invocation is one round; the round counter persists on disk between process runs.

- `read_round` (`council.py:534`) defaults to 1, clamps with `max(1, int(val))`, and swallows garbage back to 1 (measured: no file → 1, `0` → 1, `"garbage"` → 1).
- Entry guard: if `round_num > 3` the loop escalates **immediately** to arbitration and returns (`council.py:722–727`).
- Otherwise it runs pre-design → dual review → gate, then decides:
  - `passed` (combined `>=10` AND both review-pass AND gate-pass, `council.py:749`): `clear_round_state()` deletes both temp files (`council.py:553`) and finalizes.
  - failed AND `round_num >= 3`: **forced GLM arbitration** (`council.py:783–791`), then `clear_round_state()`.
  - failed AND `round_num < 3`: increment, `write_round(next_round)` (`council.py:795`), persist issues + critiques into `ISSUES_FILE` (`council.py:801–807`), print the trace, `sys.exit(1)`.

So the machine is: rounds 1 and 2 can fail-and-retry (exit 1, caller is expected to revise the draft and re-invoke), round 3 failing triggers mandatory arbitration. On the next round, prior critiques are re-hydrated from `ISSUES_FILE` only when `round_num > 1` (`council.py:713–719`) and appended to `all_critiques` for the eventual arbiter. Note the "revise" is external: `council.py` never asks Kimi to *fix* anything between rounds — it just saves issues to a file "for Claude to read" (`council.py:564`) and exits.

### `do_glm_arbitrate()` — touchpoint 3, then gate

Forced arbitration calls `do_glm_arbitrate` (`council.py:486`). It builds a `critiques_text` from every collected critique + issues (`council.py:488–492`), fills `GLM_ARBITRATE_PROMPT` (`council.py:115–123`) — "Synthesize the absolute best answer… Address every issue raised… output ONLY the final improved response" — truncating prompt to 2000, draft to 6000, critiques to 6000 chars (`council.py:495–498`), and calls GLM at **default reasoning effort** (no `reasoning=` arg, so `call_openrouter` defaults any model whose name contains `glm` to `{"effort": "high"}`, `council.py:149–150`).

Crucially, GLM's synthesized answer is then fed straight into the prose gate with `domain="general"` (`council.py:508`):

```python
gate_result = run_quality_gate(raw, "general")
final_score = gate_result.get("score", 7)
passed = final_score >= 10
```
(`council.py:508–511`)

Under `domain="general"` only `ExcellenceThreshold` and `Clarity` apply — so the arbitrated JS is judged solely on "≥100 chars and ≥3 newlines" and "balanced code-fences / no http://". A synthesized JS answer of any reasonable length trivially clears this at 10/10 (measured). The arbiter's output is what gets returned as `glm_synthesized` and, on the round-≥3 path, saved to `--output` (`council.py:789–790`).

**Output-save bug:** on every *non*-arbitration finalize, `_finalize_loop` (`council.py:860`) saves `original_draft = _read_draft(args)` — the untouched input — to `--output` (`council.py:865–866`), never an improved version. Only the round-≥3 arbitration branch saves the synthesized text. So a JS task that *passes* at round 1 writes the original draft back out verbatim; only a task that fails all the way to arbitration gets a rewritten file.

### `call_openrouter()` — reasoning, retry, backup key, degradation

`call_openrouter` (`council.py:137`) sets `temperature: 0.1` and injects reasoning effort: if a `reasoning=` kwarg is passed it's used verbatim; otherwise any model whose name contains `"kimi"` or `"glm"` defaults to `{"effort": "high"}` (`council.py:147–150`). So the effort matrix for a JS task is: pre-design = **medium** (explicit), all reviews and arbitration = **high** (defaulted).

Retry/backup logic (`council.py:155–195`):
- Up to `retries+1` = 3 attempts, with exponential backoff `2**attempt` seconds on `HTTPError`/`URLError`/generic exception (`council.py:172,177,182`).
- After exhausting a key, if `OR_KEY_BACKUP` is set and the current key is the primary, it swaps to the backup and resets `attempt = -1` (`council.py:186–189`) — a full fresh retry budget on the backup key. If already on the backup, it gives up (`council.py:190–192`).
- On total failure it prints to stderr and returns `None` (`council.py:194–195`).

Every caller treats `None` as degradation: `do_review`, `do_glm_pre_design`, `do_dual_review`, `do_glm_arbitrate` each return `degraded_result(...)` (`council.py:355`) with a hard-coded `score: 7, passed: False` and the memorable typo `"UNCUNCILED"` in the critique (`council.py:361`). Degraded runs are marked but *not* treated as passing (`passed=False`), so the pipeline knows it degraded. There is no DeepSeek fallback in code (see below) — degradation is the only fallback the council actually implements.

### Unshipped / mislabeled components

- **No Kimi generation.** The module docstring (`council.py:8`) and `nadirclaw/config.yaml` (`config.yaml:54–57`, the `kimi_generate` step; `config.yaml:88–92`, the `draft` stage) advertise "Code generation… Only if ladder empty." No such call exists — `grep` for `generat` in `council.py` hits only the docstring and the pre-design docstring. Kimi only ever *reviews*. Flow D therefore has no code-authoring stage; the "draft" must already exist.
- **DeepSeek is vaporware.** `config.yaml:34–40,136` lists `deepseek-v4-pro` as "EMERGENCY FALLBACK ONLY". The string `deepseek` appears nowhere in `council.py`. The real fallback is `degraded_result`.
- **`verify.py` step** (`config.yaml:72–73`) is never invoked from `council.py` — `verify` appears nowhere in the file.
- **Domain mismatch** (`code` vs `coding`) means the loop's advertised code scoring silently reduces to the 2 generic gates (`ExcellenceThreshold`, `Clarity`) on the draft.
- **Immune memory** records only the truncated *issues-list text* (`council.py:290`, `record_immune_memory`, storing `json.dumps(issues,…)[:200]`) and is never read back to influence a review — it is read only by itself (to append) and by the `status` command for display. Write-only pattern logging.
- **TypeSafety can never fail.** The `TypeSafety` "blocker" gate (`quality_gate.py:35`) returns `len(issues) <= 1` over a check that can only ever produce one issue — so it always passes. It looks like a code guardrail; it is inert.

### Net picture for a JS task

A JS coding task that misses the ladder is: sketched by GLM (whose sketch is then discarded by a parse bug), reviewed as opaque text by Kimi+GLM with a keyword-grepping prompt, scored by a prose gate that (a) runs only two generic gates in the loop due to the `code`/`coding` domain mismatch and (b) even under `coding` only pattern-matches substrings (with a "TypeSafety" gate that structurally cannot fail), and — if it fails three rounds — rewritten by GLM and waved through a two-check general gate. At no point is the JavaScript parsed, type-checked, or executed. The "code" type is a routing label, not a capability.

---

## Flow E — routing, benchmark, JS-biting gates, and the (missing) hook wiring

Flow E is where Onklaud 5's "JS awareness" actually lives at runtime: a keyword router that shoves JS-flavored turns to a coding model, a benchmark that manufactures the headline "JS hit rate," two quality gates that specifically pick on JS/TS output, and — critically — a claimed auto-integration layer (a `PostToolUse` syntax hook) that **is not in the repository at all**. Everything below is quoted from source and, where it makes a number, re-derived by executing the code.

### 1. `chat.py` `classify()` — the JS keyword router

The interactive chat REPL routes each turn to one of three models. When the current model is `deepseek` (the default primary), the prompt is passed through `classify()`; otherwise the user's explicit `/model` choice wins (`chat.py:164`):

```python
routed = classify(prompt) if current == "deepseek" else current
```

`classify()` is a bag-of-keywords counter (`chat.py:116-120`):

```python
def classify(prompt):
    kw = ["code","function","class","bug","fix","implement","refactor",
          "test","compile","syntax","api","endpoint","query","sql","algorithm",
          "optimize","python","js","ts","react","component","hook","import","export","type"]
    return "kimi" if sum(1 for k in kw if k in prompt.lower()) >= 2 else "deepseek"
```

The JS/TS-oriented tokens are the tail end of that list: `js`, `ts`, `react`, `component`, `hook`, `import`, `export`, `type` (`chat.py:119`). If **two or more** keywords appear, the turn is routed to Kimi K2.7 Code (the `"kimi"` model entry, `chat.py:35-36`); otherwise it stays with DeepSeek.

Two subtleties matter, both verified by running the function:

**(a) Threshold is two, so a single JS token does nothing.** A prompt that is literally just `"js"`, `"react"`, `"component"`, or `"hook"` routes to `deepseek` — the coding model is *not* engaged. You need at least two matches: `"write a react component"` → `react`+`component` → Kimi; `"add a hook"` → 1 match → DeepSeek. So the "JS tokens push a turn to Kimi" behavior only triggers when two of them (or one plus any other keyword like `code`/`fix`/`api`) co-occur.

**(b) Matching is `substring`, not word-boundary — so it misfires.** `k in prompt.lower()` (`chat.py:120`) is a raw substring test. That means `import` fires inside "import**ant**", `type` fires inside "proto**type**", and `ts` fires inside "char**ts**"/"scrip**ts**". Innocuous prose routes to the coding model: I confirmed `"these important charts"` (→ `import`+`ts`) and `"the type of scripts"` (→ `type`+`ts`) both route to Kimi, as does `"prototype the api"` (→ `type`+`api`). Conversely, `ts` is **not** a substring of "typescript" (no adjacent `t-s`), so `"typescript type"` scores only 1 (`type`) and stays on DeepSeek — a mildly ironic miss for a router meant to catch TypeScript.

This router is also the only place these JS tokens do anything: `classify()` is defined and called only in `chat.py`, is confined to the manually-run REPL, and has no connection to the gates or the benchmark below.

### 2. `benchmark_full.py` — the 10-task JS set and how the "JS number" is derived

The benchmark's task table (`benchmark_full.py:18-58`) mixes Python, JS, and native/CSS tasks. The JS block is exactly ten entries (`benchmark_full.py:36-45`, under the `# JS tasks` comment on line 35), each tagged `"js"` and each asserting `expected_stdlib = True`:

```python
    # JS tasks
    ("Read a file synchronously", "IO", "js", True),
    ("Parse a URL and extract query parameters", "Web", "js", True),
    ("Create an HTTP server on port 3000", "Server", "js", True),
    ("Generate a random integer between 1 and 100", "Utility", "js", True),
    ("Deep clone an object", "Data", "js", True),
    ("Format a date as ISO string", "Date", "js", True),
    ("Create a temporary directory", "Filesystem", "js", True),
    ("Set a timeout that can be cancelled", "Async", "js", True),
    ("Read all lines from a file", "IO", "js", True),
    ("Check if a path exists (file or directory)", "Filesystem", "js", True),
```

`run_ponytail_benchmark()` derives the number by shelling out to `ponytail_ladder.py` once per task, adding `--lang js` for anything not `native`/`""` (`benchmark_full.py:66-74`):

```python
cmd = ["python", str(MY_DIR / "ponytail_ladder.py"), "--task", task, "--json"]
if lang not in ("native", ""):
    cmd.extend(["--lang", lang])
r = subprocess.run(cmd, capture_output=True, text=True, timeout=10, ...)
```

A task counts as a "hit" only if the ladder's JSON says `found: true` (`benchmark_full.py:80`). The ladder's `check_stdlib` matches a KB pattern **only when every whitespace-split word of the pattern key is a subset of the task's whitespace-split words** (`ponytail_ladder.py:125-129`):

```python
for pattern, solution in patterns.items():
    pattern_words = set(pattern.split())
    if pattern_words.issubset(task_words):
        return {"level": "stdlib", ...}
```

The JS KB has only 14 keys (`ponytail_ladder.py:56-71`), and this subset rule is brittle, so **7 of the 10 "stdlib=True" JS tasks do not resolve.** Running the exact benchmark invocation (`ponytail_ladder.py --task "..." --lang js --json`) I measured **3/10 hits (30%)**, not the 100% the `expected_stdlib=True` column implies:

| JS task | ladder result |
|---|---|
| Read a file synchronously | HIT — `read file` |
| Parse a URL and extract query parameters | HIT — `parse url` |
| Create an HTTP server on port 3000 | MISS (`http get` key needs words `http`+`get`; task has `http` only) |
| Generate a random integer between 1 and 100 | MISS (no matching key) |
| Deep clone an object | MISS (no `deep clone` KB key) |
| Format a date as ISO string | MISS (only `parse date` exists in JS KB, and it needs the word `parse`) |
| Create a temporary directory | MISS (`temp dir` key — task words are `temporary`/`directory`, not `temp`/`dir`) |
| Set a timeout that can be cancelled | MISS (`sleep/delay` key never matches) |
| Read all lines from a file | HIT — `read file` |
| Check if a path exists (file or directory) | MISS (task token is `(file`, not `file`) |

The misses are almost all vocabulary/tokenization artifacts of the subset rule: "temp**orary** direct**ory**" never yields the words `temp`/`dir`; "Format a date" lacks the word `parse` that the JS `parse date` key demands (there is no `format date` key in the JS KB); and "(file" carries a stray paren so it isn't the token `file`. The one accidental win — "Read all lines from a file" matching `read file` — is itself a false positive: the returned solution is `readFileSync` (read the *whole* file), not a line reader.

Note also that the benchmark's `expected_stdlib` field is never actually compared against the ladder's answer — it is unpacked into the loop variable `expected` at `benchmark_full.py:66` and never read again; `run_ponytail_benchmark()` only tallies `found`/`level` (`benchmark_full.py:80-95`). So the `True` in each JS row is decorative; nothing fails when a `True`-marked task misses. The overall `hit_rate` the report prints (`benchmark_full.py:98`) blends all 35 tasks, so the weak JS resolution is diluted by the Python/native rows and never surfaces as a JS-specific figure.

### 3. `quality_gate.py` — the two gates that bite JS/TS output

Two of the ten gates specifically target JS/TS text, and both are pure substring heuristics. Both are `domain="coding"` (`quality_gate.py:28`, `quality_gate.py:35`), which matters — see the gating caveat below.

**TypeSafety — flags the token `any` (`quality_gate.py:35-40`):**

```python
@gate("TypeSafety", "blocker", "coding")
def chk_type_safety(output, ctx):
    issues = []
    if "any" in output.lower():
        issues.append("Contains 'any' type - prefer stricter types")
    return GateResult("TypeSafety", len(issues) <= 1, issues)
```

Two real subtleties, both measured:

- **It flags but does not fail.** The pass predicate is `len(issues) <= 1`, not `== 0` (`quality_gate.py:40`). Since the body can only ever append at most one issue, TypeSafety **always returns `passed: True`** while still emitting the "Contains 'any'" complaint. I ran `const x: any = getValue()` under `domain=coding` and got `TypeSafety passed=True issues=["Contains 'any' type - prefer stricter types"]`. The gate is effectively advisory noise — it never contributes a failing weight.
- **Substring false positives.** `"any" in output.lower()` fires on **m·any**, comp·**any**, **any**·thing. I ran `"This handles many companies and anything else..."` and TypeSafety raised the `any`-type issue (passed=True) on prose containing no type annotation at all.

**ErrorHandling — flags `async` without `try`/`catch`/`except` (`quality_gate.py:28-33`):**

```python
@gate("ErrorHandling", "blocker", "coding")
def chk_error_handling(output, ctx):
    issues = []
    if "async" in output and "try" not in output and "catch" not in output and "except" not in output:
        issues.append("Async operations should have error handling (try/catch)")
    return GateResult("ErrorHandling", len(issues) == 0, issues)
```

Unlike TypeSafety this one *can* fail (`len(issues) == 0`), and I confirmed it: an `async` fetch with no guard fails (`passed=False`), the same code wrapped in `try/catch` passes. But it inherits the same substring fragility in both directions:

- **False negative (gate evaded by an unrelated word):** because it looks for the raw substring `catch`, the word **catch·ment** makes `"catch" not in output` False → the gate passes. I ran `"async pipeline in the catchment area runs fast"` and it passed with zero issues despite having no error handling.
- **False positive on `async` substring:** `async` matches inside **async·hronous**; `"asynchronous flow without any guard clauses"` fails the gate. (It also conflates the Python `except` and JS `catch` keywords — any of the three anywhere in the blob silences it.)

Both gates are `blocker` severity (3× weight, `quality_gate.py:129`). The composite verdict is a rounded weighted ratio (`quality_gate.py:146-148`):

```python
raw_score = (weighted_sum / total_weight) * 10
score = round(raw_score)
passed = score >= 10
```

`passed` therefore requires `weighted_sum / total_weight` to round up to 10 (ratio ≥ 0.95). For the **coding** domain the applicable weights sum to 17 (four blockers + two criticals + one warning), so even a single failing gate — including the weight-1 `Clarity` warning, which yields 16/17 = 0.94 → rounds to 9 — drops the composite below 10. I verified no single coding gate can fail and still reach 10, so in practice every applicable coding gate must pass. Consequently any realistic JS snippet fails the composite regardless of the two JS gates, because `EdgeCases` and `ExcellenceThreshold` almost always trip on short output (measured: `const x: any = getValue()` scores **6/10** under `coding`, with `EdgeCases` and `ExcellenceThreshold` failing).

**Gating caveat — the two JS gates only exist in the `coding` domain.** `score_output` filters to gates whose domain is the requested domain or `"all"` (`quality_gate.py:133`), and the CLI defaults `domain` to `"general"` (`quality_gate.py:158`):

```python
domain = sys.argv[2] if len(sys.argv) > 2 else "general"
```

I confirmed by running the CLI without a domain argument: `python quality_gate.py 'const x: any = getValue()'` reports only `ExcellenceThreshold` and `Clarity` (score 2/10) — **TypeSafety and ErrorHandling are absent entirely.** They only fire when the caller explicitly passes `coding` as the second CLI arg. So the "JS-biting" gates are opt-in per invocation, not the default.

### 4. Integration / hooks — the auto-wiring is **not shipped**

The research paper asserts the syntax gate is automatic (`research_paper_benchmark.py:389-391`):

> "The syntax gate runs automatically after every Write/Edit via the PostToolUse hook, ensuring no broken code enters the codebase."

The PDF generator repeats it verbatim (`generate_paper_pdf.py:356-357`). But the hook files that would implement this are **absent from the repository.** A `find` over the whole tree (excluding `.git`) for anything matching `hook`, `settings`, `.ps1`, or `.claude` returns **nothing** — there is no `onklaud5-post-write`, no `onklaud5-session-start`, no `headroom-claude.ps1`, no `settings.json`, and no `.claude/` directory anywhere in the repo. (The README likewise never mentions `PostToolUse`, hooks, or `~/.claude`.)

Instead, `research_paper_benchmark.py` reaches into the *user's home directory* for these files and silently degrades to `0`/`False` when they're missing (`research_paper_benchmark.py:207-208`, `:214`):

```python
session_hook_lines = len(Path(os.path.expanduser("~/.claude/hooks/onklaud5-session-start")).read_text().splitlines()) if Path(os.path.expanduser("~/.claude/hooks/onklaud5-session-start")).exists() else 0
postwrite_hook_lines = len(Path(os.path.expanduser("~/.claude/hooks/onklaud5-post-write")).read_text().splitlines()) if Path(os.path.expanduser("~/.claude/hooks/onklaud5-post-write")).exists() else 0
...
headroom_installed = Path(os.path.expanduser("~/.claude/headroom-claude.ps1")).exists()
```

`benchmark_full.py:140` does the same `.exists()` probe for headroom:

```python
"headroom_available": Path(os.path.expanduser("~/.claude/headroom-claude.ps1")).exists(),
```

These are `Path(...).exists()` guards against files the repo never installs. On this machine all three named files are absent (verified: `~/.claude/hooks/onklaud5-post-write`, `~/.claude/hooks/onklaud5-session-start`, and `~/.claude/headroom-claude.ps1` all missing — even though a `~/.claude` directory itself exists), so `bench_context()` reports `session_start_hook: 0` and `post_write_hook: 0` lines while the paper still prints the "runs automatically after every Write/Edit" sentence unconditionally — the prose is hard-coded (`research_paper_benchmark.py:389-391`), not gated on whether the hook actually exists.

The `design-spec.md` treats these as *external* prerequisites, not shipped artifacts: it lists `~/.claude/headroom-claude.ps1` in its file table (`design-spec.md:72`) and recommends the user source it (`design-spec.md:83`: `Headroom recommended: '. ~/.claude/headroom-claude.ps1'`), and describes the "Session-start hook" and "Post-write hook" only as an anti-saturation *strategy* (`design-spec.md:58-59`).

**Conclusion.** As shipped, the syntax gate (`fast_gate.py`) and the quality gate (`quality_gate.py`) are **manually-invoked CLIs** — you run them by hand or via the benchmark's `subprocess.run` harness (`benchmark_full.py:108-112`, `research_paper_benchmark.py:123-128`). The advertised auto-integration — a `PostToolUse` hook firing the gate after every Write/Edit, a session-start hook, and headroom shell compression — is **external and unshipped**: the code only ever *checks whether* those files exist in `~/.claude` and quietly reports zeros/false when they don't. There is no wiring in the repo that would ever install or invoke them.

---

## Algorithmic critique (JS-specific) & honest paper-vs-code divergences

This section splits into two source-grounded parts. Part 1 explains, from the code and from re-run measurements, why JavaScript is the worst-performing language in Onklaud's own Ponytail ladder. Part 2 catalogs the places where the marketing artifacts (README, research-paper PDF, QUICKSTART) diverge from what the code actually computes. Every number below was reproduced by running the shipped code in `/tmp/onklaud-5` (Python 3.13, macOS); see the *measured* block.

### Part 1 — Why JS is the worst-performing language in Onklaud's own system

#### 1.1 The one algorithm that decides everything: pattern-word ⊆ task-word

All three ladder rungs (`stdlib`, `native`, `existing_dep`) share one matcher: a task "hits" a pattern only when *every* word of the pattern key is present in the set of whitespace-split task words.

```python
# ponytail_ladder.py:122-129  (check_stdlib)
task_words = set(task_lower.split())
for pattern, solution in patterns.items():
    pattern_words = set(pattern.split())
    # All pattern words must appear in task words
    if pattern_words.issubset(task_words):
        return {"level": "stdlib", "solution": solution, "language": lang, "pattern_matched": pattern}
```

`check_native` uses the identical `issubset` test (`ponytail_ladder.py:137-139`). This design has three consequences that hit JS hardest.

**(a) Word-subset brittleness — plurals and phrasings silently miss.** Because matching is exact-word-membership, any inflection breaks it. Verified against the shipped tool with `--lang js`:

| Task | JS result |
|---|---|
| `parse a url` | found ✅ (`parse url` ⊆ words) |
| `parse urls` | **not found** ❌ (`url` ∉ `{parse, urls}`) |
| `generate a uuid` | found ✅ |
| `generate uuids` | **not found** ❌ |

The matcher has no stemming, no synonyms, no embeddings — the paper itself concedes this and calls it "word-level pattern matching" (PDF §2.1). A single plural or reordering drops the hit.

**(b) False-positive hazard — subset matching ignores task intent.** Because only *containment* is checked, a task that is *about* a keyword but not *requesting* it still matches. Verified with `--lang js`:

- Task `"explain how to read a binary file without loading it"` → **found**, returns `readFileSync(...)` (pattern `read file`). The user asked to *avoid* loading the file; the ladder hands back the exact opposite.
- Task `"write a blog post about a file system"` → **found**, pattern `write file`.

These follow directly from `pattern_words.issubset(task_words)` (`ponytail_ladder.py:128`). The two-word JS keys (`read file`, `write file`) are especially prone because the two common words appear in many unrelated sentences.

**(c) The two-word JS keys also cause *intra-benchmark* misfires.** The benchmark's own JS task `"Read all lines from a file"` matches the `read file` pattern and returns `readFileSync(path, 'utf-8')` — a whole-file read, not line reading (`ponytail_ladder.py:57`). It counts as a "hit" while returning a semantically wrong solution.

#### 1.2 The JS pattern table is small and gappy — and the README's own demo doesn't reproduce

The JS stdlib table has **14 entries** (`ponytail_ladder.py:56-71`; confirmed by importing the module: `len(STDLIB_PATTERNS['js']) == 14`), versus 23 for Python (`ponytail_ladder.py:30-53`) and 16 native (`ponytail_ladder.py:75-92`). Two gaps stand out:

- **No `http server` pattern.** The only "http" JS entry is `"http get"` → `const data = await fetch(url).then(r => r.json())` (`ponytail_ladder.py:61`). In the whole file, `server` appears only in the *dependency* mappings (`express`, `fastapi` at `ponytail_ladder.py:166,194`), never in the JS stdlib table. So the benchmark task **"Create an HTTP server"** returns `found: false` (reproduced), even though Node's `node:http` `createServer` is stdlib and trivially patternable.
- No pattern for deep-clone (`structuredClone`), file-exists (`existsSync`), or `randomInt` — all of which are benchmark JS tasks, all of which miss.

**The README's flagship Ponytail demo is fabricated.** The README's "Concrete Example" section (README:125-157) is built entirely around **"Build an HTTP client with retry logic"** (README:127, 133, 151) — the README never mentions an HTTP *server*. It shows this task resolving at "Step 0 — Ponytail Ladder" to `requests.Session()` with a "built-in retry adapter" at "Cost: \$0.0000 | Time: 98ms" (README:153-157). But the shipped tool returns `found: false` for that exact task under `--lang python`, `--lang js`, and auto-detect — there is no `http`/`client`/`retry` pattern in any table (`http` appears only in the `http get` keys and in dependency mappings). The headline demonstration of the ladder's value shows a Step-0 result the ladder does not actually produce.

#### 1.3 The benchmark never even exercises the JS path

`research_paper_benchmark.py:bench_ponytail()` invokes the ladder with `--task` and `--json` but **no `--lang`** (`research_paper_benchmark.py:69-74`). Language therefore falls to `detect_language`, which returns `"python"` unless the task string literally contains a JS keyword like `javascript`, `node`, `.js`, `react` (`ponytail_ladder.py:99-114`). None of the 10 benchmark JS task strings (`research_paper_benchmark.py:32-43`) contain such a keyword. Reproduced with the shipped tool, the auto-detected language for all 10 "JS" tasks is `python`:

| Benchmark "JS" task | auto-detected lang | matched pattern | table used |
|---|---|---|---|
| Parse a URL | python | `parse url` | **Python** table |
| Format a date as ISO string | python | `format date` | **Python** table |
| (other 8) | python | — (miss) | Python table |

So the README's headline row **"JavaScript 10 / 2 / 20.0%"** (README:235) is not measuring JavaScript at all — its two "JS hits" are Python-table matches on `parse url` (`ponytail_ladder.py:35`) and `format date` (`ponytail_ladder.py:37`). When you force the *actual* JS path with `--lang js`, the honest result is **3/10 = 30%**, not 20% — and one of those three ("Read all lines from a file") is the wrong-semantics `readFileSync` false hit from §1.1(c). Either way JS is the weakest language, but for a subtler reason than the README implies: the reported JS number reflects Python-pattern coverage, and the real JS table is both small and never engaged by the benchmark harness.

#### 1.4 The JS verification paths downstream are also thin

Beyond the ladder, Onklaud's "verify" stage treats JS/TS asymmetrically:

- **TS type-check is gated on `tsconfig.json`.** `verify.py:130-134`: if `lang == "typescript"` and no `tsconfig.json` exists, it prints `"No tsconfig.json - skipping type-check"` and records `"skipped"`. A skip is *non-failing* — `_summarize` only fails on `"fail"` (`verify.py:278-283`) — so a TS project without a tsconfig sails through verification with zero type-checking.
- **Plain-JS "type-check" is `node --check`, which is syntax-only and O(files).** `verify.py:151-174` walks the whole tree and shells out to `node --check` per file (`verify.py:160-163`). `node --check` validates *parse-ability only* — no type errors, no undefined-variable, no import resolution — and spawns a fresh Node process per `.js/.ts/.jsx/.tsx/.mjs` file, so cost scales linearly with file count and Node startup dominates. The same syntax-only `node --check` is what `fast_gate.py:19` runs as the JS "syntax check". In short: for JS the strongest automated check Onklaud applies is "does it parse."

### Part 2 — Honest divergences between the marketing artifacts and the code

All five headline benchmarks were re-run against the shipped scripts. Several numbers in the README and the research-paper PDF disagree with each other and with the code.

#### 2.1 Immune Detection: 0.0% vs 50% — an internal contradiction, and an unshipped data file

The README claims **"Immune Detection 50% (5/10)"** (README:226) and the comparison table advertises **"50% detection"** (README:36). The research-paper PDF's *results table and analysis* say the opposite — **"Detection rate 0/10 (0.0%)"** (PDF §3.2) and *"The immune memory correctly detected 0 out of 10 known failure patterns"* — and the PDF's summary tile literally reads **"0.0% / Immune Detection"**. Yet a *later paragraph of the same PDF* reverts to *"Detection rate is 50% on test cases"* (PDF §4.3). So the paper contradicts itself, and the README contradicts the paper's headline number.

The code explains why the honest number is **0.0%**. `pre_check.py` reads patterns from `immune_memory.json` (path defined at `pre_check.py:22`, loaded by `load_memory` at `pre_check.py:36-42`, which returns `[]` if the file is absent, `pre_check.py:37-38`). That file is **`.gitignore`d as "Generated data (from your runs)" and is not shipped** — it is absent from `git ls-files`. It is, however, *regenerated by runs*: after exercising the pipeline it appeared on disk here with 2 patterns. Detection is 0/10 regardless of which state the file is in:

- File **absent** (as shipped): `check_task` returns early with `{"warnings": [], "score": "clean", "total_patterns": 0}` (`pre_check.py:57-58`); the immune benchmark counts a hit only when the expected category appears in `warnings` (`research_paper_benchmark.py:178-182`), so with warnings empty it can never hit → **0/10 = 0.0%**.
- File **present** with the 2 generated patterns: `total_patterns` is 2 (not 0), and one benchmark task even produces `warnings:['validation']` — but that flagged category never equals the *expected* category, so the count is still **0/10 = 0.0%**.

Note also that the paper's "19 patterns stored" is not measured at all — `bench_precheck` hardcodes `"patterns_stored": 19` (`research_paper_benchmark.py:198`) and the PDF/README simply interpolate it (README:64, PDF §2.2/§3.2). The paper's own results table (0.0%) is the correct one; the "50%" claims describe a detection rate the code cannot produce on either file state.

#### 2.2 Pipeline Integration: 96.8% (30/31) vs 96.7% (29/30) vs 24/25 — and the real run prints none of them

Four different integration numbers appear across the repo's own artifacts:

| Source | Claim |
|---|---|
| README benchmark table (README:228) | 96.7% — **29/30** — 1 warning |
| README Quick Start (README:298) | RESULTS: **30/31** passed (0 failed, 1 warnings) |
| QUICKSTART.md (QUICKSTART.md:41) | Expected: **24/25** passed |
| Research-paper PDF (§3.4) | 96.8% — **30/31** — 1 warning |

The harness prints `RESULTS: {passed}/{total}` (`test_pipeline.py:224`), and `total` is incremented once per `test()`/`warn()` call (`test_pipeline.py:46,56`). There are 19 static `test`/`warn` call sites, but that is *not* the run total: line 114 (`test(f"Syntax OK: ...")`) fires once per `.py` file — 16 files in this repo — so a real run prints far more than 19. Executed here (stubbing the missing `yaml` module so the suite can finish): **32/32** with `OPENROUTER_API_KEY` set, or **31/32 (1 warning)** without one. On a stock host it does not print a RESULTS line at all — it first raises `ModuleNotFoundError: No module named 'yaml'` at `test_pipeline.py:191` (the config test). None of 24/25, 29/30, or 30/31 is what this code prints; the doc numbers were written by hand against different snapshots.

#### 2.3 "67.2% context reduction" is line-deletion of home/config files, not runtime compression

The README lists **"67% reduction / Context compression"** as a shipped capability (README:38), quantified as **"67.2% (232→76 lines)"** (README:227); the PDF repeats "232 → 76 lines … 67.2% reduction" (PDF §3.3). The implementation is `bench_context()`:

```python
# research_paper_benchmark.py:211-219
original_lines = 232  # Original CLAUDE.md + hooks before optimization
current_lines = user_claude_lines + project_claude_lines + session_hook_lines + postwrite_hook_lines
...
"reduction_pct": round(100 * (original_lines - current_lines) / original_lines, 1),
```

`232` is a hardcoded literal (`research_paper_benchmark.py:211`). `current_lines` is the summed *line count* of four files that live **outside the repo**, under `~/.claude/` (`research_paper_benchmark.py:205-208`); each term is `0` if its file is absent. So "reduction" is `(232 − linecount)/232` — an accounting of how many lines were deleted from configuration/home files, not any runtime token compression. Run here with those files absent, `bench_context` reports `current_total_lines: 0` and `reduction_pct: 100.0` — and the PDF's own summary tile indeed reads "100.0% / Context Reduction", contradicting its own "67.2%" body text. The paper even converts the line delta to tokens via a flat "~4 tokens per line" heuristic (`research_paper_benchmark.py:408`). Marketing calls this "context compression," which the arithmetic does not support.

#### 2.4 Headroom is unshipped, and self-reports "not available"

The README sells **Headroom — "60-95% context compression … \$0"** as one of the cost-saving layers (README:65). In code, Headroom's presence is gated on a PowerShell file: `headroom_installed = Path(os.path.expanduser("~/.claude/headroom-claude.ps1")).exists()` (`research_paper_benchmark.py:214`). No `headroom*` file exists anywhere in the repository (verified: `find . -iname '*headroom*'` returns nothing). Consequently `headroom_installed` is `False` and `headroom_compression` self-reports **"not available"** (`research_paper_benchmark.py:227`, reproduced). The paper's §3.4 prose still interpolates "Combined with Headroom (…compression)" (`research_paper_benchmark.py:409`) — i.e. the component advertised as shipped is, by the code's own check, not installed and not present in the distribution.

#### 2.5 A silent measurement bug: the integration parser counts Windows-only markers

`bench_integration()` derives its pass/fail counts by string-counting `[PASS]`/`[FAIL]`/`[WARN]` in the child output (`research_paper_benchmark.py:244-247`). But `test_pipeline.py` emits those ASCII markers only when `sys.platform == "win32"`; on macOS/Linux it emits emoji `✅/❌/⚠️` (`test_pipeline.py:26-41`). So on any non-Windows host — including the one these numbers were re-run on — the counter matches nothing. Executed here, `bench_integration` returns `total_tests: 0, passed: 0`, and therefore `pass_rate_pct: 0.0` — `round(100 * 0 / max(0, 1), 1) == 0.0` (the `max(total, 1)` guard only prevents a divide-by-zero; with `passed = 0` the rate is **0.0%**, not 100%). The paper claims "Windows 11 … Python 3.12" (`research_paper_benchmark.py:304`), the only platform on which this parser produces non-zero counts — another reason the reported integration figures cannot be reproduced cross-platform.

#### 2.6 Pattern-table sizes are overstated

The paper describes "50+ Python stdlib patterns, 20+ JavaScript patterns, and 15+ native HTML/CSS patterns" (PDF §4.1; §2.1 similarly says "50+ stdlib patterns, 15+ native"). The code contains **23** Python, **14** JS, and **16** native patterns (`ponytail_ladder.py:30-92`; confirmed via module import). The native count (16 vs "15+") is honest; the Python (23 vs "50+") and JS (14 vs "20+") claims are roughly 1.4–2.2× inflated.

#### 2.7 The confidence interval undercuts the headline

The README reports the flagship hit rate as **"57.1% (20/35)"** with **"95% CI: [41%, 73%]"** (README:224). That interval is ±16 points on n = 35 — it spans from "barely better than a coin flip on whether the stdlib covers your task" to "nearly three-quarters." A range that wide on 35 hand-picked tasks does not support the paper's framing of "measurable, statistically significant improvements" (README:250-251); it is consistent with almost any hit rate a reader might guess. And, per §1.3, all 20 hits were scored under language auto-detection (no `--lang` passed), so the JavaScript third of the sample was graded against the Python table.

### Summary

The JS weakness is real but its *stated cause* is wrong: the README's "20% JS" number comes from running JS tasks through the Python pattern table (no `--lang` passed), the actual JS table is 14 shallow, two-word-keyed entries with no `http server`, and the shared `issubset` matcher is simultaneously brittle (plurals miss) and permissive (intent-blind false positives). On the honest JS path the rate is 30%, still the lowest of the three languages — and the README's own flagship "HTTP client with retry" Ponytail demo doesn't even reproduce (the tool returns `found: false`). Separately, the marketing artifacts disagree with the code and with each other on immune detection (0% vs 50%, from a data file that isn't shipped and is 0% whether present or absent), integration pass counts (four documented figures, none of which the code prints — a real run here is 31/32 or 32/32, or crashes on a stock host), "context reduction" (config-file line-deletion, not compression; 100% here vs the claimed 67.2%/232→76), and Headroom (unshipped, self-reported "not available") — with a cross-platform measurement bug that zeroes the integration parser off Windows.
