# Change 002: Enforce Task control-flow dialect — `|> discard`, `Task.when`, `Task.unless`

Three Task-context control-flow idioms are currently un-enforced, so vanilla
shapes drift into new code:

1. A discarded Task action should read `action |> discard`, not `_ <- action`.
2. `if condition then pass else action` should read `Task.unless condition do action`.
3. `if condition then action else pass` should read `Task.when condition do action`.

This change adds the enforcement at the layer that can express each rule
precisely, migrates the genuine Task-context violations already in the tree,
and replaces the blanket `Use when` / `Use unless` hlint ignores (which merely
silenced the *vanilla* `Control.Monad` suggestion) with exact NeoHaskell rules.
`pass :: Applicable f => f ()` and `discard :: Mappable m => m a -> m ()` are
generic, while `Task.when`/`Task.unless :: Bool -> Task err () -> Task err ()`
are Task-only — so the rules are engineered to fire on dialect Task code and
leave parser / JSON-decoder / arbitrary-monad code alone.

```yaml spec
issue: adhoc:lint-task-control-flow
kind: refactor
touches: [dev-pipeline]
breaking: false
new-dependency: false
new-capability: false
new-extension-point: false
```

## Contract delta

Internal tooling + mechanical migration. No `codemap/signatures/` line is added
or removed: the `.hs` edits change function *bodies* (`if … else pass` →
`Task.when …`), never a signature, and the rules live in `.hlint.yaml` and the
edit hook. The promised diff is therefore empty.

```diff signatures
```

**Enforcement-layer decision** (the crux of the request — "prefer `./dev lint` /
HLint when syntax can be expressed precisely; use the edit hook only where HLint
cannot without unacceptable false positives"):

- **Rule 1 (`_ <- action` → `action |> discard`) → edit hook** (`no-discard-bind`
  in `dialect-guard.py`). A discarded bind is a do-*statement*, not an
  expression, so HLint's custom `lhs/rhs` rewrites cannot match it (verified:
  `hlint` emits no hint for `_ <- x`). The hook's added-lines-only scope also
  grandfathers the 142 existing sites (≈110 of them idiomatic parser
  combinators — `_ <- Parser.char …` — which the request says explicitly *not*
  to churn), so the rule teaches new code without a mass rewrite. Parser
  packages (`core/syntax/**`, `core/neoql/**`) are additionally path-exempt so
  future combinators are not nagged. Backstop: none downstream — the hook is
  the sole gate (noted in the rule comment).

- **Rules 2 & 3 (`if … pass` → `Task.when`/`Task.unless`) → HLint** custom
  `error` rewrites in `.hlint.yaml`. HLint parses real Haskell, so it matches
  the full `if`-expression structure precisely (multi-line, `do`-block
  branches, `case`-arm `-> pass` correctly *excluded*) — the edit hook's
  fragment regex cannot. The rules key on the **literal** `pass` (multi-char
  identifiers are literals in HLint hints; single-char `c`/`a` are the
  wildcards), which is the dialect no-op and correlates with Task code; legacy
  non-Task branches use `pure ()`/`return ()`, so they are not matched. Residual
  risk: a non-Task Applicative that uses `pass` in an `if` would be
  mis-suggested — GHC rejects the wrong rewrite (Task.when is Task-typed) and
  the hlint escape hatch (`{- HLINT ignore -}`) covers the rare case.

**Ignore replacement.** The two blanket `- ignore: {name: Use when/unless}`
lines are removed. Un-muting the builtin re-exposes legacy `pure ()` /
`return ()`-in-`if` sites — all genuinely **non-Task** (`Console` IO,
`Service.Transport.Web` WAI `drainBody`, and the two `Q`-monad TH modules) where
`Task.when`/`Task.unless` do not type-apply and vanilla `when`/`unless` is
import-banned. The three modules where the builtin actually re-fires (`Console`,
`Service.OutboundIntegration.TH`, `Service.Transport.Web`) get a narrow,
justified module-scoped ignore — not a global mute — which is the request's
"justified, explicit grandfathering".

## Criteria

All proving is `unit`: the rules operate on source text, and each rule's proof
is a harness self-test or `./dev lint`, with no runtime boundary crossed. The
migration's compile-safety is proven by the typechecker.

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | The edit hook rejects a newly-added discarded Task bind `_ <- action` in non-parser code, quoting the `discard` pipeline alternative and the escape hatch | `dialect-guard.py --self-test` blocking case for rule `no-discard-bind` | unit |
| C2 | The edit hook does NOT flag a named bind `x <- action`, nor a parser-package `_ <- Parser.char …`, nor a list-comprehension generator | `dialect-guard.py --self-test` passing cases for rule `no-discard-bind` | unit |
| C3 | `./dev lint` rewrites `if c then a else pass` to `Task.when c a` (and not a `case`-arm `-> pass`) | `./dev lint` custom hint `NeoHaskell: use Task.when` | unit |
| C4 | `./dev lint` rewrites `if c then pass else a` to `Task.unless c a` | `./dev lint` custom hint `NeoHaskell: use Task.unless` | unit |
| C5 | All 29 genuine Task sites are migrated — 21 `if c then a else pass` to `Task.when`, 8 `if c then pass else a` (incl. multi-line) to `Task.unless`; the non-Task `pure ()`/`return ()` sites are left untouched; `./dev lint` is clean on `core/`+`testbed/src/` and the migrated modules typecheck | `./dev lint` clean plus `./dev check` typecheck-clean | unit |
| C6 | Every hook rule has a blocking and a passing case; all governance self-checks pass | `./dev doctor` harness self-check | unit |

## User impact

Not breaking. No public signature or wire-format change — the migrated `if …
pass` blocks and their `Task.when`/`Task.unless` replacements are behaviourally
identical (`Task.when c a` runs `a` iff `c`, `Task.unless c a` runs `a` iff not
`c`, each otherwise doing nothing — exactly like the `if`/`pass` forms). New Task
code is nudged toward the dialect idioms at edit
time (rule 1) and at `./dev lint`/CI (rules 2–3). Existing non-dialect parser
and `Q`-monad code is deliberately preserved via added-lines grandfathering and
a scoped ignore. Testbed: no acceptance-test change — this is a source-dialect
and tooling change with no HTTP-observable behaviour.

## ADR

Not required — no trigger (breaking / new-dependency / new-capability /
new-extension-point all false). The enforcement-layer decision and the
ignore-replacement rationale are recorded above (in **Contract delta**) and in
the rule comments, consistent with how the existing dialect rules
(`.hlint.yaml` `a $ b`, the hook's `no-pure-return` etc.) are documented rather
than each carrying an ADR.
