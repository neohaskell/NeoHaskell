---
name: neohaskell-dialect-rules
description: Add, modify, or debug NeoHaskell dialect enforcement rules (hlint law + edit-hook teacher). Use when a new dialect rule is needed, a rule misfires, or an exception should be registered or promoted.
---

# Extending the dialect enforcement

Two engines enforce the dialect; know which one you're touching:

| | `.hlint.yaml` (**the law**) | `.claude/hooks/dialect-guard.py` (**the teacher**) |
|---|---|---|
| Runs | `./dev lint` + CI gate | PreToolUse, ~50ms per edit |
| Precision | exact (parses real Haskell) | heuristic (regex on edit fragments) |
| Scope | modules, functions, expression rewrites | + syntax (`where`), types (`Either`), usage-vs-definition |
| Wins on disagreement | **always** | never — gets corrected |

## Decision tree for a new rule

1. **Expressible in hlint?** (module restriction, function restriction,
   expression rewrite) → add it to `.hlint.yaml` FIRST. That's the law.
   Optionally mirror it in the hook for instant feedback.
2. **Syntax / type-level / usage-vs-definition?** → hook only; name the
   backstop (GHC or review) in the rule's comment.
3. **One-off exception, not a rule?** → `.hlint.yaml` `within:` entry with a
   justification + `belongs-in:` note. Rule of three: the third exception for
   the same symbol means propose a Core primitive instead.

## Adding a hook rule (the mechanical contract — CI-enforced)

1. Add the rule to `RULES` in `dialect-guard.py`: `(id, regex, message)`.
   - The comment above it MUST name the canonical gate (hlint rule / GHC / review).
   - The message MUST teach: quote the alternative and the escape hatch.
   - Per-rule exemptions go in `exempt()` keyed by rule id — never by message text.
2. Add cases to `.claude/hooks/dialect-guard-cases.json`:
   - ≥1 **blocking** case: `"expect_rules": ["<your-id>"]`
   - ≥1 **passing** case: `"pass_rules": ["<your-id>"]` — the false-positive
     you engineered against, as a regression test.
3. Run `python3 .claude/hooks/dialect-guard.py --self-test` (also runs in
   `./dev doctor` and CI). **A rule without both cases fails coverage** —
   this is the gate that keeps the rule list from rotting.

## Known limitations (by design — HOOK-ALLOW covers them)

- `where` is banned only as a let-substitute. Declaration `where`
  (module/class/instance/data/newtype/GADT/closed type family) is allowed via
  line-head detection — a multi-line declaration head whose *continuation*
  line carries the `where` will false-positive: use `-- HOOK-ALLOW: <reason>`.
- The hook sees edit fragments, not files: it cannot do cross-line or
  scope-aware analysis. If a rule needs that, it belongs in hlint or GHC,
  not here.

## Debugging a misfire

1. Reproduce: `echo '{"tool_name":"Edit","tool_input":{...}}' | python3 .claude/hooks/dialect-guard.py`
2. If it's a false positive: add the payload as a **passing case** first
   (red), then fix the rule/exemption (green). The case stays forever.
3. If the rule is fundamentally unsound for fragments: delete it and note the
   canonical gate that still covers the pattern.
