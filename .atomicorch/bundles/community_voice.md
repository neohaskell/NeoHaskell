---
id: community_voice
target_max_tokens: 2000
---

# NeoHaskell Community Voice Guide

NeoHaskell's community writing targets **Jess**: a junior developer with a TypeScript or Java background, contributing 15–30 minutes per day. Every piece of documentation, PR description, issue comment, and release note must be readable and welcoming to Jess.

---

## Tone Principles

- **Friendly and encouraging** — Welcome questions. Acknowledge that learning a new language is genuinely hard.
- **Empathetic** — Remember what it felt like to not know this thing.
- **Direct** — Get to the point. Jess has 15 minutes.
- **Never condescending** — No "as you know" or "obviously this is".
- **Concrete** — Show code, not theory.

---

## Words to Avoid

Never use these terms without an explanation or a link to a guide:

| Avoid | Use Instead |
|-------|-------------|
| simply | (omit the word entirely) |
| obviously | (omit the word entirely) |
| just | (omit or say "you can") |
| monad | "a type that chains operations" |
| functor | "a type you can map over" |
| applicative | (avoid; use concrete example) |
| typeclass | "an interface" |
| kind | "the type of a type" |
| higher-kinded | "generic over container types" |
| endofunctor | (never use this) |
| referentially transparent | "pure function" |
| denotational | (avoid) |
| equational reasoning | "you can replace a name with its value" |
| isomorphism | "these two things are equivalent" |

---

## PR Body Template

```markdown
## What changed

[1–3 sentences. What does this PR do? Avoid jargon.]

## Why

[1–2 sentences. The problem this solves or feature this enables.]

## How to test

[Bullet list of steps Jess can follow to verify the change works.]

- Run `cabal test nhcore-test-<suite>` and confirm all tests pass.
- [Specific thing to try in the repl or example app.]

## Notes

[Optional. Anything non-obvious about the implementation, tradeoffs, or follow-up work.]
```

---

## Release Note Style

Release notes are written for users updating their projects, not for maintainers.

Structure:
1. **One-line summary** (present tense: "Adds X", "Fixes Y")
2. **What you can now do** (concrete example)
3. **Migration** (if breaking: exactly what to change)
4. **Thanks** (acknowledge contributors by GitHub handle)

Example:

```
### Adds `Array.groupBy`

You can now group array elements by a key function:

    let grouped = users |> Array.groupBy (.department)

This returns a `Map Text (Array User)` keyed by department name.

Thanks to @contributor for the implementation.
```

---

## Issue Comment Guidelines

When commenting on a contributor's issue or PR:

1. **Thank them first** — even if the issue is a bug report or a request to change something.
2. **Explain the why** — don't just say "we won't do this"; explain the design constraint.
3. **Offer an alternative** — if closing a request, suggest what they can do instead.
4. **Use their name** — personal acknowledgment matters.

Example:
```
Thanks for reporting this, @username! You're right that the error message here is confusing.

The reason it works this way is [explanation]. We're tracking a broader improvement to error messages in #123.

In the meantime, you can work around this by [alternative].
```

---

## Documentation Writing Rules

1. Lead with what the reader can **do**, not what the library **is**.
2. Every API function gets one example — real code, not pseudo-code.
3. Link to related functions. Jess won't know to search for them.
4. Explain error cases. "Returns `Result.err` when..." is essential.
5. Keep module-level docs to 3–5 sentences. Link to a guide for more.
