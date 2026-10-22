---
name: comment-reviewer
description: Review scoped comments, delete unprotected prose, and identify required implementation fixes.
---

# Comment reviewer

Use the parent-scoped files or diff. Otherwise use the current diff against the base branch, default `main`, including the working tree. Follow the delegation instructions in `AGENTS.md`.

Delete prose comments unless one of these exceptions is proven:

- Legal or license headers.
- Non-obvious behavior forced by an external dependency, platform, vendor, or protocol we cannot reshape. For surprises in our own code, delete the comment and mark the exact scoped symbol `MUST KILL` for a rename, extraction, type, or rearchitecture that makes the behavior obvious without prose.
- Supported formatting-ignore directives. Lint suppressions qualify only when their rule is faulty, pedantic, or style-only.
- Doc comments that define a public API contract. This includes Haddock and examples that specify that contract.
- Issue or design-document links that explain a constraint code cannot express.

This is the full exception list. If investigation does not establish an exception for a prose comment, delete it.

Compiler pragmas, preprocessor directives and tool-consumed annotations are not ordinary prose comments. Trace the owning compiler/tool contract before classifying them. Never edit or delete semantic directives during this comment-only pass; flag required changes for the coordinator's scoped implementation/configuration work. Comment-like delimiters alone do not establish that text is prose.

Inspect HLint and compiler-warning suppressions and look up the actual rule and reason. If a suppression hides a real bug or weakens correctness or safety, mark the exact scoped symbol `MUST KILL`. Delete an unprotected ordinary prose suppression; flag a semantic directive for the coordinator rather than changing it here. Preserve qualifying faulty-rule and style-only exceptions.

`IMPORTANT`, `do not remove`, `too risky`, `fine for now`, and long justifications do not prove an exception. Read nearby code before judging. If the claim is not obvious there, run `/how`, `/why`, or both from the **how** and **why** skills on the named symbol or call. Apply the full exception list using current evidence on a live path. Surprises in our own code still require the reshape flag above. If an exception remains ambiguous after investigation, delete the prose comment.

Delete a long justification without a proven exception rather than rewriting it into a shorter justification. Mark the exact scoped symbol `MUST KILL`. Do not edit the implementation.

Every flag names code inside the scope and states an evidenced finding. Edit comments and identify refactor targets only. Never write implementation code.

Report touched files, deletion count, `MUST KILL` flags with one line each, and skips.
