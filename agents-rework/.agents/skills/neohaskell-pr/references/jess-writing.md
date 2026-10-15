# Writing for Jess

Jess is the time-constrained junior developer building an app with NeoHaskell.
She can read application code and follow concrete examples; she should not need
to know the framework's implementation, CI architecture, or agent pipeline to
understand a change. This follows the persona in
[ADR-0010](../../../../docs/decisions/0010-oauth2-provider-architecture.md).

## The comprehension check

For each user-facing explanation, answer in plain language:

1. What can Jess do now, or what problem stops happening?
2. Does this affect her application or development workflow?
3. Does she need to change anything? If so, what exactly?
4. How can she tell the update worked?

The PR opening must pass this check for its scope before technical review details
follow. **Every release-note entry and migration explanation must pass it.** A
technically correct entry that Jess cannot understand is not ready to publish.
Do not use readability scores, keyword checks, or a passing CI job as a substitute
for this semantic review. Record the review honestly; do not claim Jess herself
read it unless she did.

## Make technical detail usable

Lead with the observable outcome. Use a concrete app example when it clarifies
the change. Define necessary terminology where it first appears. Exact API names,
file paths, and commands are useful when Jess must edit or run them; connect each
to its purpose and show where it belongs. Keep constraints and limits truthful.

Illustrative rewrites (not claims about a particular release):

- Instead of "Deduplicate replay/live overlap in the subscription layer":
  "After reconnecting, your app receives each new event once. It no longer adds
  the same item twice when rebuilding a screen from events."
- Instead of "Enforce cursor bounds on projection queries":
  "Large lists can be loaded one page at a time. Use the next-page value returned
  by the query to load more results without requesting the whole list."
- Instead of "Migrate harness entrypoints and consolidate hook adapters":
  "Codex now reads the project's instructions from one shared location and can
  run the same formatting and code checks while you work."

Use examples only when they accurately describe the change being released.
Avoid implementation-only announcements with no useful user consequence.

## Migrations and prompts

For a breaking change, explain who is affected, what the old behavior was, what
replaces it, and the exact steps to update. Include real before/after examples
from verified APIs and relevant commands with expected results. If user data or
configuration needs work, explain that separately from code edits. Say when a
migration needs judgment or cannot be automated safely.

The agent prompt must work when copied alone. Include the scope, concrete
old-to-new mapping, examples, verification steps, and how to report unresolved
cases. It must not rely on "the example above", conversation history, or private
implementation specs. Jess should be able to understand what she is asking her
agent to do, even if the agent performs the edits. Do not promise automatic
migration success or invite it to delete failing tests to produce a green result.

The local authoring skill performs this review before final PR review. Release
CI only validates and assembles the reviewed material; it does not call a model
or claim to have verified comprehension. Rewrite unclear prose locally before
publication, preserving the agreed `.changes/` -> changelog/release workflow.
