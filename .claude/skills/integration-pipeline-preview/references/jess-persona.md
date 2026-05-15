# The Jess persona

The single user every review phase grounds against. Carry this profile into every grounding pass and DevEx check.

## Contents

- [Who Jess is](#who-jess-is)
- [What Jess will and will not do](#what-jess-will-and-will-not-do)
- [The 15-minute rule](#the-15-minute-rule)
- [How to test against Jess](#how-to-test-against-jess)

## Who Jess is

- Junior developer, ~2 years professional experience.
- Background: TypeScript / JavaScript, with some Java or C# exposure.
- Builds NeoHaskell side projects after a day job, in stolen 15-30 minute pockets.
- Treats autocomplete and IntelliSense as the documentation she actually reads.

## What Jess will and will not do

| Will | Will not |
| --- | --- |
| Click "yes" on the first reasonable autocomplete suggestion. | Read a README longer than one screen. |
| Copy-paste the example from the ADR's "Public API" block. | Read a security advisory, NIST control, or RFC. |
| Trust the default. | Configure a knob she did not know existed. |
| Pipe values with `\|>` because that is the path of least friction. | Add an `INLINE` pragma, write a benchmark, or run a profiler. |
| Ship the feature. | Write tests beyond what the framework hints at. |

## The 15-minute rule

A NeoHaskell feature passes the Jess test if she can use it correctly within 15 minutes of seeing it for the first time, with nothing but autocomplete and the type signature.

If a reviewer's recommendation forces Jess to:

- Read prose longer than the API signature itself, or
- Add a pragma, annotation, or config flag, or
- Choose between two equally-correct-looking options, or
- Know vocabulary from cryptography, compilers, or distributed systems,

the recommendation has failed the Jess test and must be either deferred, rejected, or absorbed into the framework defaults.

## How to test against Jess

Run this thought experiment on every finding before recording it:

1. **The autocomplete test.** If Jess only saw the function name, types, and one short doc line from autocomplete, would she invoke the API correctly?
2. **The shrug test.** When Jess hits the finding's failure case, will the error message tell her what to do, or will she shrug and ship it unsafely?
3. **The default test.** Could this finding be solved once by the framework so no future Jess ever sees it? If yes, prefer that solution to per-feature instructions.
4. **The 15-minute test.** Does the recommendation fit inside the 15-minute pocket Jess has, including reading whatever doc the recommendation cites?

A finding that fails any of these is either (a) a framework bug masquerading as a feature bug, or (b) overkill. Either way, the grounding pass demotes it.
