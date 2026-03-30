#!/usr/bin/env python3
"""
NeoHaskell Feature Pipeline — Deterministic State Machine

Tracks pipeline state, enforces phase ordering and PAUSE gates,
generates agent-specific prompts with variable substitution.

Usage:
  pipeline.py init <feature> [options]   Initialize a new pipeline
  pipeline.py status                     Show pipeline status
  pipeline.py next                       Get next phase(s) as JSON
  pipeline.py complete <phase>           Mark phase complete
  pipeline.py approve <phase>            Approve a PAUSE-gated phase
  pipeline.py prompt <phase>             Get prompt for a phase (no state change)
  pipeline.py set <key> <value>          Set a pipeline variable
  pipeline.py reset                      Delete pipeline state

State is stored in .pipeline/state.json in the project root.
"""

import argparse
import fcntl
import json
import os
import re
import sys
import tempfile
from datetime import datetime, timezone

# -------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------

PIPELINE_DIR = ".pipeline"
STATE_FILE = os.path.join(PIPELINE_DIR, "state.json")
LOCK_FILE = os.path.join(PIPELINE_DIR, ".lock")

# -------------------------------------------------------------------
# Phase Definitions
# -------------------------------------------------------------------

PHASES = {
    1: {
        "name": "ADR Draft",
        "agent": "neohaskell-devex-lead",
        "skills": ["neohaskell-style-guide", "neohaskell-adr-template"],
        "category": "unspecified-high",
        "pause": True,
        "depends": [],
    },
    2: {
        "name": "Security Review (ADR)",
        "agent": "neohaskell-security-architect",
        "skills": ["neohaskell-style-guide"],
        "category": "unspecified-high",
        "pause": False,
        "depends": [1],
    },
    3: {
        "name": "Performance Review (ADR)",
        "agent": "neohaskell-performance-lead",
        "skills": ["neohaskell-style-guide"],
        "category": "unspecified-high",
        "pause": False,
        "depends": [1],
    },
    4: {
        "name": "DevEx Review",
        "agent": "neohaskell-devex-lead",
        "skills": ["neohaskell-style-guide"],
        "category": "unspecified-high",
        "pause": True,
        "depends": [2, 3],
    },
    5: {
        "name": "Architecture Design",
        "agent": "neohaskell-devex-lead",
        "skills": ["neohaskell-style-guide"],
        "category": "unspecified-high",
        "pause": True,
        "depends": [4],
    },
    6: {
        "name": "Test Spec Design",
        "agent": "neohaskell-qa-designer",
        "skills": ["neohaskell-style-guide"],
        "category": "ultrabrain",
        "pause": True,
        "depends": [5],
    },
    7: {
        "name": "Test Suite Writing",
        "agent": "neohaskell-implementer",
        "skills": ["neohaskell-style-guide"],
        "category": "unspecified-high",
        "pause": False,
        "depends": [6],
        "continue_session_from": None,
    },
    8: {
        "name": "Implementation",
        "agent": "neohaskell-implementer",
        "skills": ["neohaskell-style-guide"],
        "category": "unspecified-high",
        "pause": False,
        "depends": [7],
        "continue_session_from": 7,
    },
    9: {
        "name": "Build & Test Loop",
        "agent": "neohaskell-implementer",
        "skills": ["neohaskell-style-guide"],
        "category": "unspecified-high",
        "pause": False,
        "depends": [8],
        "continue_session_from": 8,
    },
    10: {
        "name": "Security Review (Impl)",
        "agent": "neohaskell-security-architect",
        "skills": ["neohaskell-style-guide"],
        "category": "unspecified-high",
        "pause": True,
        "depends": [9],
    },
    11: {
        "name": "Performance Review (Impl)",
        "agent": "neohaskell-performance-lead",
        "skills": ["neohaskell-style-guide"],
        "category": "unspecified-high",
        "pause": True,
        "depends": [9],
    },
    12: {
        "name": "Fix Review Notes",
        "agent": "neohaskell-implementer",
        "skills": ["neohaskell-style-guide"],
        "category": "unspecified-high",
        "pause": False,
        "depends": [10, 11],
        "continue_session_from": 9,
    },
    13: {
        "name": "Final Build & Test",
        "agent": "neohaskell-implementer",
        "skills": ["neohaskell-style-guide"],
        "category": "unspecified-high",
        "pause": False,
        "depends": [12],
        "continue_session_from": 12,
    },
    14: {
        "name": "Create PR",
        "agents": [
            {"agent": "neohaskell-community-lead", "skills": [], "category": "writing", "role": "PR description"},
            {"agent": "neohaskell-git-master", "skills": ["git-master"], "category": "git", "role": "PR submission"},
        ],
        "agent": "neohaskell-git-master",  # primary for backward compat
        "skills": ["git-master"],
        "category": "git",
        "pause": True,
        "depends": [13],
    },
    15: {
        "name": "Bot Review",
        "agent": None,
        "skills": [],
        "category": None,
        "pause": False,
        "depends": [14],
        "type": "wait",
    },
    16: {
        "name": "Fix Bot Comments",
        "agent": "neohaskell-implementer",
        "skills": ["neohaskell-style-guide"],
        "category": "unspecified-high",
        "pause": False,
        "depends": [15],
        "continue_session_from": 13,
    },
    17: {
        "name": "Final Approval & Merge",
        "agent": None,
        "skills": [],
        "category": None,
        "pause": True,
        "depends": [16],
        "type": "human",
    },
}


# -------------------------------------------------------------------
# Prompt Templates
# -------------------------------------------------------------------

PROMPTS = {
    1: """Phase 1: ADR Draft for "{feature_name}"

TASK: Create ADR-{adr_number} for {feature_name}.
EXPECTED OUTCOME: Complete ADR file at {adr_path} with Status: Proposed.

MUST DO:
- Follow the neohaskell-adr-template skill format exactly
- Include: problem statement, decision drivers, considered options, decision outcome
- Include: type definitions, module placement ({module_path}), public API signatures
- All code examples must follow NeoHaskell style (pipes, do-blocks, case, if-then-else for Bools)
- Reference issue #{issue_number}
- Design the API from Jess's perspective (15-min rule, least astonishment, least effort)

MUST NOT DO:
- Set ADR status to Accepted (stays Proposed until merge)
- Skip any ADR template sections
- Use single-letter type parameters or point-free style in examples

CONTEXT:
- Module path: {module_path}
- Test path: {test_path}
- Branch: {branch_name}""",
    2: """Phase 2: Security Review of ADR for "{feature_name}"

TASK: Review ADR-{adr_number} at {adr_path} for security implications.
EXPECTED OUTCOME: Security assessment with risk ratings using your Security Notes Template.

MUST DO:
- Read the ADR thoroughly
- Check: new types holding secrets (need Redacted?), authorization patterns (canAccess/canView),
  input validation (parse-don't-validate), error message safety, crypto needs
- Rate each finding: Critical / High / Medium / Low
- Apply the Jess Test to every mitigation
- Follow your Phase 2 instructions exactly

MUST NOT DO:
- Modify any files (read-only review)
- Suggest security configurations users must enable
- Accept deriving Show on types with secret fields

CONTEXT:
- ADR: {adr_path}
- Feature: {feature_name}
- Issue: #{issue_number}

Report whether this review is BLOCKING (Critical/High findings) or PASS.""",
    3: """Phase 3: Performance Review of ADR for "{feature_name}"

TASK: Review ADR-{adr_number} at {adr_path} for performance implications.
EXPECTED OUTCOME: Performance assessment using your Performance Notes Template.

MUST DO:
- Identify which hot paths the feature touches (Command/Event/Query/Persistence)
- Assess: serialization impact (toEncoding needed?), allocation patterns, concurrency impact
- Check proposed types for UNPACK opportunities on primitive fields
- Check for tilde (~) laziness annotations in proposed types
- Remember: Strict extension is enabled globally — all fields are strict by default
- Rate findings: Blocking / Advisory
- Target: 50,000 req/s

MUST NOT DO:
- Modify any files (read-only review)
- Advise "use strict fields" (Strict extension already does this)
- Advise "use foldl'" (nhcore's foldl IS foldl')

CONTEXT:
- ADR: {adr_path}
- Feature: {feature_name}

Report whether this review is BLOCKING or PASS.""",
    4: """Phase 4: DevEx Review for "{feature_name}"

TASK: Review ADR-{adr_number} after security and performance feedback.
EXPECTED OUTCOME: DevEx checklist with pass/fail for each criterion.

MUST DO:
- Re-read the ADR at {adr_path}
- Incorporate findings from Phase 2 (security) and Phase 3 (performance)
- Evaluate: API intuitiveness, naming conventions, pipe-friendliness,
  discoverability, consistency with existing nhcore patterns
- Apply all three design principles (Least Astonishment, Developer Happiness, Least Effort)
- Verify the API works for Jess (15-min rule)

MUST NOT DO:
- Skip any checklist item
- Ignore security/performance feedback

CONTEXT:
- ADR: {adr_path}
- Module: {module_path}
- Feature: {feature_name}""",
    5: """Phase 5: Architecture Design for "{feature_name}"

TASK: Create detailed architecture document for the implementer.
EXPECTED OUTCOME: Document with module map, complete API signatures, type definitions,
integration points, and nhcore utilities to use.

MUST DO:
- Base on the approved ADR at {adr_path}
- Specify exact file paths for all new modules
- Define ALL type signatures with full type definitions
- Show how types integrate with EventStore/Command/Query if applicable
- List specific nhcore utilities the implementer must use (Array.map, Result.mapError, etc.)
- The implementer will follow this document EXACTLY — leave NO design decisions open
- Include import conventions and module structure

MUST NOT DO:
- Write implementation code (only type signatures and API contracts)
- Leave any design decision ambiguous
- Change existing file structure without justification

CONTEXT:
- ADR: {adr_path}
- Module: {module_path}
- Test: {test_path}
- Feature: {feature_name}""",
    6: """Phase 6: Test Spec Design for "{feature_name}"

TASK: Design comprehensive test specification BEFORE implementation.
EXPECTED OUTCOME: Structured test spec document with every test case in tabular format.
Each test case must be specific enough to implement without ambiguity.

MUST DO:
- Read the architecture document from Phase 5 to understand the public API
- Read existing test files for patterns: DecimalSpec.hs, RedactedSpec.hs, OAuth2ClientSpec.hs
- Apply input analysis rubric to every function parameter
- Cover: happy paths, edge cases (empty, boundary, unicode), error conditions (every constructor),
  serialization round-trips, property-based invariants
- Minimum 3 test cases per public function, target 3:1 edge-to-happy ratio
- Include test registration instructions (cabal file, test suite, hspec-discover vs manual)
- Write the spec to a file

MUST NOT DO:
- Write Haskell test code (only the specification)
- Skip any public API function
- Use vague test descriptions

CONTEXT:
- Module: {module_path}
- Test path: {test_path}
- Feature: {feature_name}""",
    7: """Phase 7: Test Suite Writing for "{feature_name}"

TASK: Translate the test specification from Phase 6 into Haskell test code.
EXPECTED OUTCOME: Test files that compile but ALL tests fail (no implementation yet).

MUST DO:
- Implement EVERY test case from the test spec — each row = one `it` block
- Create stub type definitions with Task.throw or error for compilation
- Follow NeoHaskell test conventions: spec :: Spec Unit, pipes, do-blocks
- Register tests in nhcore.cabal (other-modules) for the appropriate test suite
- Verify tests compile with `cabal build all`
- Do NOT add tests beyond the spec, do NOT skip any

MUST NOT DO:
- Write implementation code
- Modify existing tests
- Add test cases not in the spec

CONTEXT:
- Test path: {test_path}
- Module: {module_path}
- Feature: {feature_name}""",
    8: """Phase 8: Implementation for "{feature_name}"

TASK: Implement the feature to make all tests pass.
EXPECTED OUTCOME: All source files created, following NeoHaskell conventions exactly.

MUST DO:
- Follow the architecture document from Phase 5 precisely
- All code must follow NeoHaskell style guide (pipes, do-blocks, case, if-then-else for Bools)
- Search nhcore before writing ANY utility function
- Use descriptive type parameters (forall element result., not forall a b.)
- Add INLINE pragmas on small hot-path functions
- Import nhcore modules first; only use base/hackage as last resort with Ghc prefix

MUST NOT DO:
- Modify any test files
- Use $, let..in, where, pure, return, Either, single-letter type params
- Make design decisions — follow the architecture document exactly
- Use unsafeCoerce or undefined in production code

CONTEXT:
- Module: {module_path}
- Test: {test_path}
- Feature: {feature_name}""",
    9: """Phase 9: Build & Test Loop for "{feature_name}"

TASK: Iterate until build succeeds, all tests pass, and hlint is clean.

WORKFLOW:
1. Run `cabal build all`
2. Fix compilation errors (root cause, not symptoms)
3. Run `cabal test` (appropriate suite)
4. Fix test failures in IMPLEMENTATION only (never modify tests)
5. Run `hlint` on all changed files
6. Self-review against style checklist

CONSTRAINTS:
- Maximum 10 iterations — if still failing after 10, STOP and report
- Test files are IMMUTABLE — never modify test expectations
- Fix root causes, not symptoms
- No type error suppression (as any, ts-ignore equivalents)

CONTEXT:
- Module: {module_path}
- Test: {test_path}
- Feature: {feature_name}""",
    10: """Phase 10: Security Review of Implementation for "{feature_name}"

TASK: Review the implemented code for security issues.
EXPECTED OUTCOME: Code-level findings with file:line references using your Security Implementation Notes Template.

MUST DO:
- Read all new/changed source files at {module_path} and related
- Check all 9 review criteria from your agent definition against actual code
- Reference specific file:line locations for each finding
- Verify: Redacted usage, canAccess/canView, RequestContext, parse-don't-validate,
  error message safety, constEq, Crypto.Random, parameterized SQL, unsafe usage

MUST NOT DO:
- Modify any files (read-only review)

CONTEXT:
- Source: {module_path}
- Tests: {test_path}
- Feature: {feature_name}

Report BLOCKING status (Critical/High findings block the pipeline).""",
    11: """Phase 11: Performance Review of Implementation for "{feature_name}"

TASK: Review the implemented code for performance issues.
EXPECTED OUTCOME: Code-level findings with file:line references using your Performance Implementation Notes Template.

MUST DO:
- Read all new/changed source files at {module_path} and related
- Check: INLINE pragmas on hot-path functions, UNPACK on primitive fields,
  tilde annotations, toEncoding defined, fmt in loops, unfused map chains,
  Text round-trips, ConcurrentMap contention, event update function size
- Reference specific file:line locations
- Remember: Strict is enabled (! is redundant), foldl IS foldl'

MUST NOT DO:
- Modify any files (read-only review)
- Advise adding ! to fields (redundant with Strict)

CONTEXT:
- Source: {module_path}
- Tests: {test_path}
- Feature: {feature_name}
- Target: 50,000 req/s

Report BLOCKING status.""",
    12: """Phase 12: Fix Review Notes for "{feature_name}"

TASK: Apply fixes from security (Phase 10) and performance (Phase 11) reviews.
EXPECTED OUTCOME: All Critical/High findings addressed. Code compiles and tests pass.

MUST DO:
- Address each Critical and High finding from both reviews
- Add INLINE pragmas where recommended
- Fix UNPACK annotations where recommended
- Fix any security issues identified
- Run `cabal build all && cabal test` after all fixes
- Note which files changed for potential re-review

MUST NOT DO:
- Modify test expectations
- Ignore any Critical/High findings
- Refactor while fixing — minimal changes only

CONTEXT:
- Source: {module_path}
- Tests: {test_path}
- Feature: {feature_name}""",
    13: """Phase 13: Final Build & Test for "{feature_name}"

TASK: Full verification before PR creation.

WORKFLOW:
1. Run `cabal build all` — must succeed
2. Run `cabal test` — all test suites must pass
3. Run `hlint` on all changed files — must be clean
4. Self-review all changed files against style guide
5. Report: files changed, test count, any concerns

MUST NOT DO:
- Modify test expectations
- Skip any verification step

CONTEXT:
- Feature: {feature_name}
- Module: {module_path}
- Test: {test_path}""",
    14: """Phase 14: Create PR for "{feature_name}"

This phase requires TWO agents in sequence:

STEP 1 — Community Lead (neohaskell-community-lead):
Write the PR body for {feature_name}.
- Include "Closes #{issue_number}"
- Use the PR Body Template from your agent definition
- Focus on user-facing description, not implementation details
- Include checklist (ADR, security, performance, tests, hlint)

STEP 2 — Git Master (neohaskell-git-master):
Create the PR from branch {branch_name} to main.
- Commit all changes with conventional commit format
- Push branch with -u flag
- Create PR via `gh pr create` with the body from Step 1
- Report the PR URL

After PR creation, store the PR number:
  pipeline.py set pr_number <NUMBER>
  pipeline.py set pr_url <URL>

CONTEXT:
- Branch: {branch_name}
- Issue: #{issue_number}
- ADR: {adr_number}
- Feature: {feature_name}""",
    15: """Phase 15: Bot Review for "{feature_name}"

This is a WAIT phase — no agent delegation needed.

ACTION: Check CI and bot review status:
  gh pr checks {pr_number} --watch

Wait for:
1. All CI checks to pass (green)
2. CodeRabbit review to complete

When CI and review have finished (pass or fail), mark Phase 15 complete:
  pipeline.py complete 15

If CI fails, Phase 16 will address the issues.
Phase 16 will not unlock until Phase 15 is marked complete.""",
    16: """Phase 16: Fix Bot Comments for "{feature_name}"

TASK: Address CodeRabbit comments and CI failures on PR #{pr_number}.
EXPECTED OUTCOME: All bot comments resolved, CI passes.

MUST DO:
- Read bot comments: gh api repos/neohaskell/NeoHaskell/pulls/{pr_number}/comments
- Read CI failure logs if any
- Fix each issue following NeoHaskell style
- Push fixes and wait for CI re-run
- Maximum 5 fix-and-push cycles

MUST NOT DO:
- Modify test expectations
- Dismiss bot reviews without fixing the substance
- Ignore false positives without explaining why

CONTEXT:
- PR: {pr_url}
- PR number: {pr_number}
- Feature: {feature_name}""",
    17: """Phase 17: Final Approval & Merge for \"{feature_name}\"

This is a HUMAN phase — no agent delegation.

The maintainer will:
1. Review the PR at {pr_url}
2. Approve or request changes
3. Merge when satisfied

Pipeline is complete after merge.
After the maintainer merges, mark complete and then approve:
  pipeline.py complete 17
  pipeline.py approve 17""",
}


# -------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------


def slugify(name):
    """Convert feature name to URL-friendly slug."""
    slug = name.lower().strip()
    slug = re.sub(r"[^a-z0-9\s-]", "", slug)
    slug = re.sub(r"[\s]+", "-", slug)
    slug = re.sub(r"-+", "-", slug)
    return slug.strip("-")


class SafeDict(dict):
    """Dict that returns {key} for missing keys instead of raising KeyError."""

    def __missing__(self, key):
        return "{" + key + "}"


def get_variables(state):
    """Build variable dict from state for prompt substitution."""
    variables = dict(state.get("variables", {}))
    # Compute derived variables
    variables.setdefault("slug", slugify(variables.get("feature_name", "")))
    if variables.get("adr_number") and variables.get("slug"):
        variables.setdefault(
            "adr_path",
            f"docs/decisions/{variables['adr_number']}-{variables['slug']}.md",
        )
    return variables


def get_prompt(phase_num, state):
    """Generate prompt for a phase with variable substitution."""
    template = PROMPTS.get(phase_num)
    if not template:
        return f"No prompt template for phase {phase_num}"
    variables = get_variables(state)
    return template.format_map(SafeDict(variables))


def phase_is_ready(phase_num, state):
    """Check if a phase's dependencies are all satisfied."""
    phase_def = PHASES[phase_num]
    phases_state = state["phases"]
    for dep in phase_def["depends"]:
        dep_state = phases_state[str(dep)]
        dep_def = PHASES[dep]
        if dep_state["status"] != "completed":
            return False
        # PAUSE phases must also be approved
        if dep_def["pause"] and not dep_state.get("approved_at"):
            return False
    return True


# -------------------------------------------------------------------
# State Management
# -------------------------------------------------------------------


def load_state():
    """Load pipeline state from disk (with file lock)."""
    if not os.path.exists(STATE_FILE):
        print(
            "ERROR: No pipeline initialized. Run: pipeline.py init <feature>",
            file=sys.stderr,
        )
        sys.exit(1)
    os.makedirs(PIPELINE_DIR, exist_ok=True)
    lock_fd = open(LOCK_FILE, "w")
    fcntl.flock(lock_fd, fcntl.LOCK_EX)
    try:
        with open(STATE_FILE) as f:
            state = json.load(f)
        state["_lock_fd"] = lock_fd
        return state
    except Exception:
        fcntl.flock(lock_fd, fcntl.LOCK_UN)
        lock_fd.close()
        raise


def save_state(state):
    """Save pipeline state to disk atomically (releases lock)."""
    lock_fd = state.pop("_lock_fd", None)
    os.makedirs(PIPELINE_DIR, exist_ok=True)
    state["updated_at"] = datetime.now(timezone.utc).isoformat()
    # Atomic write: temp file + rename
    fd, tmp_path = tempfile.mkstemp(dir=PIPELINE_DIR, suffix=".json")
    try:
        with os.fdopen(fd, "w") as f:
            json.dump(state, f, indent=2)
            f.write("\n")
        os.replace(tmp_path, STATE_FILE)
    except Exception:
        os.unlink(tmp_path)
        raise
    finally:
        if lock_fd:
            fcntl.flock(lock_fd, fcntl.LOCK_UN)
            lock_fd.close()


def release_lock(state):
    """Release lock without saving (for read-only operations)."""
    lock_fd = state.pop("_lock_fd", None)
    if lock_fd:
        fcntl.flock(lock_fd, fcntl.LOCK_UN)
        lock_fd.close()


# -------------------------------------------------------------------
# Commands
# -------------------------------------------------------------------


def cmd_init(args):
    """Initialize a new pipeline."""
    if os.path.exists(STATE_FILE):
        print(
            "ERROR: Pipeline already initialized. Run 'pipeline.py reset' first.",
            file=sys.stderr,
        )
        sys.exit(1)

    feature_name = args.feature
    slug = slugify(feature_name)

    state = {
        "version": 1,
        "variables": {
            "feature_name": feature_name,
            "issue_number": args.issue or "",
            "branch_name": args.branch or f"feature/{slug}",
            "module_path": args.module or "",
            "test_path": args.test or "",
            "adr_number": args.adr or "",
            "slug": slug,
            "pr_number": "",
            "pr_url": "",
        },
        "created_at": datetime.now(timezone.utc).isoformat(),
        "updated_at": datetime.now(timezone.utc).isoformat(),
        "phases": {},
    }

    # Compute derived variables
    if state["variables"]["adr_number"]:
        state["variables"]["adr_path"] = (
            f"docs/decisions/{state['variables']['adr_number']}-{slug}.md"
        )

    # Initialize all phases as pending
    for num in PHASES:
        state["phases"][str(num)] = {
            "status": "pending",
            "started_at": None,
            "completed_at": None,
            "approved_at": None,
            "session_id": None,
        }

    save_state(state)

    # Write .gitignore for pipeline state
    gitignore_path = os.path.join(PIPELINE_DIR, ".gitignore")
    if not os.path.exists(gitignore_path):
        with open(gitignore_path, "w") as f:
            f.write("*\n!.gitignore\n")

    print(f"Pipeline initialized for: {feature_name}")
    print(f"  Issue: #{args.issue or 'not set'}")
    print(f"  Branch: {state['variables']['branch_name']}")
    print(f"  Module: {args.module or 'not set'}")
    print(f"  Test: {args.test or 'not set'}")
    print(f"  ADR: {args.adr or 'not set'}")
    print(f"\nState file: {STATE_FILE}")
    print("Run 'pipeline.py next' to get the first phase.")


def cmd_status(args):
    """Show current pipeline status."""
    state = load_state()
    variables = state["variables"]

    print(f"\nNeoHaskell Feature Pipeline: {variables['feature_name']}")
    parts = []
    if variables.get("issue_number"):
        parts.append(f"Issue: #{variables['issue_number']}")
    if variables.get("branch_name"):
        parts.append(f"Branch: {variables['branch_name']}")
    if variables.get("adr_number"):
        parts.append(f"ADR: {variables['adr_number']}")
    if parts:
        print(f"  {' | '.join(parts)}")
    print()

    # Header
    print(f"{'Phase':>5}  {'Name':<30}  {'Status':<14}  {'Agent'}")
    print(f"{'─' * 5}  {'─' * 30}  {'─' * 14}  {'─' * 30}")

    for num in sorted(PHASES.keys()):
        phase_def = PHASES[num]
        phase_state = state["phases"][str(num)]
        status = phase_state["status"]

        # Determine display status
        if status == "completed" and phase_def["pause"]:
            if phase_state.get("approved_at"):
                display = "✓ approved"
            else:
                display = "⏸ needs approval"
        elif status == "completed":
            display = "✓ done"
        elif status == "in_progress":
            display = "→ in progress"
        elif status == "pending" and phase_is_ready(num, state):
            display = "◆ ready"
        else:
            display = "· pending"

        agent = phase_def.get("agent") or phase_def.get("type", "—")
        print(f"{num:>5}  {phase_def['name']:<30}  {display:<14}  {agent}")

    # Summary
    completed = sum(1 for p in state["phases"].values() if p["status"] == "completed")
    total = len(PHASES)
    awaiting = sum(
        1
        for num, p in state["phases"].items()
        if p["status"] == "completed"
        and PHASES[int(num)]["pause"]
        and not p.get("approved_at")
    )
    print(f"\n  Progress: {completed}/{total} phases complete", end="")
    if awaiting:
        print(f" ({awaiting} awaiting approval)")
    else:
        print()
    release_lock(state)


def cmd_next(args):
    """Get next actionable phase(s) as JSON."""
    state = load_state()

    # Check for phases awaiting approval
    awaiting_approval = []
    for num_str, phase_state in state["phases"].items():
        num = int(num_str)
        if (
            phase_state["status"] == "completed"
            and PHASES[num]["pause"]
            and not phase_state.get("approved_at")
        ):
            awaiting_approval.append(num)

    if awaiting_approval:
        result = {
            "status": "waiting_for_approval",
            "phases_needing_approval": awaiting_approval,
            "message": f"Phase(s) {', '.join(str(n) for n in awaiting_approval)} completed but need approval. Run: pipeline.py approve <phase>",
        }
        print(json.dumps(result, indent=2))
        return

    # Find ready phases
    ready = []
    for num in sorted(PHASES.keys()):
        phase_state = state["phases"][str(num)]
        if phase_state["status"] == "pending" and phase_is_ready(num, state):
            ready.append(num)

    if not ready:
        # Check if all done
        all_done = all(
            p["status"] == "completed"
            and (not PHASES[int(n)]["pause"] or p.get("approved_at"))
            for n, p in state["phases"].items()
        )
        if all_done:
            print(
                json.dumps(
                    {
                        "status": "complete",
                        "message": "Pipeline complete. All 17 phases done.",
                    }
                )
            )
        else:
            # Some phases blocked
            in_progress = [
                int(n)
                for n, p in state["phases"].items()
                if p["status"] == "in_progress"
            ]
            print(
                json.dumps(
                    {
                        "status": "blocked",
                        "in_progress": in_progress,
                        "message": "No ready phases. Some phases may be in progress.",
                    }
                )
            )
        return

    # Build output for each ready phase
    phases_output = []
    for num in ready:
        phase_def = PHASES[num]
        phase_info = {
            "phase": num,
            "name": phase_def["name"],
            "agent": phase_def.get("agent"),
            "skills": phase_def.get("skills", []),
            "category": phase_def.get("category"),
            "pause_after": phase_def.get("pause", False),
            "type": phase_def.get("type", "agent"),
            "prompt": get_prompt(num, state),
        }

        # Multi-agent phases: include ordered agent list
        if "agents" in phase_def:
            phase_info["agents"] = phase_def["agents"]

        # Add session continuation info for implementer phases
        continue_from = phase_def.get("continue_session_from")
        if continue_from:
            prev_session = state["phases"].get(str(continue_from), {}).get("session_id")
            if prev_session:
                phase_info["continue_session_id"] = prev_session

        phases_output.append(phase_info)

    result = {
        "status": "ready",
        "phases": phases_output,
    }
    print(json.dumps(result, indent=2))
    release_lock(state)


def cmd_complete(args):
    """Mark a phase as complete."""
    state = load_state()
    phase_num = args.phase
    num_str = str(phase_num)

    if phase_num not in PHASES:
        print(f"ERROR: Phase {phase_num} does not exist (valid: 1-17)", file=sys.stderr)
        sys.exit(1)

    phase_state = state["phases"][num_str]

    if phase_state["status"] == "completed":
        print(f"Phase {phase_num} is already completed.")
        release_lock(state)
        return

    # Validate dependencies
    if not phase_is_ready(phase_num, state):
        deps = PHASES[phase_num]["depends"]
        missing = []
        for dep in deps:
            dep_state = state["phases"][str(dep)]
            dep_def = PHASES[dep]
            if dep_state["status"] != "completed":
                missing.append(f"Phase {dep} ({dep_def['name']}): not completed")
            elif dep_def["pause"] and not dep_state.get("approved_at"):
                missing.append(f"Phase {dep} ({dep_def['name']}): not approved")
        print(f"ERROR: Phase {phase_num} dependencies not met:", file=sys.stderr)
        for m in missing:
            print(f"  - {m}", file=sys.stderr)
        sys.exit(1)

    now = datetime.now(timezone.utc).isoformat()
    phase_state["status"] = "completed"
    phase_state["completed_at"] = now
    save_state(state)

    phase_def = PHASES[phase_num]
    print(f"Phase {phase_num} ({phase_def['name']}) marked as completed.")

    if phase_def["pause"]:
        print(f"  ⏸ This phase requires approval before the pipeline continues.")
        print(f"  Run: pipeline.py approve {phase_num}")


def cmd_approve(args):
    """Approve a PAUSE-gated phase."""
    state = load_state()
    phase_num = args.phase
    num_str = str(phase_num)

    if phase_num not in PHASES:
        print(f"ERROR: Phase {phase_num} does not exist (valid: 1-17)", file=sys.stderr)
        sys.exit(1)

    phase_def = PHASES[phase_num]
    if not phase_def["pause"]:
        print(
            f"ERROR: Phase {phase_num} ({phase_def['name']}) is not a PAUSE phase.",
            file=sys.stderr,
        )
        sys.exit(1)

    phase_state = state["phases"][num_str]
    if phase_state["status"] != "completed":
        print(
            f"ERROR: Phase {phase_num} must be completed before it can be approved.",
            file=sys.stderr,
        )
        sys.exit(1)

    if phase_state.get("approved_at"):
        print(f"Phase {phase_num} is already approved.")
        release_lock(state)
        return

    phase_state["approved_at"] = datetime.now(timezone.utc).isoformat()
    save_state(state)
    print(f"Phase {phase_num} ({phase_def['name']}) approved.")
    print("Run 'pipeline.py next' to continue.")


def cmd_prompt(args):
    """Get the prompt for a specific phase (no state change)."""
    state = load_state()
    phase_num = args.phase

    if phase_num not in PHASES:
        print(f"ERROR: Phase {phase_num} does not exist (valid: 1-17)", file=sys.stderr)
        sys.exit(1)

    phase_def = PHASES[phase_num]
    prompt = get_prompt(phase_num, state)

    result = {
        "phase": phase_num,
        "name": phase_def["name"],
        "agent": phase_def.get("agent"),
        "skills": phase_def.get("skills", []),
        "category": phase_def.get("category"),
        "prompt": prompt,
    }
    print(json.dumps(result, indent=2))
    release_lock(state)


def cmd_set(args):
    """Set a pipeline variable."""
    state = load_state()
    key = args.key
    value = args.value

    # Handle dotted keys for session IDs: session_id.7 = "ses_xxx"
    if key.startswith("session_id."):
        phase_num = key.split(".", 1)[1]
        if phase_num in state["phases"]:
            state["phases"][phase_num]["session_id"] = value
            save_state(state)
            print(f"Set session_id for phase {phase_num} = {value}")
            return
        else:
            print(f"ERROR: Phase {phase_num} does not exist.", file=sys.stderr)
            sys.exit(1)

    # Regular variable
    state["variables"][key] = value
    save_state(state)
    print(f"Set {key} = {value}")


def cmd_reset(args):
    """Delete pipeline state."""
    if os.path.exists(STATE_FILE):
        os.remove(STATE_FILE)
        print("Pipeline state deleted.")
        # Clean up directory if empty (except .gitignore)
        remaining = [f for f in os.listdir(PIPELINE_DIR) if f != ".gitignore"]
        if not remaining:
            pass  # Keep directory with .gitignore
    else:
        print("No pipeline state to delete.")


# -------------------------------------------------------------------
# Main
# -------------------------------------------------------------------


def main():
    parser = argparse.ArgumentParser(
        description="NeoHaskell Feature Pipeline — Deterministic State Machine",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    subparsers = parser.add_subparsers(dest="command", help="Available commands")

    # init
    init_parser = subparsers.add_parser("init", help="Initialize a new pipeline")
    init_parser.add_argument("feature", help="Feature name (e.g., 'Decimal Type')")
    init_parser.add_argument("--issue", help="GitHub issue number")
    init_parser.add_argument(
        "--module", help="Main module path (e.g., core/decimal/Decimal.hs)"
    )
    init_parser.add_argument(
        "--test", help="Test file path (e.g., core/test/DecimalSpec.hs)"
    )
    init_parser.add_argument(
        "--branch", help="Git branch name (default: feature/<slug>)"
    )
    init_parser.add_argument("--adr", help="ADR number (e.g., 0041)")

    # status
    subparsers.add_parser("status", help="Show pipeline status")

    # next
    subparsers.add_parser("next", help="Get next phase(s) as JSON")

    # complete
    complete_parser = subparsers.add_parser("complete", help="Mark a phase as complete")
    complete_parser.add_argument("phase", type=int, help="Phase number (1-17)")

    # approve
    approve_parser = subparsers.add_parser(
        "approve", help="Approve a PAUSE-gated phase"
    )
    approve_parser.add_argument("phase", type=int, help="Phase number")

    # prompt
    prompt_parser = subparsers.add_parser("prompt", help="Get prompt for a phase")
    prompt_parser.add_argument("phase", type=int, help="Phase number (1-17)")

    # set
    set_parser = subparsers.add_parser("set", help="Set a pipeline variable")
    set_parser.add_argument("key", help="Variable name (e.g., pr_number, session_id.7)")
    set_parser.add_argument("value", help="Variable value")

    # reset
    subparsers.add_parser("reset", help="Delete pipeline state (destructive)")

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        sys.exit(1)

    commands = {
        "init": cmd_init,
        "status": cmd_status,
        "next": cmd_next,
        "complete": cmd_complete,
        "approve": cmd_approve,
        "prompt": cmd_prompt,
        "set": cmd_set,
        "reset": cmd_reset,
    }

    commands[args.command](args)


if __name__ == "__main__":
    main()
