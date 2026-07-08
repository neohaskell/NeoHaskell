"""Shared spec-set + contract-delta parsing for spec-check and spec-drift (Phase 5).

One definition of the delta-line grammar, block extraction, the review-record
naming convention, and the "which specs did this PR touch" enumeration — if any
of these evolve, both gates move together instead of one silently diverging
(a spec must never pass spec-check but fail spec-drift on syntax; a review
record the writer produces must never be rejected by one gate and required by
the other under different names).

Imported, never executed (doctor exemption: no ./dev verb).
"""

import re
import subprocess
from pathlib import Path

# `<+|-> <Module>: <signature line>` — the codemap/signatures vocabulary.
DELTA_LINE = re.compile(r"^([+-])\s+([A-Za-z][\w.]*):\s+(.+)$")

# The design-review kinds spec-check --plan can route (matches capability risk
# tags security-sensitive / perf-sensitive → "security" / "perf").
REVIEW_KINDS = ("security", "perf")


class GitDiffError(Exception):
    """`git diff` failed while enumerating changed specs (bad base ref, or not
    a git checkout). Callers translate this to their own exit-2 message."""


def delta_block(text):
    """Content of the ```diff signatures``` fence, or None when absent."""
    m = re.search(r"```diff signatures\n(.*?)```", text, re.DOTALL)
    return m.group(1) if m else None


def is_review_record(name):
    """True for a committed design-review record (`NNN-slug.<kind>-review.md`).
    Not a spec: both validators skip it; the review-record existence gate
    requires it. One suffix, one definition — the writer (design-review skills),
    the gate exclusion, and the existence check all agree."""
    return name.endswith("-review.md")


def review_record_name(spec_name, kind):
    """The design-review record a spec routes to:
    `067-foo.md` + `security` → `067-foo.security-review.md`."""
    stem = spec_name[:-len(".md")] if spec_name.endswith(".md") else spec_name
    return f"{stem}.{kind}-review.md"


def changed_specs(root, base):
    """`docs/changes/*.md` added or modified on this branch vs the merge `base`
    — the shared spec set for spec-drift's drift gate and spec-check's
    review-record gate. Raises GitDiffError on a bad base ref."""
    proc = subprocess.run(
        ["git", "diff", "--name-only", "--diff-filter=AM", f"{base}...HEAD",
         "--", "docs/changes/"],
        cwd=root, capture_output=True, text=True)
    if proc.returncode != 0:
        raise GitDiffError(proc.stderr.strip() or f"git diff against `{base}` failed")
    return [Path(root) / p for p in proc.stdout.splitlines() if p.endswith(".md")]
