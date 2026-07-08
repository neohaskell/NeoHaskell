"""Shared contract-delta parsing for spec-check and spec-drift (Phase 5).

One definition of the delta-line grammar and block extraction — if the spec
format evolves, both gates move together instead of one silently diverging
(a spec must never pass spec-check but fail spec-drift on syntax).

Imported, never executed (doctor exemption: no ./dev verb).
"""

import re

# `<+|-> <Module>: <signature line>` — the codemap/signatures vocabulary.
DELTA_LINE = re.compile(r"^([+-])\s+([A-Za-z][\w.]*):\s+(.+)$")


def delta_block(text):
    """Content of the ```diff signatures``` fence, or None when absent."""
    m = re.search(r"```diff signatures\n(.*?)```", text, re.DOTALL)
    return m.group(1) if m else None
