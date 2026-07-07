#!/usr/bin/env python3
"""Join hiedb output (Module:line:col lines on stdin) against the capability
map, so reference lists read as capabilities instead of bare modules.
Used by scripts/who-calls and scripts/where-defined."""

import collections
import re
import subprocess
import sys
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parent.parent


def module_to_capability():
    caps = yaml.safe_load((ROOT / "codemap/capabilities.yaml").read_text())["capabilities"]
    tracked = subprocess.run(["git", "ls-files", "*.hs"], capture_output=True,
                             text=True, cwd=ROOT).stdout.splitlines()

    def glob_to_re(g):
        return re.compile(g.replace(".", r"\.").replace("**", "\x00").replace("*", "[^/]*").replace("\x00", ".*"))

    globs = [(c["id"], [glob_to_re(g) for g in c["owns"]]) for c in caps]
    mapping = {}
    for f in tracked:
        mod = None
        for cid, rxs in globs:
            if any(rx.fullmatch(f) for rx in rxs):
                mod = cid
                break
        # module name from path: last path segments matching the module convention
        m = re.search(r"([A-Z][A-Za-z0-9]*(?:/[A-Z][A-Za-z0-9]*)*)\.hs$", f)
        if m:
            mapping[m.group(1).replace("/", ".")] = mod or "(unmapped)"
    return mapping


def main():
    mapping = module_to_capability()
    by_cap = collections.defaultdict(list)
    total = 0
    for line in sys.stdin:
        m = re.match(r"([A-Za-z0-9.]+):(\d+):", line.strip())
        if not m:
            continue
        total += 1
        mod = m.group(1)
        by_cap[mapping.get(mod, "(external)")].append(f"{mod}:{m.group(2)}")
    if total == 0:
        print("no references found")
        return
    parts = ", ".join(f"{cap}: {len(refs)}" for cap, refs in
                      sorted(by_cap.items(), key=lambda kv: -len(kv[1])))
    print(f"{total} refs — {parts}")
    for cap, refs in sorted(by_cap.items(), key=lambda kv: -len(kv[1])):
        print(f"\n[{cap}]")
        for r in refs[:20]:
            print(f"  {r}")
        if len(refs) > 20:
            print(f"  … {len(refs)-20} more")


if __name__ == "__main__":
    main()
