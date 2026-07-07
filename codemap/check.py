#!/usr/bin/env python3
"""Codemap validity checker (pipeline Phase 3) — the anti-rot gate.

Enforces (CI: checks.yml `codemap` job; local: ./dev codemap-check):
 1. every exposed module of nhcore/nhintegrations/nhtestbed is owned by
    EXACTLY one capability's owns-globs (orphans and double-owners fail —
    adding a module without a codemap entry fails the PR)
 2. every owns-glob matches at least one real file (no ghost entries)
 3. aliases are globally unique (ambiguous routing fails)
 4. extension-point create/register/tests paths reference real directories
    (prefix-existence, since they contain <placeholders>) and skills exist
 5. doc-ratchet: undocumented counts may never increase
 6. generated artifacts exist (signatures/, MAP.md)

Incomplete is allowed; wrong is not.
"""

import fnmatch
import re
import subprocess
import sys
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parent.parent
fails = 0


def err(msg: str):
    global fails
    print(f"codemap check: FAIL — {msg}")
    fails += 1


def exposed_modules():
    """(module_name, source_file) for every exposed module, from cabal files."""
    out = []
    for cabal, src_dirs in [
        (ROOT / "core/nhcore.cabal", None),
        (ROOT / "integrations/nhintegrations.cabal", None),
        (ROOT / "testbed/nhtestbed.cabal", None),
    ]:
        text = cabal.read_text()
        lib = text.split("library", 1)[1] if "library" in text else text
        m = re.search(r"exposed-modules:\s*\n((?:\s+[A-Z][A-Za-z0-9. ]*\n)+)", lib)
        if not m:
            continue
        mods = [x.strip() for x in m.group(1).split() if x.strip()]
        # resolve each module to its file via git ls-files
        base = cabal.parent
        files = subprocess.run(["git", "ls-files", str(base.relative_to(ROOT))],
                               capture_output=True, text=True, cwd=ROOT).stdout.splitlines()
        for mod in mods:
            rel = mod.replace(".", "/") + ".hs"
            hits = [f for f in files if f.endswith("/" + rel) or f.endswith(rel)]
            if hits:
                out.append((mod, hits[0]))
    return out


def owned_by(path: str, caps) -> list:
    owners = []
    for c in caps:
        for g in c["owns"]:
            # ** matches across dirs; fnmatch's * doesn't cross "/", so expand
            if fnmatch.fnmatch(path, g) or fnmatch.fnmatch(path, g.replace("**/", "**")) \
               or re.fullmatch(glob_to_re(g), path):
                owners.append(c["id"])
                break
    return owners


def glob_to_re(g: str) -> str:
    out = ""
    i = 0
    while i < len(g):
        if g[i:i+2] == "**":
            out += ".*"
            i += 2
        elif g[i] == "*":
            out += "[^/]*"
            i += 1
        elif g[i] in ".+()[]{}^$?":
            out += "\\" + g[i]
            i += 1
        else:
            out += g[i]
            i += 1
    return out


def main() -> int:
    caps = yaml.safe_load((ROOT / "codemap/capabilities.yaml").read_text())["capabilities"]
    ext = yaml.safe_load((ROOT / "codemap/extension-points.yaml").read_text())["extension-points"]
    tracked = subprocess.run(["git", "ls-files"], capture_output=True, text=True, cwd=ROOT).stdout.splitlines()

    # 1. ownership: every exposed module owned exactly once
    for mod, path in exposed_modules():
        owners = owned_by(path, caps)
        if not owners:
            err(f"exposed module {mod} ({path}) is owned by NO capability — add it to capabilities.yaml")
        elif len(owners) > 1:
            err(f"exposed module {mod} ({path}) owned by multiple capabilities: {owners}")

    # 2. no ghost globs
    for c in caps:
        for g in c["owns"]:
            rx = re.compile(glob_to_re(g))
            if not any(rx.fullmatch(f) for f in tracked):
                err(f"capability '{c['id']}' glob '{g}' matches no tracked file (ghost entry)")

    # 3. alias uniqueness
    seen = {}
    for c in caps:
        for a in c.get("aliases", []):
            if a in seen:
                err(f"alias '{a}' is ambiguous: {seen[a]} vs {c['id']}")
            seen[a] = c["id"]

    # 4. extension points: path prefixes + skills exist
    for e in ext:
        for field in ("create", "register", "tests"):
            for p in e.get(field, []):
                # verify only path-looking tokens (contain "/"); prose is skipped
                for token in p.split():
                    if "/" not in token or token == "/":
                        continue
                    prefix = re.split(r"[<]", token)[0].rstrip("*").rstrip("/")
                    if prefix and not (ROOT / prefix).exists() \
                       and not any(f.startswith(prefix) for f in tracked):
                        err(f"extension-point '{e['kind']}' {field} path '{prefix}' does not exist")
        skill = e.get("skill")
        if skill and not (ROOT / f".claude/skills/{skill}/SKILL.md").exists():
            err(f"extension-point '{e['kind']}' references missing skill '{skill}'")

    # 5. doc-ratchet: counters may only decrease
    ratchet_file = ROOT / "codemap/.doc-ratchet"
    if ratchet_file.exists():
        committed = subprocess.run(["git", "show", "HEAD:codemap/.doc-ratchet"],
                                   capture_output=True, text=True, cwd=ROOT).stdout
        for line in ratchet_file.read_text().splitlines():
            m = re.match(r"(undocumented_\w+): (\d+)", line)
            if not m:
                continue
            key, now = m.group(1), int(m.group(2))
            prev = re.search(rf"{key}: (\d+)", committed or "")
            if prev and now > int(prev.group(1)):
                err(f"doc-ratchet regression: {key} rose {prev.group(1)} → {now} (document what you touched)")

    # 6. generated artifacts present
    for p in ("codemap/MAP.md", "codemap/signatures"):
        if not (ROOT / p).exists():
            err(f"generated artifact missing: {p} (run ./dev codemap)")

    if fails == 0:
        n_mods = len(exposed_modules())
        print(f"codemap check: OK — {n_mods} exposed modules all owned exactly once, "
              f"{len(caps)} capabilities, {len(seen)} unique aliases, {len(ext)} extension points")
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
