#!/usr/bin/env python3
"""Type-directed + name search over the NeoHaskell API surface (Phase 4).

Searches ONLY codemap/signatures/*.txt — the dialect's own API, no GHC-lib
noise. (A local hoogle was evaluated and rejected: it leaks ghc-pkg boot
libraries into results, which is anti-dialect guidance in a repair loop.)

  ./dev api "Text -> Int"        # type search: tokens must appear in order
  ./dev api toInt                # name/substring search
"""

import re
import sys
from pathlib import Path

SIG = Path(__file__).resolve().parent / "signatures"


def entries():
    for txt in sorted(SIG.glob("*.txt")):
        module = None
        for line in txt.read_text(encoding="utf-8").splitlines():
            m = re.match(r"^module ([A-Za-z0-9.]+)", line)
            if m:
                module = m.group(1)
            elif module and "::" in line and not line.startswith("--"):
                yield module, line


def type_tokens(s: str):
    return [t for t in re.split(r"[\s()\[\],]+|->", s) if t and t != "=>"]


def main():
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    query = " ".join(sys.argv[1:])
    results = []
    if "->" in query or (query and query[0].isupper()):
        want = type_tokens(query)
        for module, line in entries():
            typ = line.split("::", 1)[1]
            toks = type_tokens(typ)
            i = 0
            for t in toks:
                if i < len(want) and t == want[i]:
                    i += 1
            if i == len(want):
                results.append((len(toks) - len(want), module, line))
    else:
        q = query.lower()
        for module, line in entries():
            name = line.split("::", 1)[0].strip()
            if q in name.lower():
                results.append((len(name), module, line))

    if not results:
        print(f"api: no matches for '{query}' in the NeoHaskell API surface")
        return 1
    for _, module, line in sorted(results)[:15]:
        print(f"{module}.{line.strip()}")
    extra = len(results) - 15
    if extra > 0:
        print(f"… {extra} more (narrow the query)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
