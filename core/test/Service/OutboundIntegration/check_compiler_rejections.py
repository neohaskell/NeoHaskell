#!/usr/bin/env python3
from pathlib import Path
import re
import subprocess
import sys
import tempfile


ROOT = Path(__file__).resolve().parents[4]
FIXTURES = ROOT / "core/test/Service/OutboundIntegration/Compile"
FLAGS = (
    "-XTemplateHaskell",
    "-XTypeFamilies",
    "-XImportQualifiedPost",
    "-XNoImplicitPrelude",
    "-XStrict",
    "-XDerivingStrategies",
    "-XDeriveGeneric",
    "-XDataKinds",
    "-package",
    "nhcore",
)
CASES = (
    ("ValidPayload.hs", None),
    ("WrongPayload.hs", r"No EventVariantOf TargetEvent .*Unmapped mapping exists"),
    ("FullEventNeedsLegacy.hs", r"Use outboundIntegrationFullEvent explicitly during migration"),
    ("WrongEntity.hs", r"handleEvent' must take Target as its first argument"),
)


def run(command: list[str], timeout: int) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=timeout)


def main() -> int:
    build = run(["cabal", "build", "lib:nhcore"], 900)
    if build.returncode != 0:
        print(build.stdout + build.stderr, file=sys.stderr)
        return 1

    with tempfile.TemporaryDirectory(prefix="outbound-payload-compile-") as temporary:
        for name, diagnostic in CASES:
            source = FIXTURES / name
            output_dir = Path(temporary) / source.stem
            output_dir.mkdir()
            command = ["cabal", "exec", "--", "ghc", "-fno-code", "-fforce-recomp", "-outputdir", str(output_dir), *FLAGS, str(source)]
            result = run(command, 180)
            output = result.stdout + result.stderr
            rejected = result.returncode == 1 and str(source) in output and "error:" in output
            matched = diagnostic is not None and re.search(diagnostic, " ".join(output.split())) is not None
            if (diagnostic is None and result.returncode != 0) or (diagnostic is not None and not (rejected and matched)):
                print(f"FAIL {name}: exit {result.returncode}", file=sys.stderr)
                print(output, file=sys.stderr)
                return 1
            print(f"PASS {name}: {'accepted' if diagnostic is None else 'rejected with expected diagnostic'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
