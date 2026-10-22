import argparse
import json
from pathlib import Path
import subprocess
import sys
import re

parser = argparse.ArgumentParser()
parser.add_argument("fixture")
parser.add_argument("--match", default="")
parser.add_argument("--expect-failure", action="store_true")
args = parser.parse_args()
root = Path(__file__).resolve().parents[3]
fixture = Path(args.fixture).resolve()
command = [str(root / ".agents/skills/neohaskell-best-practices/scripts/with-toolchain"), "cabal", "repl", "lib:nhcore", "--project-file=cabal.project.dev", "-v0", "--repl-options=-ignore-dot-ghci"]
repl = f':add {json.dumps(str(fixture))}\n:main --match {json.dumps(args.match)}\n:quit\n'
result = subprocess.run(command, cwd=root, input=repl, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=900)
label = re.sub(r"[^a-z0-9]+", "-", args.match.lower()).strip("-") or "all"
log = fixture.parent / f"{fixture.stem}-{label}-{'red' if args.expect_failure else 'pass'}.log"
log.write_text(result.stdout)
print(result.stdout[-14000:])
print(f"Transcript {log}")
summaries = re.findall(r"(?m)^\s*(\d+) examples?, (\d+) failures?\s*$", result.stdout)
setup_error = re.search(r"(?m)(?:^.*\.hs:\d+:\d+: error:|^<interactive>:.*error:|^Failed,|^Could not find module)", result.stdout)
if result.returncode or setup_error or len(summaries) != 1:
    sys.exit("SETUP OR SUMMARY FAILURE")
examples, failures = map(int, summaries[0])
if not examples or bool(failures) != args.expect_failure:
    sys.exit("UNEXPECTED ASSERTION RESULT")
print("EXPECTED ASSERTION FAILURE" if args.expect_failure else "PASS")
