#!/usr/bin/env python3
from pathlib import Path
import os
import re
import subprocess
import sys
import tempfile


ROOT = Path(__file__).resolve().parents[4]
FIXTURES = ROOT / "core/test/Service/EventModel/Compile"
BUILD_DIR = os.environ.get("EVENT_MODEL_BUILDDIR", "dist-newstyle")
PROJECT_FILE = os.environ.get("EVENT_MODEL_PROJECT_FILE", "cabal.project")
FLAGS = (
    "-XGHC2021",
    "-XNoImplicitPrelude",
    "-XNoFieldSelectors",
    "-XStrict",
    "-XOverloadedStrings",
    "-XTypeApplications",
    "-XTypeFamilies",
    "-XDataKinds",
    "-XBlockArguments",
    "-XImportQualifiedPost",
    "-package",
    "nhcore",
    "-package",
    "nhtestbed",
)
CASES = (
    ("ValidModel.hs", None),
    ("ValidRuleForms.hs", None),
    ("TwoCommands.hs", r"No instance for.*Monad.*CommandRoot|Couldn't match.*CommandRoot"),
    ("InboundEvent.hs", r"EventPart.*InboundPart"),
    ("WrongCommandTarget.hs", r"Couldn't match type.*StockEntity.*CartEntity"),
    ("WrongInboundTarget.hs", r"Couldn't match type.*CartEntity.*StockEntity"),
    ("WrongOutboundPayload.hs", r"Couldn't match type.*ItemAddedPayload.*CartEvent"),
    ("WrongQuerySource.hs", r"Query source StockEntity is absent from EntitiesOf CartSummary"),
    ("WrongViewPayload.hs", r"Couldn't match type.*StockEntity.*CartEntity"),
    ("MissingCommandImplementation.hs", r"No instance for.*Command MissingCommand"),
    ("MissingQueryImplementation.hs", r"No instance for.*Query MissingView"),
    ("MissingEventVariant.hs", r"No instance for.*EventVariantOf.*MissingPayload"),
    ("UndeclaredInboundCommand.hs", r"Inbound worker emitted undeclared command ReserveStock"),
    ("TwoPrimaryKinds.hs", r"Couldn't match type.*View CartSummary.*Change CartEntity AddItem"),
    ("TwoPrimaryQueries.hs", r"Couldn't match type.*StockLevel.*CartSummary"),
    ("WrongRuleCommand.hs", r"RuleInput.*Change CartEntity AddItem.*ReserveStock"),
    ("WrongRuleViewEvent.hs", r"Query source .*StockEntity is absent from EntitiesOf CartSummary"),
    ("CommandWithViewOutcome.hs", r"Couldn't match type.*View CartSummary.*Change CartEntity AddItem"),
    ("ViewWithCommandOutcome.hs", r"Couldn't match type.*Change.*View CartSummary"),
    ("MixedRuleOutcomes.hs", r"Couldn't match type.*Rejection.*Events"),
    ("MixedZeroEventOutcomes.hs", r"Couldn't match type.*ZeroEvents.*Events"),
    ("MixedZeroRejectionOutcomes.hs", r"Couldn't match type.*Rejection.*ZeroEvents"),
    ("RuleInTranslation.hs", r"Couldn't match type.*Change.*Outbound.*ReserveStockOnItemAdded"),
)


def run(command: list[str], timeout: int) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=timeout)


def main() -> int:
    build = run(["cabal", "build", "lib:nhtestbed", f"--project-file={PROJECT_FILE}", f"--builddir={BUILD_DIR}"], 900)
    if build.returncode != 0:
        print(build.stdout + build.stderr, file=sys.stderr)
        return 1

    with tempfile.TemporaryDirectory(prefix="event-model-compile-") as temporary:
        for name, diagnostic in CASES:
            source = FIXTURES / name
            output_dir = Path(temporary) / source.stem
            output_dir.mkdir()
            command = ["cabal", "exec", f"--project-file={PROJECT_FILE}", f"--builddir={BUILD_DIR}", "--", "ghc", "-fno-code", "-fforce-recomp", "-outputdir", str(output_dir), *FLAGS, str(source)]
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
