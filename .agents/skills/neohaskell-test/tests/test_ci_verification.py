from __future__ import annotations

import fnmatch
import os
import re
import subprocess
import sys
import tempfile
import unittest
from dataclasses import dataclass
from pathlib import Path

sys.dont_write_bytecode = True
REPOSITORY = Path(__file__).resolve().parents[4]
WORKFLOWS = REPOSITORY / ".github/workflows"

DOCTEST_DEPENDENCIES = (
    ".agents/skills/neohaskell-test/scripts/run-doctest",
    ".agents/skills/neohaskell-test/scripts/test_runtime.py",
    ".agents/skills/neohaskell-best-practices/scripts/bounded_process.py",
    ".agents/skills/neohaskell-best-practices/scripts/repo-root",
    ".agents/skills/neohaskell-best-practices/scripts/toolchain-fp",
    ".agents/skills/neohaskell-best-practices/scripts/with-toolchain",
)
CI_GATES = {
    "test.yml": ("ci-gate", ("build", "doctest", "test-core", "test-auth", "test-integration", "test-service", "test-hurl")),
    "neo-ci.yml": ("neo-ci-gate", ("rust", "ide", "nix-package", "consumer-contract")),
    "installer-ci.yml": ("installer-ci-gate", ("lint", "test", "build")),
}
EXPENSIVE_JOB_SELECTORS = {
    "test.yml": {"build": "compiled", "doctest": "compiled"},
    "neo-ci.yml": {"rust": "touched", "ide": "touched", "nix-package": "touched", "consumer-contract": "contract"},
    "installer-ci.yml": {"lint": "touched", "test": "touched", "build": "touched"},
}


@dataclass(frozen=True)
class AdmissionCase:
    name: str
    action: str
    draft: bool
    label_event: bool
    label_present: bool
    changed_paths: tuple[str, ...]
    runs_expensive: bool
    selects_component: bool


ADMISSION_CASES = (
    AdmissionCase("ready PR", "opened", False, False, False, ("core/core/Task.hs",), True, True),
    AdmissionCase("unlabelled draft", "opened", True, False, False, ("core/core/Task.hs",), False, True),
    AdmissionCase("labelled draft", "labeled", True, True, True, ("core/core/Task.hs",), True, True),
    AdmissionCase("held label on draft update", "synchronize", True, False, True, ("core/core/Task.hs",), True, True),
    AdmissionCase("unrelated label on ready PR", "labeled", False, False, False, ("core/core/Task.hs",), True, True),
    AdmissionCase("unrelated label while request label remains", "labeled", True, False, True, ("core/core/Task.hs",), True, True),
    AdmissionCase("unrelated label on ordinary draft", "labeled", True, False, False, ("core/core/Task.hs",), False, True),
    AdmissionCase("unrelated source on ready PR", "opened", False, False, False, ("docs/guide.md",), True, False),
    AdmissionCase("labelled draft with unrelated source", "labeled", True, True, True, ("docs/guide.md",), True, False),
)


def workflow(name: str) -> str:
    return (WORKFLOWS / name).read_text(encoding="utf-8")


def job_block(text: str, name: str) -> str:
    lines = text.splitlines()
    start = lines.index(f"  {name}:") + 1
    selected = []
    for line in lines[start:]:
        if line.startswith("  ") and not line.startswith("    "):
            break
        selected.append(line)
    return "\n".join(selected)


def shell_script(text: str, job: str) -> str:
    lines = job_block(text, job).splitlines()
    run_index = next(index for index, line in enumerate(lines) if line.strip() == "run: |" and len(line) - len(line.lstrip()) == 8)
    script = []
    for line in lines[run_index + 1 :]:
        if line.strip() and len(line) - len(line.lstrip()) < 10:
            break
        script.append(line[10:] if line.strip() else "")
    return "\n".join(script)


def temp_root() -> Path:
    path = REPOSITORY / ".agents-state/neohaskell-test-ci"
    path.mkdir(parents=True, exist_ok=True)
    return path


def run_change_detection(name: str, *, event: str = "pull_request", action: str, draft: bool, label_event: bool, label_present: bool, changed_paths: tuple[str, ...]) -> tuple[subprocess.CompletedProcess[str], dict[str, str]]:
    script = shell_script(workflow(name), "changes")
    with tempfile.TemporaryDirectory(dir=temp_root()) as directory:
        temporary = Path(directory)
        bin_dir = temporary / "bin"
        bin_dir.mkdir()
        fake_git = bin_dir / "git"
        fake_git.write_text("#!/bin/sh\n[ \"$1\" = diff ] || exit 2\nprintf '%s\\n' \"$TEST_CHANGED_PATHS\"\n", encoding="utf-8")
        fake_git.chmod(0o755)
        output = temporary / "github-output"
        environment = os.environ.copy()
        environment.update(
            {
                "ACTION": action,
                "BASE_REF": "main",
                "EVENT": event,
                "FULL_CI_LABEL_EVENT": str(label_event).lower(),
                "FULL_CI_LABEL_PRESENT": str(label_present).lower(),
                "GITHUB_OUTPUT": str(output),
                "IS_DRAFT": str(draft).lower(),
                "PATH": f"{bin_dir}:{environment.get('PATH', '')}",
                "TEST_CHANGED_PATHS": "\n".join(changed_paths),
            }
        )
        result = subprocess.run(["bash", "-c", script], cwd=REPOSITORY, env=environment, capture_output=True, text=True, timeout=10)
        values = {}
        if output.exists():
            for line in output.read_text(encoding="utf-8").splitlines():
                key, value = line.split("=", 1)
                values[key] = value
        return result, values


def job_runs(name: str, job: str, outputs: dict[str, str]) -> bool:
    lines = job_block(workflow(name), job).splitlines()
    start = next(index for index, line in enumerate(lines) if line.strip() == "if: >-")
    guard_lines = []
    for line in lines[start + 1 :]:
        if line.strip() and len(line) - len(line.lstrip()) < 6:
            break
        if line.strip():
            guard_lines.append(line.strip())
    guard = " ".join(guard_lines)
    predicate = re.compile(r"needs\.changes\.outputs\.([A-Za-z0-9_-]+)\s*==\s*'true'")
    evaluated = predicate.sub(lambda match: str(outputs.get(match.group(1)) == "true").lower(), guard)
    if not re.fullmatch(r"(?:true|false)(?:\s*&&\s*(?:true|false))*", evaluated):
        raise AssertionError(f"unsupported {name} job condition for {job}: {guard}")
    return all(value.strip() == "true" for value in evaluated.split("&&"))


def run_gate(name: str, *, run_expensive: bool, selected: dict[str, str], changes_result: str, job_results: dict[str, str]) -> subprocess.CompletedProcess[str]:
    gate, _ = CI_GATES[name]
    script = shell_script(workflow(name), gate)
    result_pattern = re.compile(r"\$\{\{\s*needs\.([A-Za-z0-9_-]+)\.result\s*\}\}")
    script = result_pattern.sub(lambda match: job_results.get(match.group(1), "success"), script)
    if "${{" in script:
        raise AssertionError(f"unresolved workflow expression in {name} gate")
    environment = os.environ.copy()
    environment.update(selected)
    environment["CHANGES_RESULT"] = changes_result
    environment["RUN_EXPENSIVE"] = str(run_expensive).lower()
    return subprocess.run(["bash", "-c", script], cwd=REPOSITORY, env=environment, capture_output=True, text=True, timeout=10)


def selected_values(name: str, selected: bool) -> dict[str, str]:
    if name == "test.yml":
        return {"COMPILED": str(selected).lower()}
    if name == "neo-ci.yml":
        return {"TOUCHED": str(selected).lower(), "CONTRACT": str(selected).lower()}
    return {"TOUCHED": str(selected).lower()}


class CiVerificationTests(unittest.TestCase):
    def test_pr_selector_matches_real_doctest_dependencies_and_rejects_controls(self) -> None:
        text = workflow("test.yml")
        pattern = re.search(r"PATTERN='([^']+)'", job_block(text, "changes"))[1]
        for path in (*DOCTEST_DEPENDENCIES, "core/core/Task.hs"):
            with self.subTest(path=path):
                self.assertIsNotNone(re.search(pattern, path))
        for path in (
            ".agents/skills/neohaskell-test/scripts/run-testbed",
            ".agents/skills/unrelated/scripts/helper.py",
            "docs/guide.md",
        ):
            with self.subTest(path=path):
                self.assertIsNone(re.search(pattern, path))

    def test_push_path_filter_selects_real_doctest_dependencies_and_rejects_controls(self) -> None:
        text = workflow("test.yml")
        section = re.search(r"(?m)^    paths:\n((?:      - [^\n]*\n?)+)", text)
        self.assertIsNotNone(section)
        patterns = [line.strip()[2:].strip("\"'") for line in section[1].splitlines()]
        for path in (*DOCTEST_DEPENDENCIES, "core/core/Task.hs"):
            with self.subTest(path=path):
                self.assertTrue(any(fnmatch.fnmatchcase(path, pattern) for pattern in patterns))
        for path in (".agents/skills/unrelated/scripts/helper.py", "docs/guide.md"):
            with self.subTest(path=path):
                self.assertFalse(any(fnmatch.fnmatchcase(path, pattern) for pattern in patterns))

    def test_pull_request_events_include_the_optional_label_request(self) -> None:
        for name in CI_GATES:
            text = workflow(name)
            event = re.search(r"(?ms)^  pull_request:\n(.*?)(?=^  push:|^permissions:)", text)[1]
            types = re.search(r"(?m)^\s+types:\s*\[(.*?)\]", event)[1]
            with self.subTest(workflow=name):
                self.assertIn("labeled", {part.strip() for part in types.split(",")})

    def test_actual_change_scripts_resolve_admission_matrix(self) -> None:
        selected_paths = {
            "test.yml": ("core/core/Task.hs", "compiled"),
            "neo-ci.yml": ("neo/src/main.rs", "touched"),
            "installer-ci.yml": ("installer/src/main.rs", "touched"),
        }
        for name, (selected_path, selector) in selected_paths.items():
            for case in ADMISSION_CASES:
                paths = (selected_path,) if case.selects_component else case.changed_paths
                with self.subTest(workflow=name, case=case.name):
                    result, outputs = run_change_detection(
                        name,
                        action=case.action,
                        draft=case.draft,
                        label_event=case.label_event,
                        label_present=case.label_present,
                        changed_paths=paths,
                    )
                    self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                    self.assertEqual(outputs.get("run_expensive"), str(case.runs_expensive).lower())
                    self.assertEqual(outputs.get(selector), str(case.selects_component).lower())
                    for job, component_selector in EXPENSIVE_JOB_SELECTORS[name].items():
                        selected = outputs.get(component_selector, "false") == "true"
                        self.assertEqual(job_runs(name, job, outputs), case.runs_expensive and selected, job)

    def test_neo_contract_only_change_runs_consumer_without_component_jobs(self) -> None:
        result, outputs = run_change_detection(
            "neo-ci.yml",
            action="synchronize",
            draft=False,
            label_event=False,
            label_present=False,
            changed_paths=("core/core/Task.hs",),
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(outputs.get("run_expensive"), "true")
        self.assertEqual(outputs.get("touched"), "false")
        self.assertEqual(outputs.get("contract"), "true")
        self.assertTrue(job_runs("neo-ci.yml", "consumer-contract", outputs))
        for job in ("rust", "ide", "nix-package"):
            self.assertFalse(job_runs("neo-ci.yml", job, outputs))

    def test_push_event_keeps_existing_full_run_behavior(self) -> None:
        for name, selector in (("test.yml", "compiled"), ("neo-ci.yml", "touched"), ("installer-ci.yml", "touched")):
            result, outputs = run_change_detection(
                name,
                event="push",
                action="",
                draft=False,
                label_event=False,
                label_present=False,
                changed_paths=(),
            )
            with self.subTest(workflow=name):
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertEqual(outputs.get("run_expensive"), "true")
                self.assertEqual(outputs.get(selector), "true")

    def test_aggregates_enforce_selected_jobs_for_ready_and_labelled_prs(self) -> None:
        for name, (gate, jobs) in CI_GATES.items():
            with self.subTest(workflow=name, state="ready PR success"):
                success = run_gate(name, run_expensive=True, selected=selected_values(name, True), changes_result="success", job_results={})
                self.assertEqual(success.returncode, 0, success.stdout + success.stderr)
            with self.subTest(workflow=name, state="ready PR skipped jobs"):
                skipped = run_gate(name, run_expensive=True, selected=selected_values(name, True), changes_result="success", job_results={job: "skipped" for job in jobs})
                self.assertNotEqual(skipped.returncode, 0)
            with self.subTest(workflow=name, state="unlabelled draft"):
                draft = run_gate(name, run_expensive=False, selected=selected_values(name, True), changes_result="success", job_results={job: "skipped" for job in jobs})
                self.assertEqual(draft.returncode, 0, draft.stdout + draft.stderr)
            with self.subTest(workflow=name, state="labelled draft skipped jobs"):
                labelled = run_gate(name, run_expensive=True, selected=selected_values(name, True), changes_result="success", job_results={job: "skipped" for job in jobs})
                self.assertNotEqual(labelled.returncode, 0)
            with self.subTest(workflow=name, state="source unrelated"):
                unrelated = run_gate(name, run_expensive=True, selected=selected_values(name, False), changes_result="success", job_results={job: "skipped" for job in jobs})
                self.assertEqual(unrelated.returncode, 0, unrelated.stdout + unrelated.stderr)
            with self.subTest(workflow=name, state="selected job failure"):
                failed = run_gate(name, run_expensive=True, selected=selected_values(name, True), changes_result="success", job_results={job: "failure" for job in jobs})
                self.assertNotEqual(failed.returncode, 0)
            for changes_result in ("failure", "cancelled", "skipped"):
                with self.subTest(workflow=name, changes_result=changes_result):
                    blocked = run_gate(name, run_expensive=False, selected=selected_values(name, False), changes_result=changes_result, job_results={job: "skipped" for job in jobs})
                    self.assertNotEqual(blocked.returncode, 0)
                    self.assertIn("changes-detection job did not succeed", blocked.stdout + blocked.stderr)

    def test_neo_contract_gate_requires_a_selected_consumer_run(self) -> None:
        all_skipped = {job: "skipped" for job in CI_GATES["neo-ci.yml"][1]}
        contract_only = {"TOUCHED": "false", "CONTRACT": "true"}
        skipped = run_gate("neo-ci.yml", run_expensive=True, selected=contract_only, changes_result="success", job_results=all_skipped)
        self.assertNotEqual(skipped.returncode, 0)
        completed = run_gate(
            "neo-ci.yml",
            run_expensive=True,
            selected=contract_only,
            changes_result="success",
            job_results={**all_skipped, "consumer-contract": "success"},
        )
        self.assertEqual(completed.returncode, 0, completed.stdout + completed.stderr)


if __name__ == "__main__":
    unittest.main(verbosity=2)
