from __future__ import annotations

import datetime as dt
import glob
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

BEST_PRACTICES_SCRIPTS = Path(__file__).resolve().parents[2] / "neohaskell-best-practices" / "scripts"
sys.path.insert(0, str(BEST_PRACTICES_SCRIPTS))
from bounded_process import DEFAULT_MAX_OUTPUT_BYTES, ProcessResult, run_bounded

MAX_OUTPUT_BYTES = DEFAULT_MAX_OUTPUT_BYTES


@dataclass(frozen=True)
class CabalPackage:
    name: str
    cabal_file: Path
    source_dirs: tuple[Path, ...]
    extensions: tuple[str, ...]
    language: str | None
    dependencies: tuple[str, ...]


def _without_comment(line: str) -> str:
    return line.split("--", 1)[0].rstrip()


def _field_blocks(lines: list[str], field: str) -> list[list[str]]:
    blocks: list[list[str]] = []
    position = 0
    field_pattern = re.compile(rf"^\s*{re.escape(field)}\s*:\s*(.*)$", re.IGNORECASE)
    next_field = re.compile(r"^\s*[A-Za-z][A-Za-z0-9-]*\s*:")
    while position < len(lines):
        match = field_pattern.match(_without_comment(lines[position]))
        if match is None:
            position += 1
            continue
        values = [match.group(1)] if match.group(1).strip() else []
        position += 1
        while position < len(lines):
            raw = _without_comment(lines[position])
            if not raw.strip() or not raw[:1].isspace() or next_field.match(raw):
                break
            values.append(raw.strip())
            position += 1
        blocks.append(values)
    return blocks


def _field_tokens(lines: list[str], field: str) -> tuple[str, ...]:
    blocks = _field_blocks(lines, field)
    for block in blocks:
        tokens: list[str] = []
        for value in block:
            tokens.extend(token.strip() for token in value.replace(",", " ").split() if token.strip())
        if tokens:
            return tuple(tokens)
    return ()


def cabal_project_files(root: Path) -> tuple[Path, ...]:
    project_file = root / "cabal.project"
    if not project_file.is_file():
        raise ValueError(f"missing Cabal project file {project_file}")
    lines = project_file.read_text(encoding="utf-8").splitlines()
    patterns: list[str] = []
    collecting = False
    for raw in lines:
        line = _without_comment(raw)
        if not line.strip():
            continue
        match = re.match(r"^\s*packages\s*:\s*(.*)$", line, re.IGNORECASE)
        if match:
            collecting = True
            tail = match.group(1).strip()
            if tail:
                patterns.extend(shlex.split(tail))
            continue
        if collecting and line[:1].isspace():
            patterns.extend(shlex.split(line.strip()))
            continue
        if collecting:
            break

    files: set[Path] = set()
    for pattern in patterns:
        if "://" in pattern:
            continue
        for candidate_name in glob.glob(str(root / pattern), recursive=True):
            candidate = Path(candidate_name).resolve()
            if candidate.is_dir():
                files.update(path.resolve() for path in candidate.glob("*.cabal"))
            elif candidate.suffix == ".cabal":
                files.add(candidate)
    return tuple(sorted(path for path in files if path.is_file() and path.is_relative_to(root.resolve())))


def cabal_packages(root: Path) -> tuple[CabalPackage, ...]:
    descriptors: list[CabalPackage] = []
    for cabal_file in cabal_project_files(root):
        lines = cabal_file.read_text(encoding="utf-8").splitlines()
        name_match = None
        for line in lines:
            match = re.match(r"^\s*name\s*:\s*([A-Za-z0-9][A-Za-z0-9-]*)", _without_comment(line), re.IGNORECASE)
            if match is not None:
                name_match = match
                break
        if name_match is None:
            continue
        source_tokens = _field_tokens(lines, "hs-source-dirs")
        source_dirs = tuple((cabal_file.parent / token).resolve() for token in source_tokens)
        extension_tokens = _field_tokens(lines, "default-extensions")
        language_tokens = _field_tokens(lines, "default-language")
        dependencies: set[str] = set()
        for block in _field_blocks(lines, "build-depends"):
            joined = " ".join(block)
            for item in joined.split(","):
                dependency = re.match(r"\s*([A-Za-z0-9][A-Za-z0-9-]*)", item)
                if dependency is not None:
                    dependencies.add(dependency.group(1))
        descriptors.append(
            CabalPackage(
                name=name_match.group(1),
                cabal_file=cabal_file,
                source_dirs=source_dirs,
                extensions=extension_tokens,
                language=language_tokens[0] if language_tokens else None,
                dependencies=tuple(sorted(dependencies)),
            )
        )
    return tuple(descriptors)


def cabal_test_targets(root: Path) -> tuple[tuple[str, str], ...]:
    targets: list[tuple[str, str]] = []
    for package in cabal_packages(root):
        for line in package.cabal_file.read_text(encoding="utf-8").splitlines():
            match = re.match(r"^test-suite\s+([A-Za-z0-9][A-Za-z0-9-]*)\s*$", _without_comment(line), re.IGNORECASE)
            if match is not None:
                targets.append((package.name, match.group(1)))
    return tuple(targets)


def dependency_source_dirs(package: CabalPackage, packages: Iterable[CabalPackage]) -> tuple[Path, ...]:
    by_name = {candidate.name.lower(): candidate for candidate in packages}
    found: list[Path] = []
    seen: set[str] = {package.name.lower()}

    def add_dependencies(current: CabalPackage) -> None:
        for dependency in current.dependencies:
            key = dependency.lower()
            candidate = by_name.get(key)
            if candidate is None or key in seen:
                continue
            seen.add(key)
            found.extend(candidate.source_dirs)
            add_dependencies(candidate)

    add_dependencies(package)
    return tuple(dict.fromkeys(found))


def transcript_destination(root: Path, owner: str, requested: str | None, invocation_cwd: Path) -> Path:
    if requested:
        path = Path(requested).expanduser()
        return (path if path.is_absolute() else invocation_cwd / path).resolve()
    directory = root / ".agents-state" / "neohaskell-test" / owner
    directory.mkdir(parents=True, exist_ok=True)
    stamp = dt.datetime.now(dt.timezone.utc).strftime("%Y%m%dT%H%M%S.%fZ")
    return directory / f"run-{stamp}-{os.getpid()}.log"


def write_transcript(path: Path, command: list[str], result: ProcessResult, verdict: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    body = (
        f"command: {json.dumps(command, ensure_ascii=False)}\n"
        f"exit_code: {result.returncode}\n"
        f"timed_out: {str(result.timed_out).lower()}\n"
        f"output_limited: {str(result.output_limited).lower()}\n"
        f"verdict: {verdict}\n"
        "--- output ---\n"
        + result.output.decode("utf-8", errors="replace")
    )
    temporary: str | None = None
    try:
        with tempfile.NamedTemporaryFile("w", encoding="utf-8", dir=path.parent, prefix=f".{path.name}.", delete=False) as file:
            temporary = file.name
            file.write(body)
        os.replace(temporary, path)
    finally:
        if temporary and os.path.exists(temporary):
            os.unlink(temporary)


def append_result(lines: list[str], label: str, result: ProcessResult) -> bool:
    output = result.output.decode("utf-8", errors="replace")
    if output:
        lines.append(output.rstrip("\n"))
    if result.timed_out:
        lines.append(f"FAIL {label}: timed out")
        return False
    if result.output_limited:
        lines.append(f"FAIL {label}: output exceeded {MAX_OUTPUT_BYTES} bytes")
        return False
    if result.returncode != 0:
        lines.append(f"FAIL {label}: command exited with status {result.returncode}")
        return False
    lines.append(f"PASS {label}")
    return True


def run_testbed(root: Path, env: dict[str, str], timeout: float) -> tuple[str, str, ProcessResult | None]:
    missing = [name for name in ("hurl", "pg_isready", "bash", "curl") if shutil.which(name) is None]
    if missing:
        return "SKIP", f"missing prerequisite(s): {', '.join(missing)}", None
    readiness = run_bounded(
        ["pg_isready", "-h", "127.0.0.1", "-U", "neohaskell", "-q"],
        cwd=root,
        env=env,
        timeout=10,
    )
    if readiness.returncode != 0 or readiness.timed_out or readiness.output_limited:
        return "SKIP", "PostgreSQL is not ready at 127.0.0.1:5432", readiness
    runner = root / "testbed/scripts/run-tests.sh"
    if not runner.is_file():
        return "FAIL", f"maintained testbed runner is missing at {runner}", None
    result = run_bounded(["bash", str(runner)], cwd=root, env=env, timeout=timeout)
    if result.timed_out:
        return "FAIL", f"testbed runner timed out after {timeout:g} seconds", result
    if result.output_limited:
        return "FAIL", f"testbed output exceeded {MAX_OUTPUT_BYTES} bytes", result
    if result.returncode != 0:
        return "FAIL", f"testbed runner exited with status {result.returncode}", result
    return "PASS", "maintained Hurl testbed runner completed", result


def resolve_root(script_dir: Path, invocation_cwd: Path, env: dict[str, str]) -> Path:
    root_helper = script_dir.parents[1] / "neohaskell-best-practices" / "scripts" / "repo-root"
    result = subprocess.run(
        [str(root_helper)],
        cwd=invocation_cwd,
        env=env,
        capture_output=True,
        text=True,
        timeout=10,
        check=False,
    )
    if result.returncode != 0:
        raise RuntimeError(result.stderr.strip() or "repository root was not found")
    return Path(result.stdout.strip()).resolve()


def invoke_toolchain(
    *,
    script: Path,
    root: Path,
    invocation_cwd: Path,
    env: dict[str, str],
    timeout: float,
    owner: str,
    transcript: Path,
    inner_args: list[str],
) -> int:
    toolchain_dir = script.parents[2] / "neohaskell-best-practices" / "scripts"
    command = [
        str(toolchain_dir / "with-toolchain"),
        "python3",
        str(script),
        "--_inside-toolchain",
        *inner_args,
    ]
    child_env = env.copy()
    child_env["NEOHASKELL_ROOT"] = str(root)
    try:
        result = run_bounded(command, cwd=root, env=child_env, timeout=timeout)
    except OSError as error:
        print(f"{owner}: could not start the shared toolchain wrapper: {error}", file=sys.stderr)
        return 2
    output = result.output.decode("utf-8", errors="replace")
    if output:
        print(output, end="" if output.endswith("\n") else "\n")
    if result.timed_out:
        verdict = "FAIL: toolchain child timed out"
    elif result.output_limited:
        verdict = f"FAIL: toolchain output exceeded {MAX_OUTPUT_BYTES} bytes"
    elif result.returncode != 0:
        verdict = f"FAIL: toolchain command exited with status {result.returncode}"
    elif re.search(r"(?m)^(?:doctest|full-suite|testbed): INCOMPLETE\b", output):
        verdict = "INCOMPLETE"
    else:
        verdict = "PASS"
    if "with-toolchain: nix not found; using unpinned host tools" in output:
        verdict += " (unpinned host tools)"
    write_transcript(transcript, command, result, verdict)
    print(f"{owner}: transcript {transcript}")
    if result.timed_out or result.output_limited:
        return 1
    return result.returncode or 0
