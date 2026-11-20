#!/usr/bin/env python3
from __future__ import annotations

import argparse
import contextlib
import dataclasses
import hashlib
import json
import os
import re
import shutil
import selectors
import sqlite3
import subprocess
import sys
import time
from pathlib import Path
from typing import Any, Optional, Sequence, Tuple


TOTAL_DEADLINE_SECONDS = 12.0
DB_LOCK_SECONDS = 0.25
CHILD_CLEANUP_SECONDS = 0.05
MAX_FILES = 12000
MAX_FILE_BYTES = 2 * 1024 * 1024
MAX_TOTAL_BYTES = 128 * 1024 * 1024
MAX_FALLBACK_FILES = 1800
MAX_FALLBACK_BYTES = 24 * 1024 * 1024
MAX_HIE_FILES = 12000
MAX_HIE_FILE_BYTES = 8 * 1024 * 1024
MAX_HIE_TOTAL_BYTES = 256 * 1024 * 1024
MAX_HIE_WALK_DIRS = 30000
MAX_HIE_OUTPUT_BYTES = 64 * 1024
MAX_HIE_LINE_BYTES = 4096
MAX_RESULTS = 80
MAX_LINE_CHARS = 240
SCHEMA_VERSION = 1
INDEXER_VERSION = 3
CATALOG_RELATIVE_PATH = ".agents/skills/how/references/codemap-catalog.json"
DATABASE_RELATIVE_PATH = ".agents-state/how/codemap.sqlite3"
HIE_DATABASE_RELATIVE_PATH = ".agents-state/how/compiler.hiedb"
EXCLUDED_DIRS = {
    ".git",
    ".agents-state",
    "dist-newstyle",
    "node_modules",
    "target",
    "dist",
    "build",
    "coverage",
    ".next",
    "vendor",
}
HASKELL_EXTENSIONS = {".hs", ".lhs", ".hsc"}
IDENTIFIER_RE = re.compile(r"(?<![A-Za-z0-9_'])((?:[A-Z][A-Za-z0-9_']*\.)*[A-Za-z_][A-Za-z0-9_']*)")
MODULE_RE = re.compile(r"^\s*module\s+([A-Z][A-Za-z0-9_']*(?:\.[A-Z][A-Za-z0-9_']*)*)\b")
IMPORT_RE = re.compile(
    r"^\s*import\s+(?:(?:\{-#\s*SOURCE\s*#-\})\s+)?(?:safe\s+)?(?:qualified\s+)?(?:\"[^\"]+\"\s+)?([A-Z][A-Za-z0-9_']*(?:\.[A-Z][A-Za-z0-9_']*)*)\b"
)
TOP_SIGNATURE_RE = re.compile(r"^([A-Za-z_][A-Za-z0-9_']*)\s*::")
BARE_SIGNATURE_RE = re.compile(r"^([A-Za-z_][A-Za-z0-9_']*)\s*$")
INDENTED_SIGNATURE_RE = re.compile(r"^\s*[{, ]*([A-Za-z_][A-Za-z0-9_']*)\s*::")
INDENTED_BARE_SIGNATURE_RE = re.compile(r"^(\s+)[{, ]*([A-Za-z_][A-Za-z0-9_']*)\s*$")
TYPE_DECL_RE = re.compile(r"^(?:data|newtype|type|class)\s+([A-Z][A-Za-z0-9_']*)\b")
STRING_OR_CHAR_RE = re.compile(r'"(?:\\.|[^"\\])*"|(?<![A-Za-z0-9_])\'(?:\\.|[^\'\\])\'')
QUALIFIED_PARTS_RE = re.compile(r"[A-Z][A-Za-z0-9_']*|[A-Za-z_][A-Za-z0-9_']*")


class CodemapError(Exception):
    pass


class DeadlineExceeded(CodemapError):
    pass


@dataclasses.dataclass(frozen=True)
class Deadline:
    expires_at: float

    @classmethod
    def start(cls) -> "Deadline":
        return cls(time.monotonic() + TOTAL_DEADLINE_SECONDS)

    def remaining(self) -> float:
        remaining = self.expires_at - time.monotonic()
        if remaining <= 0:
            raise DeadlineExceeded("foreground deadline exceeded")
        return remaining

    def check(self) -> None:
        self.remaining()


@dataclasses.dataclass(frozen=True)
class SourceFile:
    path: str
    digest: str
    language: str
    body: str


@dataclasses.dataclass
class Inventory:
    files: dict[str, SourceFile]
    digest: str
    complete: bool
    flags: list[str]
    roots_digest: str


@dataclasses.dataclass(frozen=True)
class Signature:
    name: str
    kind: str
    line: int
    text: str
    normalized: str


@dataclasses.dataclass(frozen=True)
class ParsedSource:
    module: Optional[str]
    module_line: int
    imports: list[tuple[str, int]]
    signatures: list[Signature]
    occurrences: list[tuple[str, str, int]]


def normalize_space(value: str) -> str:
    return " ".join(value.split())


def text_terms(value: str) -> set[str]:
    split_camel = re.sub(r"([a-z0-9])([A-Z])", r"\1 \2", value)
    return set(re.findall(r"[A-Za-z0-9_]+", split_camel.casefold()))


def sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def bounded_text(value: str, width: int = MAX_LINE_CHARS) -> str:
    collapsed = " ".join(value.split())
    if len(collapsed) <= width:
        return collapsed
    return collapsed[: width - 1] + "…"


def bounded_signature(value: str) -> Tuple[str, bool]:
    width = 5000
    if len(value) <= width:
        return value, False
    return value[: width - 1] + "…", True


def is_definition_line(line: str, name: str) -> bool:
    pattern = r"^\s*" + re.escape(name) + r"(?:\s+[^=]*)?\s*="
    return re.match(pattern, line) is not None


def resolve_root(script_file: str) -> Path:
    configured = os.environ.get("CODEMAP_ROOT")
    if configured:
        root = Path(configured).expanduser().resolve()
    else:
        root = Path(script_file).resolve().parents[4]
    if not root.is_dir():
        raise CodemapError("repository root is not a directory")
    return root


def read_catalog(root: Path) -> dict[str, Any]:
    catalog_path = root / CATALOG_RELATIVE_PATH
    try:
        raw = catalog_path.read_bytes()
        catalog = json.loads(raw)
    except (OSError, UnicodeError, json.JSONDecodeError) as error:
        raise CodemapError("codemap catalog could not be read") from error
    if not isinstance(catalog, dict) or catalog.get("schema_version") != 1:
        raise CodemapError("codemap catalog has an unsupported shape")
    for key in ("source_roots", "capabilities", "extension_points"):
        if not isinstance(catalog.get(key), list):
            raise CodemapError("codemap catalog is missing " + key)
        if not all(isinstance(item, dict) for item in catalog[key]):
            raise CodemapError("codemap catalog contains an invalid " + key + " record")
    for source_root in catalog["source_roots"]:
        path = source_root.get("path")
        if not isinstance(path, str) or not path or Path(path).is_absolute() or ".." in Path(path).parts:
            raise CodemapError("codemap source root escapes the repository")
        extensions = source_root.get("extensions")
        if not isinstance(extensions, list) or not all(isinstance(item, str) for item in extensions):
            raise CodemapError("codemap source root has no extension list")
        exact_files = source_root.get("files", [])
        if not isinstance(exact_files, list) or any(
            not isinstance(item, str) or Path(item).is_absolute() or ".." in Path(item).parts
            for item in exact_files
        ):
            raise CodemapError("codemap source file escapes the repository")
    return catalog


def is_within(path: Path, root: Path) -> bool:
    try:
        path.resolve().relative_to(root.resolve())
        return True
    except (OSError, RuntimeError, ValueError):
        return False


def state_paths(root: Path) -> tuple[Path, Path]:
    state_dir = root / ".agents-state" / "how"
    database = state_dir / "codemap.sqlite3"
    if not is_within(state_dir, root) or not is_within(database, root):
        raise CodemapError("state directory escapes the repository root")
    if state_dir.is_symlink() or database.is_symlink():
        raise CodemapError("state path is a symlink")
    return state_dir, database


def find_hiedb() -> tuple[Optional[str], Optional[str]]:
    configured = os.environ.get("CODEMAP_HIEDB")
    if configured:
        executable = Path(configured).expanduser()
        if not executable.is_file() or not os.access(executable, os.X_OK):
            return None, "CODEMAP_HIEDB is not an executable file"
        return str(executable.resolve()), None
    executable = shutil.which("hiedb")
    if executable:
        return executable, None
    return None, "hiedb is not available on PATH"


def scan_hie_inventory(root: Path, deadline: Deadline) -> dict[str, Any]:
    artifact_root = root / "dist-newstyle"
    digest = hashlib.sha256()
    digest.update(str(root).encode("utf-8", "surrogateescape"))
    if not artifact_root.exists():
        return {"files": [], "directories": [], "digest": digest.hexdigest(), "flags": []}
    if artifact_root.is_symlink() or not is_within(artifact_root, root):
        return {"files": [], "directories": [], "digest": digest.hexdigest(), "flags": ["unsafe-hie-root"]}
    files: list[tuple[str, str]] = []
    directories: set[str] = set()
    total = 0
    walked = 0
    flags: list[str] = []
    for current, dirs, filenames in os.walk(artifact_root, followlinks=False):
        deadline.check()
        walked += 1
        if walked > MAX_HIE_WALK_DIRS:
            flags.append("hie-directory-count-bound")
            break
        safe_dirs = []
        for name in dirs:
            if (Path(current) / name).is_symlink():
                flags.append("unsafe-hie-directory")
            else:
                safe_dirs.append(name)
        dirs[:] = safe_dirs
        for filename in filenames:
            if not filename.endswith(".hie"):
                continue
            deadline.check()
            if len(files) >= MAX_HIE_FILES:
                flags.append("hie-file-count-bound")
                break
            path = Path(current) / filename
            try:
                if path.is_symlink() or not is_within(path, root) or not path.is_file():
                    flags.append("unsafe-hie-path")
                    continue
                size = path.stat().st_size
                if size > MAX_HIE_FILE_BYTES:
                    flags.append("oversized-hie-file")
                    continue
                if total + size > MAX_HIE_TOTAL_BYTES:
                    flags.append("hie-total-byte-bound")
                    break
                raw = path.read_bytes()
            except OSError:
                flags.append("unreadable-hie-file")
                continue
            if len(raw) > MAX_HIE_FILE_BYTES:
                flags.append("oversized-hie-file")
                continue
            if total + len(raw) > MAX_HIE_TOTAL_BYTES:
                flags.append("hie-total-byte-bound")
                break
            total += len(raw)
            relative = path.relative_to(root).as_posix()
            file_digest = sha256_bytes(raw)
            files.append((relative, file_digest))
            directories.add(str(path.parent.resolve()))
        if flags and flags[-1] in {"hie-file-count-bound", "hie-total-byte-bound"}:
            break
    files.sort()
    for relative, file_digest in files:
        digest.update(relative.encode("utf-8", "surrogateescape"))
        digest.update(b"\0")
        digest.update(file_digest.encode("ascii"))
        digest.update(b"\0")
    return {
        "files": files,
        "directories": sorted(directories),
        "digest": digest.hexdigest(),
        "flags": sorted(set(flags)),
    }


def hiedb_database(root: Path) -> Path:
    state_dir, _ = state_paths(root)
    state_dir.mkdir(parents=True, exist_ok=True)
    database = root / HIE_DATABASE_RELATIVE_PATH
    if not is_within(database, root) or database.is_symlink():
        raise CodemapError("HIE database path escapes the repository root")
    return database


def stop_child(process: subprocess.Popen[Any], deadline: Deadline) -> None:
    with contextlib.suppress(OSError):
        process.kill()
    try:
        timeout = min(CHILD_CLEANUP_SECONDS, deadline.remaining())
    except DeadlineExceeded:
        timeout = 0
    with contextlib.suppress(OSError, subprocess.TimeoutExpired):
        process.wait(timeout=timeout)


def run_hiedb_index(
    executable: str,
    database: Path,
    root: Path,
    directories: Sequence[str],
    reindex: bool,
    deadline: Deadline,
) -> Optional[str]:
    command = [executable, "--database", str(database), "--quiet", "--src-base-dir", str(root)]
    if reindex:
        command.append("--reindex")
    command.append("index" if directories else "ls")
    command.extend(directories)
    try:
        if deadline.remaining() <= CHILD_CLEANUP_SECONDS:
            return "HIE index deadline exceeded"
        process = subprocess.Popen(
            command,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        try:
            return_code = process.wait(timeout=max(0, deadline.remaining() - CHILD_CLEANUP_SECONDS))
        except (subprocess.TimeoutExpired, DeadlineExceeded):
            stop_child(process, deadline)
            return "HIE index deadline exceeded"
        if return_code != 0:
            return "HIE index command failed"
        return None
    except OSError:
        return "HIE index command could not start"


def run_hiedb_query(
    executable: str,
    database: Path,
    command: str,
    name: str,
    module: Optional[str],
    limit: int,
    deadline: Deadline,
) -> dict[str, Any]:
    arguments = [executable, "--database", str(database), "--quiet", command, name]
    if module:
        arguments.append(module)
    rows: list[str] = []
    buffer = bytearray()
    total_bytes = 0
    truncated = False
    try:
        if deadline.remaining() <= CHILD_CLEANUP_SECONDS:
            return {"results": [], "truncated": True, "reason": "HIE query deadline exceeded"}
        process = subprocess.Popen(
            arguments,
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        )
    except DeadlineExceeded:
        return {"results": [], "truncated": True, "reason": "HIE query deadline exceeded"}
    except OSError:
        return {"results": [], "truncated": False, "reason": "HIE query command could not start"}
    try:
        if process.stdout is None:
            stop_child(process, deadline)
            return {"results": [], "truncated": False, "reason": "HIE query output is unavailable"}
        selector = selectors.DefaultSelector()
        selector.register(process.stdout, selectors.EVENT_READ)
        while True:
            try:
                wait_timeout = deadline.remaining() - CHILD_CLEANUP_SECONDS
                if wait_timeout <= 0:
                    stop_child(process, deadline)
                    return {"results": [], "truncated": True, "reason": "HIE query deadline exceeded"}
                ready = selector.select(wait_timeout)
            except DeadlineExceeded:
                stop_child(process, deadline)
                return {"results": [], "truncated": True, "reason": "HIE query deadline exceeded"}
            if not ready:
                stop_child(process, deadline)
                return {"results": [], "truncated": True, "reason": "HIE query deadline exceeded"}
            chunk = os.read(process.stdout.fileno(), min(4096, MAX_HIE_OUTPUT_BYTES - total_bytes + 1))
            if not chunk:
                break
            total_bytes += len(chunk)
            if total_bytes > MAX_HIE_OUTPUT_BYTES:
                stop_child(process, deadline)
                return {"results": [], "truncated": True, "reason": "HIE query output exceeded its bound"}
            buffer.extend(chunk)
            while b"\n" in buffer:
                raw_line, _, remainder = buffer.partition(b"\n")
                buffer = bytearray(remainder)
                if len(raw_line) > MAX_HIE_LINE_BYTES:
                    stop_child(process, deadline)
                    return {"results": [], "truncated": True, "reason": "HIE query line exceeded its bound"}
                rows.append(raw_line.decode("utf-8", errors="replace").strip())
                if len(rows) > limit:
                    truncated = True
                    stop_child(process, deadline)
                    break
            if truncated:
                break
            if len(buffer) > MAX_HIE_LINE_BYTES:
                stop_child(process, deadline)
                return {"results": [], "truncated": True, "reason": "HIE query line exceeded its bound"}
        if not truncated:
            if buffer:
                rows.append(bytes(buffer).decode("utf-8", errors="replace").strip())
            try:
                return_code = process.wait(timeout=max(0, deadline.remaining() - CHILD_CLEANUP_SECONDS))
            except (subprocess.TimeoutExpired, DeadlineExceeded):
                stop_child(process, deadline)
                return {"results": [], "truncated": True, "reason": "HIE query deadline exceeded"}
            if return_code != 0:
                return {"results": [], "truncated": False, "reason": "HIE query command failed"}
        parsed: list[dict[str, Any]] = []
        invalid = 0
        for row in rows[:limit]:
            match = re.fullmatch(r"(.+):(\d+):(\d+)-(\d+):(\d+)", row)
            if not match:
                invalid += 1
                continue
            parsed.append(
                {
                    "module": match.group(1),
                    "line": int(match.group(2)),
                    "column_start": int(match.group(3)),
                    "end_line": int(match.group(4)),
                    "column_end": int(match.group(5)),
                    "provenance": "compiler-artifact-historical-or-unknown",
                }
            )
        return {
            "results": parsed,
            "truncated": truncated,
            "reason": "HIE query returned an unrecognized location" if invalid else None,
        }
    except (OSError, ValueError):
        stop_child(process, deadline)
        return {"results": [], "truncated": True, "reason": "HIE query output could not be read"}
    finally:
        with contextlib.suppress(Exception):
            selector.close()
        if process.stdout is not None:
            with contextlib.suppress(Exception):
                process.stdout.close()
        if process.poll() is None:
            stop_child(process, deadline)


def ensure_hiedb_index(
    root: Path,
    connection: sqlite3.Connection,
    deadline: Deadline,
) -> dict[str, Any]:
    executable, tool_error = find_hiedb()
    if not executable:
        return {
            "state": "unavailable",
            "reason": tool_error,
            "artifact_files": None,
            "source_build_correspondence": "unknown",
        }
    try:
        inventory = scan_hie_inventory(root, deadline)
        if inventory["flags"]:
            return {
                "state": "incomplete",
                "reason": ", ".join(inventory["flags"]),
                "artifact_files": len(inventory["files"]),
                "source_build_correspondence": "unknown",
            }
        database = hiedb_database(root)
        previous_digest = read_meta(connection, "hiedb_artifact_digest")
        previous_tool = read_meta(connection, "hiedb_executable")
        database_exists = database.is_file()
        changed = (
            not database_exists
            or previous_digest != inventory["digest"]
            or previous_tool != executable
        )
        if not inventory["files"] and not database_exists:
            write_meta(connection, "hiedb_artifact_digest", inventory["digest"])
            write_meta(connection, "hiedb_artifact_count", "0")
            write_meta(connection, "hiedb_executable", executable)
            return {
                "state": "no-artifacts",
                "reason": None,
                "artifact_files": 0,
                "source_build_correspondence": "unknown",
            }
        if changed:
            reason = run_hiedb_index(
                executable,
                database,
                root,
                inventory["directories"],
                reindex=database_exists,
                deadline=deadline,
            )
            if reason:
                return {
                    "state": "stale-or-incomplete",
                    "reason": reason,
                    "artifact_files": len(inventory["files"]),
                    "source_build_correspondence": "unknown",
                }
            current = scan_hie_inventory(root, deadline)
            if current["flags"] or current["digest"] != inventory["digest"]:
                return {
                    "state": "artifact-race",
                    "reason": "HIE artifacts changed during indexing",
                    "artifact_files": len(current["files"]),
                    "source_build_correspondence": "unknown",
                }
            write_meta(connection, "hiedb_artifact_digest", inventory["digest"])
            write_meta(connection, "hiedb_artifact_count", str(len(inventory["files"])))
            write_meta(connection, "hiedb_executable", executable)
        return {
            "state": "current-artifact-inventory",
            "reason": None,
            "artifact_files": len(inventory["files"]),
            "artifact_digest": inventory["digest"],
            "database": HIE_DATABASE_RELATIVE_PATH,
            "source_build_correspondence": "unknown; HIE results are historical or freshness-unknown evidence",
        }
    except (CodemapError, DeadlineExceeded, OSError, sqlite3.Error) as error:
        return {
            "state": "unavailable",
            "reason": str(error),
            "artifact_files": None,
            "source_build_correspondence": "unknown",
        }


def compiler_lookup(
    root: Path,
    connection: sqlite3.Connection,
    command: str,
    query: str,
    limit: int,
    deadline: Deadline,
) -> dict[str, Any]:
    status = ensure_hiedb_index(root, connection, deadline)
    if status["state"] != "current-artifact-inventory" or status.get("artifact_files") == 0:
        return {"status": status, "results": [], "truncated": False}
    match = re.fullmatch(
        r"(?:(?P<module>[A-Z][A-Za-z0-9_']*(?:\.[A-Z][A-Za-z0-9_']*)*)\.)?(?P<name>[A-Za-z_][A-Za-z0-9_']*)",
        query.strip(),
    )
    if not match:
        status = dict(status)
        status["query"] = "HIE lookup supports Haskell identifiers, with an optional module qualifier"
        return {"status": status, "results": [], "truncated": False}
    name = match.group("name")
    module = match.group("module")
    query_command = (
        ("type-def" if name[0].isupper() else "name-def")
        if command == "symbol"
        else ("type-refs" if name[0].isupper() else "name-refs")
    )
    executable, tool_error = find_hiedb()
    if not executable:
        status = dict(status)
        status["reason"] = tool_error
        return {"status": status, "results": [], "truncated": False}
    database = root / HIE_DATABASE_RELATIVE_PATH
    if not database.is_file() or database.is_symlink():
        status = dict(status)
        status["state"] = "unavailable"
        status["reason"] = "HIE database is unavailable"
        return {"status": status, "results": [], "truncated": False}
    query_result = run_hiedb_query(
        executable,
        database,
        query_command,
        name,
        module,
        limit,
        deadline,
    )
    try:
        current_inventory = scan_hie_inventory(root, deadline)
        if current_inventory["flags"] or current_inventory["digest"] != status["artifact_digest"]:
            status = dict(status)
            status["state"] = "artifact-race"
            status["reason"] = "HIE artifacts changed during lookup"
            return {"status": status, "results": [], "truncated": False}
    except (CodemapError, DeadlineExceeded) as error:
        status = dict(status)
        status["state"] = "incomplete"
        status["reason"] = str(error)
        return {"status": status, "results": [], "truncated": False}
    if query_result.get("reason"):
        status = dict(status)
        status["reason"] = query_result["reason"]
    return {
        "status": status,
        "results": query_result["results"],
        "truncated": query_result["truncated"],
    }


def language_for(path: str) -> str:
    suffix = Path(path).suffix.lower()
    if suffix in HASKELL_EXTENSIONS:
        return "haskell"
    if suffix in {".rs"}:
        return "rust"
    if suffix in {".ts", ".tsx", ".js", ".jsx"}:
        return "javascript"
    if suffix in {".nix"}:
        return "nix"
    if suffix in {".sh"}:
        return "shell"
    if suffix in {".cabal", ".toml", ".json", ".css", ".j2"}:
        return "config"
    return "text"


def relative_matches_catalog(path: str, catalog: dict[str, Any]) -> bool:
    normalized = path.replace(os.sep, "/")
    path_obj = Path(normalized)
    if path_obj.is_absolute() or ".." in path_obj.parts:
        return False
    if any(part in EXCLUDED_DIRS for part in path_obj.parts):
        return False
    for source_root in catalog["source_roots"]:
        root_path = source_root.get("path")
        extensions = source_root.get("extensions")
        files = source_root.get("files", [])
        if not isinstance(root_path, str) or not isinstance(extensions, list):
            continue
        if root_path == "." and normalized in files:
            return True
        if root_path == ".":
            is_under = True
            relative = normalized
        else:
            prefix = root_path.rstrip("/") + "/"
            is_under = normalized.startswith(prefix)
            relative = normalized[len(prefix) :] if is_under else ""
        if not is_under:
            if normalized in files and root_path == ".":
                return True
            continue
        if normalized == root_path:
            continue
        if not any(Path(relative).suffix.lower() == ext.lower() for ext in extensions):
            continue
        excluded = source_root.get("exclude_dirs", [])
        if any(part in excluded for part in Path(relative).parts):
            continue
        excluded_files = source_root.get("exclude_files", [])
        if Path(relative).name in excluded_files:
            continue
        return True
    return False


def git_paths(root: Path, deadline: Deadline) -> tuple[list[str], str]:
    try:
        result = subprocess.run(
            ["git", "-C", str(root), "ls-files", "--cached", "--others", "--exclude-standard", "-z"],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=min(deadline.remaining(), 2.0),
        )
    except (OSError, subprocess.TimeoutExpired) as error:
        raise CodemapError("bounded Git source inventory failed") from error
    if result.returncode != 0:
        raise CodemapError("bounded Git source inventory failed")
    if len(result.stdout) > 32 * 1024 * 1024:
        raise CodemapError("Git source inventory exceeded its output bound")
    paths = [os.fsdecode(part) for part in result.stdout.split(b"\0") if part]
    return paths, "git-index-and-nonignored-untracked"


def walk_paths(root: Path, catalog: dict[str, Any], deadline: Deadline) -> tuple[list[str], str]:
    found: list[str] = []
    for source_root in catalog["source_roots"]:
        deadline.check()
        rel_root = source_root.get("path")
        if not isinstance(rel_root, str):
            continue
        if rel_root == "." and not source_root.get("extensions"):
            for relative in source_root.get("files", []):
                deadline.check()
                if isinstance(relative, str) and relative_matches_catalog(relative, catalog):
                    path = root / relative
                    if path.is_file() and not path.is_symlink() and is_within(path, root):
                        found.append(relative)
            continue
        base = root if rel_root == "." else root / rel_root
        if not is_within(base, root) or base.is_symlink() or not base.is_dir():
            continue
        for current, dirs, filenames in os.walk(base, followlinks=False):
            deadline.check()
            dirs[:] = [
                name
                for name in dirs
                if name not in EXCLUDED_DIRS
                and name not in source_root.get("exclude_dirs", [])
                and not (Path(current) / name).is_symlink()
            ]
            for filename in filenames:
                path = Path(current) / filename
                try:
                    relative = path.relative_to(root).as_posix()
                except ValueError:
                    continue
                if relative_matches_catalog(relative, catalog) and not path.is_symlink():
                    found.append(relative)
                if len(found) > MAX_FILES:
                    raise CodemapError("filesystem inventory exceeded its file bound")
    return sorted(set(found)), "bounded-filesystem-fallback-incomplete"


def enumerate_paths(root: Path, catalog: dict[str, Any], deadline: Deadline) -> tuple[list[str], str, bool]:
    try:
        paths, method = git_paths(root, deadline)
        return sorted({path for path in paths if relative_matches_catalog(path, catalog)}), method, True
    except CodemapError:
        paths, method = walk_paths(root, catalog, deadline)
        return paths, method, False


def root_configuration_digest(catalog: dict[str, Any]) -> str:
    payload = json.dumps(
        {"source_roots": catalog["source_roots"], "schema_version": catalog["schema_version"]},
        sort_keys=True,
        separators=(",", ":"),
    ).encode("utf-8")
    return sha256_bytes(payload)


def inventory_digest(files: dict[str, SourceFile], roots_digest: str) -> str:
    digest = hashlib.sha256()
    digest.update(roots_digest.encode("ascii"))
    for path in sorted(files):
        record = files[path]
        digest.update(path.encode("utf-8", "surrogateescape"))
        digest.update(b"\0")
        digest.update(record.digest.encode("ascii"))
        digest.update(b"\0")
    return digest.hexdigest()


def scan_inventory(
    root: Path,
    catalog: dict[str, Any],
    deadline: Deadline,
    include_body: bool = True,
) -> Inventory:
    paths, method, complete = enumerate_paths(root, catalog, deadline)
    roots_digest = root_configuration_digest(catalog)
    flags: list[str] = []
    if not complete:
        flags.append(method)
    if len(paths) > MAX_FILES:
        paths = paths[:MAX_FILES]
        complete = False
        flags.append("file-count-bound")
    files: dict[str, SourceFile] = {}
    total = 0
    for relative in paths:
        deadline.check()
        path = root / relative
        try:
            if path.is_symlink() or not is_within(path, root) or not path.is_file():
                continue
            size = path.stat().st_size
            if size > MAX_FILE_BYTES:
                complete = False
                flags.append("oversized-file:" + relative)
                continue
            if total + size > MAX_TOTAL_BYTES:
                complete = False
                flags.append("total-byte-bound")
                break
            raw = path.read_bytes()
        except OSError:
            complete = False
            flags.append("unreadable-file:" + relative)
            continue
        if len(raw) > MAX_FILE_BYTES:
            complete = False
            flags.append("oversized-file:" + relative)
            continue
        if total + len(raw) > MAX_TOTAL_BYTES:
            complete = False
            flags.append("total-byte-bound")
            break
        total += len(raw)
        digest = sha256_bytes(raw)
        body = raw.decode("utf-8", errors="replace") if include_body else ""
        files[relative] = SourceFile(relative, digest, language_for(relative), body)
    digest = inventory_digest(files, roots_digest)
    return Inventory(files, digest, complete, sorted(set(flags)), roots_digest)


def strip_haskell_noncode(line: str, block_depth: int = 0) -> tuple[str, int]:
    output = list(line)
    index = 0
    in_string = False
    escaped = False
    while index < len(line):
        if block_depth:
            if line.startswith("{-", index):
                block_depth += 1
                output[index : index + 2] = [" ", " "]
                index += 2
            elif line.startswith("-}", index):
                block_depth -= 1
                output[index : index + 2] = [" ", " "]
                index += 2
            else:
                output[index] = " "
                index += 1
            continue
        if in_string:
            output[index] = " "
            if escaped:
                escaped = False
            elif line[index] == "\\":
                escaped = True
            elif line[index] == '"':
                in_string = False
            index += 1
            continue
        if line.startswith("--", index):
            output[index:] = [" "] * (len(line) - index)
            break
        if line.startswith("{-", index):
            block_depth = 1
            output[index : index + 2] = [" ", " "]
            index += 2
            continue
        char_match = STRING_OR_CHAR_RE.match(line, index)
        if char_match and char_match.group(0).startswith('"'):
            in_string = True
            output[index] = " "
            index += 1
            continue
        if char_match:
            output[index : char_match.end()] = [" "] * (char_match.end() - index)
            index = char_match.end()
            continue
        index += 1
    return "".join(output), block_depth


def haskell_code_lines(text: str) -> list[str]:
    code: list[str] = []
    block_depth = 0
    for line in text.splitlines():
        if line.startswith(">"):
            line = line[1:]
        stripped, block_depth = strip_haskell_noncode(line, block_depth)
        code.append(stripped)
    return code


def collect_signature(lines: Sequence[str], start: int, base_indent: int, name: str, kind: str) -> Signature:
    collected = [lines[start].rstrip()]
    for index in range(start + 1, len(lines)):
        line = lines[index]
        if not line.strip():
            if index + 1 < len(lines) and (len(lines[index + 1]) - len(lines[index + 1].lstrip()) > base_indent):
                continue
            break
        indent = len(line) - len(line.lstrip())
        if indent <= base_indent:
            break
        if kind == "indented-signature" and INDENTED_SIGNATURE_RE.match(line):
            break
        collected.append(line.rstrip())
    text = "\n".join(part for part in collected if part.strip()).strip()
    return Signature(name, kind, start + 1, text, normalize_space(text))


def parse_signatures(lines: Sequence[str]) -> list[Signature]:
    signatures: list[Signature] = []
    seen: set[tuple[str, str, int]] = set()
    inside_type_declaration = False
    index = 0
    while index < len(lines):
        line = lines[index]
        indent = len(line) - len(line.lstrip())
        if line.strip() and indent == 0:
            type_declaration = TYPE_DECL_RE.match(line)
            inside_type_declaration = type_declaration is not None
            if type_declaration:
                signature = collect_signature(lines, index, 0, type_declaration.group(1), "type")
                signatures.append(signature)
                seen.add((signature.name, signature.kind, signature.line))
                index += 1
                continue
        top = TOP_SIGNATURE_RE.match(line)
        if top:
            inside_type_declaration = False
            signature = collect_signature(lines, index, 0, top.group(1), "signature")
            signatures.append(signature)
            seen.add((signature.name, signature.kind, signature.line))
            index += 1
            continue
        bare = BARE_SIGNATURE_RE.match(line)
        if bare and index + 1 < len(lines) and re.match(r"^\s+::", lines[index + 1]):
            inside_type_declaration = False
            signature = collect_signature(lines, index, 0, bare.group(1), "signature")
            signatures.append(signature)
            seen.add((signature.name, signature.kind, signature.line))
            index += 1
            continue
        indented = INDENTED_SIGNATURE_RE.match(line) if inside_type_declaration else None
        if indented:
            signature = collect_signature(
                lines,
                index,
                len(line) - len(line.lstrip()),
                indented.group(1),
                "indented-signature",
            )
            key = (signature.name, signature.kind, signature.line)
            if key not in seen:
                signatures.append(signature)
                seen.add(key)
            index += 1
            continue
        indented_bare = INDENTED_BARE_SIGNATURE_RE.match(line) if inside_type_declaration else None
        if indented_bare and index + 1 < len(lines) and re.match(r"^\s+::", lines[index + 1]):
            signature = collect_signature(
                lines,
                index,
                len(indented_bare.group(1)),
                indented_bare.group(2),
                "indented-signature",
            )
            key = (signature.name, signature.kind, signature.line)
            if key not in seen:
                signatures.append(signature)
                seen.add(key)
        index += 1
    return signatures


def parse_haskell(path: str, text: str) -> ParsedSource:
    code_lines = haskell_code_lines(text)
    module_name: Optional[str] = None
    module_line = 0
    imports: list[tuple[str, int]] = []
    occurrences: list[tuple[str, str, int]] = []
    for number, line in enumerate(code_lines, start=1):
        if module_name is None:
            module_match = MODULE_RE.match(line)
            if module_match:
                module_name = module_match.group(1)
                module_line = number
        import_match = IMPORT_RE.match(line)
        if import_match:
            imports.append((import_match.group(1), number))
        for match in IDENTIFIER_RE.finditer(line):
            qualified = match.group(1)
            token = qualified.rsplit(".", 1)[-1]
            occurrences.append((token, qualified, number))
    return ParsedSource(
        module_name,
        module_line,
        imports,
        parse_signatures(code_lines),
        occurrences,
    )


def catalog_digest(catalog: dict[str, Any]) -> str:
    return sha256_bytes(
        json.dumps(catalog, sort_keys=True, separators=(",", ":")).encode("utf-8")
    )


def ensure_schema(connection: sqlite3.Connection) -> None:
    version = connection.execute("PRAGMA user_version").fetchone()[0]
    if version not in (0, SCHEMA_VERSION):
        raise CodemapError("codemap database schema version is unsupported")
    statements = (
        "CREATE TABLE IF NOT EXISTS file (path TEXT PRIMARY KEY, sha256 TEXT NOT NULL, language TEXT NOT NULL)",
        "CREATE TABLE IF NOT EXISTS source_text (path TEXT PRIMARY KEY REFERENCES file(path) ON DELETE CASCADE, body TEXT NOT NULL)",
        "CREATE TABLE IF NOT EXISTS module (path TEXT PRIMARY KEY REFERENCES file(path) ON DELETE CASCADE, name TEXT NOT NULL, line INTEGER NOT NULL)",
        "CREATE INDEX IF NOT EXISTS module_name_idx ON module(name, path)",
        "CREATE TABLE IF NOT EXISTS symbol (path TEXT NOT NULL REFERENCES file(path) ON DELETE CASCADE, name TEXT NOT NULL, kind TEXT NOT NULL, line INTEGER NOT NULL, signature TEXT NOT NULL, normalized_signature TEXT NOT NULL, PRIMARY KEY(path, name, kind, line))",
        "CREATE INDEX IF NOT EXISTS symbol_name_idx ON symbol(name, path)",
        "CREATE TABLE IF NOT EXISTS import (path TEXT NOT NULL REFERENCES file(path) ON DELETE CASCADE, module_name TEXT NOT NULL, line INTEGER NOT NULL, PRIMARY KEY(path, module_name, line))",
        "CREATE INDEX IF NOT EXISTS import_module_idx ON import(module_name, path)",
        "CREATE TABLE IF NOT EXISTS occurrence (path TEXT NOT NULL REFERENCES file(path) ON DELETE CASCADE, token TEXT NOT NULL, qualified TEXT NOT NULL, line INTEGER NOT NULL, PRIMARY KEY(path, line, token, qualified))",
        "CREATE INDEX IF NOT EXISTS occurrence_token_idx ON occurrence(token, path)",
        "CREATE INDEX IF NOT EXISTS occurrence_qualified_idx ON occurrence(qualified, path)",
        "CREATE TABLE IF NOT EXISTS cache_meta (key TEXT PRIMARY KEY, value TEXT NOT NULL)",
    )
    for statement in statements:
        connection.execute(statement)
    connection.execute("PRAGMA user_version = " + str(SCHEMA_VERSION))


def write_meta(connection: sqlite3.Connection, key: str, value: str) -> None:
    connection.execute(
        "INSERT INTO cache_meta(key, value) VALUES(?, ?) "
        "ON CONFLICT(key) DO UPDATE SET value = excluded.value",
        (key, value),
    )


def read_meta(connection: sqlite3.Connection, key: str) -> Optional[str]:
    row = connection.execute("SELECT value FROM cache_meta WHERE key = ?", (key,)).fetchone()
    return row[0] if row else None


def parse_file(source: SourceFile) -> ParsedSource:
    if source.language == "haskell":
        return parse_haskell(source.path, source.body)
    return ParsedSource(None, 0, [], [], [])


def index_file(connection: sqlite3.Connection, source: SourceFile, deadline: Deadline) -> None:
    deadline.check()
    parsed = parse_file(source)
    connection.execute(
        "INSERT INTO file(path, sha256, language) VALUES(?, ?, ?)",
        (source.path, source.digest, source.language),
    )
    connection.execute(
        "INSERT INTO source_text(path, body) VALUES(?, ?)",
        (source.path, source.body),
    )
    if parsed.module:
        connection.execute(
            "INSERT INTO module(path, name, line) VALUES(?, ?, ?)",
            (source.path, parsed.module, parsed.module_line),
        )
    connection.executemany(
        "INSERT INTO symbol(path, name, kind, line, signature, normalized_signature) "
        "VALUES(?, ?, ?, ?, ?, ?)",
        [
            (source.path, item.name, item.kind, item.line, item.text, item.normalized)
            for item in parsed.signatures
        ],
    )
    connection.executemany(
        "INSERT INTO import(path, module_name, line) VALUES(?, ?, ?)",
        [(source.path, module_name, line) for module_name, line in parsed.imports],
    )
    connection.executemany(
        "INSERT OR IGNORE INTO occurrence(path, token, qualified, line) VALUES(?, ?, ?, ?)",
        [
            (source.path, token, qualified, line)
            for token, qualified, line in parsed.occurrences
        ],
    )


def database_context(root: Path, deadline: Deadline) -> Tuple[sqlite3.Connection, Path]:
    state_dir, database = state_paths(root)
    state_dir.mkdir(parents=True, exist_ok=True)
    if not is_within(state_dir, root) or state_dir.is_symlink():
        raise CodemapError("state directory escapes the repository root")
    timeout = min(DB_LOCK_SECONDS, deadline.remaining())
    connection = sqlite3.connect(
        str(database),
        timeout=timeout,
        isolation_level=None,
    )
    connection.execute("PRAGMA foreign_keys = ON")
    connection.execute("PRAGMA busy_timeout = " + str(max(1, int(timeout * 1000))))
    return connection, database


def update_database(root: Path, catalog: dict[str, Any], first: Inventory, deadline: Deadline) -> dict[str, Any]:
    connection, database = database_context(root, deadline)
    changed = 0
    deleted = 0
    try:
        connection.execute("BEGIN IMMEDIATE")
        ensure_schema(connection)
        parser_changed = read_meta(connection, "indexer_version") != str(INDEXER_VERSION)
        if parser_changed:
            connection.execute("DELETE FROM file")
            prior_paths: set[str] = set()
        else:
            prior_paths = {
                row[0] for row in connection.execute("SELECT path FROM file")
            }
        incoming_paths = set(first.files)
        deleted_paths = prior_paths - incoming_paths
        for path in sorted(deleted_paths):
            deadline.check()
            connection.execute("DELETE FROM file WHERE path = ?", (path,))
            deleted += 1
        for path in sorted(incoming_paths):
            deadline.check()
            source = first.files[path]
            row = connection.execute(
                "SELECT sha256 FROM file WHERE path = ?",
                (path,),
            ).fetchone()
            if row and row[0] == source.digest:
                continue
            if row:
                connection.execute("DELETE FROM file WHERE path = ?", (path,))
            index_file(connection, source, deadline)
            changed += 1
        write_meta(connection, "source_digest", first.digest)
        write_meta(connection, "roots_digest", first.roots_digest)
        write_meta(connection, "catalog_digest", catalog_digest(catalog))
        write_meta(connection, "indexer_version", str(INDEXER_VERSION))
        write_meta(connection, "last_refresh_utc", time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()))
        write_meta(connection, "source_file_count", str(len(first.files)))
        write_meta(connection, "coverage_flags", json.dumps(first.flags))
        if not first.complete:
            connection.rollback()
            raise CodemapError("current source inventory is incomplete")
        second = scan_inventory(root, catalog, deadline, include_body=False)
        if not second.complete or second.digest != first.digest:
            connection.rollback()
            return {
                "stable": False,
                "changed": changed,
                "deleted": deleted,
                "database": str(database),
                "source_digest": first.digest,
                "fresh_digest": second.digest,
                "flags": second.flags,
            }
        deadline.check()
        connection.commit()
        return {
            "stable": True,
            "changed": changed,
            "deleted": deleted,
            "database": str(database),
            "source_digest": first.digest,
            "fresh_digest": second.digest,
            "flags": first.flags,
        }
    except sqlite3.Error:
        with contextlib.suppress(sqlite3.Error):
            connection.rollback()
        raise
    finally:
        connection.close()


def quarantine_database(database: Path, root: Path) -> bool:
    if not database.exists() or database.is_symlink() or not is_within(database, root):
        return False
    state_dir = database.parent
    stamp = str(int(time.time() * 1000))
    moved = False
    for suffix in ("", "-wal", "-shm"):
        source = Path(str(database) + suffix)
        if not source.exists():
            continue
        if source.is_symlink() or not is_within(source, root):
            return False
        destination = state_dir / ("codemap.corrupt-" + stamp + suffix)
        try:
            os.replace(source, destination)
            moved = True
        except OSError:
            return False
    return moved


def refresh(root: Path, catalog: dict[str, Any], deadline: Deadline) -> Tuple[Optional[sqlite3.Connection], Inventory, dict[str, Any]]:
    first = scan_inventory(root, catalog, deadline, include_body=True)
    if not first.complete:
        return None, first, {
            "stable": False,
            "changed": 0,
            "deleted": 0,
            "database": str(root / DATABASE_RELATIVE_PATH),
            "source_digest": first.digest,
            "fresh_digest": None,
            "flags": first.flags,
            "reason": "source inventory is incomplete",
        }
    attempts = 0
    while attempts < 2:
        attempts += 1
        try:
            result = update_database(root, catalog, first, deadline)
            if not result["stable"]:
                fresh = scan_inventory(root, catalog, deadline, include_body=True)
                return None, fresh, result
            connection, _database = database_context(root, deadline)
            return connection, first, result
        except sqlite3.DatabaseError as error:
            message = str(error).lower()
            corruption = any(
                marker in message
                for marker in (
                    "file is not a database",
                    "database disk image is malformed",
                    "malformed database",
                    "not a database",
                )
            )
            if not corruption:
                return None, first, {
                    "stable": False,
                    "changed": 0,
                    "deleted": 0,
                    "database": str(root / DATABASE_RELATIVE_PATH),
                    "source_digest": first.digest,
                    "fresh_digest": None,
                    "flags": first.flags,
                    "reason": "cache unavailable: " + str(error),
                }
            state_dir, database = state_paths(root)
            if attempts >= 2 or not quarantine_database(database, root):
                return None, first, {
                    "stable": False,
                    "changed": 0,
                    "deleted": 0,
                    "database": str(database),
                    "source_digest": first.digest,
                    "fresh_digest": None,
                    "flags": first.flags,
                    "reason": "cache corruption: " + str(error),
                }
        except (sqlite3.OperationalError, OSError, CodemapError, DeadlineExceeded) as error:
            return None, first, {
                "stable": False,
                "changed": 0,
                "deleted": 0,
                "database": str(root / DATABASE_RELATIVE_PATH),
                "source_digest": first.digest,
                "fresh_digest": None,
                "flags": first.flags,
                "reason": str(error),
            }
    return None, first, {
        "stable": False,
        "changed": 0,
        "deleted": 0,
        "database": str(root / DATABASE_RELATIVE_PATH),
        "source_digest": first.digest,
        "fresh_digest": None,
        "flags": first.flags,
        "reason": "cache refresh could not stabilize",
    }


def valid_curated_records(
    catalog: dict[str, Any],
    inventory: Inventory,
    kind: str,
) -> list[dict[str, Any]]:
    records = catalog[kind]
    valid: list[dict[str, Any]] = []
    file_paths = set(inventory.files)
    for record in records:
        paths = record.get("paths", [])
        if not isinstance(paths, list):
            continue
        live_paths: list[str] = []
        for raw_path in paths:
            if not isinstance(raw_path, str):
                continue
            if raw_path in file_paths or any(path.startswith(raw_path.rstrip("/") + "/") for path in file_paths):
                live_paths.append(raw_path)
        if not live_paths:
            continue
        symbols = record.get("symbols", [])
        if symbols:
            if not isinstance(symbols, list) or not all(isinstance(symbol, str) for symbol in symbols):
                continue
            live_symbols: set[str] = set()
            for path in live_paths:
                source = inventory.files.get(path)
                if source is None:
                    continue
                for symbol in symbols:
                    if re.search(r"\b" + re.escape(symbol) + r"\b", source.body):
                        live_symbols.add(symbol)
            if not set(symbols).issubset(live_symbols):
                continue
        retained = dict(record)
        retained["paths"] = live_paths
        valid.append(retained)
    return valid


def current_source_fallback(
    root: Path,
    catalog: dict[str, Any],
    command: str,
    query: str,
    limit: int,
    deadline: Deadline,
) -> tuple[list[dict[str, Any]], dict[str, Any]]:
    paths, method, git_complete = enumerate_paths(root, catalog, deadline)
    results: list[dict[str, Any]] = []
    scanned = 0
    total = 0
    complete = git_complete
    flags = [] if git_complete else [method]
    normalized_query = normalize_space(query).casefold()
    for relative in paths:
        deadline.check()
        if scanned >= MAX_FALLBACK_FILES or total >= MAX_FALLBACK_BYTES:
            complete = False
            flags.append("fallback-bound")
            break
        path = root / relative
        try:
            if path.is_symlink() or not is_within(path, root) or not path.is_file():
                continue
            size = path.stat().st_size
            if size > MAX_FILE_BYTES or total + size > MAX_FALLBACK_BYTES:
                complete = False
                flags.append("fallback-byte-bound")
                continue
            raw = path.read_bytes()
        except OSError:
            complete = False
            flags.append("fallback-unreadable-source")
            continue
        if len(raw) > MAX_FILE_BYTES or total + len(raw) > MAX_FALLBACK_BYTES:
            complete = False
            flags.append("fallback-byte-bound")
            continue
        scanned += 1
        total += len(raw)
        body = raw.decode("utf-8", errors="replace")
        language = language_for(relative)
        if command == "symbol" and language == "haskell":
            parsed = parse_haskell(relative, body)
            for item in parsed.signatures:
                module = parsed.module or ""
                if "." in query:
                    wanted_module, wanted_name = query.rsplit(".", 1)
                    matches = item.name.casefold() == wanted_name.casefold() and module.casefold() == wanted_module.casefold()
                else:
                    matches = item.name.casefold() == normalized_query or module.casefold() == normalized_query
                if matches:
                    signature, signature_truncated = bounded_signature(item.text)
                    results.append(
                        {
                            "path": relative,
                            "line": item.line,
                            "name": item.name,
                            "module": parsed.module,
                            "kind": item.kind,
                            "signature": signature,
                            "signature_truncated": signature_truncated,
                            "provenance": "current-source-lexical-fallback",
                        }
                    )
                    if len(results) >= limit:
                        break
        elif command == "api" and language == "haskell":
            parsed = parse_haskell(relative, body)
            for item in parsed.signatures:
                if normalized_query in item.normalized.casefold():
                    signature, signature_truncated = bounded_signature(item.text)
                    results.append(
                        {
                            "path": relative,
                            "line": item.line,
                            "name": item.name,
                            "module": parsed.module,
                            "kind": item.kind,
                            "signature": signature,
                            "signature_truncated": signature_truncated,
                            "provenance": "current-source-literal-signature-search",
                        }
                    )
                    if len(results) >= limit:
                        break
        elif command == "callers" and language == "haskell":
            parsed = parse_haskell(relative, body)
            base = query.rsplit(".", 1)[-1]
            wanted_qualified = query if "." in query else None
            declaration_lines = {
                item.line
                for item in parsed.signatures
                if item.name == base
            }
            code_lines = haskell_code_lines(body)
            for token, qualified, line_no in parsed.occurrences:
                if token != base or line_no in declaration_lines:
                    continue
                if wanted_qualified and qualified not in (wanted_qualified, base):
                    continue
                raw_line = code_lines[line_no - 1] if line_no <= len(code_lines) else ""
                if is_definition_line(raw_line, base):
                    continue
                results.append(
                    {
                        "path": relative,
                        "line": line_no,
                        "name": query,
                        "excerpt": bounded_text(raw_line),
                        "provenance": "current-source-lexical-reference",
                    }
                )
                if len(results) >= limit:
                    break
        if len(results) >= limit:
            complete = False
            flags.append("result-limit")
            break
    return results[:limit], {
        "provenance": "current-source-lexical-fallback",
        "complete": complete,
        "flags": sorted(set(flags)),
        "files_scanned": scanned,
        "bytes_scanned": total,
    }


def sql_results(
    connection: sqlite3.Connection,
    command: str,
    query: str,
    limit: int,
) -> tuple[list[dict[str, Any]], int, bool]:
    needle = normalize_space(query).casefold()
    if command == "symbol":
        if "." in query:
            module_name, symbol_name = query.rsplit(".", 1)
            predicate = "lower(s.name) = ? AND lower(coalesce(m.name, '')) = ?"
            parameters = (symbol_name.casefold(), module_name.casefold())
        else:
            predicate = "lower(s.name) = ? OR lower(coalesce(m.name, '')) = ?"
            parameters = (needle, needle)
        count = connection.execute(
            "SELECT COUNT(*) FROM symbol s LEFT JOIN module m ON m.path = s.path WHERE " + predicate,
            parameters,
        ).fetchone()[0]
        rows = connection.execute(
            "SELECT s.path, s.line, s.name, m.name, s.kind, s.signature "
            "FROM symbol s LEFT JOIN module m ON m.path = s.path "
            "WHERE " + predicate + " "
            "ORDER BY CASE s.kind WHEN 'signature' THEN 0 ELSE 1 END, s.path, s.line LIMIT ?",
            parameters + (limit + 1,),
        ).fetchall()
        output = [
            {
                "path": row[0],
                "line": row[1],
                "name": row[2],
                "module": row[3],
                "kind": row[4],
                "signature": bounded_signature(row[5])[0],
                "signature_truncated": bounded_signature(row[5])[1],
                "provenance": "current-source-signature-index",
            }
            for row in rows
        ]
        return output[:limit], count, count > limit
    if command == "api":
        count = connection.execute(
            "SELECT COUNT(*) FROM symbol WHERE instr(lower(normalized_signature), ?) > 0",
            (needle,),
        ).fetchone()[0]
        rows = connection.execute(
            "SELECT s.path, s.line, s.name, m.name, s.kind, s.signature "
            "FROM symbol s LEFT JOIN module m ON m.path = s.path "
            "WHERE instr(lower(s.normalized_signature), ?) > 0 "
            "ORDER BY CASE s.kind WHEN 'signature' THEN 0 ELSE 1 END, s.path, s.line LIMIT ?",
            (needle, limit + 1),
        ).fetchall()
        output = [
            {
                "path": row[0],
                "line": row[1],
                "name": row[2],
                "module": row[3],
                "kind": row[4],
                "signature": bounded_signature(row[5])[0],
                "signature_truncated": bounded_signature(row[5])[1],
                "provenance": "current-source-literal-signature-search",
            }
            for row in rows
        ]
        return output[:limit], count, count > limit
    if command == "callers":
        base = query.rsplit(".", 1)[-1]
        exact_qualified = query if "." in query else None
        if exact_qualified:
            count = connection.execute(
                "SELECT COUNT(*) FROM occurrence o "
                "WHERE (o.qualified = ? OR o.qualified = ?) "
                "AND NOT EXISTS (SELECT 1 FROM symbol s WHERE s.path = o.path AND s.line = o.line AND s.name = o.token)",
                (exact_qualified, base),
            ).fetchone()[0]
            rows = connection.execute(
                "SELECT o.path, o.line, o.token, o.qualified, st.body "
                "FROM occurrence o JOIN source_text st ON st.path = o.path "
                "WHERE (o.qualified = ? OR o.qualified = ?) "
                "AND NOT EXISTS (SELECT 1 FROM symbol s WHERE s.path = o.path AND s.line = o.line AND s.name = o.token) "
                "ORDER BY o.path, o.line LIMIT ?",
                (exact_qualified, base, limit + 1),
            ).fetchall()
        else:
            count = connection.execute(
                "SELECT COUNT(*) FROM occurrence o WHERE token = ? "
                "AND NOT EXISTS (SELECT 1 FROM symbol s WHERE s.path = o.path AND s.line = o.line AND s.name = o.token)",
                (base,),
            ).fetchone()[0]
            rows = connection.execute(
                "SELECT o.path, o.line, o.token, o.qualified, st.body "
                "FROM occurrence o JOIN source_text st ON st.path = o.path "
                "WHERE o.token = ? "
                "AND NOT EXISTS (SELECT 1 FROM symbol s WHERE s.path = o.path AND s.line = o.line AND s.name = o.token) "
                "ORDER BY o.path, o.line LIMIT ?",
                (base, limit + 1),
            ).fetchall()
        output: list[dict[str, Any]] = []
        for path, line, token, qualified, body in rows:
            source_lines = body.splitlines()
            line_text = source_lines[line - 1] if line <= len(source_lines) else ""
            if is_definition_line(line_text, base):
                continue
            output.append(
                {
                    "path": path,
                    "line": line,
                    "name": query,
                    "qualified_match": qualified,
                    "excerpt": bounded_text(line_text),
                    "provenance": "current-source-lexical-reference",
                }
            )
        return output[:limit], count, count > limit
    return [], 0, False


def module_matches(query: str, name: str, path: str, query_terms: set[str]) -> bool:
    exact_parts = {part.casefold() for part in name.split(".")}
    exact_parts.update(part.casefold() for part in Path(path).with_suffix("").parts)
    if query.casefold() in exact_parts:
        return True
    if len(query_terms) > 1:
        return query_terms.issubset(text_terms(name + " " + path))
    return False


def current_module_rows(connection: sqlite3.Connection, query: str, limit: int) -> tuple[list[dict[str, Any]], int]:
    module_query = query.split(".", 1)[0] if "." in query else query
    rows = connection.execute(
        "SELECT name, path, line FROM module ORDER BY name, path LIMIT ?",
        (MAX_FILES + 1,),
    ).fetchall()
    needles = text_terms(module_query)
    matched = [
        row
        for row in rows
        if module_matches(module_query, row[0], row[1], needles)
    ]
    return [
        {
            "name": row[0],
            "path": row[1],
            "line": row[2],
            "provenance": "current-source-module-index",
        }
        for row in matched[:limit]
    ], len(matched)


def current_module_fallback(inventory: Inventory, query: str, limit: int) -> Tuple[list[dict[str, Any]], bool]:
    module_query = query.split(".", 1)[0] if "." in query else query
    needles = text_terms(module_query)
    matches: list[dict[str, Any]] = []
    for source in inventory.files.values():
        if source.language != "haskell":
            continue
        parsed = parse_haskell(source.path, source.body)
        if not parsed.module:
            continue
        if module_matches(module_query, parsed.module, source.path, needles):
            matches.append(
                {
                    "name": parsed.module,
                    "path": source.path,
                    "line": parsed.module_line,
                    "provenance": "current-source-module-fallback",
                }
            )
            if len(matches) > limit:
                break
    return matches[:limit], len(matches) > limit


def current_root_counts(inventory: Inventory, catalog: dict[str, Any]) -> list[dict[str, Any]]:
    counts: list[dict[str, Any]] = []
    for source_root in catalog["source_roots"]:
        root_path = source_root.get("path", "")
        if root_path == ".":
            exact_paths = set(source_root.get("files", []))
            count = sum(1 for path in inventory.files if path in exact_paths)
        else:
            prefix = root_path.rstrip("/") + "/"
            count = sum(1 for path in inventory.files if path.startswith(prefix))
        if count:
            counts.append({"path": root_path, "source_files": count})
    return counts


def curate_map(
    root: Path,
    catalog: dict[str, Any],
    inventory: Inventory,
    query: str,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    capabilities = valid_curated_records(catalog, inventory, "capabilities")
    extensions = valid_curated_records(catalog, inventory, "extension_points")
    query_terms = text_terms(query)
    if not query_terms:
        return capabilities, extensions

    def matches(item: dict[str, Any], fields: tuple[str, ...]) -> bool:
        values: list[str] = []
        for field in fields:
            raw = item.get(field, []) if field != "name" else [item.get(field, "")]
            if isinstance(raw, list):
                values.extend(str(value) for value in raw)
            else:
                values.append(str(raw))
        terms = {
            token
            for value in values
            for token in text_terms(value)
        }
        return query_terms.issubset(terms)

    selected_capabilities = [item for item in capabilities if matches(item, ("name", "aliases", "paths"))]
    selected_extensions = [item for item in extensions if matches(item, ("name", "aliases", "paths", "symbols"))]
    return selected_capabilities, selected_extensions


def status_value(connection: sqlite3.Connection, refresh_info: dict[str, Any], inventory: Inventory) -> dict[str, Any]:
    file_count = connection.execute("SELECT COUNT(*) FROM file").fetchone()[0]
    module_count = connection.execute("SELECT COUNT(*) FROM module").fetchone()[0]
    symbol_count = connection.execute("SELECT COUNT(*) FROM symbol").fetchone()[0]
    return {
        "cache": "current" if refresh_info.get("stable") else "fallback",
        "source_files": file_count,
        "modules": module_count,
        "signatures": symbol_count,
        "source_digest": inventory.digest,
        "indexed_digest": read_meta(connection, "source_digest"),
        "last_refresh_utc": read_meta(connection, "last_refresh_utc"),
        "coverage_flags": inventory.flags,
        "callers": "lexical current-source references; incomplete by design",
        "type_search": "literal declared-signature text; no unification",
        "compiler_artifacts": {
            "adapter": "optional local hiedb executable",
            "indexed_files": read_meta(connection, "hiedb_artifact_count"),
            "source_build_correspondence": "unknown; HIE results are historical or freshness-unknown evidence",
        },
    }


def render_json(value: dict[str, Any]) -> None:
    print(json.dumps(value, ensure_ascii=False, indent=2))


def render_text(command: str, value: dict[str, Any]) -> None:
    coverage = value.get("coverage", {})
    provenance = value.get("provenance", "current-source")
    print("Codemap " + command + " (" + str(provenance) + ")")
    if coverage:
        state = "complete inventory" if coverage.get("complete") else "incomplete coverage"
        print("Coverage: " + state)
        flags = coverage.get("flags", [])
        if flags:
            print("Coverage details: " + ", ".join(flags))
    if value.get("note"):
        print("Note: " + value["note"])
    if command == "status":
        for key, item in value.get("status", {}).items():
            print(str(key).replace("_", " ") + ": " + str(item))
        return
    if command == "refresh":
        info = value.get("refresh", {})
        print("Source files: " + str(info.get("source_files", 0)))
        print("Changed: " + str(info.get("changed", 0)) + ", deleted: " + str(info.get("deleted", 0)))
        print("Current digest: " + str(info.get("source_digest", "unavailable")))
        compiler = info.get("compiler_artifacts")
        if isinstance(compiler, dict):
            print("Compiler artifacts: " + str(compiler.get("state", "unknown")))
            if compiler.get("reason"):
                print("Compiler artifact detail: " + str(compiler["reason"]))
        return
    compiler_status = value.get("compiler_status")
    if compiler_status:
        print("Compiler artifact status: " + str(compiler_status.get("state", "unknown")))
        if compiler_status.get("reason"):
            print("Compiler artifact detail: " + str(compiler_status["reason"]))
    for group_name in ("capabilities", "extension_points", "modules", "results", "compiler_results", "roots"):
        rows = value.get(group_name)
        if not rows:
            continue
        print(group_name.replace("_", " ").title() + ":")
        for row in rows:
            if group_name == "capabilities":
                print("- " + row.get("name", row.get("id", "capability")) + " [" + ", ".join(row.get("paths", [])) + "]")
                if row.get("summary"):
                    print("  " + row["summary"])
            elif group_name == "extension_points":
                symbols = ", ".join(row.get("symbols", []))
                print("- " + row.get("name", row.get("id", "extension point")) + " [" + ", ".join(row.get("paths", [])) + "]")
                if symbols:
                    print("  Symbols: " + symbols)
                if row.get("hint"):
                    print("  " + row["hint"])
            elif group_name == "modules":
                print("- " + str(row.get("name")) + "  " + str(row.get("path")) + ":" + str(row.get("line")))
            elif group_name == "compiler_results":
                location = str(row.get("module")) + ":" + str(row.get("line")) + ":" + str(row.get("column_start"))
                print("- " + location + "  historical or freshness-unknown HIE result")
            elif group_name == "roots":
                print("- " + str(row.get("path")) + "  " + str(row.get("source_files")) + " source files")
            else:
                prefix = str(row.get("path", "")) + ":" + str(row.get("line", ""))
                label = str(row.get("module") or row.get("qualified_match") or row.get("name", "match"))
                print("- " + prefix + "  " + label)
                if row.get("signature"):
                    for line in row["signature"].splitlines():
                        print("    " + bounded_text(line, 500))
                if row.get("excerpt"):
                    print("    " + row["excerpt"])
    if value.get("truncated"):
        print("Results truncated at the requested limit.")


def run_command(root: Path, command: str, query: str, limit: int, as_json: bool) -> int:
    deadline = Deadline.start()
    try:
        catalog = read_catalog(root)
        connection, inventory, refresh_info = refresh(root, catalog, deadline)
        coverage = {
            "complete": bool(inventory.complete and refresh_info.get("stable")),
            "flags": sorted(set(inventory.flags + refresh_info.get("flags", []))),
        }
        if not refresh_info.get("stable") and refresh_info.get("reason"):
            coverage["flags"] = sorted(set(coverage["flags"] + [refresh_info["reason"]]))
        provenance = (
            "current-source-index"
            if refresh_info.get("stable")
            else "current-source-lexical-fallback"
        )
        note = ""
        result_rows: list[dict[str, Any]] = []
        truncated = False
        value: dict[str, Any] = {"provenance": provenance, "coverage": coverage}
        try:
            if connection is None:
                if command in ("symbol", "callers", "api"):
                    result_rows, fallback_coverage = current_source_fallback(
                        root, catalog, command, query, limit, deadline
                    )
                    coverage = fallback_coverage
                    value["coverage"] = coverage
                    provenance = fallback_coverage["provenance"]
                    value["provenance"] = provenance
                    truncated = "result-limit" in fallback_coverage["flags"]
                elif command == "map":
                    capabilities, extensions = curate_map(root, catalog, inventory, query)
                    value["capabilities"] = capabilities
                    value["extension_points"] = extensions
                    if not query:
                        value["roots"] = current_root_counts(inventory, catalog)
                    else:
                        modules, truncated = current_module_fallback(inventory, query, min(limit, 20))
                        value["modules"] = modules
                    value["truncated"] = truncated
                    note = "Curated records and module paths are read from the current source inventory."
                if refresh_info.get("reason"):
                    note = note + " " + refresh_info["reason"] if note else "Cache unavailable or source changed during refresh. " + refresh_info["reason"]
                elif not note:
                    note = "Cache unavailable or source changed during refresh. Results use bounded current-source search."
            else:
                if command in ("symbol", "callers", "api"):
                    result_rows, total, truncated = sql_results(connection, command, query, limit)
                    if command in ("symbol", "callers"):
                        modules, module_total = current_module_rows(connection, query, min(limit, 20))
                        value["modules"] = modules
                        truncated = truncated or module_total > min(limit, 20)
                    if command == "callers":
                        note = "Lexical reference candidates are incomplete and may include non-call references."
                    elif command == "api":
                        note = "This is literal declared-signature text search. It does not perform type unification."
                elif command == "map":
                    capabilities, extensions = curate_map(root, catalog, inventory, query)
                    value["capabilities"] = capabilities
                    value["extension_points"] = extensions
                    roots = current_root_counts(inventory, catalog)
                    if query:
                        modules, module_total = current_module_rows(connection, query, min(limit, 20))
                        value["modules"] = modules
                        truncated = module_total > len(modules)
                    else:
                        value["roots"] = roots
                    note = "Curated records are filtered against the current maintained source inventory."
                elif command == "status":
                    value["status"] = status_value(connection, refresh_info, inventory)
                elif command == "refresh":
                    pass
            if command in ("symbol", "callers"):
                if connection is None:
                    compiler_info = {
                        "status": {
                            "state": "skipped",
                            "reason": "current-source cache was unavailable",
                            "source_build_correspondence": "unknown",
                        },
                        "results": [],
                        "truncated": False,
                    }
                else:
                    source_revalidation_reserve = deadline.remaining() / 2
                    compiler_deadline = Deadline(
                        deadline.expires_at - source_revalidation_reserve
                    )
                    compiler_info = compiler_lookup(
                        root, connection, command, query, limit, compiler_deadline
                    )
                if connection is not None:
                    final_inventory = scan_inventory(root, catalog, deadline, include_body=False)
                    deadline.check()
                    if not final_inventory.complete or final_inventory.digest != inventory.digest:
                        result_rows, fallback_coverage = current_source_fallback(
                            root, catalog, command, query, limit, deadline
                        )
                        deadline.check()
                        fallback_coverage["complete"] = bool(
                            fallback_coverage["complete"] and final_inventory.complete
                        )
                        fallback_coverage["flags"] = sorted(
                            set(fallback_coverage["flags"] + final_inventory.flags)
                        )
                        value["modules"] = []
                        coverage = fallback_coverage
                        provenance = fallback_coverage["provenance"]
                        truncated = "result-limit" in fallback_coverage["flags"]
                        if note:
                            note += " "
                        note += "Source changed during compiler lookup. Current-source results use a bounded lexical scan."
                value["compiler_results"] = compiler_info["results"]
                value["compiler_status"] = compiler_info["status"]
                value["current_source_truncated"] = truncated
                value["compiler_results_truncated"] = compiler_info["truncated"]
                truncated = truncated or compiler_info["truncated"]
                if compiler_info["results"]:
                    note = (note + " " if note else "") + "Compiler results are shown separately and have unknown source and build correspondence."
                elif compiler_info["status"].get("reason") and not note:
                    note = "Compiler artifact lookup was unavailable. Current-source results remain available."
            if command == "refresh":
                value["refresh"] = {
                    "source_files": len(inventory.files),
                    "changed": refresh_info.get("changed", 0),
                    "deleted": refresh_info.get("deleted", 0),
                    "source_digest": inventory.digest if inventory.complete else None,
                    "cache": "current" if refresh_info.get("stable") else "fallback",
                    "reason": refresh_info.get("reason"),
                }
            if command in ("status", "refresh") and connection is not None:
                compiler_status = ensure_hiedb_index(root, connection, deadline)
                if command == "status":
                    value["status"]["compiler_artifacts"] = compiler_status
                else:
                    value["refresh"]["compiler_artifacts"] = compiler_status
            if command in ("symbol", "callers", "api"):
                value["results"] = result_rows
                value["truncated"] = truncated
                if not result_rows and not note:
                    note = "No matching current-source records were found."
            if command == "map":
                value["truncated"] = truncated
                if query and not value.get("capabilities") and not value.get("extension_points") and not value.get("modules"):
                    note = "No curated or module records matched; search source with symbol or api."
            if command == "status" and connection is None:
                value["status"] = {
                    "cache": "fallback",
                    "source_files": len(inventory.files),
                    "source_digest": inventory.digest if inventory.complete else "unavailable",
                    "reason": refresh_info.get("reason", "cache unavailable"),
                    "coverage_flags": coverage["flags"],
                    "callers": "lexical current-source references; incomplete by design",
                    "type_search": "literal declared-signature text; no unification",
                    "compiler_artifacts": {
                        "state": "skipped",
                        "reason": "current-source cache was unavailable",
                        "source_build_correspondence": "unknown",
                    },
                }
            value["provenance"] = provenance
            value["coverage"] = coverage
            if note:
                value["note"] = note
            if command in ("symbol", "callers") and connection is not None:
                deadline.check()
            if as_json:
                render_json(value)
            else:
                render_text(command, value)
            return 0
        finally:
            if connection is not None:
                connection.close()
    except sqlite3.Error as error:
        try:
            fallback_inventory = scan_inventory(root, catalog, deadline, include_body=True)
            value = {
                "provenance": "current-source-lexical-fallback",
                "coverage": {
                    "complete": False,
                    "flags": fallback_inventory.flags + ["cache query failed: " + str(error)],
                },
                "note": "The SQLite query failed. This answer uses current-source fallback.",
            }
            if command in ("symbol", "callers", "api"):
                results, fallback_coverage = current_source_fallback(
                    root, catalog, command, query, limit, deadline
                )
                value["results"] = results
                value["coverage"] = fallback_coverage
                value["truncated"] = "result-limit" in fallback_coverage["flags"]
            elif command == "map":
                capabilities, extensions = curate_map(root, catalog, fallback_inventory, query)
                value["capabilities"] = capabilities
                value["extension_points"] = extensions
                if query:
                    modules, truncated = current_module_fallback(fallback_inventory, query, min(limit, 20))
                    value["modules"] = modules
                    value["truncated"] = truncated
                else:
                    value["roots"] = current_root_counts(fallback_inventory, catalog)
                    value["truncated"] = False
            elif command == "status":
                value["status"] = {
                    "cache": "fallback",
                    "source_files": len(fallback_inventory.files),
                    "source_digest": fallback_inventory.digest if fallback_inventory.complete else "unavailable",
                    "reason": "cache query failed: " + str(error),
                    "callers": "lexical current-source references; incomplete by design",
                    "type_search": "literal declared-signature text; no unification",
                    "compiler_artifacts": {
                        "state": "skipped",
                        "reason": "current-source cache query failed",
                        "source_build_correspondence": "unknown",
                    },
                }
            elif command == "refresh":
                value["refresh"] = {
                    "source_files": len(fallback_inventory.files),
                    "cache": "fallback",
                    "source_digest": fallback_inventory.digest if fallback_inventory.complete else None,
                    "reason": "cache query failed: " + str(error),
                }
            if as_json:
                render_json(value)
            else:
                render_text(command, value)
            return 0
        except (CodemapError, DeadlineExceeded) as fallback_error:
            failure = {
                "provenance": "unavailable",
                "coverage": {"complete": False, "flags": [str(fallback_error)]},
                "note": "No current-source answer was available within the foreground deadline.",
            }
            if as_json:
                render_json(failure)
            else:
                render_text(command, failure)
            return 0
    except (CodemapError, DeadlineExceeded) as error:
        failure = {
            "provenance": "unavailable",
            "coverage": {"complete": False, "flags": [str(error)]},
            "note": "No current-source answer was available within the foreground deadline.",
        }
        if as_json:
            render_json(failure)
        else:
            render_text(command, failure)
        return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="codemap.py",
        description="Search current maintained source through a worktree-local SQLite index.",
    )
    subparsers = parser.add_subparsers(dest="command", required=True)
    for name in ("map", "symbol", "callers", "api"):
        subparser = subparsers.add_parser(name)
        if name == "map":
            subparser.add_argument("query", nargs="*", help="capability, extension point, module, or source area")
        else:
            subparser.add_argument("query", nargs="+", help="literal symbol or signature text")
        subparser.add_argument("--limit", type=int, default=20)
        subparser.add_argument("--json", action="store_true")
    for name in ("refresh", "status"):
        subparser = subparsers.add_parser(name)
        subparser.add_argument("--json", action="store_true")
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    limit = max(1, min(getattr(args, "limit", 20), MAX_RESULTS))
    try:
        root = resolve_root(__file__)
    except CodemapError as error:
        print(str(error), file=sys.stderr)
        return 2
    query_value = " ".join(getattr(args, "query", []))
    return run_command(root, args.command, query_value, limit, args.json)


if __name__ == "__main__":
    raise SystemExit(main())
