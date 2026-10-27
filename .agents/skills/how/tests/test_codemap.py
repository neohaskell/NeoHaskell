import contextlib
import importlib.util
import io
import json
import os
import re
import shutil
import socket
import sqlite3
import subprocess
import tempfile
import threading
import time
import unittest
from pathlib import Path
from unittest import mock


SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "codemap.py"
SPEC = importlib.util.spec_from_file_location("how_codemap", SCRIPT)
CODEMAP = importlib.util.module_from_spec(SPEC)
import sys
sys.modules[SPEC.name] = CODEMAP
SPEC.loader.exec_module(CODEMAP)


TASK_SOURCE = """module Task where
yield
  :: value
  -> Task _ value
yield value = value
"""


def catalog_data():
    return {
        "schema_version": 1,
        "source_roots": [{"path": "core", "extensions": [".hs"]}],
        "capabilities": [
            {
                "id": "core",
                "name": "Core",
                "aliases": ["Task"],
                "paths": ["core"],
            },
            {
                "id": "stale",
                "name": "Stale record",
                "aliases": ["missing"],
                "paths": ["core/deleted"],
            },
        ],
        "extension_points": [
            {
                "id": "yield",
                "name": "Yield function",
                "aliases": ["yield"],
                "paths": ["core/Task.hs"],
                "symbols": ["yield"],
                "hint": "Yield a value.",
            },
            {
                "id": "stale-extension",
                "name": "Stale extension",
                "aliases": ["missing"],
                "paths": ["core/deleted.hs"],
                "symbols": ["removed"],
                "hint": "No longer present.",
            },
        ],
    }


def make_repo(root, sources):
    catalog_path = root / CODEMAP.CATALOG_RELATIVE_PATH
    catalog_path.parent.mkdir(parents=True, exist_ok=True)
    catalog_path.write_text(json.dumps(catalog_data()), encoding="utf-8")
    for relative, body in sources.items():
        path = root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(body, encoding="utf-8")
    subprocess.run(["git", "init", "--quiet", str(root)], check=True)
    subprocess.run(["git", "-C", str(root), "add", "--all"], check=True)
    subprocess.run(
        [
            "git",
            "-C",
            str(root),
            "-c",
            "user.name=Codemap Fixture",
            "-c",
            "user.email=codemap-fixture@example.invalid",
            "commit",
            "--quiet",
            "-m",
            "fixture",
        ],
        check=True,
    )


class CodemapFixtures(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name) / "repo"
        self.root.mkdir()
        make_repo(
            self.root,
            {
                "core/Task.hs": TASK_SOURCE,
                "core/Caller.hs": """module Caller where
run = Task.yield input
again = Task.yield input
third = Task.yield input
-- Task.yield in a comment
quoted = "Task.yield"
""",
                "core/A.hs": """module Duplicate where
public :: Unit
public = unit
""",
                "core/B.hs": """module Duplicate where
public :: Unit
public = unit
""",
                "core/Service.hs": """module Service where\ndata Service event = Service
  { execute ::
      EventStore event
      -> Task Text Unit,
    notify :: Task Text Unit
  }
""",
                "core/Old.hs": """module Old where
oldSymbol :: Unit
oldSymbol = unit
""",
                ".gitignore": "core/Ignored.hs\n",
            },
        )

    def tearDown(self):
        self.temp.cleanup()

    def invoke(self, command, query="", limit=20):
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            result = CODEMAP.run_command(self.root, command, query, limit, True)
        self.assertEqual(result, 0)
        return json.loads(output.getvalue())

    def test_skill_frontmatter_and_local_links_are_valid(self):
        skill_dir = SCRIPT.parent.parent
        skill_text = (skill_dir / "SKILL.md").read_text(encoding="utf-8")
        self.assertTrue(skill_text.startswith("---\n"))
        frontmatter = skill_text.split("---", 2)[1]
        self.assertRegex(frontmatter, r"(?m)^name:\s*how\s*$")
        self.assertRegex(frontmatter, r"(?m)^description:\s*.+$")
        for target in re.findall(r"\]\(([^)]+)\)", skill_text):
            if "://" in target:
                continue
            self.assertTrue((skill_dir / target.split("#", 1)[0]).is_file(), target)

    def test_multiline_signature_duplicate_modules_and_curated_filter(self):
        symbol = self.invoke("symbol", "Task.yield")
        self.assertEqual(symbol["results"][0]["path"], "core/Task.hs")
        self.assertIn("Task _ value", symbol["results"][0]["signature"])
        self.assertEqual(
            [item["path"] for item in symbol["modules"] if item["name"] == "Task"],
            ["core/Task.hs"],
        )
        duplicates = self.invoke("symbol", "public")
        self.assertEqual(
            {item["path"] for item in duplicates["results"]},
            {"core/A.hs", "core/B.hs"},
        )
        duplicate_modules = self.invoke("map", "Duplicate")
        self.assertEqual(
            {item["path"] for item in duplicate_modules["modules"]},
            {"core/A.hs", "core/B.hs"},
        )
        service_fields = self.invoke("symbol", "execute")
        self.assertTrue(
            any(item["path"] == "core/Service.hs" and item["kind"] == "indented-signature" for item in service_fields["results"])
        )
        mapped = self.invoke("map")
        self.assertEqual([item["id"] for item in mapped["capabilities"]], ["core"])
        self.assertEqual([item["id"] for item in mapped["extension_points"]], ["yield"])

    def test_callers_are_current_lexical_candidates(self):
        result = self.invoke("callers", "Task.yield")
        self.assertEqual(result["provenance"], "current-source-index")
        self.assertEqual(
            [(item["path"], item["line"]) for item in result["results"]],
            [("core/Caller.hs", 2), ("core/Caller.hs", 3), ("core/Caller.hs", 4)],
        )
        self.assertIn("incomplete", result["note"])

    def test_signature_search_is_literal_and_parameterized(self):
        result = self.invoke("api", "Task _ value")
        self.assertTrue(any(item["name"] == "yield" for item in result["results"]))
        injection = self.invoke("symbol", "' OR 1=1 --")
        self.assertEqual(injection["results"], [])

    def test_same_size_edit_is_found_without_mtime_change(self):
        first = self.invoke("symbol", "Old.oldSymbol")
        self.assertTrue(first["results"])
        path = self.root / "core/Old.hs"
        old_stat = path.stat()
        original = path.read_text(encoding="utf-8")
        updated = original.replace("Old", "New").replace("oldSymbol", "newSymbol")
        self.assertEqual(len(original), len(updated))
        path.write_text(updated, encoding="utf-8")
        os.utime(path, ns=(old_stat.st_atime_ns, old_stat.st_mtime_ns))
        old_result = self.invoke("symbol", "Old.oldSymbol")
        new_result = self.invoke("symbol", "New.newSymbol")
        self.assertEqual(old_result["results"], [])
        self.assertEqual(new_result["results"][0]["module"], "New")

    def test_untracked_nonignored_source_is_indexed_and_ignored_source_is_skipped(self):
        new_path = self.root / "core/Untracked.hs"
        new_path.write_text("module Untracked where\nnewSymbol :: Unit\n", encoding="utf-8")
        ignored_path = self.root / "core/Ignored.hs"
        ignored_path.write_text("module Ignored where\nignoredSymbol :: Unit\n", encoding="utf-8")
        new_result = self.invoke("symbol", "newSymbol")
        ignored_result = self.invoke("symbol", "ignoredSymbol")
        self.assertEqual(new_result["results"][0]["path"], "core/Untracked.hs")
        self.assertEqual(ignored_result["results"], [])

    def test_rename_and_deletion_cascade_stale_rows(self):
        old_path = self.root / "core/Old.hs"
        new_path = self.root / "core/Renamed.hs"
        old_path.rename(new_path)
        new_path.write_text(
            "module Renamed where\nnewSymbol :: Unit\nnewSymbol = unit\n",
            encoding="utf-8",
        )
        old_result = self.invoke("symbol", "Old.oldSymbol")
        new_result = self.invoke("symbol", "Renamed.newSymbol")
        self.assertEqual(old_result["results"], [])
        self.assertEqual(new_result["results"][0]["path"], "core/Renamed.hs")
        status = self.invoke("status")
        self.assertEqual(status["status"]["cache"], "current")

    def test_source_race_rolls_back_and_searches_fresh_source(self):
        original_scan = CODEMAP.scan_inventory
        calls = 0

        def changing_scan(root, catalog, deadline, include_body=True):
            nonlocal calls
            calls += 1
            if calls == 2:
                path = self.root / "core/Task.hs"
                path.write_text(
                    "module Task where\nfreshSymbol :: Unit\nfreshSymbol = unit\n",
                    encoding="utf-8",
                )
            return original_scan(root, catalog, deadline, include_body)

        output = io.StringIO()
        with mock.patch.object(CODEMAP, "scan_inventory", side_effect=changing_scan):
            with contextlib.redirect_stdout(output):
                CODEMAP.run_command(self.root, "symbol", "freshSymbol", 20, True)
        value = json.loads(output.getvalue())
        self.assertEqual(value["provenance"], "current-source-lexical-fallback")
        self.assertTrue(any(item["name"] == "freshSymbol" for item in value["results"]))
        self.assertIn("changed during refresh", value["note"])
        self.assertGreaterEqual(calls, 3)

    def test_locked_database_falls_back_without_quarantining_it(self):
        self.invoke("symbol", "yield")
        database = self.root / CODEMAP.DATABASE_RELATIVE_PATH
        before = database.stat().st_ino
        locker = sqlite3.connect(str(database), timeout=0.1, isolation_level=None)
        locker.execute("BEGIN EXCLUSIVE")
        started = time.monotonic()
        try:
            result = self.invoke("symbol", "yield")
            callers = self.invoke("callers", "Task.yield", limit=1)
            mapped = self.invoke("map", "Duplicate")
        finally:
            locker.rollback()
            locker.close()
        elapsed = time.monotonic() - started
        self.assertLess(elapsed, 3.0)
        self.assertEqual(result["provenance"], "current-source-lexical-fallback")
        self.assertTrue(result["results"])
        self.assertTrue(callers["truncated"])
        self.assertEqual(
            {item["path"] for item in mapped["modules"]},
            {"core/A.hs", "core/B.hs"},
        )
        self.assertEqual(database.stat().st_ino, before)
        self.assertFalse(list(database.parent.glob("codemap.corrupt-*")))

    def test_corrupt_database_is_quarantined_and_rebuilt(self):
        self.invoke("symbol", "yield")
        database = self.root / CODEMAP.DATABASE_RELATIVE_PATH
        database.write_bytes(b"broken sqlite cache")
        result = self.invoke("symbol", "yield")
        self.assertTrue(result["results"])
        self.assertEqual(result["provenance"], "current-source-index")
        self.assertTrue(list(database.parent.glob("codemap.corrupt-*")))
        with sqlite3.connect(str(database)) as connection:
            self.assertEqual(connection.execute("PRAGMA quick_check").fetchone()[0], "ok")

    def test_cache_symlink_escape_uses_source_without_writing_outside(self):
        outside = Path(self.temp.name) / "outside"
        outside.mkdir()
        state_parent = self.root / ".agents-state"
        state_parent.symlink_to(outside, target_is_directory=True)
        result = self.invoke("symbol", "yield")
        self.assertEqual(result["provenance"], "current-source-lexical-fallback")
        self.assertTrue(result["results"])
        self.assertEqual(list(outside.iterdir()), [])

    def test_expired_foreground_deadline_returns_promptly(self):
        expired = CODEMAP.Deadline(time.monotonic() - 0.01)
        started = time.monotonic()
        with mock.patch.object(CODEMAP.Deadline, "start", return_value=expired):
            result = self.invoke("symbol", "yield")
        self.assertLess(time.monotonic() - started, 0.2)
        self.assertEqual(result["provenance"], "unavailable")
        self.assertFalse(result["coverage"]["complete"])

    def test_concurrent_refreshes_publish_valid_index(self):
        script = SCRIPT
        env = dict(os.environ)
        env["CODEMAP_ROOT"] = str(self.root)

        def run_process():
            return subprocess.run(
                [sys.executable, "-B", str(script), "symbol", "yield", "--json"],
                env=env,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                timeout=8,
            )

        barrier = threading.Barrier(2)
        results = []

        def wait_then_run():
            barrier.wait()
            results.append(run_process())

        workers = [threading.Thread(target=wait_then_run) for _ in range(2)]
        for worker in workers:
            worker.start()
        for worker in workers:
            worker.join(timeout=10)
        self.assertTrue(all(not worker.is_alive() for worker in workers))
        self.assertEqual(len(results), 2)
        for result in results:
            self.assertEqual(result.returncode, 0, result.stderr)
            value = json.loads(result.stdout)
            self.assertTrue(value["results"])
        status = self.invoke("status")
        self.assertEqual(status["status"]["cache"], "current")

    def test_lookup_runs_only_git_and_makes_no_network_request(self):
        real_run = CODEMAP.subprocess.run
        commands = []

        def guarded_run(command, *args, **kwargs):
            commands.append(command)
            self.assertEqual(Path(command[0]).name, "git")
            return real_run(command, *args, **kwargs)

        def reject_connect(*args, **kwargs):
            raise AssertionError("codemap attempted a network connection")

        with mock.patch.object(CODEMAP.shutil, "which", return_value=None):
            with mock.patch.object(CODEMAP.subprocess, "run", side_effect=guarded_run):
                with mock.patch.object(socket.socket, "connect", side_effect=reject_connect):
                    result = self.invoke("symbol", "yield")
        self.assertTrue(result["results"])
        self.assertTrue(commands)
        self.assertTrue(all(Path(command[0]).name == "git" for command in commands))

    def test_hiedb_results_stay_separate_and_reindex_after_artifact_deletion(self):
        hie_file = self.root / "dist-newstyle/build/hie/Task.hie"
        hie_file.parent.mkdir(parents=True)
        hie_file.write_bytes(b"first artifact")
        fake_hiedb = self.root / "fake-hiedb"
        fake_hiedb.write_text(
            "#!/usr/bin/env python3\n"
            "import json, os, pathlib, sys\n"
            "args = sys.argv[1:]\n"
            "root = pathlib.Path(os.environ['CODEMAP_ROOT'])\n"
            "log = root / '.agents-state/how/hiedb-calls.jsonl'\n"
            "log.parent.mkdir(parents=True, exist_ok=True)\n"
            "with log.open('a', encoding='utf-8') as stream: stream.write(json.dumps(args) + '\\n')\n"
            "database = pathlib.Path(args[args.index('--database') + 1])\n"
            "if 'index' in args or 'ls' in args: database.write_bytes(b'fixture db')\n"
            "elif 'name-def' in args: print('Task:5:1-5:6')\n"
            "elif 'name-refs' in args and list((root / 'dist-newstyle').rglob('*.hie')):\n"
            " print('Caller:8:3-8:8')\n"
            " print('AnotherCaller:9:3-9:8')\n",
            encoding="utf-8",
        )
        fake_hiedb.chmod(0o755)
        with mock.patch.dict(
            os.environ,
            {"CODEMAP_HIEDB": str(fake_hiedb), "CODEMAP_ROOT": str(self.root)},
        ):
            symbol = self.invoke("symbol", "Task.yield")
            callers = self.invoke("callers", "Task.yield", limit=1)
            hie_file.unlink()
            after_delete = self.invoke("callers", "Task.yield", limit=1)
        self.assertEqual(symbol["results"][0]["provenance"], "current-source-signature-index")
        self.assertEqual(symbol["compiler_results"][0]["module"], "Task")
        self.assertEqual(
            symbol["compiler_results"][0]["provenance"],
            "compiler-artifact-historical-or-unknown",
        )
        self.assertEqual(
            symbol["compiler_status"]["source_build_correspondence"],
            "unknown; HIE results are historical or freshness-unknown evidence",
        )
        self.assertEqual(callers["compiler_results"][0]["module"], "Caller")
        self.assertTrue(callers["current_source_truncated"])
        self.assertTrue(callers["compiler_results_truncated"])
        self.assertTrue(callers["truncated"])
        self.assertTrue(after_delete["results"])
        self.assertEqual(after_delete["compiler_results"], [])
        calls = [json.loads(line) for line in (self.root / ".agents-state/how/hiedb-calls.jsonl").read_text().splitlines()]
        self.assertTrue(any("--reindex" in call and "ls" in call for call in calls))

    def test_hiedb_deadline_keeps_current_source_results(self):
        self.invoke("symbol", "Task.yield")
        hie_file = self.root / "dist-newstyle/build/hie/Task.hie"
        hie_file.parent.mkdir(parents=True)
        hie_file.write_bytes(b"artifact")
        marker = self.root / ".agents-state/how/hiedb-started"
        fake_hiedb = self.root / "fake-hiedb"
        fake_hiedb.write_text(
            "#!/bin/sh\n"
            "printf started > \"$CODEMAP_ROOT/.agents-state/how/hiedb-started\"\n"
            "sleep 2\n",
            encoding="utf-8",
        )
        fake_hiedb.chmod(0o755)
        original_scan = CODEMAP.scan_inventory
        delayed_final_scan = False

        def delay_final_scan(root, catalog, deadline, include_body=True):
            nonlocal delayed_final_scan
            if marker.exists() and not delayed_final_scan:
                delayed_final_scan = True
                time.sleep(0.1)
            return original_scan(root, catalog, deadline, include_body)

        started = time.monotonic()
        with mock.patch.object(CODEMAP, "TOTAL_DEADLINE_SECONDS", 2.0):
            with mock.patch.object(CODEMAP, "scan_inventory", side_effect=delay_final_scan):
                with mock.patch.dict(
                    os.environ,
                    {"CODEMAP_HIEDB": str(fake_hiedb), "CODEMAP_ROOT": str(self.root)},
                ):
                    result = self.invoke("symbol", "Task.yield")
        self.assertLess(time.monotonic() - started, 2.5)
        self.assertTrue(delayed_final_scan)
        self.assertEqual(result["provenance"], "current-source-index")
        self.assertTrue(result["results"])
        self.assertEqual(result["compiler_results"], [])
        self.assertEqual(result["compiler_status"]["state"], "stale-or-incomplete")
        self.assertEqual(result["compiler_status"]["reason"], "HIE index deadline exceeded")

    def test_output_revalidation_deadline_returns_incomplete_result(self):
        original_scan = CODEMAP.scan_inventory
        calls = 0

        def expiring_scan(root, catalog, deadline, include_body=True):
            nonlocal calls
            calls += 1
            if calls == 3:
                raise CODEMAP.DeadlineExceeded("foreground deadline exceeded")
            return original_scan(root, catalog, deadline, include_body)

        output = io.StringIO()
        with mock.patch.object(CODEMAP, "scan_inventory", side_effect=expiring_scan):
            with contextlib.redirect_stdout(output):
                CODEMAP.run_command(self.root, "symbol", "Task.yield", 20, True)
        result = json.loads(output.getvalue())
        self.assertEqual(calls, 3)
        self.assertEqual(result["provenance"], "unavailable")
        self.assertFalse(result["coverage"]["complete"])
        self.assertNotIn("results", result)

    def test_source_edit_during_hiedb_query_uses_fresh_fallback_and_unchanged_query_keeps_index(self):
        original = (self.root / "core/Task.hs").read_text(encoding="utf-8")
        hie_file = self.root / "dist-newstyle/build/hie/Task.hie"
        hie_file.parent.mkdir(parents=True)
        hie_file.write_bytes(b"fixture artifact")
        fake_hiedb = self.root / "fake-hiedb"
        fake_hiedb.write_text(
            "#!/usr/bin/env python3\n"
            "import os,pathlib,sys\n"
            "args=sys.argv[1:]\n"
            "database=pathlib.Path(args[args.index('--database')+1])\n"
            "if 'index' in args or 'ls' in args:\n"
            " database.write_bytes(b'fixture database')\n"
            "else:\n"
            " pathlib.Path(os.environ['CODEMAP_ROOT'], 'hiedb.pid').write_text(str(os.getpid()))\n"
            " if os.environ.get('CODEMAP_FAKE_EDIT') == '1':\n"
            "  pathlib.Path(os.environ['CODEMAP_ROOT'], 'core/Task.hs').write_text('module Task where\\nyield :: After\\nyield = value\\n', encoding='utf-8')\n"
            " print('Task:2:1-2:6')\n",
            encoding="utf-8",
        )
        fake_hiedb.chmod(0o755)
        base_environment = {"CODEMAP_HIEDB": str(fake_hiedb), "CODEMAP_ROOT": str(self.root)}
        with mock.patch.dict(os.environ, {**base_environment, "CODEMAP_FAKE_EDIT": "1"}):
            changed = self.invoke("symbol", "Task.yield")
        self.assertIn("After", (self.root / "core/Task.hs").read_text(encoding="utf-8"))
        self.assertEqual(changed["provenance"], "current-source-lexical-fallback")
        self.assertTrue(changed["coverage"]["complete"])
        self.assertIn("After", changed["results"][0]["signature"])
        self.assertEqual(changed["compiler_results"][0]["provenance"], "compiler-artifact-historical-or-unknown")
        with self.assertRaises(ProcessLookupError):
            os.kill(int((self.root / "hiedb.pid").read_text()), 0)

        (self.root / "core/Task.hs").write_text(original, encoding="utf-8")
        with mock.patch.dict(os.environ, {**base_environment, "CODEMAP_FAKE_EDIT": "0"}):
            unchanged = self.invoke("symbol", "Task.yield")
        self.assertEqual(unchanged["provenance"], "current-source-index")
        self.assertTrue(unchanged["coverage"]["complete"])
        self.assertIn("Task _ value", unchanged["results"][0]["signature"])
        self.assertEqual(unchanged["compiler_results"][0]["provenance"], "compiler-artifact-historical-or-unknown")
        with self.assertRaises(ProcessLookupError):
            os.kill(int((self.root / "hiedb.pid").read_text()), 0)

if __name__ == "__main__":
    unittest.main()
