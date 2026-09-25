#!/usr/bin/env python3
"""Disposable executable/transport fixtures; no existing Haskell expectations change."""
import importlib.machinery
import importlib.util
import json
import os
import re
import sys
import textwrap
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
loader = importlib.machinery.SourceFileLoader('components', str(ROOT / 'scripts/nix-components'))
spec = importlib.util.spec_from_loader(loader.name, loader)
c = importlib.util.module_from_spec(spec)
loader.exec_module(c)


class Components(unittest.TestCase):
    def test_store_path_rejects_missing_and_injected_paths(self):
        for value in ['', '/tmp/binary', '/nix/store/' + 'a'*32 + '-out/bin/test', '/nix/store/' + 'a'*32 + '-out\nother', None]:
            with self.subTest(value=value), self.assertRaises(ValueError):
                c.store_path(value)
        self.assertEqual(c.store_path('/nix/store/' + 'a'*32 + '-out'), '/nix/store/' + 'a'*32 + '-out')

    def test_artifact_revision_must_match(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'root.txt').write_text('/nix/store/' + 'a'*32 + '-bundle\n')
            (root / 'producer.json').write_text(json.dumps({'revision': 'old'}))
            with patch.object(c, 'output', return_value='new'), self.assertRaisesRegex(ValueError, 'different checkout'):
                c.load_bundle(root)
            with patch.object(c, 'output', return_value='old'):
                self.assertTrue(c.load_bundle(root).endswith('-bundle'))

    def test_actual_executable_exit_and_nonempty_summary(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(c, 'store_path', side_effect=lambda x: x):
            root = Path(directory)
            (root / 'bin').mkdir()
            binary = root / 'bin/nhcore-test-core'
            manifest = {'components': {c.SUITES['nhcore-test-core']: {'path': str(root)}}}
            for summary, status, passes in [('3 examples, 0 failures', 0, True),
                                             ('3 examples, 1 failure', 1, False),
                                             ('3 examples, 0 failures', 17, False),
                                             ('0 examples, 0 failures', 0, False),
                                             ('not an Hspec run', 0, False)]:
                binary.write_text(f'#!/bin/sh\necho "{summary}"\nexit {status}\n')
                binary.chmod(0o755)
                with self.subTest(summary=summary, status=status):
                    if passes:
                        self.assertEqual(c.run_suite(manifest, os.environ.copy(), 'nhcore-test-core', root/'report')['examples'], 3)
                    else:
                        with self.assertRaises((ValueError, subprocess.CalledProcessError)):
                            c.run_suite(manifest, os.environ.copy(), 'nhcore-test-core', root/'report')
            binary.unlink()
            with self.assertRaisesRegex(ValueError, 'missing executable'):
                c.run_suite(manifest, os.environ.copy(), 'nhcore-test-core', root/'report')

    def test_missing_postgres_cannot_run_tests(self):
        with patch.object(c, 'command', side_effect=FileNotFoundError('pg_isready')):
            with self.assertRaises(FileNotFoundError):
                c.postgres(os.environ.copy())
        with patch.object(c, 'command', side_effect=subprocess.CalledProcessError(2, ['psql'])):
            with self.assertRaises(subprocess.CalledProcessError):
                c.postgres(os.environ.copy())

    def test_supplied_testbed_binary_never_falls_back_to_cabal(self):
        for script in ['run-tests.sh', 'cold-start-readiness.sh']:
            for binary in ['', '/does-not-exist']:
                result = subprocess.run(['bash', str(ROOT/'testbed/scripts'/script)],
                                        env={**os.environ, 'NHTESTBED_BINARY': binary},
                                        capture_output=True, text=True)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn('not an executable file', result.stderr)

    def test_fetch_disables_builds_and_propagates_cache_misses(self):
        with tempfile.TemporaryDirectory() as directory:
            root = '/nix/store/' + 'a'*32 + '-bundle'
            calls = []
            with patch.object(sys, 'argv', ['nix-components', 'fetch', '--directory', directory]), \
                 patch.object(c, 'load_bundle', return_value=root), \
                 patch.object(c, 'environment'), \
                 patch.object(c, 'command', side_effect=lambda args, **kw: calls.append(args)):
                self.assertEqual(c.main(), 0)
            self.assertEqual(calls[0], ['nix', 'copy', '--from', (Path(directory).resolve()/'cache').as_uri(),
                                       '--no-check-sigs', '--option', 'max-jobs', '0',
                                       '--option', 'builders', '', root])
            with patch.object(sys, 'argv', ['nix-components', 'fetch', '--directory', directory]), \
                 patch.object(c, 'load_bundle', return_value=root), \
                 patch.object(c, 'command', side_effect=subprocess.CalledProcessError(1, ['nix'])):
                self.assertEqual(c.main(), 1)

    def test_actual_aggregate_gate_fails_closed(self):
        source = (ROOT/'.github/workflows/test.yml').read_text().split('  ci-gate:', 1)[1]
        source = source.split('\n  baseline-measurement:', 1)[0]
        gate = textwrap.dedent(source.split('        run: |\n', 1)[1])
        for draft, compiled, detection, failed, expected in [
            ('false', 'true', 'success', None, 0),
            ('false', 'true', 'success', 'test-core', 1),
            ('false', 'true', 'success', 'build', 1),
            ('false', 'true', 'success', 'test-integrations', 1),
            ('true', 'true', 'success', 'skipped', 0),
            ('false', 'true', 'success', 'skipped', 1),
            ('false', 'false', 'success', 'skipped', 0),
            ('true', '', 'failure', 'skipped', 1),
            ('true', 'true', 'success', 'test-core', 1),
        ]:
            def status(match):
                return 'skipped' if failed == 'skipped' else ('failure' if match[1] == failed else 'success')
            script = re.sub(r'\$\{\{ needs\.([a-z-]+)\.result \}\}', status, gate)
            result = subprocess.run(['bash', '-c', script], capture_output=True,
                                    env={**os.environ, 'IS_DRAFT': draft, 'COMPILED': compiled,
                                         'CHANGES_RESULT': detection})
            with self.subTest(draft=draft, compiled=compiled, failed=failed):
                self.assertEqual(result.returncode, expected, result.stdout + result.stderr)


if __name__ == '__main__':
    unittest.main()
