#!/usr/bin/env python3
"""Exercise experiment orchestration with disposable commands, no Nix/Docker."""
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]


class ColocatedTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.base = Path(self.temp.name)
        self.repo = self.base / 'repo'
        harness = self.repo / 'docs/build-cache'
        harness.mkdir(parents=True)
        for name in ('colocated.sh', 'measure.py'):
            shutil.copy2(ROOT / 'docs/build-cache' / name, harness / name)
        (harness / 'cache-state.py').write_text(
            'import pathlib,sys\npathlib.Path(sys.argv[2]).write_text(\'{"comparison_unusable": false}\')\n')
        for group in ('commands', 'queries', 'scenarios', 'integrations'):
            folder = self.repo / 'testbed/tests' / group
            folder.mkdir(parents=True)
            (folder / 'test.hurl').write_text('GET http://localhost/test\nHTTP 200\n')
        self.bin = self.base / 'bin'
        self.bin.mkdir()
        self.executable(self.bin / 'nix', '#!/bin/sh\necho fake-nix-store\n')
        self.executable(self.bin / 'docker', '''#!/bin/sh
if [ "$1 $2" = 'container inspect' ]; then exit 1; fi
printf '%s\\n' "$*" >> "$DOCKER_TRACE"
echo fake-docker
''')
        self.executable(self.repo / 'dev', '''#!/usr/bin/env python3
import json,os,pathlib,sys
args=sys.argv[1:]; action=args[1]
directory=pathlib.Path(args[args.index('--directory')+1]); directory.mkdir(exist_ok=True)
if '--suite' in args:
 suite=args[args.index('--suite')+1]
 if os.environ.get('FAKE_FAILURE') == suite: sys.exit(42)
 pathlib.Path(args[args.index('--report')+1]).write_text('3 examples, 0 failures\\n')
 entry={'action':action,'exit_code':0,'suite':suite,'examples':3,'pending':0,'executed':3}
else: entry={'action':action,'exit_code':0}
if action=='build':
 root='same-root' if not (os.environ.get('FAKE_ROOT_CHANGE') and 'warm' in str(directory)) else 'wrong-root'
 (directory/'root.txt').write_text(root+'\\n')
with (directory/'timings.jsonl').open('a') as output: output.write(json.dumps(entry)+'\\n')
''')
        (self.repo / 'flake.lock').write_text('{}\n')
        for args in (['init', '-q'], ['add', '.'], ['-c', 'user.name=Test', '-c', 'user.email=test@example.invalid', 'commit', '-qm', 'fixture']):
            subprocess.run(['git', *args], cwd=self.repo, check=True, capture_output=True)
        sha = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=self.repo, text=True).strip()
        self.evidence = self.base / 'evidence'
        self.env = {**os.environ, 'PATH': str(self.bin)+os.pathsep+os.environ['PATH'],
                    'GITHUB_ACTIONS': 'true', 'RUNNER_OS': 'Linux', 'GITHUB_SHA': sha,
                    'GITHUB_RUN_ID': '123', 'GITHUB_RUN_ATTEMPT': '1',
                    'DOCKER_TRACE': str(self.base / 'docker.log')}

    def executable(self, path, text):
        path.write_text(text)
        path.chmod(0o755)

    def run_script(self):
        return subprocess.run(['bash', 'docs/build-cache/colocated.sh', str(self.evidence)],
                              cwd=self.repo, env=self.env, text=True, capture_output=True)

    def test_both_passes_execute_and_reset_fixture(self):
        result = self.run_script()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        summary = json.loads((self.evidence / 'summary.json').read_text())
        self.assertEqual(len(summary['passes']['first']['suites']), 5)
        self.assertEqual(summary['passes']['first'], summary['passes']['warm'])
        trace = (self.base / 'docker.log').read_text()
        for label in ('first', 'warm'):
            self.assertIn('run -d --name nh-colocated-123-1-'+label, trace)
            self.assertIn('rm -f -v nh-colocated-123-1-'+label, trace)
            for action in ('hurl', 'cold-start'):
                self.assertTrue((self.evidence / label / action / 'observation.json').is_file())

    def test_failing_suite_is_red_and_cleans_owned_fixture(self):
        self.env['FAKE_FAILURE'] = 'nhcore-test-core'
        result = self.run_script()
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse((self.evidence / 'summary.json').exists())
        self.assertFalse((self.evidence / 'warm').exists())
        entry = json.loads((self.evidence / 'first/test-nhcore-test-core/observation.json').read_text())
        self.assertEqual(entry['exit_code'], 42)
        self.assertIn('rm -f -v nh-colocated-123-1-first', (self.base / 'docker.log').read_text())

    def test_changed_warm_output_is_red(self):
        self.env['FAKE_ROOT_CHANGE'] = '1'
        result = self.run_script()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('warm pass changed output paths or suite counts', result.stderr)
        self.assertFalse((self.evidence / 'summary.json').exists())

    def test_existing_evidence_is_not_overwritten(self):
        self.evidence.mkdir()
        result = self.run_script()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('Evidence directory must be absolute and new', result.stderr)
        self.assertFalse((self.base / 'docker.log').exists())


if __name__ == '__main__':
    unittest.main()
