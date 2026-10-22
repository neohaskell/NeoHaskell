import sys
sys.dont_write_bytecode = True
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

SCRIPT = Path(__file__).with_name('worktree-audit.sh').resolve()


class NeutralToolingTests(unittest.TestCase):
    def setUp(self):
        parent = Path(os.environ.get('WORKTREE_AUDIT_TEST_ROOT', '.agents-state/worktree-audit-tests')).resolve()
        parent.mkdir(parents=True, exist_ok=True)
        self.temporary = tempfile.TemporaryDirectory(dir=parent)
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.repo = self.root / 'repo'
        self.child = self.root / 'child'
        self.selected = self.root / 'project history'
        self.bin = self.root / 'bin'
        for p in (self.repo, self.child, self.selected, self.bin, self.root / 'tmp'):
            p.mkdir()
        self.events = self.root / 'events.jsonl'
        self.environment = dict(os.environ, PATH=str(self.bin) + os.pathsep + os.environ['PATH'],
                                WORKTREE_AUDIT_FIXTURE=str(self.root),
                                TMPDIR=str(self.root / 'tmp'), PYTHONDONTWRITEBYTECODE='1')
        common = '''import json, os, sys
from pathlib import Path
root = Path(os.environ['WORKTREE_AUDIT_FIXTURE'])
name = Path(sys.argv[0]).name
args = sys.argv[1:]
with (root / 'events.jsonl').open('a') as f:
    f.write(json.dumps([name, args]) + '\\n')
'''
        commands = {
            'git': '''a = args[2:] if args[:1] == ['-C'] else args
head = 'a' * 40
if a == ['worktree', 'list', '--porcelain']:
    print('worktree ' + str(root / 'repo') + '\\nHEAD ' + head + '\\nbranch refs/heads/trunk\\n')
    print('worktree ' + str(root / 'child') + '\\nHEAD ' + head + '\\nbranch refs/heads/topic\\n')
elif a[:2] == ['rev-parse', '--git-dir']: print('.git')
elif a[:2] == ['rev-parse', '--verify']: print(head)
elif a[:1] == ['fetch']: pass
elif a == ['rev-parse', 'HEAD'] or a == ['rev-parse', 'origin/topic']: print(head)
elif a[:1] == ['log']: print('0')
elif a[:1] == ['merge-base']: pass
elif a == ['status', '--porcelain']: pass
elif a[:1] == ['symbolic-ref']: print('topic')
elif a[:1] == ['show-ref']: pass
else:
    print('unexpected stub git command ' + repr(a), file=sys.stderr)
    sys.exit(99)
''',
            'gh': "print('[]')\n",
            'jq': "sys.stdin.read()\nif '-r' not in args: print('[]')\n",
            'rg': 'sys.exit(1)\n',
            'stat': 'pass\n',
            'origin': 'sys.exit(99)\n',
        }
        for name, body in commands.items():
            p = self.bin / name
            p.write_text('#!' + sys.executable + '\n' + common + body)
            p.chmod(0o755)

    def run_audit(self, args):
        return subprocess.run(['bash', str(SCRIPT), *args], cwd=self.repo,
                              env=self.environment, text=True, capture_output=True)

    def calls(self):
        return [json.loads(line) for line in self.events.read_text().splitlines()] if self.events.exists() else []

    def test_help_explains_explicit_project_transcript_selection(self):
        result = self.run_audit(['--help'])
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn('--transcripts-dir', result.stdout)
        self.assertEqual(self.calls(), [])

    def test_missing_transcript_argument_fails_before_external_tools(self):
        result = self.run_audit(['--transcripts-dir'])
        self.assertEqual(result.returncode, 2)
        self.assertIn('needs a directory', result.stderr)
        self.assertEqual(self.calls(), [])

    def test_nonexistent_transcript_directory_fails_before_external_tools(self):
        result = self.run_audit(['--transcripts-dir', str(self.root / 'missing')])
        self.assertEqual(result.returncode, 2)
        self.assertIn('directory not found', result.stderr)
        self.assertEqual(self.calls(), [])

    def test_default_uses_gh_without_scanning_implicit_history(self):
        result = self.run_audit(['-b', 'origin/trunk', str(self.repo)])
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn(str(self.child), result.stdout)
        self.assertIn('\tsafe\t', result.stdout)
        names = [name for name, _ in self.calls()]
        self.assertIn('gh', names)
        self.assertNotIn('origin', names)
        self.assertNotIn('rg', names)

    def test_explicit_relative_transcript_directory_is_resolved_before_repo_change(self):
        result = self.run_audit(['--transcripts-dir', '../project history', '-b', 'origin/trunk', str(self.repo)])
        self.assertEqual(result.returncode, 0, result.stderr)
        scans = [args for name, args in self.calls() if name == 'rg']
        self.assertEqual(len(scans), 1)
        self.assertEqual(scans[0][-1], str(self.selected.resolve()))


if __name__ == '__main__':
    unittest.main(verbosity=2)
