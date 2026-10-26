import importlib.util
import argparse
import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock

sys.dont_write_bytecode = True
spec = importlib.util.spec_from_file_location('regression_evidence', Path(__file__).resolve().parents[1] / 'scripts/evidence.py')
evidence = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = evidence
spec.loader.exec_module(evidence)
from bounded_process import ProcessCancelled


class RuntimeProofTests(unittest.TestCase):
    def test_exact_assertion_failure_is_distinct_from_setup_and_other_failures(self):
        output = 'Failures:\n  1) component, rejects stale revision\n       EXPECTED_REVISION\n  To rerun use: --match revision\n\n1 example, 1 failure\n'
        result = evidence.classify_runtime(1, output, 'fail', 'rejects stale revision', 'EXPECTED_REVISION')
        self.assertEqual(result['failures'], 1)
        for code, text, test, expected in [(0, output, 'rejects stale revision', 'EXPECTED_REVISION'), (1, output, 'another test', 'EXPECTED_REVISION'), (1, output, 'rejects stale revision', 'OTHER_ASSERTION'), (1, 'Connection refused\n' + output, 'rejects stale revision', 'EXPECTED_REVISION'), (1, output + '\nEXPECTED_REVISION', 'rejects stale revision', 'EXPECTED_REVISION'), (1, 'Compilation failed', 'rejects stale revision', 'EXPECTED_REVISION')]:
            with self.subTest(code=code, text=text), self.assertRaises(ValueError):
                evidence.classify_runtime(code, text, 'fail', test, expected)

    def test_passing_check_rejects_empty_or_failed_selection(self):
        self.assertEqual(evidence.classify_runtime(0, '2 examples, 0 failures', 'pass', 'x')['selected_examples'], 2)
        for code, text in [(0, '0 examples, 0 failures'), (1, '2 examples, 0 failures'), (0, '1 example, 1 failure')]:
            with self.subTest(code=code, text=text), self.assertRaises(ValueError):
                evidence.classify_runtime(code, text, 'pass', 'x')

    def test_compiler_rejection_requires_positive_acceptance_and_expected_fixture_diagnostic(self):
        path = Path('/fixture/Negative.hs')
        output = '/fixture/Negative.hs:3:9: error: Could not match expected type Int with Bool'
        result = evidence.classify_compiler(0, 1, output, path, 'Int with Bool')
        self.assertEqual(result['negative'], 'rejected')
        for positive, negative, text, pattern in [(1, 1, output, 'Int'), (0, 0, output, 'Int'), (0, -9, output, 'Int'), (0, 1, 'missing package', 'Int'), (0, 1, output.replace('Negative.hs', 'Other.hs'), 'Int'), (0, 1, output, 'different diagnostic')]:
            with self.subTest(positive=positive, negative=negative, text=text), self.assertRaises(ValueError):
                evidence.classify_compiler(positive, negative, text, path, pattern)

    def test_compiler_classification_requires_both_processes_to_exit(self):
        path = Path('/fixture/Negative.hs')
        output = '/fixture/Negative.hs:3:9: error: EXPECTED_TYPE'
        for outcomes in [
            {'positive_process_outcome': 'timed-out'},
            {'negative_process_outcome': 'timed-out'},
            {'negative_process_outcome': 'output-limited'},
            {'negative_process_outcome': 'cancelled'},
        ]:
            with self.subTest(outcomes=outcomes), self.assertRaisesRegex(ValueError, 'process outcome'):
                evidence.classify_compiler(0, 1, output, path, 'EXPECTED_TYPE', **outcomes)

    def temporary_root(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        return Path(directory.name)

    def test_runtime_command_capture_rejects_source_drift_and_keeps_evidence(self):
        root = self.temporary_root()
        source = root / 'input.txt'
        source.write_text('before')
        program = root / 'test.py'
        program.write_text('from pathlib import Path\nPath("input.txt").write_text("after")\nprint("exact example\\n1 example, 0 failures")\n')
        args = argparse.Namespace(repo=root, mode='runtime', input=['input.txt', 'test.py'], command=[sys.executable, str(program)], expect='pass', test='exact example', failure=None, timeout=3)
        with self.assertRaisesRegex(ValueError, 'inputs changed'):
            evidence.capture(args)
        path = next((root / '.agents-state/regression-evidence').glob('*/receipt.json'))
        receipt = json.loads(path.read_text())
        self.assertEqual(receipt['status'], 'incomplete')
        self.assertEqual((path.parent / 'source/input.txt').read_text(), 'before')
        self.assertTrue((path.parent / 'runtime.log').exists())

    def test_compiler_capture_executes_both_fixtures_through_owned_toolchain(self):
        root = self.temporary_root()
        helper = root / '.agents/skills/neohaskell-best-practices/scripts/with-toolchain'
        helper.parent.mkdir(parents=True)
        helper.write_text('#!' + sys.executable + '\nimport sys\nsource=sys.argv[-1]\nprint(source + ":3:9: error: EXPECTED_TYPE" if source.endswith("Negative.hs") else "accepted")\nsys.exit(1 if source.endswith("Negative.hs") else 0)\n')
        helper.chmod(0o755)
        for name in ['Positive.hs', 'Negative.hs']:
            (root / name).write_text('compiler orchestration fixture')
        args = argparse.Namespace(repo=root, mode='compiler', input=[], command=[], positive=Path('Positive.hs'), negative=Path('Negative.hs'), diagnostic='EXPECTED_TYPE', timeout=3)
        receipt = evidence.capture(args)
        self.assertEqual([row['exit'] for row in receipt['commands']], [0, 1])
        self.assertEqual(receipt['observation']['kind'], 'compiler-accept-reject')
        self.assertEqual(receipt['status'], 'observed')

    def test_compiler_capture_keeps_timed_out_negative_child_incomplete(self):
        root = self.temporary_root()
        helper = root / '.agents/skills/neohaskell-best-practices/scripts/with-toolchain'
        helper.parent.mkdir(parents=True)
        helper.write_text(
            '#!' + sys.executable + '\n'
            'import os,signal,sys,time\n'
            'source=sys.argv[-1]\n'
            'if source.endswith("Negative.hs"):\n'
            ' open("negative.pid", "w").write(str(os.getpid()))\n'
            ' signal.signal(signal.SIGTERM, lambda *_: sys.exit(1))\n'
            ' print(source + ":3:9: error: EXPECTED_TYPE", flush=True)\n'
            ' time.sleep(10)\n'
            'else:\n'
            ' print("accepted", flush=True)\n'
        )
        helper.chmod(0o755)
        for name in ['Positive.hs', 'Negative.hs']:
            (root / name).write_text('compiler orchestration fixture')
        args = argparse.Namespace(repo=root, mode='compiler', input=[], command=[], positive=Path('Positive.hs'), negative=Path('Negative.hs'), diagnostic='EXPECTED_TYPE', timeout=0.5)
        with self.assertRaisesRegex(ValueError, 'process outcome `timed-out`'):
            evidence.capture(args)
        receipt_path = next((root / '.agents-state/regression-evidence').glob('*/receipt.json'))
        receipt = json.loads(receipt_path.read_text())
        self.assertEqual(receipt['status'], 'incomplete')
        self.assertEqual([row['process_outcome'] for row in receipt['commands']], ['exited', 'timed-out'])
        self.assertEqual(receipt['commands'][1]['exit'], 1)
        self.assertIn('EXPECTED_TYPE', (receipt_path.parent / 'negative.log').read_text())
        self.assertNotIn('observation', receipt)
        with self.assertRaises(ProcessLookupError):
            os.kill(int((root / 'negative.pid').read_text()), 0)

    def test_compiler_capture_keeps_output_limited_negative_child_incomplete(self):
        root = self.temporary_root()
        helper = root / '.agents/skills/neohaskell-best-practices/scripts/with-toolchain'
        helper.parent.mkdir(parents=True)
        helper.write_text(
            '#!' + sys.executable + '\n'
            'import os,sys\n'
            'source=sys.argv[-1]\n'
            'if source.endswith("Negative.hs"):\n'
            ' open("negative.pid", "w").write(str(os.getpid()))\n'
            ' print(source + ":3:9: error: EXPECTED_TYPE", flush=True)\n'
            ' print("x" * 1000, flush=True)\n'
            'else:\n'
            ' print("accepted", flush=True)\n'
        )
        helper.chmod(0o755)
        for name in ['Positive.hs', 'Negative.hs']:
            (root / name).write_text('compiler orchestration fixture')
        args = argparse.Namespace(repo=root, mode='compiler', input=[], command=[], positive=Path('Positive.hs'), negative=Path('Negative.hs'), diagnostic='EXPECTED_TYPE', timeout=3)
        with mock.patch.object(evidence, 'MAX_OUTPUT_BYTES', 512):
            with self.assertRaisesRegex(ValueError, 'process outcome `output-limited`'):
                evidence.capture(args)
        receipt_path = next((root / '.agents-state/regression-evidence').glob('*/receipt.json'))
        receipt = json.loads(receipt_path.read_text())
        self.assertEqual(receipt['status'], 'incomplete')
        self.assertEqual(receipt['commands'][1]['process_outcome'], 'output-limited')
        self.assertIn('EXPECTED_TYPE', (receipt_path.parent / 'negative.log').read_text())
        with self.assertRaises(ProcessLookupError):
            os.kill(int((root / 'negative.pid').read_text()), 0)

    def test_compiler_capture_keeps_cancellation_receipt_incomplete(self):
        root = self.temporary_root()
        helper = root / '.agents/skills/neohaskell-best-practices/scripts/with-toolchain'
        helper.parent.mkdir(parents=True)
        helper.write_text('#!' + sys.executable + '\n')
        helper.chmod(0o755)
        for name in ['Positive.hs', 'Negative.hs']:
            (root / name).write_text('compiler orchestration fixture')
        args = argparse.Namespace(repo=root, mode='compiler', input=[], command=[], positive=Path('Positive.hs'), negative=Path('Negative.hs'), diagnostic='EXPECTED_TYPE', timeout=3)
        with mock.patch.object(evidence, 'run', side_effect=ProcessCancelled(15)):
            with self.assertRaises(ProcessCancelled):
                evidence.capture(args)
        receipt_path = next((root / '.agents-state/regression-evidence').glob('*/receipt.json'))
        receipt = json.loads(receipt_path.read_text())
        self.assertEqual(receipt['status'], 'incomplete')
        self.assertEqual(receipt['commands'], [])

    def test_compiler_capture_keeps_setup_failure_receipt_incomplete(self):
        root = self.temporary_root()
        helper = root / '.agents/skills/neohaskell-best-practices/scripts/with-toolchain'
        helper.parent.mkdir(parents=True)
        helper.write_text('#!' + sys.executable + '\nprint("unreachable")\n')
        for name in ['Positive.hs', 'Negative.hs']:
            (root / name).write_text('compiler orchestration fixture')
        args = argparse.Namespace(repo=root, mode='compiler', input=[], command=[], positive=Path('Positive.hs'), negative=Path('Negative.hs'), diagnostic='EXPECTED_TYPE', timeout=3)
        with self.assertRaises(PermissionError):
            evidence.capture(args)
        receipt_path = next((root / '.agents-state/regression-evidence').glob('*/receipt.json'))
        receipt = json.loads(receipt_path.read_text())
        self.assertEqual(receipt['status'], 'incomplete')
        self.assertIn('Permission denied', receipt['error'])
        self.assertEqual(receipt['commands'], [])


if __name__ == '__main__':
    unittest.main()
