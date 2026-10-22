import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest

sys.dont_write_bytecode = True
SCRIPT = Path(__file__).resolve().parents[1] / 'scripts/compare.py'
spec = importlib.util.spec_from_file_location('framework_compare', SCRIPT)
runner = importlib.util.module_from_spec(spec)
spec.loader.exec_module(runner)


class EvidenceTests(unittest.TestCase):
    def setUp(self):
        root = Path(__file__).resolve()
        while not (root / '.git').exists():
            if root.parent == root:
                raise RuntimeError('Run fixture tests from a repository checkout')
            root = root.parent
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.root = Path(self.directory.name)
        self.work = self.root / 'work'
        self.work.mkdir()
        (self.work / 'input.txt').write_text('fixed workload')

    def protocol(self, script, **overrides):
        (self.work / 'driver.py').write_text(script)
        side = {'cwd': 'work', 'argv': [sys.executable, 'driver.py'], 'inputs': ['driver.py', 'input.txt']}
        protocol = {'claim': 'compare a controlled fixture', 'kind': 'runtime', 'completion': 'fixture result independently checked', 'settings': {'workload': 'fixed fixture'}, 'samples': 5, 'warmups': 1, 'timeout_seconds': 3, 'record': 'driver-json', 'metric': 'fixture', 'unit': 'items', 'baseline': dict(side), 'treatment': dict(side)}
        protocol.update(overrides)
        path = self.root / 'protocol.json'
        path.write_text(json.dumps(protocol))
        return path

    def test_records_alternating_raw_samples_and_separate_warmups(self):
        path = self.protocol('import json\nprint(json.dumps({"metric":"fixture","unit":"items","value":7,"completed":2,"errors":0,"rejected":1,"retries":3,"outcome_verified":True}))\n')
        result = runner.measure(path, self.root)
        self.assertEqual([sample['side'] for sample in result['samples']], ['baseline', 'treatment'] * 6)
        self.assertEqual(sum(sample['warmup'] for sample in result['samples']), 2)
        self.assertEqual(result['summary']['baseline'], {'median': 7, 'minimum': 7, 'maximum': 7, 'count': 5})
        self.assertEqual(result['samples'][0]['observation']['rejected'], 1)

    def test_rejects_errors_zero_work_nonfinite_values_and_wrong_units(self):
        protocol = {'record': 'driver-json', 'metric': 'x', 'unit': 'ms'}
        valid = {'metric': 'x', 'unit': 'ms', 'value': 2, 'completed': 1, 'errors': 0, 'rejected': 0, 'retries': 0, 'outcome_verified': True}
        for update in [{'errors': 1}, {'completed': 0}, {'completed': True}, {'completed': 1.5}, {'value': float('nan')}, {'value': float('inf')}, {'unit': 's'}, {'outcome_verified': False}]:
            with self.subTest(update=update), self.assertRaises(ValueError):
                runner.observation(protocol, 0, 1, json.dumps({**valid, **update}))

    def test_suite_duration_requires_actual_passing_examples(self):
        protocol = {'record': 'hspec', 'metric': 'elapsed', 'unit': 's'}
        for code, text in [(0, '0 examples, 0 failures'), (1, '2 examples, 0 failures'), (0, '2 examples, 1 failure'), (0, '1 example, 0 failures\n1 example, 0 failures')]:
            with self.subTest(code=code, text=text), self.assertRaises(ValueError):
                runner.observation(protocol, code, 1, text)
        self.assertEqual(runner.observation(protocol, 0, 1.25, '2 examples, 0 failures')['value'], 1.25)

    def test_compiler_distinguishes_work_from_unchanged_build(self):
        protocol = {'record': 'compiler', 'metric': 'elapsed', 'unit': 's', 'scenario': 'clean', 'work_pattern': r'Compiling Consumer'}
        with self.assertRaises(ValueError):
            runner.observation(protocol, 0, 1, 'Up to date')
        self.assertEqual(runner.observation(protocol, 0, 1, '[1 of 1] Compiling Consumer')['completed'], 1)
        unchanged = {**protocol, 'scenario': 'unchanged', 'work_pattern': None}
        self.assertEqual(runner.observation(unchanged, 0, 1, 'Up to date')['completed'], 0)

    def test_input_drift_keeps_incomplete_evidence(self):
        path = self.protocol('from pathlib import Path\nPath("input.txt").write_text("changed")\nprint(\'{"metric":"fixture","unit":"items","value":1,"completed":1,"errors":0,"rejected":0,"retries":0,"outcome_verified":true}\')\n')
        output = self.root / '.agents-state/failed-run'
        with self.assertRaisesRegex(ValueError, 'inputs changed during execution'):
            runner.measure(path, self.root, output)
        result = json.loads((output / 'results.json').read_text())
        self.assertEqual(result['status'], 'incomplete')
        self.assertTrue((output / '000-baseline.log').exists())

    def test_preparation_is_outside_measurement_and_can_define_an_incremental_edit(self):
        path = self.protocol('print("1 example, 0 failures")\n', kind='suite', record='hspec', metric='elapsed', unit='s', warmups=0)
        data = json.loads(path.read_text())
        for name in ('baseline', 'treatment'):
            data[name]['prepare'] = [sys.executable, '-c', 'from pathlib import Path; Path("input.txt").write_text("defined edit")']
        path.write_text(json.dumps(data))
        result = runner.measure(path, self.root)
        self.assertEqual(result['status'], 'recorded')
        self.assertEqual(len(result['samples']), 10)

    def test_timeout_retains_failure_log(self):
        path = self.protocol('import time\ntime.sleep(5)\n', timeout_seconds=0.05)
        output = self.root / '.agents-state/timed-out'
        with self.assertRaisesRegex(ValueError, 'timed out'):
            runner.measure(path, self.root, output)
        self.assertIn('timed out', json.loads((output / 'results.json').read_text())['error'])


if __name__ == '__main__':
    unittest.main()
