import importlib.util
from pathlib import Path
import sys
import tempfile
import unittest

sys.dont_write_bytecode = True
spec = importlib.util.spec_from_file_location('adr_index', Path(__file__).resolve().parents[1] / 'scripts/adr-index-check.py')
checker = importlib.util.module_from_spec(spec)
spec.loader.exec_module(checker)


class IndexTests(unittest.TestCase):
    def setUp(self):
        root = next(path for path in Path(__file__).resolve().parents if (path / '.git').exists())
        parent = root / '.agents-state/adr-index-tests'
        parent.mkdir(parents=True, exist_ok=True)
        directory = tempfile.TemporaryDirectory(dir=parent)
        self.addCleanup(directory.cleanup)
        self.root = Path(directory.name)
        (self.root / '0001-example.md').write_text('# Example\n\n## Status\n\nAccepted\n')
        (self.root / 'README.md').write_text('| ADR | Title | Status |\n|---|---|---|\n| [0001](0001-example.md) | Example | Accepted |\n')

    def test_consistent_index_passes(self):
        self.assertEqual(checker.check(self.root, self.root / 'README.md'), ([], 1))

    def test_duplicate_number_does_not_hide_a_file(self):
        (self.root / '0001-other.md').write_text('# Other\n\n## Status\n\nAccepted\n')
        errors, _ = checker.check(self.root, self.root / 'README.md')
        self.assertTrue(any('used by two files' in error for error in errors))

    def test_stale_status_and_missing_target_fail(self):
        (self.root / 'README.md').write_text('| [0001](0001-missing.md) | Example | Proposed |\n')
        errors, _ = checker.check(self.root, self.root / 'README.md')
        self.assertTrue(any('status mismatch' in error for error in errors))
        self.assertTrue(any('index link points' in error for error in errors))

    def test_historical_status_remains_distinct_from_accepted(self):
        (self.root / '0001-example.md').write_text('# Example\n\n## Status\n\nHistorical.\n')
        errors, _ = checker.check(self.root, self.root / 'README.md')
        self.assertTrue(any('status mismatch' in error for error in errors))
        (self.root / 'README.md').write_text('| [0001](0001-example.md) | Example | Historical |\n')
        self.assertEqual(checker.check(self.root, self.root / 'README.md'), ([], 1))


if __name__ == '__main__':
    unittest.main()
