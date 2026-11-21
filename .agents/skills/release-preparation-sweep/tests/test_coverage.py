import hashlib
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'scripts'))

from coverage import compare_notes, notes_by_path


def note(path, text):
    return {'path': path, 'text': text, 'hash': hashlib.sha256(text.encode()).hexdigest()}


class CoverageTests(unittest.TestCase):
    def test_revised_fragment_uses_current_source_text(self):
        old = notes_by_path({'notes': [note('.changes/api.md', 'Use old API.\n')]})
        current = notes_by_path({'notes': [note('.changes/api.md', 'Use new API.\n')]})
        missing, revised = compare_notes(old, current, {'.changes/api.md': 'Use new API.\n'})
        self.assertEqual(missing, [])
        self.assertEqual([item['path'] for item in revised], ['.changes/api.md'])

    def test_missing_fragment_is_reported(self):
        old = notes_by_path({'notes': [note('.changes/api.md', 'Migration.\n')]})
        missing, revised = compare_notes(old, {}, {})
        self.assertEqual(missing, ['.changes/api.md'])
        self.assertEqual(revised, [])

    def test_revision_without_current_source_is_rejected(self):
        old = notes_by_path({'notes': [note('.changes/api.md', 'Old.\n')]})
        current = notes_by_path({'notes': [note('.changes/api.md', 'New.\n')]})
        with self.assertRaisesRegex(ValueError, 'current source fragment'):
            compare_notes(old, current, {'.changes/api.md': 'Different.\n'})


if __name__ == '__main__':
    unittest.main()
