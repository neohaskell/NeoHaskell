import importlib.util
import json
import os
from pathlib import Path
import shutil
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.dont_write_bytecode = True
SKILL = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('event_store_smoke', SKILL / 'scripts/smoke.py')
smoke = importlib.util.module_from_spec(spec)
spec.loader.exec_module(smoke)


class RecipeTests(unittest.TestCase):
    def setUp(self):
        self.repository = SKILL
        while not (self.repository / '.git').exists():
            if self.repository.parent == self.repository:
                raise RuntimeError('Tests need the working repository')
            self.repository = self.repository.parent
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.root = Path(directory.name)

    def test_recipe_locations_match_maintained_tests(self):
        smoke.check(self.repository, smoke.catalog())

    def test_known_good_and_controlled_bad_reach_the_same_example(self):
        data = smoke.catalog()
        recipe = data['recipes'][0]
        for source in [data['helper'], *(row['source'] for row in data['recipes'])]:
            target = self.root / source
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(self.repository / source, target)
        evidence_source = SKILL.parent / 'regression-evidence/scripts/evidence.py'
        target = self.root / '.agents/skills/regression-evidence/scripts/evidence.py'
        target.parent.mkdir(parents=True)
        shutil.copy2(evidence_source, target)
        bounded_process_source = SKILL.parent / 'neohaskell-best-practices/scripts/bounded_process.py'
        bounded_process_target = self.root / '.agents/skills/neohaskell-best-practices/scripts/bounded_process.py'
        bounded_process_target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(bounded_process_source, bounded_process_target)
        helper = self.root / '.agents/skills/tdd/scripts/test-match'
        helper.parent.mkdir(parents=True)
        helper.write_text('#!' + sys.executable + '\nimport argparse,os,sys\nfrom pathlib import Path\np=argparse.ArgumentParser(); p.add_argument("--transcript"); p.add_argument("pattern"); p.add_argument("suite"); a=p.parse_args()\nassert a.pattern == ' + repr(recipe['test']) + '\nassert a.suite == ' + repr(recipe['suite']) + '\nbad=os.environ.get("NEOHASKELL_REGRESSION_BAD")\nif bad:\n assert bad == ' + repr(recipe['name']) + '\n assert os.environ["NEOHASKELL_REGRESSION_BACKEND"] == ' + repr(recipe['backend']) + '\n text="Failures:\\n  1) component, " + a.pattern + "\\n       ' + recipe['assertion'] + '\\n  To rerun use: --match fixture\\n\\n2 examples, 1 failure\\n"\n outcome="assertion-failure"\n code=1\nelse:\n text="2 examples, 0 failures\\n"\n outcome="passed"\n code=0\nPath(a.transcript).write_text("exit_code: " + str(code) + "\\nverdict: " + ("PASS" if code == 0 else "FAIL") + "\\noutcome: " + outcome + "\\nsummary: fixture\\n--- output ---\\n" + text)\nsys.stdout.write(text)\nsys.exit(code)\n')
        helper.chmod(0o755)
        with patch.dict(os.environ, {'NEOHASKELL_REGRESSION_BAD': 'must-be-cleared', 'NEOHASKELL_REGRESSION_BACKEND': 'wrong'}):
            result = smoke.smoke(self.root, [recipe['name']], 3)
        self.assertEqual(result['status'], 'observed')
        self.assertEqual([row['exit'] for row in result['recipes'][0]['runs']], [0, 1])
        runs = result['recipes'][0]['runs']
        self.assertEqual([row['runner_outcome'] for row in runs], ['passed', 'assertion-failure'])
        receipt = next((self.root / '.agents-state/event-store-regression').glob('*/receipt.json'))
        evidence = json.loads(receipt.read_text())
        self.assertEqual(evidence['recipes'][0]['runs'][1]['observed']['failures'], 1)
        self.assertEqual(evidence['recipes'][0]['runs'][1]['runner_receipt_sha256'], result['recipes'][0]['runs'][1]['runner_receipt_sha256'])
        self.assertTrue(result['recipes'][0]['runs'][1]['runner_receipt'])

    def test_missing_or_duplicate_recipe_cannot_pass_metadata_check(self):
        data = smoke.catalog()
        for invalid in [{**data, 'recipes': []}, {**data, 'recipes': data['recipes'] + [data['recipes'][0]]}]:
            with self.assertRaises(ValueError):
                smoke.check(self.repository, invalid)


if __name__ == '__main__':
    unittest.main()
