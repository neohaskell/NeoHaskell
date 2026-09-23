"""Offline guards for the experiment protocol and failure accounting."""
import copy
import json
from pathlib import Path
import sys
import unittest
from unittest.mock import patch

import intake_experiment as experiment


class ExperimentTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.contract = json.loads((experiment.HERE / "intake_contract.json").read_text())
        cls.dataset, cls.cases = experiment.load_cases(experiment.HERE / "intake_cases.json", cls.contract, "evaluation")

    def row(self, provider="jev", success=True):
        expected = self.cases[0]["expected"]
        return {"case_id": self.cases[0]["id"], "repeat": 1, "provider": provider,
                "status": "ok" if success else "timeout", "elapsed_s": 1,
                "profile": copy.deepcopy(expected) if success else None,
                "expected": expected, "grade": experiment.grade(expected if success else None, expected)}

    def test_corpus_covers_every_proposed_type(self):
        self.assertEqual({c["expected"]["work"] for c in self.cases}, set(self.contract["work"]))
        self.assertEqual({c["expected"]["mode"] for c in self.cases}, set(self.contract["mode"]))
        self.assertEqual({a for c in self.cases for a in c["expected"]["actions"]}, set(self.contract["actions"]))

    def test_input_does_not_contain_gold_or_rationale(self):
        case = self.cases[0]
        pack = experiment.question_pack(case["request"], self.contract)
        self.assertEqual(set(pack), {"state", "questions"})
        self.assertEqual(set(pack["state"]), {"request", "policy", "task_definitions"})
        prompt = experiment.luna_prompt(pack)
        self.assertNotIn(case["rationale"], prompt)
        self.assertNotIn('"expected"', prompt)
        self.assertNotIn('"split"', prompt)

    def test_wrong_actions_are_not_masked_by_correct_work(self):
        gold = self.cases[0]["expected"]
        value = {**gold, "actions": ["shipping"]}
        result = experiment.grade(value, gold)
        self.assertTrue(result["work"])
        self.assertFalse(result["profile_exact"])

    def test_action_order_is_not_scored_and_score_is_separate(self):
        gold = {**self.cases[0]["expected"], "actions": ["opening_a_pr", "shipping"]}
        value = {**gold, "actions": list(reversed(gold["actions"])), "readiness_score": 2.2}
        self.assertTrue(experiment.grade(value, gold)["profile_exact"])
        self.assertAlmostEqual(experiment.grade(value, gold)["score_absolute_error"], .8)

    def test_invalid_profiles_fail_closed(self):
        good = self.cases[0]["expected"]
        for delta in ({"work": "invented"}, {"actions": ["shipping", "shipping"]},
                      {"needs_clarification": 1}, {"readiness_score": float("nan")},
                      {"readiness_score": True}, {"surprise": "x"}):
            with self.subTest(delta=delta), self.assertRaises(ValueError):
                experiment.validate_profile({**good, **delta}, self.contract)

    def test_errors_stay_in_accuracy_and_usage_denominators(self):
        result = experiment.summary([self.row(), self.row(success=False)], self.contract["actions"])
        self.assertEqual(result["profile_exact_accuracy"], .5)
        self.assertEqual(result["errors"], 1)
        self.assertEqual(result["score_observations"], 1)
        self.assertIsNone(result["input_tokens"]["total_observed"])
        self.assertEqual(result["input_tokens"]["observations"], 0)

    def test_schedule_reproducible_complete_and_interleaved(self):
        jobs = experiment.schedule(self.cases, 2, 1729)
        self.assertEqual(jobs, experiment.schedule(self.cases, 2, 1729))
        self.assertEqual(len(jobs), 4 * len(self.cases))
        for first, second in zip(jobs[::2], jobs[1::2]):
            self.assertEqual(first[:2], second[:2])
            self.assertNotEqual(first[2], second[2])
        self.assertEqual({j[2] for j in jobs[::2]}, set(experiment.PROVIDERS))

    def test_incomplete_or_duplicate_pairs_cannot_be_reported(self):
        meta = {"cases": self.cases[:1], "repeat": 1, "contract": self.contract}
        with self.assertRaises(ValueError):
            experiment.build_report(meta, [self.row()])
        with self.assertRaises(ValueError):
            experiment.build_report(meta, [self.row(), self.row(), self.row("luna")])
        report = experiment.build_report(meta, [self.row(), self.row("luna", False)])
        self.assertEqual(report["paired"], {"jev_only": 1})

    def test_tool_calls_or_incomplete_turns_invalidate_codex_response(self):
        for events in ([{"type": "turn.started"}],
                       [{"type": "item.completed", "item": {"type": "command_execution"}}, {"type": "turn.completed"}],
                       [{"type": "turn.failed"}]):
            with self.assertRaises(ValueError):
                experiment.parse_events("\n".join(json.dumps(e) for e in events))

    def test_subprocess_timeout_is_preserved(self):
        result = experiment.run_process([sys.executable, "-c", "import time; print('started', flush=True); time.sleep(10)"], "", .1)
        self.assertEqual(result["status"], "timeout")
        self.assertIn("started", result["stdout"])
        self.assertLess(result["elapsed_s"], 5)

    def test_fast_wrong_candidate_fails_regression_gate(self):
        metadata = {"contract": self.contract, "cases": self.cases[:1], "repeat": 1,
                    "split": "evaluation", "seed": 1729, "requested_models": {},
                    "luna_effort": "max", "codex_version": "fixture", "disabled_skill_paths": []}
        baseline = experiment.build_report(metadata, [self.row(), self.row("luna")])
        wrong = self.row()
        wrong["profile"]["actions"] = ["shipping"]
        wrong["grade"] = experiment.grade(wrong["profile"], wrong["expected"])
        wrong["elapsed_s"] = .01
        candidate = experiment.build_report(metadata, [wrong, self.row("luna")])
        with patch.object(experiment, "load_run", side_effect=[(metadata, baseline), (metadata, candidate)]):
            result = experiment.compare_runs(Path("before"), Path("after"), 1.25)
        self.assertFalse(result["passed"])
        self.assertFalse(result["checks"]["jev"]["unrequested_actions_no_increase"])
        self.assertTrue(result["checks"]["jev"]["median_latency_within_budget"])

    def test_changed_labels_are_not_a_comparable_run(self):
        metadata = {"contract": self.contract, "cases": self.cases[:1]}
        changed = {**metadata, "cases": self.cases[1:2]}
        with patch.object(experiment, "load_run", side_effect=[(metadata, {}), (changed, {})]):
            with self.assertRaisesRegex(ValueError, "Incomparable runs: cases"):
                experiment.compare_runs(Path("before"), Path("after"), 1.25)


if __name__ == "__main__":
    unittest.main()
