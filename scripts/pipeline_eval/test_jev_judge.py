"""Offline regression tests for evidence handling and automatic verdicts."""
import copy
import unittest

import jev_judge as judge
import measure_jev


class JudgeTests(unittest.TestCase):
    def setUp(self):
        self.catalog = {
            "auth": {"kind": "capability", "evidence": "OAuth token refresh", "source": "map"},
            "queries": {"kind": "capability", "evidence": "Read model projection", "source": "map"},
        }
        self.payload = judge.build_request("Fix OAuth refresh", "auth", self.catalog)
        self.response = {
            "model": judge.MODEL,
            "answers": {
                "assessment": {"type": "choice", "choice": "supported", "confidence": .95,
                               "probabilities": {"supported": .98, "insufficient_evidence": .01, "unsupported": .01}},
                "route_supported": {"type": "noul", "noul": .95},
                "evidence_fit": {"type": "score", "score": 2.9, "confidence": .9,
                                 "probabilities": {"0": 0, "1": 0, "2": .1, "3": .9},
                                 "legend": {str(i): str(i) for i in range(4)}},
            },
        }

    def test_accept_requires_resolved_ownership(self):
        self.assertEqual(judge.verdict(self.response, self.payload, "auth")["verdict"], "accept")
        self.response["answers"]["route_supported"]["noul"] = .48
        self.assertEqual(judge.verdict(self.response, self.payload, "auth")["verdict"], "review")

    def test_low_confidence_negative_does_not_reject(self):
        a = self.response["answers"]
        a["assessment"].update(choice="unsupported", confidence=.11,
                             probabilities={"supported": .4, "unsupported": .41, "insufficient_evidence": .19})
        a["route_supported"]["noul"] = .46
        a["evidence_fit"]["score"] = 1.85
        self.assertEqual(judge.verdict(self.response, self.payload, "auth")["verdict"], "review")

    def test_clear_wrong_route_rejected(self):
        a = self.response["answers"]
        a["assessment"].update(choice="unsupported", probabilities={"supported": .01, "unsupported": .98, "insufficient_evidence": .01})
        a["route_supported"]["noul"] = .05
        a["evidence_fit"].update(score=.1, probabilities={"0": .9, "1": .1, "2": 0, "3": 0})
        self.assertEqual(judge.verdict(self.response, self.payload, "auth")["verdict"], "reject")

    def test_payload_contains_evidence_but_no_gold(self):
        self.assertEqual(self.payload["state"]["observed_route"], "auth")
        self.assertIn("queries", self.payload["state"]["maps"])
        self.assertEqual(set(self.payload["state"]), {"request", "observed_route", "maps"})

    def test_confident_judge_cannot_override_reference_failure(self):
        self.assertEqual(judge.verdict(self.response, self.payload, "auth")["verdict"], "accept")
        self.assertEqual(judge.reference_verdict("auth", "queries"), "fail")
        self.assertEqual(judge.reference_verdict("auth", "auth"), "pass")
        self.assertEqual(judge.reference_verdict("auth", None), "unlabeled")

    def test_candidate_identity_is_bound(self):
        with self.assertRaises(ValueError):
            judge.verdict(self.response, self.payload, "queries")

    def test_failed_attempts_and_missing_usage_are_not_success_or_zero(self):
        rows = [{"status": "error", "gold_correct": False, "elapsed_s": 45}]
        summary = measure_jev.summarize(rows)
        self.assertEqual(summary["attempts"], 1)
        self.assertEqual(summary["errors"], 1)
        self.assertEqual(summary["automatic_coverage"], 0)
        self.assertIsNone(summary["automatic_accuracy"])
        self.assertIsNone(summary["input_tokens_observed"])

    def test_false_acceptance_remains_visible_with_fast_response(self):
        rows = [{"status": "ok", "gold_correct": False, "verdict": "accept",
                 "raw_supported": True, "support_probability": .99,
                 "elapsed_s": .01, "result": {"usage": {"input_tokens": None}}}]
        summary = measure_jev.summarize(rows)
        self.assertEqual(summary["false_acceptances"], 1)
        self.assertEqual(summary["automatic_accuracy"], 0)
        self.assertEqual(summary["automatic_coverage"], 1)
        self.assertIsNone(summary["input_tokens_observed"])

    def test_unknown_identifier_never_calls_model(self):
        with self.assertRaises(ValueError):
            judge.build_request("Fix OAuth refresh", "invented", self.catalog)

    def test_invalid_responses_never_accept(self):
        mutations = [
            lambda r: r.update(model="jev-latest"),
            lambda r: r["answers"].pop("route_supported"),
            lambda r: r["answers"]["route_supported"].update(noul=float("nan")),
            lambda r: r["answers"]["route_supported"].update(noul=True),
            lambda r: r["answers"]["assessment"].update(choice="invented"),
            lambda r: r["answers"]["assessment"]["probabilities"].update(supported=.5),
            lambda r: r["answers"]["evidence_fit"].update(score=4),
        ]
        for mutation in mutations:
            with self.subTest(mutation=mutation):
                response = copy.deepcopy(self.response)
                mutation(response)
                with self.assertRaises(ValueError):
                    judge.verdict(response, self.payload, "auth")

    def test_rows_preserve_ownership_and_tests(self):
        source = "capabilities:\n  - id: auth\n    responsibility: OAuth\n    tests: [auth/**]\n  - id: queries\n    responsibility: Read models\n"
        catalog = judge.catalog_from_sources({"capabilities.yaml": source})
        self.assertEqual(set(catalog), {"auth", "queries"})
        self.assertIn("tests: [auth/**]", catalog["auth"]["evidence"])
        with self.assertRaises(ValueError):
            judge.catalog_from_sources({"capabilities.yaml": source.replace("id: queries", "name: queries")})


if __name__ == "__main__":
    unittest.main()
